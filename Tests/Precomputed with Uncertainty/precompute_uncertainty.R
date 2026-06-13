# =============================================================================
# precompute_uncertainty.R
#
# Generates data/precomputed_uncertainty.rds: Monte Carlo uncertainty bands for
# every reachable parameter combination (33 scenarios), each run at the maximum
# 10-year timeframe.
#
# For each scenario we draw N_SIMS sets of survival rates from their Beta priors
# (see model_core.R::.cat_survival_priors, taken from the RC_* scripts in
# CodeFromJenni2_uncertainty), run the model for each draw, and store:
#   - per-month quantiles (2.5/97.5%) for Total/Owned/Feral/Stray/Shelter, so the
#     app can draw a 95% CI ribbon and slice it to the chosen timeframe.
#   - per-month median, for reference.
#
# Because the timeframe only slices the monthly series (the prefix property:
# parameters are constant over time within a run), we store the full 10-year
# month-by-month quantiles and let the app compute the growth-rate (PG) CI by
# re-deriving it from the requested timeframe slice... EXCEPT growth rates are
# ratios of sums and cannot be recovered from per-month quantiles. We therefore
# also store, per scenario AND per timeframe (2..10 years), the quantiles of the
# PG_ growth metrics computed across the simulations.
#
# Run from the project root with:
#   Rscript precompute_uncertainty.R
#
# Re-run this whenever the model parameters or code in model_core.R change.
# =============================================================================

this_file <- tryCatch(
  normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_
)
script_dir <- if (!is.na(this_file)) dirname(this_file) else getwd()

source(file.path(script_dir, "model_core.R"))

set.seed(10)

MAX_YEARS <- 10
N_SIMS    <- 20000                    # Monte Carlo draws per scenario
PROBS     <- c(0.025, 0.5, 0.975)     # lower / median / upper for the 95% CI
YEAR_GRID <- 2:MAX_YEARS              # timeframes the slider supports
CATS      <- c("Total", "Owned", "Feral", "Stray", "Shelter")

out_dir <- file.path(script_dir, "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "precomputed_uncertainty.rds")

combos <- all_run_combinations()
cat(sprintf("Precomputing uncertainty for %d scenarios x %d sims...\n",
            length(combos), N_SIMS))

# Per-timeframe growth-rate quantiles helper. Given a category's full 10-year
# simulation matrix (N_SIMS x months), compute the quantiles of the PG metric
# at every timeframe in YEAR_GRID.
pg_quantiles_by_year <- function(mat) {
  res <- list()
  base <- rowSums(mat[, 1:12, drop = FALSE])        # denominator (year 1)
  for (ny in YEAR_GRID) {
    L  <- 12 * ny
    sw <- max(1, L - 11)
    pg <- rowSums(mat[, sw:L, drop = FALSE]) / base  # per-sim growth ratio
    res[[as.character(ny)]] <- quantile(pg, probs = PROBS, names = FALSE)
  }
  res
}

runs <- list()
t0 <- Sys.time()
for (ci in seq_along(combos)) {
  combo <- combos[[ci]]
  key <- run_key(combo$useNational, combo$neut_A, combo$neut_K)

  months <- 12 * MAX_YEARS
  # storage: one (N_SIMS x months) matrix per category
  sims <- lapply(CATS, function(x) matrix(NA_real_, nrow = N_SIMS, ncol = months))
  names(sims) <- CATS

  for (j in 1:N_SIMS) {
    r <- run_cat_model(
      neut_K      = combo$neut_K,
      neut_A      = combo$neut_A,
      useNational = combo$useNational,
      nyears      = MAX_YEARS,
      surv        = sample_survival()
    )
    for (cat in CATS) sims[[cat]][j, ] <- r[[cat]]
  }

  # per-month quantiles (months x 3) for the CI ribbon
  band <- lapply(CATS, function(cat) {
    t(apply(sims[[cat]], 2, quantile, probs = PROBS, names = FALSE))
  })
  names(band) <- CATS

  # per-timeframe growth-rate quantiles
  pg <- lapply(CATS, function(cat) pg_quantiles_by_year(sims[[cat]]))
  names(pg) <- CATS

  runs[[key]] <- list(band = band, pg = pg)

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  [%2d/%2d] %s  (%.0fs elapsed)\n", ci, length(combos), key, elapsed))
}

cache <- list(
  meta = list(
    max_years = MAX_YEARS,
    n_sims    = N_SIMS,
    probs     = PROBS,
    year_grid = YEAR_GRID,
    cats      = CATS,
    n_runs    = length(runs),
    created   = as.character(Sys.time()),
    model_version = "1.0"
  ),
  runs = runs
)

saveRDS(cache, out_path)
cat(sprintf("Saved uncertainty for %d scenarios to %s\n", length(runs), out_path))
