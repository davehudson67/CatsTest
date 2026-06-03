# =============================================================================
# precompute_runs.R
#
# Generates data/precomputed_runs.rds containing the raw monthly population
# series for every reachable parameter combination (33 scenarios), each run at
# the maximum 10-year timeframe. The app loads this file at startup and slices
# the chosen timeframe out of it for instant results.
#
# Run from the project root with:
#   Rscript precompute_runs.R
#
# Re-run this whenever the model parameters or code in model_core.R change.
# =============================================================================

# Locate model_core.R relative to this script so it works regardless of cwd.
this_file <- tryCatch(
  normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
  error = function(e) NA_character_
)
script_dir <- if (!is.na(this_file)) dirname(this_file) else getwd()

source(file.path(script_dir, "model_core.R"))

MAX_YEARS <- 10
out_dir <- file.path(script_dir, "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "precomputed_runs.rds")

combos <- all_run_combinations()
cat(sprintf("Precomputing %d scenarios at %d years...\n", length(combos), MAX_YEARS))

runs <- list()
for (combo in combos) {
  key <- run_key(combo$useNational, combo$neut_A, combo$neut_K)
  runs[[key]] <- run_cat_model(
    neut_K      = combo$neut_K,
    neut_A      = combo$neut_A,
    useNational = combo$useNational,
    nyears      = MAX_YEARS
  )
  cat(sprintf("  done: %s  (final total = %.1f)\n", key, tail(runs[[key]]$Total, 1)))
}

# Store with a small metadata header so the app can validate compatibility.
cache <- list(
  meta = list(
    max_years = MAX_YEARS,
    n_runs    = length(runs),
    created   = as.character(Sys.time()),
    model_version = "1.0"
  ),
  runs = runs
)

saveRDS(cache, out_path)
cat(sprintf("Saved %d runs to %s\n", length(runs), out_path))
