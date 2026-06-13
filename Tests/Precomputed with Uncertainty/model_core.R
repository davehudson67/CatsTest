# =============================================================================
# model_core.R
#
# Self-contained, NON-reactive implementation of the domestic cat population
# projection model. Depends only on base R (no shiny).
#
# Used in two places:
#   1. precompute_runs.R  -> generate data/precomputed_runs.rds (all scenarios)
#   2. app.R              -> live fallback when a scenario is not in the cache
#
# The model output is fully determined by three inputs:
#   - useNational : logical (use national-average kitten neutering rate of 41%)
#   - neut_A      : owned adult neutering prevalence, one of "90%" / "95%" / "98%"
#   - neut_K      : owned kitten neutering rate (%), 5..50 by 5 (forced to 41 if
#                   useNational is TRUE)
#
# Timeframe (nyears) does NOT change the per-month trajectory: the seasonal
# reproduction vector is a periodic 12-month pattern and all other parameters
# are constant over time, so a 10-year run's first N years equal an N-year run.
# Therefore the model is always run at nyears = 10 and sliced on load.
#
# NOTE ON THE MATRIX BUILD: the original app built the projection matrix in two
# places (Setup for MATS[[1]] and the Go loop for MATS[[2..]]). Those are the
# same matrix formula, and one can show MATS[[k]] = f(state(VM[k]), Season[k])
# for every k. This file therefore builds a single matrix per step from VM[i]
# and Season[i], producing results identical to the original app.
# =============================================================================

# Initial stage-structured population vector (28 stages). Hardcoded in the app
# (rv$Catvec2); reproduced here so the model is standalone.
.cat_initial_vector <- c(
  2603.18, 876.23,    8.85, 1597.83,   16.14,  103.09,    1.04,
   109.57, 308.21,   12.84,  723.64, 1085.46,   29.81,   69.56,
    77.64,   0.00,    7.85,    0.00,  129.92,    0.00,   16.85,
  1941.52, 990.78, 1921.51, 4756.73, 63196.54, 392.23, 19022.99
)

# -----------------------------------------------------------------------------
# Canonical cache key for a parameter combination. National-average runs always
# use kitten rate 41, regardless of the slider value, so the key normalises that.
# -----------------------------------------------------------------------------
run_key <- function(useNational, neut_A, neut_K) {
  if (isTRUE(useNational)) neut_K <- 41
  paste(isTRUE(useNational), neut_A, neut_K, sep = "|")
}

# -----------------------------------------------------------------------------
# Uncertainty support.
#
# The survival rates are the uncertain parameters (see CodeFromJenni2_uncertainty
# RC_* scripts). Each is given a mean and variance; a Beta distribution with
# those moments is sampled per Monte Carlo draw. estBetaParams() converts a
# (mean, variance) pair into Beta shape parameters; .cat_survival_priors holds
# the mean/variance for every survival rate; sample_survival() draws one set.
# -----------------------------------------------------------------------------
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu^2
  beta  <- alpha * (1 / mu - 1)
  list(alpha = alpha, beta = beta)
}

# mean and variance for each uncertain survival rate (from the RC_* scripts)
.cat_survival_priors <- list(
  σFK  = c(0.81,  0.001),
  σFJ  = c(0.92,  0.0005),
  σFA  = c(0.96,  0.0001),
  σFE  = c(0.9,   0.0001),
  σShK = c(0.974, 0.00001),
  σShJ = c(0.993, 0.00001),
  σShA = c(0.985, 0.00001),
  σShE = c(0.9,   0.00001),
  σOK  = c(0.97,  0.000001),
  σOJ  = c(0.995, 0.000001),
  σOA  = c(0.995, 0.000001),
  σOE  = c(0.98,  0.000001),
  σStK = c(0.918, 0.00003),
  σStJ = c(0.97,  0.00003),
  σStA = c(0.97,  0.00003),
  σStE = c(0.9,   0.00003)
)

# Draw one set of survival rates from the Beta priors. Returns a named list
# suitable for the `surv` argument of run_cat_model().
sample_survival <- function() {
  out <- lapply(.cat_survival_priors, function(mv) {
    ab <- estBetaParams(mv[1], mv[2])
    rbeta(1, ab$alpha, ab$beta)
  })
  names(out) <- names(.cat_survival_priors)
  out
}

# -----------------------------------------------------------------------------
# Run the model for one parameter combination.
# Returns a list with the raw monthly population series (length nyears*12) and
# the parameters used. Growth rates / averages are derived later by finalize_run
# so they can reflect the user's chosen timeframe.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Internal: update density-dependent transition and birth parameters in `rv`
# from a population vector and the month's seasonal factor. Returns the
# (owned, shelter) carrying-capacity state labels for matrix caching.
# -----------------------------------------------------------------------------
.cat_step_params <- function(rv, popvec, season, ownedBase, shelterBase) {
    NOwned <- sum(popvec[22:28])
    NShelter <- sum(popvec[15:21])

    #carrying capacity functions for homes and shelters
    if(NOwned>ownedBase*1.1){
      rv$TOtoStK <- 0.0009 * 2
      rv$TOtoStJ <- 0.0009 * 2
      rv$TOtoStA <- 0.0009 * 2
      rv$TOtoStE <- 0.0009 * 2
      rv$TFtoO <- 0.02*0.01
      rv$TSttoO <- 0.04*0.01
      ownedState <- "high"
    }else if(NOwned>(ownedBase*1.01) & NOwned<(ownedBase*1.1)){
      rv$TOtoStK <- 0.0009*1.25
      rv$TOtoStJ <- 0.0009*1.25
      rv$TOtoStA <- 0.0009*1.25
      rv$TOtoStE <- 0.0009*1.25
      rv$TFtoO <- 0.02*0.25
      rv$TSttoO <- 0.04*0.45
      ownedState <- "mid"
    }else {
      rv$TOtoStK <- 0.0009
      rv$TOtoStJ <- 0.0009
      rv$TOtoStA <- 0.0009
      rv$TOtoStE <- 0.0009
      rv$TFtoO <- 0.02
      rv$TSttoO <- 0.04
      ownedState <- "low"
    }

    if(NShelter>(shelterBase*1.1)){
      rv$TFtoSh <- 0.003*0.25
      rv$TSttoSh <- 0.03*0.25
      rv$TOtoShK <- 0.002*0.5
      rv$TOtoShJ <- 0.002*0.5
      rv$TOtoShA <- 0.002*0.5
      rv$TOtoShE <- 0.002*0.5
      shelterState <- "high"
    }
    else if(NShelter>(shelterBase*1.01) & NShelter<(shelterBase*1.1)){
      rv$TFtoSh <- 0.003*0.75
      rv$TSttoSh <- 0.03*0.75
      rv$TOtoShK <- 0.002*0.75
      rv$TOtoShJ <- 0.002*0.75
      rv$TOtoShA <- 0.002*0.75
      rv$TOtoShE <- 0.002*0.75
      shelterState <- "mid"
    }
    else {
      rv$TFtoSh <- 0.003
      rv$TSttoSh <- 0.03
      rv$TOtoShK <- 0.002
      rv$TOtoShJ <- 0.002
      rv$TOtoShA <- 0.002
      rv$TOtoShE <- 0.002
      shelterState <- "low"
    }

    #birth rates change monthly to account for seasonality
    rv$bFJ <- season*rv$overallbFJ
    rv$bFA <- season*rv$overallbFA
    rv$bOJ <- season*rv$overallbOJ
    rv$bOA <- season*rv$overallbOA
    rv$bStJ <- season*rv$overallbStJ
    rv$bStA <- season*rv$overallbStA

  c(ownedState, shelterState)
}

# -----------------------------------------------------------------------------
# Internal: build the combined projection matrix (survival/transition +
# fecundity) from the current parameter state in `rv`.
# -----------------------------------------------------------------------------
.cat_build_MatA <- function(rv) {
        rv$MatU <- matrix(c(rv$σFK * (1-rv$TFKtoJ)*(1- rv$TFtoSh-rv$TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            rv$σFK * (rv$TFKtoJ)*(1-rv$TFKUtoFKN)	,	rv$σFJ * (1-rv$TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            rv$σFK * (rv$TFKtoJ)*(rv$TFKUtoFKN)	,	0	,	rv$σFJ * (1-rv$TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	rv$σFJ * rv$TFJtoA*(1-rv$TFUtoFN)	,	0	,	rv$σFA*(1- rv$TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	rv$σFJ * rv$TFJtoA*rv$TFUtoFN	,	rv$σFJ * rv$TFJtoA	,	0	,	rv$σFA*(1- rv$TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	rv$σFA* rv$TFAtoE	,	0	,	rv$σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	rv$σFA* rv$TFAtoE	,	0	,	rv$σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*(1- rv$TSttoSh-rv$TSttoO-rv$TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*(1- rv$TSttoSh-rv$TSttoO)	,	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoStJ*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA*(1-rv$TOAUtoOAN)	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA*rv$TOAUtoOAN	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoStE	,	0	,	rv$σOE * rv$TOtoStE	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoStE	,	0	,	rv$σOE * rv$TOtoStE	,
                            rv$σFK * (1-rv$TFKtoJ)*rv$TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σShK * (1-rv$TShKtoJ)*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoSh	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoShJ*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	rv$σShK * rv$TShKtoJ*(1- rv$TShtoO)	,	rv$σShJ* (1-rv$TShJtoA)*(1- rv$TShtoO)	,	rv$σShJ * (1-rv$TShJtoA)*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoSh	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA*(1-rv$TOAUtoOAN)	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoSh	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	,	0	,	0	,	0	,	rv$σShJ* rv$TShJtoA*(1- rv$TShtoO)	,	rv$σShJ * rv$TShJtoA*(1- rv$TShtoO)	,	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	,	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA*rv$TOAUtoOAN	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoSh	,	0	,	rv$σStE *rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoShE	,	0	,	rv$σOE * rv$TOtoShE	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoSh	,	0	,	rv$σStE *rv$TSttoSh	,	0	,	0	,	0	,	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	,	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	,	rv$σShE*(1- rv$TShtoO)	,	rv$σShE*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoShE	,	0	,	rv$σOE * rv$TOtoShE	,
                            rv$σFK * (1-rv$TFKtoJ)*rv$TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σShK * (1-rv$TShKtoJ)*rv$TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*(1- rv$TOtoShK-rv$TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoO *(1-rv$TStUtoOKN)	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*(1-rv$TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoO *rv$TStUtoOKN	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*rv$TStUtoOJN	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO	,	0	,	0	,	0	,	0	,	rv$σShK * rv$TShKtoJ*rv$TShtoO	,	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	,	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*(1-rv$TOAUtoOAN)	,	0	,	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoO*rv$TStUtoOAN	,	rv$σStJ * rv$TStJtoA*rv$TSttoO	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*rv$TStUtoOAN	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO	,	0	,	0	,	0	,	rv$σShJ * rv$TShJtoA*rv$TShtoO	,	rv$σShJ * rv$TShJtoA*rv$TShtoO	,	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	,	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*rv$TOAUtoOAN	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	rv$σStE *rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	,	0	,	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoO*rv$TStUtoOAN	,	rv$σStA * rv$TStAtoE*rv$TSttoO	,	0	,	rv$σStE *rv$TSttoO	,	0	,	0	,	0	,	rv$σShA *rv$TShAtoE*rv$TShtoO	,	rv$σShA *rv$TShAtoE*rv$TShtoO	,	rv$σShE*rv$TShtoO	,	rv$σShE*rv$TShtoO	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	,	0	,	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	
        ) ,nrow=28, byrow=T)
        rv$MatF <- matrix(c(    rv$σFK *rv$TFKtoJ*rv$bFJ *(1- rv$TFtoSh-rv$TFtoO)*(1-rv$TFKUtoFKN)	,	rv$σFJ * rv$bFJ	,	0	,	rv$σFA * rv$bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK *rv$TStKtoJ*rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	,	rv$σStJ *rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA *rv$bStA *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoStK*(1-rv$TOKUtoOKN)	,	rv$σOJ *rv$bOJ*rv$TOtoStJ*(1-rv$TOJUtoOJN)	,	0	,	rv$σOA *rv$bOA* rv$TOtoStA	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	(rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoSh)/2	,	(rv$σStJ *rv$bStJ *rv$TSttoSh)/2	,	0	,	(rv$σStA *rv$bStA *rv$TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoShK*(1-rv$TOKUtoOKN))/2	,	(rv$σOJ *rv$bOJ*rv$TOtoShJ*(1-rv$TOJUtoOJN))/2	,	0	,	(rv$σOA *rv$bOA* rv$TOtoShA)/2	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoO	,	rv$σStJ *rv$bStJ *rv$TSttoO	,	0	,	rv$σStA *rv$bStA *rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK *rv$TOKtoJ*rv$bOJ *(1- rv$TOtoShK-rv$TOtoStK)*(1-rv$TOKUtoOKN)	,	rv$σOJ *rv$bOJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	,	0	,	rv$σOA *rv$bOA*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                                0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
        ), nrow=28, byrow=T)
  rv$MatU + rv$MatF
}

run_cat_model <- function(neut_K, neut_A, useNational, nyears = 10, legacy = FALSE,
                          surv = NULL) {
  rv <- new.env()
  rv$Catvec2 <- .cat_initial_vector

  lengthoftime <- 12 * nyears
  rv$lengthoftime <- lengthoftime

  # --- user input neuter rates ------------------------------------------------
  owned_neut_rate_K <- if (isTRUE(useNational)) 0.41 else neut_K / 100
  selected_rate_K_index <- round((owned_neut_rate_K - 0.05) / 0.05 + 1)
  rv$TOKUtoOKN <- owned_neut_rate_K
  rv$TStUtoOKN <- owned_neut_rate_K

  # --- set TOAUtoOAN and TStUtoOAN -------------------------------------------
  if (isTRUE(useNational)) {
    if (neut_A == "90%") {
      rv$TOAUtoOAN <- 0.61
      rv$TStUtoOAN <- 0.9
      rv$TStUtoOJN <- 0.61
    } else if (neut_A == "95%") {
      rv$TOAUtoOAN <- 0.804565
      rv$TStUtoOAN <- 0.95
      rv$TStUtoOJN <- 0.61
    } else {
      rv$TOAUtoOAN <- 0.92
      rv$TStUtoOAN <- 0.98
      rv$TStUtoOJN <- 0.61
    }
  } else {
    rv$TStUtoOJN <- c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,
                      0.6048982,0.63782335,0.6707485)

    if (neut_A == "90%") {
      rv$TOAUtoOAN <- c(0.757249083,0.743762921,0.728690152,0.711733286,0.692515505,0.670552327,
                      0.645210198,0.615644382,0.580702962,0.538773258)
      rv$TStUtoOAN <- 0.9
    } else if (neut_A == "95%") {
      rv$TOAUtoOAN <- c(0.878624542,0.871881461,0.864345076,0.855866643,0.846257753,0.835276164,
                      0.822605099,0.807822191,0.790351481,0.769386629)
      rv$TStUtoOAN <- 0.95
      } else {
        rv$TOAUtoOAN <- c(0.951449817, 0.948752584, 0.94573803,  0.942346657, 0.938503101, 0.934110465,
                      0.92904204,0.923128876,0.916140592, 0.907754652)
        rv$TStUtoOAN <- 0.98
      }
    # Select the corresponding TOAUtoOAN and TStUtoOJN value based on the selected_rate_K_index
    rv$TOAUtoOAN <- rv$TOAUtoOAN[selected_rate_K_index]
    rv$TStUtoOJN <- rv$TStUtoOJN[selected_rate_K_index]
  }

  ## Derived or set parameters #################
  # seasonality in breeding - periodic 12-month pattern repeated nyears times
  rv$SeasonalReproduction = c(rep(c(rep(0.0366667, 3), rep(0.13, 6), rep(0.0366667, 3)), nyears), 0.0366667)

  rv$TOJUtoOJN <- 0.13

  #FERAL PARAMETERS
  rv$TFKUtoFKN <- 0
  rv$TFUtoFN <- 0.01
  rv$σFK <- 0.81
  rv$σFJ <- 0.92
  rv$σFA <- 0.96
  rv$σFE <- 0.9
  rv$TFtoSh <- 0.003
  rv$TFtoO <- 0.02
  rv$overallbFJ <- 1.5
  rv$overallbFA <- 2.5
  rv$bFJ <- rv$SeasonalReproduction[1] * rv$overallbFJ
  rv$bFA <- rv$SeasonalReproduction[1] * rv$overallbFA

  #SHELTER PARAMETERS
  rv$σShK <- 0.974
  rv$σShJ <- 0.993
  rv$σShA <- 0.985
  rv$σShE <- 0.9
  rv$TShtoO <- 0.63

  #OWNED PARAMETERS
  rv$σOK <- 0.97
  rv$σOJ <- 0.995
  rv$σOA <- 0.995
  rv$σOE <- 0.98
  rv$TOtoStK <- 0.0009
  rv$TOtoStJ <- 0.0009
  rv$TOtoStA <- 0.0009
  rv$TOtoStE <- 0.0009
  rv$TOtoShK <- 0.002
  rv$TOtoShJ <- 0.002
  rv$TOtoShA <- 0.002
  rv$TOtoShE <- 0.002
  rv$overallbOJ <- 1.4
  rv$overallbOA <- 2.1
  rv$bOJ <- rv$SeasonalReproduction[1] * rv$overallbOJ
  rv$bOA <- rv$SeasonalReproduction[1] * rv$overallbOA

  #STRAY PARAMETERS
  rv$σStK <- 0.918
  rv$σStJ <- 0.97
  rv$σStA <- 0.97
  rv$σStE <- 0.9
  rv$TSttoSh <- 0.03
  rv$TSttoO <- 0.04
  rv$TSttoF <- 0.14186
  rv$overallbStJ <- 1.5
  rv$overallbStA <- 2.5
  rv$bStJ <- rv$SeasonalReproduction[1] * rv$overallbStJ
  rv$bStA <- rv$SeasonalReproduction[1] * rv$overallbStA

  # --- optional survival-rate overrides (uncertainty Monte Carlo draws) -------
  # When `surv` is supplied, its named entries replace the default survival
  # rates above. Aging parameters below are derived from these, so the override
  # must happen here (after defaults, before the aging block).
  if (!is.null(surv)) {
    for (nm in names(surv)) {
      assign(nm, surv[[nm]], envir = rv)
    }
  }

  #Aging parameters - SAS
  p <- 0:6; rv$TFKtoJ <- rv$σFK^5 / sum(rv$σFK^p)
  p <- 0:6; rv$TFJtoA <- rv$σFJ^5 / sum(rv$σFJ^p)
  p <- 0:120; rv$TFAtoE <- rv$σFA^119 / sum(rv$σFA^p)
  p <- 0:6; rv$TStKtoJ <- rv$σStK^5 / sum(rv$σStK^p)
  p <- 0:6; rv$TStJtoA <- rv$σStJ^5 / sum(rv$σStJ^p)
  p <- 0:120; rv$TStAtoE <- rv$σStA^119 / sum(rv$σStA^p)
  p <- 0:6; rv$TShKtoJ <- rv$σShK^5 / sum(rv$σShK^p)
  p <- 0:6; rv$TShJtoA <- rv$σShJ^5 / sum(rv$σShJ^p)
  p <- 0:120; rv$TShAtoE <- rv$σShA^119 / sum(rv$σShA^p)
  p <- 0:6; rv$TOKtoJ <- rv$σOK^5 / sum(rv$σOK^p)
  p <- 0:6; rv$TOJtoA <- rv$σOJ^5 / sum(rv$σOJ^p)
  p <- 0:120; rv$TOAtoE <- rv$σOA^119 / sum(rv$σOA^p)

  # invariant base population sizes (carrying-capacity thresholds)
  ownedBase <- sum(rv$Catvec2[22:28])
  shelterBase <- sum(rv$Catvec2[15:21])

  VM <- matrix(ncol = 28, nrow = lengthoftime)
  VM[1, ] <- rv$Catvec2

  # Matrix cache: matrix is fully determined by owned-state, shelter-state and
  # seasonal factor (<= 3 x 3 x 2 distinct matrices).
  if (isTRUE(legacy)) {
    # ---- Original two-stage algorithm (kept for equivalence testing) -------
    # MATS[[1]] is built from the initial (low-capacity) state and Season[1];
    # subsequent matrices are built from the projected population.
    .cat_step_params(rv, VM[1, ], rv$SeasonalReproduction[1], ownedBase, shelterBase)
    curMat <- .cat_build_MatA(rv)
    for (i in 1:(lengthoftime - 1)) {
      VM[i + 1, ] <- curMat %*% VM[i, ]
      .cat_step_params(rv, VM[i + 1, ], rv$SeasonalReproduction[i + 1], ownedBase, shelterBase)
      curMat <- .cat_build_MatA(rv)
    }
  } else {
    # ---- Unified loop: matrix for step i is f(state(VM[i]), Season[i]) ------
    matCache <- list()
    for (i in 1:(lengthoftime - 1)) {
      st <- .cat_step_params(rv, VM[i, ], rv$SeasonalReproduction[i], ownedBase, shelterBase)
      matKey <- paste(st[1], st[2], rv$SeasonalReproduction[i], sep = "_")
      if (is.null(matCache[[matKey]])) {
        MatA <- .cat_build_MatA(rv)
        matCache[[matKey]] <- MatA
      } else {
        MatA <- matCache[[matKey]]
      }
      VM[i + 1, ] <- MatA %*% VM[i, ]
    }
  }

  list(
    Total   = rowSums(VM),
    Owned   = rowSums(VM[, 22:28]),
    Feral   = rowSums(VM[, 1:7]),
    Stray   = rowSums(VM[, 8:14]),
    Shelter = rowSums(VM[, 15:21]),
    parameters = list(
      owned_neut_rate_K = rv$TOKUtoOKN,
      owned_neut_rate_A = rv$TStUtoOAN
    )
  )
}

# -----------------------------------------------------------------------------
# Turn a raw run (computed at the max timeframe) into a "run" record for the
# requested timeframe: slice the monthly series to nyears*12 months and compute
# the growth-rate and average summaries exactly as the app's Go handler did.
# -----------------------------------------------------------------------------
finalize_run <- function(raw, nyears) {
  L <- 12 * nyears

  Total   <- raw$Total[1:L]
  Owned   <- raw$Owned[1:L]
  Feral   <- raw$Feral[1:L]
  Stray   <- raw$Stray[1:L]
  Shelter <- raw$Shelter[1:L]

  start_window <- max(1, L - 11)

  list(
    Total = Total, Owned = Owned, Feral = Feral, Stray = Stray, Shelter = Shelter,
    PG_Total   = sum(Total[start_window:L])   / sum(Total[1:12]),
    PG_Owned   = sum(Owned[start_window:L])   / sum(Owned[1:12]),
    PG_Feral   = sum(Feral[start_window:L])   / sum(Feral[1:12]),
    PG_Stray   = sum(Stray[start_window:L])   / sum(Stray[1:12]),
    PG_Shelter = sum(Shelter[start_window:L]) / sum(Shelter[1:12]),
    parameters = raw$parameters
  )
}

# -----------------------------------------------------------------------------
# Enumerate every reachable parameter combination (33 in total).
# -----------------------------------------------------------------------------
all_run_combinations <- function() {
  combos <- list()
  A_levels <- c("90%", "95%", "98%")
  K_levels <- seq(5, 50, by = 5)

  # Manual mode: 3 adult levels x 10 kitten rates
  for (A in A_levels) {
    for (K in K_levels) {
      combos[[length(combos) + 1]] <- list(useNational = FALSE, neut_A = A, neut_K = K)
    }
  }
  # National-average mode: kitten rate forced to 41, 3 adult levels
  for (A in A_levels) {
    combos[[length(combos) + 1]] <- list(useNational = TRUE, neut_A = A, neut_K = 41)
  }
  combos
}
