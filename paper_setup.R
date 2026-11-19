# ==============================================================================
# paper_setup.R  -  load the parent paper's modelling code, patched to run today
#
# SOURCED, NOT RUN. Used by 05_paper_as_published.R and 06_paper_benchmark.R.
# Fits nothing and writes nothing on load.
#
# What the paper's code is
#   Cavus & Biecek train their models with forester (ModelOriented's AutoML
#   package) and override seven of its functions with their own copies, the
#   Changes/ folder of github.com/mcavs/Explainable_xG_model_paper.
#
# What is in paper_forester/
#   original/          the seven Changes/ files, byte-for-byte, for reference
#   *.R (7 files)      the same seven files with eight small patches, each
#                      marked "DS340W PATCH n". Full list: PAPER_PATCHES.diff
#   forester_*.R       forester's five internal helpers, verbatim from commit
#                      4813a95 (2022-03-11). No commits landed between then and
#                      Aug 2022, so this is the forester the paper ran in June
#                      2022.
#
# Why vendor forester instead of installing it
#   remotes::install_github("ModelOriented/forester@4813a9525fb3b59d5716d87e67bf0fb7a374f944")
#   also works and runs the same code. But it pulls in tidymodels, Boruta and
#   rBayesianOptimization, which only the tuning and feature-selection branches
#   touch, and the paper used neither (Rcodes.Rmd never sets tune or
#   num_features). Vendoring the five helpers avoids all of that on R 4.3 /
#   Windows. If you install the package instead, it doesn't matter: PAPER's
#   own copies are found first.
#
# Isolation
#   Everything loads into one environment, PAPER, so the paper's names
#   (evaluate, forester, prepare_data, ...) can't collide with 02_model_utils.R.
#   Call them as PAPER$forester(...), PAPER$model_performancex(...).
# ==============================================================================

# install.packages(c("ROSE", "DALEX", "recipes", "ranger", "xgboost",
#                    "lightgbm", "knitr", "dplyr", "readr"))
#
# catboost is optional. It is the paper's model1 and is not on CRAN. On Windows:
# remotes::install_url(
#   "https://github.com/catboost/catboost/releases/download/v1.2.8/catboost-R-windows-x86_64-1.2.8.tgz",
#   INSTALL_opts = c("--no-multiarch", "--no-test-load"))
# If it isn't installed, forester prints a one-line reason and carries on with
# the other three; model1 is then NULL and dropped from the tables.

# ranger is required: it fits model3, the paper's headline random forest.
# The other three model packages are optional; a missing one is skipped with a
# warning and its slot stays NULL.
PAPER_REQUIRED <- c("ROSE", "DALEX", "recipes", "ranger", "knitr", "dplyr", "readr")
PAPER_OPTIONAL <- c("xgboost", "lightgbm", "catboost")
.have <- function(p) vapply(p, requireNamespace, logical(1), quietly = TRUE)
.missing <- PAPER_REQUIRED[!.have(PAPER_REQUIRED)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\nInstall them (see the commented line at the top of paper_setup.R).")
}
.missing <- PAPER_OPTIONAL[!.have(PAPER_OPTIONAL)]
if (length(.missing)) {
  warning("Not installed, so these paper models will be skipped: ",
          paste(.missing, collapse = ", "), call. = FALSE, immediate. = TRUE)
}
rm(.missing, .have)

suppressPackageStartupMessages({
  library(ROSE)    # ovun.sample(), the paper's balancing step
  library(DALEX)   # explainers; forester returns DALEX explainer objects
})

PAPER_DIR <- "paper_forester"
PAPER_FILES <- c(
  # forester internals, verbatim @ 4813a95
  "forester_check_conditions.R", "forester_split_data.R",
  "forester_prepare_data.R",     "forester_check_metric.R",
  "forester_calculate_metric.R",
  # the paper's Changes/ files, patched
  "evaluate.R", "forester.R", "make_ranger.R", "make_xgboost.R",
  "make_lightgbm.R", "make_catboost.R", "model_performancex.R"
)

PAPER <- new.env(parent = globalenv())
for (.f in PAPER_FILES) {
  .p <- file.path(PAPER_DIR, .f)
  if (!file.exists(.p)) stop("Missing ", .p, " - is the working directory the project root?")
  sys.source(.p, envir = PAPER, keep.source = FALSE)
}
rm(.f, .p)


# ---- Paper constants ----------------------------------------------------------

# Column set and order exactly as Rcodes.Rmd builds train_data.
PAPER_COLS <- c("status", "minute", "h_a", "situation", "shotType",
                "lastAction", "distanceToGoal", "angleToGoal")

# forester() returns its four models in fixed slots. The paper's Table III
# labels follow from this order (see DS340W PATCH 4 in evaluate.R).
PAPER_SLOTS <- c(model1 = "catboost", model2 = "xgboost",
                 model3 = "ranger",   model4 = "lightgbm")

PAPER_SEED <- 123   # Rcodes.Rmd calls set.seed(123) before every random step

# Table III of the paper, the three probability metrics only, as transcribed
# from arXiv:2206.07212 (Sept 2022 version). The over-sampled rows are
# cross-checked against Table IV, which repeats them. Check against the
# published PDF before quoting any of these numbers in the report.
PAPER_TABLE3 <- data.frame(
  sampling = c(rep("over", 4), rep("original", 4)),
  model    = rep(c("ranger", "catboost", "xgboost", "lightgbm"), 2),
  auc      = c(0.985, 0.839, 0.821, 0.818,   0.975, 0.823, 0.819, 0.817),
  log_loss = c(0.270, 0.495, 0.517, 0.520,   0.173, 0.261, 0.263, 0.264),
  brier    = c(0.071, 0.164, 0.172, 0.173,   0.051, 0.074, 0.075, 0.075)
)


# ---- Helpers ------------------------------------------------------------------

# The paper's modelling frame: eight columns, plain data.frame, numeric 0/1
# target. A tibble here would crash forester on R >= 4.2:
# check_conditions() runs `if (class(data[, target]) == "factor")`, and on a
# tibble data[, target] has a length-3 class.
paper_frame <- function(df) {
  out <- as.data.frame(df)[, PAPER_COLS]
  out$status <- as.numeric(out$status)
  rownames(out) <- NULL
  out
}

# forester's prepare_data() stops and waits for keyboard input when the first
# class it sees is under 10% or over 90% of the rows. Under Rscript, readline()
# returns "" immediately and the loop never ends. Our goal rate is 10.67%, just
# above the threshold, so this only bites on a subsample or a single season.
assert_no_forester_prompt <- function(d) {
  share <- 100 * mean(d$status == d$status[1])
  if (share < 10 || share > 90) {
    stop(sprintf(paste0(
      "First-class share is %.2f%%. forester would open an interactive ",
      "prompt here and hang under Rscript. Use a stratified sample that ",
      "keeps the goal rate above 10%%."), share))
  }
  invisible(TRUE)
}

# Stratified subsample that preserves the goal rate exactly.
stratified_subsample <- function(d, n, seed) {
  if (n >= nrow(d)) return(d)
  set.seed(seed)
  rate  <- mean(d$status == 1)
  n_pos <- round(n * rate)
  pos   <- which(d$status == 1)
  neg   <- which(d$status == 0)
  idx   <- sort(c(sample(pos, n_pos), sample(neg, n - n_pos)))
  d[idx, , drop = FALSE]
}

# The paper's balancing step, with the paper's seed placement.
paper_resample <- function(d, sampling) {
  if (sampling == "original") return(d)
  set.seed(PAPER_SEED)
  # do.call, not a direct call: ovun.sample() re-evaluates its own matched call
  # inside ROSE, where local variables like `d` and `sampling` are not visible.
  # The Rmd got away with it by calling it at top level on global objects.
  out <- do.call(ovun.sample, list(formula = status ~ ., data = d,
                                   method = sampling))$data
  rownames(out) <- NULL
  out
}

# P(goal) from one of forester's explainers, for any rows.
paper_predict <- function(expl, newdata) {
  x <- newdata[, setdiff(PAPER_COLS, "status"), drop = FALSE]
  as.numeric(expl$predict_function(expl$model, x))
}

# The paper's own metric code (model_performancex) on any predictions. Same
# function that produces Table III, so differences between rows are due to the
# data scored, never to metric definitions.
paper_measures <- function(expl, p, y) {
  e <- expl
  e$y_hat     <- p
  e$y         <- y
  e$residuals <- y - p
  as.data.frame(PAPER$model_performancex(e)$measures)
}

# Exactly what Table III reports. The explainer forester returns was built on
# its TRAINING rows and precalculated y_hat there. evaluate() scores the test
# set on a loop-local copy (`m <- DALEX::update_data(m, ...)`), which is thrown
# away, so the returned model still carries training data.
paper_measures_as_published <- function(expl) {
  as.data.frame(PAPER$model_performancex(expl)$measures)
}

# Undo the class-prior shift from over/under-sampling (Elkan 2001; Saerens et
# al. 2002). Gives the resampled models their best fair chance on calibration
# metrics. Ranking metrics (AUC) are unchanged, since the map is monotone.
prior_correct <- function(p, rate_trained, rate_true) {
  a <- rate_true / rate_trained
  b <- (1 - rate_true) / (1 - rate_trained)
  (p * a) / (p * a + (1 - p) * b)
}

# Share of evaluation rows that are exact copies of a training row. On
# continuous features (distance, angle, minute) exact matches essentially only
# arise from resampling duplicates, i.e. leakage.
duplicate_share <- function(train_df, eval_df) {
  key <- function(d) do.call(paste, c(d[, PAPER_COLS], sep = "|"))
  mean(key(eval_df) %in% key(train_df))
}

clip01 <- function(p) pmin(pmax(p, 1e-15), 1 - 1e-15)
