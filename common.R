# common.R -- shared paths, style, labels, geometry and check helpers for the
# paper's figures (09_paper_figures.R) and tables (10_paper_tables.R).
# Always run from the kit root:  Rscript scripts/09_paper_figures.R
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
  library(scales); library(patchwork); library(stringr)
})

# ---- paths (CLAUDE_CODE_FIGURES_AND_TABLES.md, section 1) -------------------
RESULTS <- if (file.exists("../results/07_profiles.csv")) "../results" else "results_snapshot"
SPLITS  <- if (file.exists("../data/splits/train.csv")) "../data/splits" else NA_character_
FIG_DIR <- "paper/figures"
TAB_DIR <- "paper/tables"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
cat("Results from:", normalizePath(RESULTS), "\n")
cat("Mode:", if (RESULTS == "../results") "in the repo" else "standalone (results_snapshot/)", "\n")
cat("Shots from:", if (is.na(SPLITS)) "data_extract/focus_shots_2020_21.csv" else normalizePath(SPLITS), "\n")
rd <- function(f) readr::read_csv(file.path(RESULTS, f), show_col_types = FALSE)

# TRUE for the 26-27 Oct snapshot. Set FALSE after the validation run: failed
# checks then become warnings that are logged in paper/NUMBERS.md.
STRICT_CHECKS <- TRUE

# ---- style (section 3) ------------------------------------------------------
theme_paper <- function(base = 8) {
  theme_bw(base_size = base, base_family = "serif") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90", linewidth = 0.25),
          panel.border     = element_rect(colour = "grey40", linewidth = 0.3, fill = NA),
          strip.background = element_blank(),
          strip.text       = element_text(face = "bold", hjust = 0, size = base),
          axis.title       = element_text(size = base),
          axis.text        = element_text(size = base - 1),
          legend.position  = "bottom", legend.title = element_blank(),
          legend.text      = element_text(size = base - 1),
          legend.key.height = unit(3, "mm"), legend.margin = margin(0, 0, 0, 0),
          plot.margin      = margin(2, 4, 2, 2))
}
save_fig <- function(p, name, w, h) {
  dev <- if (capabilities("cairo")) cairo_pdf else function(...) pdf(..., useDingbats = FALSE)
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), p, width = w, height = h, units = "in", device = dev)
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = w, height = h, units = "in", dpi = 300, bg = "white")
  cat("  wrote", file.path(FIG_DIR, paste0(name, ".pdf")), "(+ .png)\n")
}
W1 <- 3.5    # single column (in)
W2 <- 7.16   # full width (in)
PT <- 1 / ggplot2::.pt   # ggplot text size for 1 pt

COL <- c(paper     = "#D7301F",  # paper's forest (as published / in-sample)
         paper_pc  = "#FC8D59",  # paper's forest, cross-fitted + prior-corrected
         xgboost   = "#6250D6",  # ours (same purple as 07's figures)
         logreg    = "#1BAF7A",  # ours (same green as 07's figures)
         rf        = "#E69F00",  # ours
         understat = "#7F7F7F")  # reference: grey, dashed lines / open symbols
PLAYER_COL <- c("Lionel Messi" = "#2A78D6", "Robert Lewandowski" = "#EB6834", "Burak Yilmaz" = "#3B3B3B")
TEAM_COL   <- c("Schalke 04" = "#2A78D6", "Bayern Munich" = "#3B3B3B")
MODEL_LAB <- c(paper_insample = "Paper's forest, as published",
               paper_crossfit = "Paper's forest, cross-fitted",
               paper_crossfit_pc = "Paper's forest, cross-fitted + prior-corrected",
               ours_crossfit = "XGBoost, cross-fitted",
               logreg_crossfit = "Logistic regression, cross-fitted",
               understat = "Understat (reference)",
               logistic_regression = "Logistic regression", xgboost = "XGBoost",
               random_forest = "Random forest", understat_xG = "Understat (reference)")
# model key -> colour key, so one model always gets one colour
MODEL_COL <- c(paper_insample = COL[["paper"]], paper_crossfit = COL[["paper"]],
               paper_crossfit_pc = COL[["paper_pc"]],
               ours_crossfit = COL[["xgboost"]], logreg_crossfit = COL[["logreg"]],
               understat = COL[["understat"]],
               logistic_regression = COL[["logreg"]], xgboost = COL[["xgboost"]],
               random_forest = COL[["rf"]], understat_xG = COL[["understat"]])
SHORT_NAME <- c("Lionel Messi" = "Messi", "Robert Lewandowski" = "Lewandowski",
                "Burak Yilmaz" = "Yilmaz", "Schalke 04" = "Schalke 04",
                "Bayern Munich" = "Bayern Munich")
FOCUS_IDS <- c("Lionel Messi" = 2097, "Robert Lewandowski" = 227, "Burak Yilmaz" = 8637)

# ---- geometry, copied from 07_aggregated_profiles.R (section 4, Fig. 2) -----
paper_angle  <- function(dx, dy) abs(atan((7.32 * dx) / (dx^2 + dy^2 - (7.32 / 2)^2)) * 180 / pi)
max_angle_at <- function(d) paper_angle(d, 0)                      # straight in front of goal
min_angle_at <- function(d) { dy <- pmin(d, 34)                    # widest spot still on the pitch
  a <- paper_angle(sqrt(pmax(d^2 - dy^2, 0)), dy); ifelse(is.nan(a), 0, a) }
is_feasible  <- function(d, a, tol = 1e-9) a <= max_angle_at(d) + tol & a >= min_angle_at(d) - tol
# shot location: dx = 105 - X * 105, dy = 34 - Y * 68

# ---- number formatting ------------------------------------------------------
f_dp  <- function(x, d) formatC(x, format = "f", digits = d)
f_sgn <- function(x, d) paste0(ifelse(x >= 0, "+", "−"), f_dp(abs(x), d))   # for plots
f_sgn_tex <- function(x, d) paste0(ifelse(x >= 0, "$+$", "$-$"), f_dp(abs(x), d)) # for LaTeX
f_n   <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
f_n_tex <- function(n) gsub(",", "{,}", f_n(n))

# ---- number log and checks (written to paper/NUMBERS.md) --------------------
NUM_LOG   <- list()
CHECK_LOG <- list()
log_num <- function(float, cell, value, source, filter = "", check = "--") {
  NUM_LOG[[length(NUM_LOG) + 1]] <<- tibble(float = float, cell = cell, value = as.character(value),
                                           source = source, filter = filter, check = check)
  invisible(value)
}
# check(): compare actual vs expected. Numeric: within tol. Otherwise identical().
check <- function(where, what, actual, expected, tol = 0) {
  ok <- if (is.numeric(actual) && is.numeric(expected)) {
    length(actual) == length(expected) && all(abs(actual - expected) <= tol + 1e-12)
  } else identical(as.character(actual), as.character(expected))
  fmt <- function(v) paste(if (is.numeric(v)) signif(v, 6) else v, collapse = ", ")
  CHECK_LOG[[length(CHECK_LOG) + 1]] <<- tibble(where = where, check = what,
                                               expected = fmt(expected), actual = fmt(actual),
                                               tol = tol, status = if (ok) "PASS" else "FAIL")
  cat(sprintf("  [%s] %s: %s (expected %s)\n", if (ok) "PASS" else "FAIL", what, fmt(actual), fmt(expected)))
  if (!ok) {
    if (STRICT_CHECKS) { write_numbers(); stop("Check failed (STRICT_CHECKS = TRUE): ", where, " / ", what) }
    warning("Check failed: ", where, " / ", what)
  }
  invisible(ok)
}
TOL3 <- 0.0005   # 3-4 dp values
TOL1 <- 0.05     # 1 dp values

# Each script stores its own log in paper/.numbers/<script>.rds; NUMBERS.md is
# rebuilt from all logs present, so running either script keeps the other's.
SCRIPT_ID <- "unknown"
write_numbers <- function() {
  dir.create("paper/.numbers", showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(nums = bind_rows(NUM_LOG), checks = bind_rows(CHECK_LOG),
               when = format(Sys.time(), "%Y-%m-%d %H:%M"), results = normalizePath(RESULTS),
               strict = STRICT_CHECKS),
          file.path("paper/.numbers", paste0(SCRIPT_ID, ".rds")))
  logs <- lapply(sort(list.files("paper/.numbers", "\\.rds$", full.names = TRUE)), readRDS)
  names(logs) <- sub("\\.rds$", "", basename(sort(list.files("paper/.numbers", "\\.rds$"))))
  esc <- function(x) gsub("\\|", "\\\\|", x)
  out <- c("# NUMBERS: every number printed in a float, with its source and the checks", "",
           "Generated by `scripts/09_paper_figures.R` and `scripts/10_paper_tables.R`; do not edit by hand.",
           "Re-running either script rewrites this file.", "")
  for (nm in names(logs)) {
    L <- logs[[nm]]
    out <- c(out, paste0("## ", nm), "",
             paste0("- Run: ", L$when, "; results read from `", L$results, "`; STRICT_CHECKS = ", L$strict), "")
    if (nrow(L$checks)) {
      out <- c(out, "### Checks (CLAUDE_CODE_FIGURES_AND_TABLES.md, section 7)", "",
               paste0("Passed ", sum(L$checks$status == "PASS"), " of ", nrow(L$checks), "."), "",
               "| Where | Check | Expected | Actual | Tolerance | Status |", "|---|---|---|---|---|---|",
               with(L$checks, paste("|", esc(where), "|", esc(check), "|", esc(expected), "|", esc(actual),
                                    "|", tol, "|", status, "|")), "")
    }
    if (nrow(L$nums)) {
      out <- c(out, "### Numbers printed", "",
               "| Float | Cell / annotation | Value | Source | Filter | Check |", "|---|---|---|---|---|---|",
               with(L$nums, paste("|", esc(float), "|", esc(cell), "|", esc(value), "|", esc(source),
                                  "|", esc(filter), "|", esc(check), "|")), "")
    }
  }
  write_lf(out, "paper/NUMBERS.md")
  cat("  wrote paper/NUMBERS.md\n")
}

# ---- shots for Fig. 2 (train + test only; validation is never read) ---------
load_focus_shots <- function() {
  if (!is.na(SPLITS)) {
    sh <- bind_rows(read_csv(file.path(SPLITS, "train.csv"), show_col_types = FALSE),
                    read_csv(file.path(SPLITS, "test.csv"),  show_col_types = FALSE))
    pl <- sh %>% filter(season_start == 2020, player_id %in% FOCUS_IDS) %>%
      mutate(group = names(FOCUS_IDS)[match(player_id, FOCUS_IDS)])
    # the paper's example match: Bayern Munich v Schalke 04, 2020-21 (match 14173)
    mt <- sh %>% filter(match_id == 14173) %>%
      mutate(team = ifelse(h_a == "h", h_team, a_team), group = team)
    bind_rows(pl, mt)
  } else {
    read_csv("data_extract/focus_shots_2020_21.csv", show_col_types = FALSE)
  }
}

# write text files with LF line endings on every OS (main.tex uses LF)
write_lf <- function(x, path) { con <- file(path, open = "wb"); on.exit(close(con)); writeLines(x, con, sep = "\n", useBytes = TRUE) }
