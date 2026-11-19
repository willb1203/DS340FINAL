# 10_paper_tables.R -- builds Tables II-VII, paper/floats.tex and
# paper/main_skeleton.tex from the results CSVs.
# Run from the kit root:  Rscript scripts/10_paper_tables.R
source("scripts/common.R")
SCRIPT_ID <- "10_paper_tables"

INCLUDE_DECILE9 <- FALSE  # Table VII: optional decile 9 column (overflows 3.5 in by ~60 pt, so off)

# cell(): format a value, log it to NUMBERS.md, return the LaTeX string
cell <- function(float, name, txt, source, filter = "", chk = "--") {
  log_num(float, name, txt, source, filter, chk); txt
}
b <- function(x, bold) ifelse(bold, paste0("\\textbf{", x, "}"), x)
write_table <- function(lines, name) {
  write_lf(lines, file.path(TAB_DIR, paste0(name, ".tex")))
  cat("  wrote", file.path(TAB_DIR, paste0(name, ".tex")), "\n")
}
row_tex <- function(...) paste0(paste(..., sep = " & "), " \\\\")
table_head <- function(caption, label, colspec, size = "\\footnotesize", sep = "4pt", env = "table") {
  c(paste0("\\begin{", env, "}[!t]"), "\\centering", paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"), size, paste0("\\setlength{\\tabcolsep}{", sep, "}"),
    paste0("\\begin{tabular}{", colspec, "}"), "\\toprule")
}
table_note <- function(txt) c("\\par\\smallskip", paste0("\\parbox{\\linewidth}{\\scriptsize\\raggedright ", txt, "}"))

# =============================================================================
# Table II: data and evaluation sets
# =============================================================================
cat("\nTable II\n")
ss <- rd("01_split_summary.csv")
check("Fig. 1 / Table II", "n: train, test, validation, out_of_time",
      ss$n[match(c("train", "test", "validation", "out_of_time"), ss$set)], c(221200, 63200, 31602, 184758))
era <- ss %>% filter(set %in% c("train", "test", "validation"))
tot_n <- sum(era$n); tot_g <- sum(era$goals); tot_r <- 100 * tot_g / tot_n
check("Table II", "total shots / goals", c(tot_n, tot_g), c(316002, 33715))
check("Table II", "total goal rate (%)", round(tot_r, 2), 10.67, 0.005)
check("Fig. 1", "out-of-time goal rate (%)", round(100 * ss$goals[ss$set == "out_of_time"] / ss$n[ss$set == "out_of_time"], 2), 10.83, 0.005)
SRC <- "01_split_summary.csv"
PAPER_SHOTS <- 315430; PAPER_RATE <- 10.66; PAPER_MATCHES <- 12655; OUR_MATCHES <- 12679  # given in the spec
t2 <- c(table_head("Rebuilt data compared with the parent study, and the evaluation sets", "tab:data", "l rrr"),
        "\\multicolumn{4}{l}{\\textit{2014--15 to 2020--21: ours vs.\\ the paper}} \\\\",
        row_tex("", "Ours", "Paper", ""), "\\midrule",
        row_tex("Shots", cell("Table II", "Shots, ours", f_n_tex(tot_n), SRC, "sum n over train, test, validation", "PASS"),
                cell("Table II", "Shots, paper", f_n_tex(PAPER_SHOTS), "spec (paper's published count)", "", "exception"), ""),
        row_tex("Goal rate", cell("Table II", "Goal rate, ours", paste0(f_dp(tot_r, 2), "\\%"), SRC, "sum goals / sum n", "PASS"),
                cell("Table II", "Goal rate, paper", paste0(f_dp(PAPER_RATE, 2), "\\%"), "spec (paper's published rate)", "", "exception"), ""),
        row_tex("Matches", cell("Table II", "Matches, ours", f_n_tex(OUR_MATCHES), "spec (12,679 matches in our data)", "", "exception"),
                cell("Table II", "Matches, paper", f_n_tex(PAPER_MATCHES), "spec (paper's published count)", "", "exception"), ""),
        "\\midrule",
        "\\multicolumn{4}{l}{\\textit{Evaluation sets}} \\\\",
        row_tex("", "Shots", "Goals", "Goal rate"), "\\midrule")
set_lab <- function(s) {
  r <- ss[ss$set == s, ]
  switch(s, train = paste0("Train (", r$pct_of_paper_era, "\\%)"), test = paste0("Test (", r$pct_of_paper_era, "\\%)"),
         validation = paste0("Validation (", r$pct_of_paper_era, "\\%, sealed)"), out_of_time = "Out-of-time (2021--22 on)")
}
for (s in c("train", "test", "validation", "out_of_time")) {
  r <- ss[ss$set == s, ]
  t2 <- c(t2, row_tex(set_lab(s),
                      cell("Table II", paste0(s, ": shots"), f_n_tex(r$n), SRC, paste0("set == '", s, "'"), "PASS"),
                      cell("Table II", paste0(s, ": goals"), f_n_tex(r$goals), SRC, paste0("set == '", s, "'")),
                      cell("Table II", paste0(s, ": goal rate"), paste0(f_dp(100 * r$goals / r$n, 2), "\\%"), SRC,
                           paste0("set == '", s, "', goals / n"))))
}
t2 <- c(t2, "\\bottomrule", "\\end{tabular}", "\\end{table}")
write_table(t2, "tab02_data")

# =============================================================================
# Table III: does the paper's performance hold?
# =============================================================================
cat("\nTable III\n")
t3s <- rd("05_table3_side_by_side.csv")
bm  <- rd("06_benchmark_test_metrics.csv")
dia <- rd("05_split_diagnostics.csv")
ALG <- c(catboost = "CatBoost", xgboost = "XGBoost", lightgbm = "LightGBM", ranger = "Random forest")
SAMP <- c(original = "Original data", over = "Over-sampled data")
clean_auc <- function(a, s) bm$auc[bm$model == paste0("paper_", a, "_", s)]
for (s in names(SAMP)) {
  r <- t3s %>% filter(sampling == s, model == "ranger")
  e <- if (s == "original") c(0.975, 0.975, 0.798, 0.804) else c(0.985, 0.985, 0.966, 0.798)
  check("Table III", paste0("RF ", s, ": reported / train rows / resampled / clean"),
        round(c(r$auc_paper, r$auc_ours_train_rows, r$auc_ours_heldout, clean_auc("ranger", s)), 3), e, TOL3)
}
dup <- setNames(100 * dia$test_rows_duplicating_train, dia$sampling)
check("Table III", "duplicate shares original, over (%)", round(dup[c("original", "over")], 1), c(1.4, 50.2), TOL1)
t3 <- table_head("AUC of the parent study's models: as reported, on their own training rows, on the resampled test split, and on the clean test set",
                 "tab:reproduction", "l rrrr", sep = "3.5pt")
t3 <- c(t3, row_tex("", "", "Own", "Resampled", "Clean"), row_tex("Model", "Reported", "training rows", "test split", "test set"))
for (s in names(SAMP)) {
  t3 <- c(t3, "\\midrule", paste0("\\multicolumn{5}{l}{\\textit{", SAMP[[s]], "}} \\\\"))
  for (a in names(ALG)) {
    r <- t3s %>% filter(sampling == s, model == a)
    rf <- a == "ranger"; ck <- if (rf) "PASS" else "--"
    flt <- paste0("sampling == '", s, "', model == '", a, "'")
    t3 <- c(t3, row_tex(b(ALG[[a]], rf),
      b(cell("Table III", paste(s, a, "reported"), f_dp(r$auc_paper, 3), "05_table3_side_by_side.csv", paste(flt, "auc_paper"), ck), rf),
      b(cell("Table III", paste(s, a, "own training rows"), f_dp(r$auc_ours_train_rows, 3), "05_table3_side_by_side.csv", paste(flt, "auc_ours_train_rows"), ck), rf),
      b(cell("Table III", paste(s, a, "resampled test split"), f_dp(r$auc_ours_heldout, 3), "05_table3_side_by_side.csv", paste(flt, "auc_ours_heldout"), ck), rf),
      b(cell("Table III", paste(s, a, "clean test set"), f_dp(clean_auc(a, s), 3), "06_benchmark_test_metrics.csv",
             paste0("model == 'paper_", a, "_", s, "', auc"), ck), rf)))
  }
}
dup_o <- cell("Table III", "note: duplicate share, original", paste0(f_dp(dup[["original"]], 1), "\\%"),
              "05_split_diagnostics.csv", "sampling == 'original', test_rows_duplicating_train x 100", "PASS")
dup_v <- cell("Table III", "note: duplicate share, over-sampled", paste0(f_dp(dup[["over"]], 1), "\\%"),
              "05_split_diagnostics.csv", "sampling == 'over', test_rows_duplicating_train x 100", "PASS")
t3 <- c(t3, "\\bottomrule", "\\end{tabular}",
        table_note(paste0("Share of the resampled test rows that duplicate a training row: original ", dup_o,
                          ", over-sampled ", dup_v, ".")), "\\end{table}")
write_table(t3, "tab03_reproduction")

# =============================================================================
# Table IV: held-out test set
# =============================================================================
cat("\nTable IV\n")
slp <- rd("08_calibration_slope.csv")
rows4 <- tribble(~model, ~lab, ~slope_key,
  "ours_logistic_regression", "LR (ours)", "logistic_regression",
  "ours_xgboost", "XGBoost (ours)", "xgboost",
  "ours_random_forest", "RF (ours)", "random_forest",
  "paper_lightgbm_over_priorcorrected", "Best paper variant: LightGBM, over + prior-corr.", NA,
  "paper_ranger_over", "Paper's forest, over-sampled", NA,
  "paper_ranger_over_priorcorrected", "Paper's forest, over + prior-corr.", NA,
  "understat_xG_insample", "Understat (in-sample ref.)", "understat_xG")
d4 <- rows4 %>% left_join(bm, by = "model") %>% left_join(slp, by = c("slope_key" = "model"))
stopifnot(nrow(d4) == 7, !anyNA(d4$auc))
lr <- d4[1, ]
check("Table IV", "LR AUC / Brier / log loss / ECE", c(round(lr$auc, 3), round(lr$brier, 4), round(lr$log_loss, 4), round(lr$ece, 4)),
      c(0.810, 0.0775, 0.2702, 0.0027), TOL3)
check("Table IV", "LR slope [CI]", round(c(lr$slope, lr$slope_lo, lr$slope_hi), 3), c(0.972, 0.947, 0.997), TOL3)
pr <- d4 %>% filter(model == "paper_ranger_over")
check("Table IV", "paper_ranger_over AUC / Brier / log loss / ECE",
      c(round(pr$auc, 3), round(pr$brier, 4), round(pr$log_loss, 4), round(pr$ece, 4)), c(0.798, 0.1163, 0.3811, 0.1626), TOL3)
best <- d4$model[which.min(d4$ece)]
check("Table IV", "lowest ECE among the 7 rows", best, "ours_logistic_regression")
check("Table IV", "ECE: LR, paper LightGBM over+pc", round(c(lr$ece, d4$ece[d4$model == "paper_lightgbm_over_priorcorrected"]), 6),
      c(0.002706, 0.002722), 0.0000005)
n_test <- unique(d4$n)
stopifnot(length(n_test) == 1)
log_num("Table IV", "caption: test shots", f_n(n_test), "06_benchmark_test_metrics.csv", "n", "matches Table II test n")
t4 <- table_head(paste0("Discrimination and calibration on the held-out test set (", f_n_tex(n_test), " shots)"),
                 "tab:testset", ">{\\raggedright\\arraybackslash}p{2.25cm} rrrrl", size = "\\scriptsize", sep = "3pt")
t4 <- c(t4, row_tex("Model", "AUC", "Brier", "Log loss", "ECE", "Slope [95\\% CI]"), "\\midrule")
for (i in seq_len(nrow(d4))) {
  r <- d4[i, ]; src <- "06_benchmark_test_metrics.csv"; flt <- paste0("model == '", r$model, "'")
  ck <- if (r$model %in% c("ours_logistic_regression", "paper_ranger_over")) "PASS" else "--"
  slope_txt <- if (is.na(r$slope_key)) "--" else
    cell("Table IV", paste(r$model, "slope [CI]"), paste0(f_dp(r$slope, 3), " [", f_dp(r$slope_lo, 3), ", ", f_dp(r$slope_hi, 3), "]"),
         "08_calibration_slope.csv", paste0("model == '", r$slope_key, "'"), ck)
  t4 <- c(t4, row_tex(r$lab,
    cell("Table IV", paste(r$model, "AUC"), f_dp(r$auc, 3), src, flt, ck),
    cell("Table IV", paste(r$model, "Brier"), f_dp(r$brier, 4), src, flt, ck),
    cell("Table IV", paste(r$model, "log loss"), f_dp(r$log_loss, 4), src, flt, ck),
    b(cell("Table IV", paste(r$model, "ECE"), f_dp(r$ece, 4), src, flt, if (r$model == best) "PASS (lowest, bold)" else ck), r$model == best),
    slope_txt))
}
ints <- d4 %>% filter(!is.na(slope_key))
int_txt <- paste0(ints$lab, " ", sapply(seq_len(nrow(ints)), function(i)
  cell("Table IV", paste(ints$model[i], "note: intercept [CI]"),
       paste0(f_sgn_tex(ints$intercept[i], 3), " [", f_sgn_tex(ints$intercept_lo[i], 3), ", ", f_sgn_tex(ints$intercept_hi[i], 3), "]"),
       "08_calibration_slope.csv", paste0("model == '", ints$slope_key[i], "'"))), collapse = "; ")
t4 <- c(t4, "\\bottomrule", "\\end{tabular}",
        table_note(paste0("ECE: expected calibration error, 10 equal-frequency bins; lowest in bold. Calibration intercept [95\\% CI]: ",
                          int_txt, ". Slope and intercept are not computed for the paper's models.")),
        "\\end{table}")
write_table(t4, "tab04_test_set")

# =============================================================================
# Table V: permutation tests
# =============================================================================
cat("\nTable V\n")
pt <- rd("07_profile_difference_tests.csv")
pairs5 <- tribble(~g1, ~g2,
  "Lionel Messi", "Robert Lewandowski", "Burak Yilmaz", "Lionel Messi",
  "Burak Yilmaz", "Robert Lewandowski", "Schalke 04", "Bayern Munich")
m5 <- c("paper_insample", "ours_crossfit", "logreg_crossfit")
getp <- function(g1, g2, m) pt %>% filter(group_1 == g1, group_2 == g2, model == m)
check("Table V", "p Messi-Lewandowski (paper / XGBoost / LR)",
      round(sapply(m5, function(m) getp("Lionel Messi", "Robert Lewandowski", m)$p_value), 3), c(0.001, 0.001, 0.033), TOL3)
check("Table V", "p Schalke-Bayern (paper / XGBoost / LR)",
      round(sapply(m5, function(m) getp("Schalke 04", "Bayern Munich", m)$p_value), 3), c(0.455, 0.937, 0.398), TOL3)
p_min <- min(pt$p_value); n_perm <- round(1 / p_min) - 1
log_num("Table V", "caption: permutations", f_n(n_perm), "07_profile_difference_tests.csv",
        "smallest p = 1/(n_perm + 1)", "--")
t5 <- table_head(paste0("Permutation tests (", f_n_tex(n_perm), " permutations) of the difference between two groups' aggregated distance profiles: p-values"),
                 "tab:permutation", "l rrr", size = "\\scriptsize", sep = "3.5pt")
t5 <- c(t5, row_tex("", "Paper", "XGBoost", "LR"),
        row_tex("", "(partial,", "(cross-", "(cross-"),
        row_tex("Pair (shots)", "in-sample)", "fitted)", "fitted)"), "\\midrule")
for (i in seq_len(nrow(pairs5))) {
  g1 <- pairs5$g1[i]; g2 <- pairs5$g2[i]
  r1 <- getp(g1, g2, m5[1])
  lab <- paste0(SHORT_NAME[[g1]], " vs.\\ ", SHORT_NAME[[g2]], " (", r1$n_1, "/", r1$n_2, ")")
  log_num("Table V", paste0(g1, " vs ", g2, ": n_1/n_2"), paste0(r1$n_1, "/", r1$n_2), "07_profile_difference_tests.csv", "n_1, n_2")
  ps <- sapply(m5, function(m) {
    p <- getp(g1, g2, m)$p_value
    txt <- if (abs(p - p_min) < 1e-12) "$\\le$0.001" else f_dp(p, 3)
    b(cell("Table V", paste(g1, "vs", g2, m), txt, "07_profile_difference_tests.csv",
           paste0("group_1 == '", g1, "', group_2 == '", g2, "', model == '", m, "'"),
           if (g1 %in% c("Lionel Messi", "Schalke 04")) "PASS" else "--"), p < 0.05)
  })
  t5 <- c(t5, row_tex(lab, ps[1], ps[2], ps[3]))
}
t5 <- c(t5, "\\bottomrule", "\\end{tabular}",
        table_note("Bold: $p<0.05$. Paper: the parent study's forest with its partial profile, in-sample; XGBoost and LR: geometry-consistent profiles, cross-fitted by match."),
        "\\end{table}")
write_table(t5, "tab05_permutation")

# =============================================================================
# Table VI: player-level evaluation
# =============================================================================
cat("\nTable VI\n")
pool <- rd("07_pool_metrics.csv"); psc <- rd("07_player_season_calibration.csv"); mem <- rd("07_memorisation.csv")
rows6 <- tribble(~model, ~lab, ~mem_key,
  "paper_insample", "Paper's forest, as published", "paper_forest",
  "paper_crossfit", "\\ldots, cross-fitted", NA,
  "paper_crossfit_pc", "\\ldots, cross-fitted + prior-corr.", NA,
  "ours_crossfit", "XGBoost, cross-fitted", "ours_xgboost",
  "logreg_crossfit", "LR, cross-fitted", "ours_logreg",
  "understat", "Understat (ref.)", NA)
d6 <- rows6 %>% left_join(pool %>% select(model, n, auc, ece), by = "model") %>%
  left_join(psc %>% select(model, player_seasons, xg_per_goal, coverage_95, rmse), by = "model") %>%
  left_join(mem %>% select(mem_key = model, memorisation_index), by = "mem_key")
stopifnot(!anyNA(d6$auc), !anyNA(d6$xg_per_goal))
exp6 <- list(paper_insample = c(0.986, 0.1763, 2.41, 9.0, 10.13, 0.2975),
             ours_crossfit = c(0.814, 0.0008, 0.95, 95.9, 2.33, 0.0071),
             logreg_crossfit = c(0.811, 0.0019, 0.95, 95.7, 2.34, 0.0004))
for (m in names(exp6)) {
  r <- d6 %>% filter(model == m)
  act <- c(round(r$auc, 3), round(r$ece, 4), round(r$xg_per_goal, 2), round(100 * r$coverage_95, 1), round(r$rmse, 2),
           round(r$memorisation_index, 4))
  check("Table VI", paste0(m, ": AUC, ECE, xG/goal, coverage %, RMSE, memorisation"), act, exp6[[m]],
        c(TOL3, TOL3, 0.005, TOL1, 0.005, TOL3))
}
n_pool <- unique(d6$n); n_ps <- unique(d6$player_seasons)
stopifnot(length(n_pool) == 1, length(n_ps) == 1)
log_num("Table VI", "caption: pool shots", f_n(n_pool), "07_pool_metrics.csv", "n")
log_num("Table VI", "caption: player-seasons", f_n(n_ps), "07_player_season_calibration.csv", "player_seasons", "PASS (3,122)")
t6 <- table_head(paste0("Player-level evaluation on the ", f_n_tex(n_pool), " train and test shots: discrimination, calibration, and expected vs.\\ actual goals for ",
                        f_n_tex(n_ps), " player-seasons"),
                 "tab:playerlevel", ">{\\raggedright\\arraybackslash}p{2.35cm} rrrrrr", size = "\\scriptsize", sep = "2.6pt")
t6 <- c(t6, row_tex("", "", "", "xG per", "Inside", "RMSE", "Memori-"),
        row_tex("Model", "AUC", "ECE", "goal", "95\\% range", "(goals)", "sation"), "\\midrule")
for (i in seq_len(nrow(d6))) {
  r <- d6[i, ]; flt <- paste0("model == '", r$model, "'"); ck <- if (r$model %in% names(exp6)) "PASS" else "--"
  memtxt <- if (is.na(r$mem_key)) "--" else
    cell("Table VI", paste(r$model, "memorisation"), f_dp(r$memorisation_index, 4), "07_memorisation.csv",
         paste0("model == '", r$mem_key, "'"), ck)
  t6 <- c(t6, row_tex(r$lab,
    cell("Table VI", paste(r$model, "AUC"), f_dp(r$auc, 3), "07_pool_metrics.csv", flt, ck),
    cell("Table VI", paste(r$model, "ECE"), f_dp(r$ece, 4), "07_pool_metrics.csv", flt, ck),
    cell("Table VI", paste(r$model, "xG per goal"), f_dp(r$xg_per_goal, 2), "07_player_season_calibration.csv", flt, ck),
    cell("Table VI", paste(r$model, "inside 95% range"), paste0(f_dp(100 * r$coverage_95, 1), "\\%"),
         "07_player_season_calibration.csv", paste(flt, "coverage_95 x 100"), ck),
    cell("Table VI", paste(r$model, "RMSE"), f_dp(r$rmse, 2), "07_player_season_calibration.csv", flt, ck),
    memtxt))
}
t6 <- c(t6, "\\bottomrule", "\\end{tabular}",
        table_note("Memorisation: mean in-sample minus cross-fitted prediction for goals, minus the same for misses."),
        "\\end{table}")
write_table(t6, "tab06_player_level")

# =============================================================================
# Table VII: out of time
# =============================================================================
cat("\nTable VII\n")
em <- rd("04_era_metrics.csv"); cbe <- rd("04_calibration_by_era.csv")
rows7 <- tribble(~model, ~lab,
  "logistic_regression", "LR", "xgboost", "XGBoost", "random_forest", "RF", "understat_xG", "Understat (ref.)")
emv <- function(m, s, col) em[[col]][em$model == m & em$eval_set == s]
gapv <- function(m, s, k) { g <- cbe$gap[cbe$model == m & cbe$eval_set == s & cbe$bin == k]; if (length(g)) 100 * g else NA_real_ }
check("Table VII", "AUC test -> 2021+ (LR, XGB, RF, Understat)",
      round(unlist(lapply(rows7$model, function(m) c(emv(m, "in_time_test", "auc"), emv(m, "out_of_time", "auc")))), 3),
      c(0.810, 0.799, 0.813, 0.801, 0.807, 0.795, 0.831, 0.820), TOL3)
n_oot <- unique(em$n[em$eval_set == "out_of_time"]); stopifnot(length(n_oot) == 1)
log_num("Table VII", "caption: out-of-time shots", f_n(n_oot), "04_era_metrics.csv", "eval_set == 'out_of_time', n", "matches Table II")
ncol7 <- if (INCLUDE_DECILE9) 5 else 4
t7 <- table_head(paste0("Held-out test set (2014--15 to 2020--21) vs.\\ all shots from 2021--22 onward (", f_n_tex(n_oot), ")"),
                 "tab:outoftime", paste0("l ", strrep("c", ncol7)), size = "\\scriptsize", sep = "2.5pt")
hdr1 <- c("", "", "", "", if (INCLUDE_DECILE9) "Decile 9", "Decile 10")
hdr  <- c("Model", "AUC", "Log loss", "ECE", if (INCLUDE_DECILE9) "gap (pp)", "gap (pp)")
t7 <- c(t7, do.call(row_tex, as.list(hdr1)), do.call(row_tex, as.list(hdr)), "\\midrule")
arrow <- function(a, b) paste0(a, "$\\to$", b)
for (i in seq_len(nrow(rows7))) {
  m <- rows7$model[i]; flt <- paste0("model == '", m, "'")
  ck_auc <- "PASS"
  mcell <- function(col, d, nm) cell("Table VII", paste(m, nm), arrow(f_dp(emv(m, "in_time_test", col), d), f_dp(emv(m, "out_of_time", col), d)),
                                     "04_era_metrics.csv", paste(flt, "eval_set in_time_test / out_of_time", col),
                                     if (col == "auc") ck_auc else "--")
  gcell <- function(k) {
    a <- gapv(m, "in_time_test", k); o <- gapv(m, "out_of_time", k)
    if (is.na(a)) return("--")
    cell("Table VII", paste(m, "decile", k, "gap"), arrow(f_sgn_tex(a, 2), f_sgn_tex(o, 2)), "04_calibration_by_era.csv",
         paste(flt, "bin ==", k, ", gap x 100"), if (k == 10 && m != "understat_xG") "PASS (2021+ value)" else "--")
  }
  cells <- c(rows7$lab[i], mcell("auc", 3, "AUC"), mcell("log_loss", 4, "log loss"), mcell("ece", 4, "ECE"),
             if (INCLUDE_DECILE9) gcell(9), gcell(10))
  t7 <- c(t7, do.call(row_tex, as.list(cells)))
}
t7 <- c(t7, "\\bottomrule", "\\end{tabular}",
        table_note("Each cell: held-out test set $\\to$ 2021--22 onward. Gap: mean predicted xG minus observed goal rate in that decile of predicted xG. Understat is not in the decile file."),
        "\\end{table}")
write_table(t7, "tab07_out_of_time")

# =============================================================================
# floats.tex and main_skeleton.tex
# =============================================================================
cat("\nLaTeX assembly\n")
feas <- rd("07_profile_feasibility.csv")
imp_messi <- feas$impossible[feas$group == "Lionel Messi" & feas$profile == "partial_distance"]
imp_txt <- cell("Fig. 2", "caption: share of impossible points", paste0(f_dp(100 * imp_messi, 1), "\\%"),
                "07_profile_feasibility.csv", "group == 'Lionel Messi', profile == 'partial_distance'", "PASS")
ps5 <- cell("Fig. 5", "caption: player-seasons", f_n_tex(n_ps), "07_player_season_calibration.csv", "player_seasons", "PASS")
log_num("Fig. 3", "caption: 1,000 bootstrap replicates", "1,000", "spec draft caption (not stored in a CSV)", "", "not checkable")
log_num("Fig. 4", "caption: 500 bootstrap replicates", "500", "spec draft caption (not stored in a CSV)", "", "not checkable")
log_num("Fig. 5", "caption: at least 30 shots", "30", "spec draft caption (07's player-season filter)", "", "not checkable")

fig_block <- function(label, file, caption, wide = FALSE, tikz = FALSE) {
  env <- if (wide) "figure*" else "figure"
  body <- if (tikz) "\\resizebox{\\textwidth}{!}{\\input{figures/fig01_study_design.tex}}" else
    paste0("\\includegraphics[width=", if (wide) "\\textwidth" else "\\columnwidth", "]{figures/", file, ".pdf}")
  c(paste0("\\begin{", env, "}[!t]"), "\\centering", body, paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"), paste0("\\end{", env, "}"))
}
tab_block <- function(file) paste0("\\input{tables/", file, ".tex}")

BLOCKS <- list(
  tab1 = list(where = "% place after the paragraph of Sec. II-D (Summary and Research Gap)", tex = tab_block("tab01_studies")),
  fig1 = list(where = "% place after the first mention in Sec. III-C (Reproducing the parent study)",
              tex = fig_block("fig:design", NULL, "Study design. The 2014--15 to 2020--21 shots are split once; the validation set stays sealed until the models are final, and the 2021+ shots are never used for fitting. Each analysis box names the change to the parent study that it implements (Section~IV).", wide = TRUE, tikz = TRUE)),
  tab2 = list(where = "% place after the first mention in Sec. III-A (Data)", tex = tab_block("tab02_data")),
  fig2 = list(where = "% place after the first mention in Sec. IV-C (Change 2: geometry-consistent profiles)",
              tex = fig_block("fig:geometry", "fig02_profile_geometry", paste0("Distance profiles for Lionel Messi's 2020--21 shots. Grey band: (distance, angle) pairs that exist on the pitch under the paper's angle formula. (a) The paper's partial profile moves each shot along a distance grid while holding its angle fixed; red points (", imp_txt, ") are shots that cannot be taken. (b) The geometry-consistent profile slides each shot along its own line to goal and recomputes the angle."), wide = TRUE)),
  tab3 = list(where = "% place after the first mention in Sec. V-A (Does the parent paper's performance hold?)", tex = tab_block("tab03_reproduction")),
  tab4 = list(where = "% place after the first mention in Sec. V-B (Change 1: calibration on held-out shots)", tex = tab_block("tab04_test_set")),
  fig3 = list(where = "% place after the first mention in Sec. V-B",
              tex = fig_block("fig:deciles", "fig03_calibration_deciles", "Calibration gap (mean predicted xG minus observed goal rate) by decile of predicted xG on the held-out test set, with 95\\% match-level bootstrap intervals (1{,}000 replicates). Filled points: interval excludes 0. Understat's xG is in-sample and shown for reference.")),
  fig4 = list(where = "% place after the first mention in Sec. V-C (Change 2: profiles built only from possible shots)",
              tex = fig_block("fig:profiles", "fig04_distance_profiles", "Aggregated distance profiles for (a) the three players the paper studied, 2020--21, and (b) the two teams in the paper's example match. Bands: 95\\% match-level bootstrap intervals (500 replicates). The paper's partial profile (left) is computed in-sample and includes impossible shots; the improved profiles move each shot along its own line to goal and are cross-fitted by match. XGBoost's rise beyond about 35~m is extrapolation where few shots are taken.", wide = TRUE)),
  tab5 = list(where = "% place after the first mention in Sec. V-C", tex = tab_block("tab05_permutation")),
  tab6 = list(where = "% place after the first mention in Sec. V-D (Change 3: assessing players)", tex = tab_block("tab06_player_level")),
  fig5 = list(where = "% place after the first mention in Sec. V-D",
              tex = fig_block("fig:playerseasons", "fig05_player_season_calibration", paste0("Expected vs.\\ actual goals for all ", ps5, " player-seasons with at least 30 shots, 2014--15 to 2020--21. Dark points fall outside the model's own 95\\% range. Labelled: the paper's three example players in 2020--21."), wide = TRUE)),
  fig6 = list(where = "% place after the first mention in Sec. V-D",
              tex = fig_block("fig:focusplayers", "fig06_focus_players", "Goals scored by the paper's three example players in 2020--21 against each model's expected goals and 95\\% range (Poisson-binomial over the player's own shots). Train and test shots only until the validation run.")),
  tab7 = list(where = "% place after the first mention in Sec. V-E (Out of time)", tex = tab_block("tab07_out_of_time")),
  fig7 = list(where = "% place after the first mention in Sec. V-E",
              tex = fig_block("fig:outoftime", "fig07_out_of_time", "Out-of-time performance of models trained on 2014--15 to 2020--21. (a) Log loss by season; in-time seasons are scored on the held-out test set, seasons from 2021--22 on are never used for fitting (2025--26 omitted: partial season). (b) Calibration gap by decile on the in-time test set (solid) and on all 2021+ shots (dashed)."))
)
block_tex <- function(k) c(BLOCKS[[k]]$where, BLOCKS[[k]]$tex, "")
floats <- c("% floats.tex -- every figure and table of the paper, in order of first mention.",
            "% Generated by scripts/10_paper_tables.R; captions are drafts for the authors to edit.",
            "% Preamble needs: \\usepackage{graphicx,booktabs,array,tikz} and \\usetikzlibrary{arrows.meta}.", "",
            unlist(lapply(names(BLOCKS), block_tex)))
write_lf(floats, "paper/floats.tex"); cat("  wrote paper/floats.tex\n")

# ---- main_skeleton.tex ----
main <- readLines("paper/main.tex", warn = FALSE, encoding = "UTF-8")
sub_ <- function(title, todo, floats = character(0), refs = character(0)) {
  c(paste0("\\subsection{", title, "}"), paste0("% TODO: ", todo),
    if (length(refs)) paste0("% Refer to ", paste(refs, collapse = ", "), "."), "",
    unlist(lapply(floats, block_tex)))
}
REF <- function(lbl, kind) paste0(kind, "~\\ref{", lbl, "}")
sections <- c(
  "\\section{Data and Experimental Design}", "",
  sub_("Data", c("Source: Understat shot events, top five leagues (same as the paper); worldfootballR archived Sept 2025 and Understat changed its site, so shots come from worldfootballR's static archive \\cite{understat,worldfootballr}.",
                 "The rebuild matches the paper within 0.2% (Table II): shots, goal rate, matches.",
                 "Features: the paper's seven. Give the paper's angle formula as Eq. (1); Spearman correlation of distance and angle (03_feature_correlation.csv) sets up Change 2.",
                 "Understat's own xG is a reference only (never a feature; in-sample for Understat).",
                 "Coverage in one sentence: Ligue 1 2019-20 ended early; Ligue 1 has 18 clubs from 2023-24; 2025-26 is partial."),
       "tab2", REF("tab:data", "Table")),
  sub_("Evaluation Sets", c("One 70/20/10 split of the 2014-15 to 2020-21 shots, stratified on goal, seed 2026, made once; validation sealed until models are final.",
                            "Out-of-time set: all shots from 2021-22 onward, never used for fitting.",
                            "Be candid: the split is by shot, not by match; analyses needing match independence group by match."),
       refs = REF("tab:data", "Table")),
  sub_("Reproducing the Parent Study", c("Ran the authors' code with forester pinned to its 2022 version and nine patches for package changes.",
                                         "Two protocols: as published (resample everything, then forester's 80/20 split) and a fair benchmark (train on train split, score on test split; resampling only on training rows; prior-corrected versions)."),
       "fig1", REF("fig:design", "Fig.")),
  "\\section{Methods}", "",
  sub_("Change 1a: Calibrated Model Families", c("LR with natural splines (4 df) for distance and angle, separate curves per shot type; why this form; why not Platt/isotonic recalibration; say plainly the form was chosen on the test set.",
                                                 "Random forest (ranger \\cite{wright2017}): 300 trees, min node size 50. XGBoost \\cite{chen2016}: 400 rounds, eta 0.05, depth 5, subsample 0.8, colsample 0.8, min child weight 10.",
                                                 "Baselines: the paper's 12 models + 8 prior-corrected = 20 paper variants (ROSE \\cite{lunardon2014}); Understat as a reference.")),
  sub_("Change 1b: Cross-Fitting", c("Train and test shots split into 5 folds grouped by match; every shot predicted by a model that never saw its match; in-sample versions kept.",
                                     "The paper's forest is also cross-fitted and prior-corrected.")),
  sub_("Change 2: Geometry-Consistent Profiles", c("Define aggregated (ceteris-paribus) profiles \\cite{biecek2021}.",
                                                   "The paper's partial profile moves distance over a 97-point grid at fixed angle; feasibility condition; example: from 30 m the goal opens at most 13.9 deg but Messi's mean shot angle is 21.7 deg (Fig. 2a).",
                                                   "Improved distance profile: slide each shot along its own line to goal (4-40 m), recompute angle, drop off-pitch points. Improved angle profile: move along the arc, keep reachable angles (>=10 shots and >=10% of the group).",
                                                   "Why the grid starts at 4 m: the angle formula folds within 3.66 m. Cite Hooker et al. \\cite{hooker2021} on extrapolation."),
       "fig2", REF("fig:geometry", "Fig.")),
  sub_("Change 3: Uncertainty and Player Assessment", c("Profile bands: 500 match-level bootstrap replicates. Permutation test: 1,000 permutations of group labels on the mean absolute gap.",
                                                        "Expected goals with a 95% range: Poisson-binomial over a player's shots; verdict above / in line / below.",
                                                        "Player-season calibration: player-seasons with at least 30 shots; xG per goal, RMSE, coverage. Memorisation index.")),
  sub_("Out-of-Time Test", c("Models trained on 2014-20 scored on 2021-22 onward; scoring Understat on the same shots separates model ageing from the sport changing; covariate shift via PSI.")),
  sub_("Metrics and Tests", c("AUC, Brier, log loss, ECE (10 equal-frequency bins); calibration intercept and slope \\cite{vancalster2019}; decile gaps with 1,000-replicate match-level bootstrap intervals; DeLong tests with Holm adjustment \\cite{delong1988}; repeated CV 5x3 on a 50,000-shot subsample.",
                              "Threshold metrics not used: at a 10.7% base rate almost nothing but penalties clears 0.5.")),
  "\\section{Results}", "",
  sub_("Does the Parent Paper's Performance Hold?", c("Reported metrics match each model's score on its own training rows.",
                                                      "Over-sampling before the split leaks (duplicate shares in the Table III note).",
                                                      "On the clean test set the over-sampled forest has the lowest AUC of all 24 models; raw resampled models are badly calibrated."),
       "tab3", REF("tab:reproduction", "Table")),
  sub_("Change 1: Calibration on Held-Out Shots", c("AUC: XGBoost vs LR vs RF (Understat in-sample); DeLong p after Holm.",
                                                    "LR has the lowest ECE of the 24 models benchmarked, level with the best of the paper's 20 variants.",
                                                    "Fig. 3: LR is the only model with no decile gap beyond its 95% interval; intercepts and slopes (Table IV).",
                                                    "Subgroups (08_subgroup_gaps.csv) in one or two sentences: penalties over-predicted; header fix held; XGBoost over-predicts tap-ins."),
       c("tab4", "fig3"), c(REF("tab:testset", "Table"), REF("fig:deciles", "Fig."))),
  sub_("Change 2: Profiles Built Only from Possible Shots", c("Share of impossible points in the paper's profiles (Fig. 2).",
                                                              "Fig. 4(a): the paper's profile stays flat past 20 m; the improved profiles fall to 0.02-0.05 by 25-30 m. Supports hypothesis part 1.",
                                                              "Players converge; Table V: only Messi vs Lewandowski still differ under the improved models. Supports hypothesis part 2.",
                                                              "Fig. 4(b): Schalke vs Bayern differ under no model. Caveat: XGBoost's curve rises past ~35 m."),
       c("fig4", "tab5"), c(REF("fig:profiles", "Fig."), REF("tab:permutation", "Table"))),
  sub_("Change 3: Assessing Players", c("Table VI / Fig. 5: xG per goal, coverage of the 95% range, RMSE, memorisation.",
                                        "Fig. 6: focus players' verdicts; supports hypothesis part 3. Optional non-penalty nuance for Yilmaz.",
                                        "Note the shot counts leave out the 10% validation shots until the final run."),
       c("tab6", "fig5", "fig6"), c(REF("tab:playerlevel", "Table"), REF("fig:playerseasons", "Fig."), REF("fig:focusplayers", "Fig."))),
  sub_("Out of Time", c("Every model loses about 0.011 AUC after 2021 (Table VII).",
                        "Calibration drifts: ECE rises; the top two deciles are over-predicted.",
                        "PSI below 0.02 for every feature: concept drift, not covariate drift \\cite{gama2014}.",
                        "Frozen models vs Understat."),
       c("tab7", "fig7"), c(REF("tab:outoftime", "Table"), REF("fig:outoftime", "Fig."))),
  "\\section{Discussion and Limitations}",
  "% TODO: Answer the research question in one paragraph.",
  "% TODO: Why the paper's model failed: evaluation on training rows, resampling before the split, memorisation \\cite{kapoor2023,goorbergh2022}.",
  "% TODO: Which improved model to use (LR vs XGBoost).",
  "% TODO: What a coach can read: a profile shows a player's shot mix, not finishing; goals vs the 95% range is the finishing test.",
  "% TODO: Drift: retrain or recalibrate on recent seasons.",
  "% TODO: Limitations: no tracking data; no player term; some Understat locations look wrong; split by shot; LR form chosen on the test set; validation set not yet used; penalties over-predicted; Understat in-sample; three players and one match; 2025-26 partial; the paper's forest needs ~1 h and 8 GB RAM.",
  "% TODO: Future work: validation run, recalibration on recent seasons, per-player finishing skill, tracking data.", "",
  "\\section{Conclusion}",
  "% TODO: Mirror the midterm's conclusion slide: the idea (aggregated profiles) is useful; the parent model over-predicts goals and explains shots it already saw; keep the idea and fix what is under it (calibrated model, cross-fitting, possible shots only, intervals); a player assessment you can trust. One line on drift.", "")

bib_new <- c(
  "% --- added from PAPER_OUTLINE.md section 8 ---",
  "\\bibitem{pollard1997} R. Pollard and C. Reep, ``Measuring the effectiveness of playing strategies at soccer,'' \\textit{J. Roy. Stat. Soc. D (The Statistician)}, vol. 46, no. 4, pp. 541--550, 1997, doi: 10.1111/1467-9884.00108.",
  "\\bibitem{rathke2017} A. Rathke, ``An examination of expected goals and shot efficiency in soccer,'' \\textit{J. Hum. Sport Exerc.}, vol. 12, no. 2, pp. S514--S529, 2017, doi: 10.14198/jhse.2017.12.Proc2.05.",
  "\\bibitem{understat} Understat. Accessed: Sep. 25, 2026. [Online]. Available: https://understat.com/",
  "% FLAG: fill in the worldfootballR version from packageVersion(\"worldfootballR\").",
  "\\bibitem{worldfootballr} J. Zivkovic, \\textit{worldfootballR: Extract and Clean World Football (Soccer) Data}, R package version X.Y.Z. [Online]. Available: https://github.com/JaseZiv/worldfootballR",
  "\\bibitem{lunardon2014} N. Lunardon, G. Menardi, and N. Torelli, ``ROSE: A package for binary imbalanced learning,'' \\textit{R J.}, vol. 6, no. 1, pp. 79--89, 2014, doi: 10.32614/RJ-2014-008.",
  "\\bibitem{chen2016} T. Chen and C. Guestrin, ``XGBoost: A scalable tree boosting system,'' in \\textit{Proc. 22nd ACM SIGKDD Int. Conf. Knowl. Discov. Data Min.}, San Francisco, CA, USA, 2016, pp. 785--794, doi: 10.1145/2939672.2939785.",
  "\\bibitem{wright2017} M. N. Wright and A. Ziegler, ``ranger: A fast implementation of random forests for high dimensional data in C++ and R,'' \\textit{J. Stat. Softw.}, vol. 77, no. 1, pp. 1--17, 2017, doi: 10.18637/jss.v077.i01.",
  "\\bibitem{biecek2021} P. Biecek and T. Burzykowski, \\textit{Explanatory Model Analysis: Explore, Explain, and Examine Predictive Models}. Boca Raton, FL, USA: Chapman \\& Hall/CRC, 2021.",
  "\\bibitem{hooker2021} G. Hooker, L. Mentch, and S. Zhou, ``Unrestricted permutation forces extrapolation: Variable importance requires at least one more model, or there is no free variable importance,'' \\textit{Stat. Comput.}, vol. 31, no. 6, Art. no. 82, 2021, doi: 10.1007/s11222-021-10057-z.",
  "\\bibitem{vancalster2019} B. Van Calster, D. J. McLernon, M. van Smeden, L. Wynants, and E. W. Steyerberg, ``Calibration: The Achilles heel of predictive analytics,'' \\textit{BMC Med.}, vol. 17, Art. no. 230, 2019, doi: 10.1186/s12916-019-1466-7.",
  "\\bibitem{delong1988} E. R. DeLong, D. M. DeLong, and D. L. Clarke-Pearson, ``Comparing the areas under two or more correlated receiver operating characteristic curves: A nonparametric approach,'' \\textit{Biometrics}, vol. 44, no. 3, pp. 837--845, 1988, doi: 10.2307/2531595.",
  "\\bibitem{gama2014} J. Gama, I. \\v{Z}liobait\\.{e}, A. Bifet, M. Pechenizkiy, and A. Bouchachia, ``A survey on concept drift adaptation,'' \\textit{ACM Comput. Surv.}, vol. 46, no. 4, Art. no. 44, 2014, doi: 10.1145/2523813.")

i_pkg <- which(main == "\\usepackage{xcolor}")
i_bib <- which(main == "\\begin{thebibliography}{00}")
i_end <- which(main == "\\end{thebibliography}")
stopifnot(length(i_pkg) == 1, length(i_bib) == 1, length(i_end) == 1, i_pkg < i_bib, i_bib < i_end)
skel <- c("% main_skeleton.tex -- generated by scripts/10_paper_tables.R from paper/main.tex.",
          "% Sections I-II are unchanged from main.tex; Sections III-VII are stubs with the floats placed.",
          main[1:i_pkg],
          "\\usepackage{booktabs}", "\\usepackage{array}", "\\usepackage{tikz}", "\\usetikzlibrary{arrows.meta}",
          main[(i_pkg + 1):(i_bib - 1)],
          "", block_tex("tab1"),
          sections,
          main[i_bib:(i_end - 1)], bib_new, main[i_end:length(main)])
write_lf(skel, "paper/main_skeleton.tex"); cat("  wrote paper/main_skeleton.tex\n")

write_numbers()
cat("\nDone: Tables II-VII in", TAB_DIR, "+ paper/floats.tex + paper/main_skeleton.tex\n")
