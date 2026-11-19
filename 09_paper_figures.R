# 09_paper_figures.R -- builds Figs. 2-7 for the paper from the results CSVs.
# Run from the kit root:  Rscript scripts/09_paper_figures.R
# Reads only CSVs (and, in repo mode, train/test splits). Never reads validation.
source("scripts/common.R")
SCRIPT_ID <- "09_paper_figures"

CUT_XGB_AT <- Inf   # Fig. 4: set to 35 to stop XGBoost's curves where shots get sparse

# =============================================================================
# Fig. 2: why the paper's profiles leave the pitch
# =============================================================================
cat("\nFig. 2\n")
shots <- load_focus_shots()
prof  <- rd("07_profiles.csv")
feas  <- rd("07_profile_feasibility.csv")

grid <- prof %>%
  filter(model == "paper_insample", profile == "partial_distance", group == "Lionel Messi") %>%
  pull(x) %>% unique() %>% sort()
messi <- shots %>% filter(group == "Lionel Messi")

check("Fig. 2", "Messi 2020-21 train+test shots", nrow(messi), 178)
check("Fig. 2", "Messi mean angleToGoal (deg)", round(mean(messi$angleToGoal), 1), 21.7, TOL1)
check("Fig. 2", "grid points", length(grid), 97)
check("Fig. 2", "grid first, last (m)", round(range(grid), 3), c(0.322, 84.490), TOL3)
check("Fig. 2", "max_angle_at(30) (deg)", round(max_angle_at(30), 1), 13.9, TOL1)

# recount the impossible share of the paper's partial distance profile per group
recount <- shots %>%
  select(group, row_id, angleToGoal) %>%
  tidyr::crossing(grid_d = grid) %>%
  group_by(group) %>%
  summarise(recount = mean(!is_feasible(grid_d, angleToGoal)), .groups = "drop")
feas_pd <- feas %>% filter(profile == "partial_distance") %>% select(group, impossible) %>%
  left_join(recount, by = "group")
exp_imp <- c("Lionel Messi" = 0.2969, "Robert Lewandowski" = 0.5436, "Burak Yilmaz" = 0.3314,
             "Schalke 04" = 0.3751, "Bayern Munich" = 0.3772)
for (g in names(exp_imp)) {
  r <- feas_pd %>% filter(group == g)
  check("Fig. 2", paste0("impossible share, ", g, ": CSV"), round(r$impossible, 4), exp_imp[[g]], TOL3)
  check("Fig. 2", paste0("impossible share, ", g, ": recount"), round(r$recount, 4), exp_imp[[g]], TOL3)
}

imp_messi <- feas_pd$impossible[feas_pd$group == "Lionel Messi"]
imp_lab   <- paste0(f_dp(100 * imp_messi, 1), "%")
log_num("Fig. 2", "annotation (a): share of impossible points", imp_lab,
        "07_profile_feasibility.csv", "group == 'Lionel Messi', profile == 'partial_distance', impossible x 100",
        "matches recount and section 7")

band <- tibble(d = seq(0.5, 85, by = 0.1)) %>% mutate(lo = min_angle_at(d), hi = max_angle_at(d))

pts_a <- messi %>% select(row_id, angleToGoal) %>% tidyr::crossing(grid_d = grid) %>%
  mutate(ok = is_feasible(grid_d, angleToGoal))
stopifnot(nrow(pts_a) == length(grid) * nrow(messi))   # 178 x 97 = 17,266

paths_b <- messi %>%
  mutate(dx = 105 - X * 105, dy = 34 - Y * 68, theta = atan2(dy, dx)) %>%
  select(row_id, theta) %>%
  tidyr::crossing(dp = seq(4, 40, by = 0.5)) %>%
  mutate(ndx = dp * cos(theta), ndy = dp * sin(theta), ang = paper_angle(ndx, ndy)) %>%
  filter(abs(ndy) <= 34, ndx <= 105) %>%
  arrange(row_id, dp)

geo_base <- function() {
  list(geom_ribbon(data = band, aes(x = d, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = "grey85"),
       scale_x_continuous("Distance to goal (m)", limits = c(0, 85), breaks = seq(0, 80, 20), expand = c(0, 0)),
       scale_y_continuous("Angle to goal (°)", limits = c(0, 90), breaks = seq(0, 90, 30), expand = c(0, 0)),
       theme_paper())
}
p2a <- ggplot() + geo_base() +
  geom_point(data = pts_a, aes(grid_d, angleToGoal, colour = ok), size = 0.25, stroke = 0, shape = 16) +
  scale_colour_manual(values = c(`TRUE` = "grey55", `FALSE` = COL[["paper"]]), guide = "none") +
  geom_point(data = messi, aes(distanceToGoal, angleToGoal), colour = "black", size = 0.6, stroke = 0, shape = 16) +
  annotate("text", x = 83, y = 86, hjust = 1, vjust = 1, size = 7 * PT, family = "serif",
           label = paste0(imp_lab, " of points are impossible shots"))
p2b <- ggplot() + geo_base() +
  geom_path(data = paths_b, aes(dp, ang, group = row_id), colour = COL[["xgboost"]], alpha = 0.25, linewidth = 0.2) +
  geom_point(data = messi, aes(distanceToGoal, angleToGoal), colour = "black", size = 0.6, stroke = 0, shape = 16) +
  annotate("text", x = 83, y = 86, hjust = 1, vjust = 1, size = 7 * PT, family = "serif",
           label = "every point is a real spot on the pitch")
fig2 <- (p2a | p2b) + plot_annotation(tag_levels = list(c("(a)", "(b)"))) &
  theme(plot.tag = element_text(size = 8, family = "serif", face = "bold"))
save_fig(fig2, "fig02_profile_geometry", W2, 2.7)

# =============================================================================
# Fig. 3: decile calibration with 95% intervals
# =============================================================================
cat("\nFig. 3\n")
dec <- rd("08_decile_gaps_ci.csv")
m3  <- c("logistic_regression", "xgboost", "random_forest", "understat_xG")
beyond <- function(m) dec %>% filter(model == m, beyond_noise) %>% pull(bin) %>% sort()
check("Fig. 3", "LR deciles beyond noise", length(beyond("logistic_regression")), 0)
check("Fig. 3", "XGBoost deciles beyond noise", beyond("xgboost"), 10)
check("Fig. 3", "RF deciles beyond noise", beyond("random_forest"), c(1, 10))
check("Fig. 3", "Understat deciles beyond noise", beyond("understat_xG"), c(4, 9, 10))
lr10 <- dec %>% filter(model == "logistic_regression", bin == 10)
check("Fig. 3", "LR decile 10 gap [lo, hi] (pp)", round(100 * c(lr10$gap, lr10$lo, lr10$hi), 2),
      c(0.89, -0.30, 2.08), 0.005)

d3 <- dec %>% filter(model %in% m3) %>%
  mutate(lab = factor(MODEL_LAB[model], levels = MODEL_LAB[m3]))
fig3 <- ggplot(d3, aes(bin, 100 * gap, colour = model)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.3, linewidth = 0.35) +
  geom_point(aes(shape = beyond_noise), fill = "white", size = 1.3, stroke = 0.45) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  scale_colour_manual(values = MODEL_COL[m3], guide = "none") +
  scale_x_continuous("Decile of predicted xG", breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous("Gap: predicted − observed (pp)") +
  facet_wrap(~lab, ncol = 2) +
  theme_paper()
save_fig(fig3, "fig03_calibration_deciles", W1, 3.0)

# =============================================================================
# Fig. 4: aggregated distance profiles, paper vs improved
# =============================================================================
cat("\nFig. 4\n")
cols4 <- tibble(model   = c("paper_insample", "ours_crossfit", "logreg_crossfit"),
                profile = c("partial_distance", "gc_distance", "gc_distance"),
                col_lab = c("Paper's forest: partial profile, in-sample",
                            "XGBoost: geometry-consistent, cross-fitted",
                            "Logistic regression: geometry-consistent, cross-fitted"))
d4_all <- prof %>% inner_join(cols4, by = c("model", "profile")) %>%
  mutate(col_lab = factor(col_lab, levels = cols4$col_lab))
# strip text: break after the colon so labels fit a 2.2 in panel
strip4 <- labeller(col_lab = function(v) sub(": ", ":
", v))

# checks: ap at the grid point nearest 30 m
exp30 <- tribble(~group, ~paper_insample, ~ours_crossfit, ~logreg_crossfit,
                 "Lionel Messi", 0.255, 0.031, 0.029,
                 "Robert Lewandowski", 0.268, 0.020, 0.029,
                 "Burak Yilmaz", 0.235, 0.024, 0.032)
at30 <- d4_all %>% filter(kind == "player") %>% group_by(group, model) %>%
  slice_min(abs(x - 30), n = 1, with_ties = FALSE) %>% ungroup()
for (i in seq_len(nrow(exp30))) for (m in cols4$model) {
  v <- at30 %>% filter(group == exp30$group[i], model == m) %>% pull(ap)
  check("Fig. 4", paste0("ap nearest 30 m, ", exp30$group[i], ", ", m), round(v, 3), exp30[[m]][i], TOL3)
}
far <- d4_all %>% filter(kind == "player", model == "paper_insample", x >= 20)
AB  <- round(range(far$ap), 2)
check("Fig. 4", "paper ap range for x >= 20 (3 players)", AB, c(0.23, 0.29), 0.005)
grid_end <- f_dp(max(far$x), 1)
ann4 <- paste0("stays at ", f_dp(AB[1], 2), "–", f_dp(AB[2], 2), "
out to ", grid_end, " m")
log_num("Fig. 4", "annotation (a), column 1", ann4, "07_profiles.csv",
        "kind == 'player', model == 'paper_insample', profile == 'partial_distance', x >= 20 (incl. 84.5 m point); min/max of ap, 2 dp; end = max(x)",
        "PASS (0.23-0.29)")

d4 <- d4_all %>% filter(shown, x <= 40) %>%
  filter(!(model == "ours_crossfit" & x > CUT_XGB_AT))

row4 <- function(k, pal, ann = NULL) {
  dd <- d4 %>% filter(kind == k) %>% mutate(group = factor(group, levels = names(pal)))
  ymax <- max(dd$hi)
  p <- ggplot(dd, aes(x, ap, colour = group, fill = group)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.45) +
    scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
    scale_x_continuous("Distance to goal (m)", limits = c(0, 40), breaks = seq(0, 40, 10), expand = c(0, 0.5)) +
    scale_y_continuous("Average predicted xG", limits = c(0, ymax), expand = expansion(mult = c(0, 0.03))) +
    facet_wrap(~col_lab, nrow = 1, labeller = strip4) +
    theme_paper() + theme(legend.key.width = unit(4, "mm"))
  if (!is.null(ann)) p <- p + geom_text(data = tibble(col_lab = factor(cols4$col_lab[1], levels = cols4$col_lab)),
                                        aes(x = 39, y = 0.47 * ymax, label = ann), inherit.aes = FALSE,
                                        hjust = 1, lineheight = 0.9, size = 7 * PT, family = "serif")
  p
}
fig4 <- (row4("player", PLAYER_COL, ann4) / row4("match", TEAM_COL)) +
  plot_annotation(tag_levels = list(c("(a)", "(b)"))) &
  theme(plot.tag = element_text(size = 8, family = "serif", face = "bold"))
save_fig(fig4, "fig04_distance_profiles", W2, 4.3)

# =============================================================================
# Fig. 5: player-season calibration
# =============================================================================
cat("\nFig. 5\n")
m5  <- c("paper_insample", "paper_crossfit_pc", "ours_crossfit", "logreg_crossfit")
psx <- rd("07_player_season_xg.csv") %>% filter(model %in% m5)
psc <- rd("07_player_season_calibration.csv")
for (m in m5) check("Fig. 5", paste0("rows, ", m), sum(psx$model == m), 3122)
exp5 <- tribble(~model, ~cov, ~xpg,
                "paper_insample", 0.090, 2.41, "paper_crossfit_pc", 0.748, 0.59,
                "ours_crossfit", 0.959, 0.95, "logreg_crossfit", 0.957, 0.95)
for (i in seq_len(nrow(exp5))) {
  r <- psc %>% filter(model == exp5$model[i])
  check("Fig. 5", paste0("coverage_95, ", exp5$model[i]), round(r$coverage_95, 3), exp5$cov[i], TOL3)
  check("Fig. 5", paste0("xg_per_goal, ", exp5$model[i]), round(r$xg_per_goal, 2), exp5$xpg[i], 0.005)
}
strip5 <- psc %>% filter(model %in% m5) %>%
  mutate(lab = paste0(str_wrap(MODEL_LAB[model], 30), "\nxG per goal ", f_dp(xg_per_goal, 2),
                      "\ninside 95% range ", f_dp(100 * coverage_95, 1), "%"))
for (i in seq_len(nrow(strip5))) {
  log_num("Fig. 5", paste0("strip, ", strip5$model[i], ": xG per goal"), f_dp(strip5$xg_per_goal[i], 2),
          "07_player_season_calibration.csv", paste0("model == '", strip5$model[i], "'"), "PASS")
  log_num("Fig. 5", paste0("strip, ", strip5$model[i], ": inside 95% range"),
          paste0(f_dp(100 * strip5$coverage_95[i], 1), "%"),
          "07_player_season_calibration.csv", paste0("model == '", strip5$model[i], "', coverage_95 x 100"), "PASS")
}
labs5 <- setNames(strip5$lab, strip5$model)[m5]
psx <- psx %>% mutate(lab = factor(labs5[model], levels = labs5))
lim5 <- max(c(psx$xg, psx$goals))
foc5 <- psx %>% filter(player_id %in% FOCUS_IDS, season_start == 2020) %>%
  mutate(pname = names(FOCUS_IDS)[match(player_id, FOCUS_IDS)], short = SHORT_NAME[pname],
         left = xg > lim5 / 2, lx = ifelse(left, xg - 2.5, xg + 2.5), hj = ifelse(left, 1, 0))
stopifnot(nrow(foc5) == 3 * length(m5))
fig5 <- ggplot(psx, aes(xg, goals)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed", linewidth = 0.3) +
  geom_point(data = psx %>% filter(inside), colour = "grey70", size = 0.35, stroke = 0, shape = 16) +
  geom_point(data = psx %>% filter(!inside), colour = "grey25", size = 0.35, stroke = 0, shape = 16) +
  geom_point(data = foc5, aes(colour = pname), size = 1.4, stroke = 0, shape = 16) +
  geom_text(data = foc5, aes(x = lx, label = short, colour = pname, hjust = hj), size = 7 * PT,
            family = "serif") +
  scale_colour_manual(values = PLAYER_COL, guide = "none") +
  scale_x_continuous("Expected goals", limits = c(0, lim5), expand = c(0, 1)) +
  scale_y_continuous("Goals scored", limits = c(0, lim5), expand = c(0, 1)) +
  coord_equal() +
  facet_wrap(~lab, nrow = 1) +
  theme_paper()
save_fig(fig5, "fig05_player_season_calibration", W2, 2.35)

# =============================================================================
# Fig. 6: the paper's three players, goals vs 95% range
# =============================================================================
cat("\nFig. 6\n")
m6 <- c("paper_insample", "ours_crossfit", "logreg_crossfit", "understat")
pa <- rd("07_player_assessment.csv") %>% filter(shots_used == "all shots", model %in% m6)
chk6 <- function(p, m, e) {
  r <- pa %>% filter(player == p, model == m)
  check("Fig. 6", paste0("xg [lo, hi], ", p, ", ", m), c(round(r$xg, 1), r$lo, r$hi), e, TOL1)
}
chk6("Lionel Messi", "paper_insample", c(62.4, 52, 73))
chk6("Robert Lewandowski", "ours_crossfit", c(26.5, 19, 34))
chk6("Burak Yilmaz", "logreg_crossfit", c(9.0, 5, 14))
chk6("Lionel Messi", "understat", c(25.2, 18, 33))

facet6 <- pa %>% distinct(player, goals, shots) %>%
  mutate(flab = paste0(player, ": ", goals, " goals from ", shots, " shots"),
         player = factor(player, levels = names(FOCUS_IDS))) %>% arrange(player)
stopifnot(nrow(facet6) == 3)
for (i in seq_len(nrow(facet6)))
  log_num("Fig. 6", paste0("facet label, ", facet6$player[i]), facet6$flab[i], "07_player_assessment.csv",
          "shots_used == 'all shots'; goals, shots", "Messi 178 shots: PASS (Fig. 2 check)")
d6 <- pa %>% left_join(facet6 %>% select(player = player, flab) %>% mutate(player = as.character(player)), by = "player") %>%
  mutate(flab = factor(flab, levels = facet6$flab),
         mlab = factor(MODEL_LAB[model], levels = rev(MODEL_LAB[m6])))
vl6 <- facet6 %>% mutate(flab = factor(flab, levels = facet6$flab))
fig6 <- ggplot(d6, aes(xg, mlab, colour = model)) +
  geom_vline(data = vl6, aes(xintercept = goals), colour = "black", linewidth = 0.45) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.6) +
  geom_point(size = 1.4, shape = 16) +
  scale_colour_manual(values = MODEL_COL[m6], limits = m6, labels = MODEL_LAB[m6],
                      guide = guide_legend(nrow = 2)) +
  scale_x_continuous("Goals scored / expected goals", limits = c(0, NA), expand = expansion(mult = c(0, 0.03))) +
  scale_y_discrete(NULL, labels = NULL) +
  facet_wrap(~flab, ncol = 1) +
  theme_paper() + theme(axis.ticks.y = element_blank(), panel.grid.major.y = element_blank())
save_fig(fig6, "fig06_focus_players", W1, 2.9)

# =============================================================================
# Fig. 7: out of time
# =============================================================================
cat("\nFig. 7\n")
m7 <- c("logistic_regression", "xgboost", "random_forest", "understat_xG")
bys <- rd("04_by_season_metrics.csv") %>% filter(model %in% m7, season_start >= 2014, season_start <= 2024)
xg_ll <- bys %>% filter(model == "xgboost", season_start %in% 2021:2024) %>% arrange(season_start) %>% pull(log_loss)
check("Fig. 7", "XGBoost log loss 2021-2024", round(xg_ll, 4), c(0.2711, 0.2724, 0.2726, 0.2798), TOL3)
cbe <- rd("04_calibration_by_era.csv")
oot10 <- cbe %>% filter(eval_set == "out_of_time", bin == 10)
check("Fig. 7", "out-of-time decile 10 gap LR / XGBoost / RF (pp)",
      round(100 * oot10$gap[match(m7[1:3], oot10$model)], 2), c(2.52, 2.35, 2.67), 0.005)

season_lab <- function(s) sprintf("%02d–%02d", s %% 100, (s + 1) %% 100)
col7 <- scale_colour_manual(values = MODEL_COL[m7], limits = m7, labels = MODEL_LAB[m7],
                            guide = guide_legend(nrow = 2, override.aes = list(linetype = c(1, 1, 1, 2))))
p7a <- ggplot(bys, aes(season_start, log_loss, colour = model)) +
  annotate("rect", xmin = 2020.5, xmax = 2024.5, ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("text", x = 2022.5, y = Inf, vjust = 1.4, label = "out of time", size = 7 * PT, family = "serif") +
  geom_line(aes(linetype = model == "understat_xG"), linewidth = 0.45) +
  geom_point(size = 0.8, shape = 16) +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
  col7 +
  scale_x_continuous(NULL, breaks = 2014:2024, labels = season_lab, expand = c(0, 0.3)) +
  scale_y_continuous("Log loss", expand = expansion(mult = c(0.05, 0.15))) +
  theme_paper() + theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1))
d7b <- cbe %>% filter(model %in% m7) %>%
  mutate(era = factor(ifelse(eval_set == "in_time_test", "In-time test (2014–21)", "2021+ (out of time)"),
                      levels = c("In-time test (2014–21)", "2021+ (out of time)")))
p7b <- ggplot(d7b, aes(bin, 100 * gap, colour = model, linetype = era)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(linewidth = 0.45) + geom_point(size = 0.8, shape = 16) +
  col7 +
  scale_linetype_manual(values = c("solid", "dashed"),
                        guide = guide_legend(nrow = 1, override.aes = list(colour = "grey30"))) +
  scale_x_continuous("Decile of predicted xG", breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous("Gap: predicted − observed (pp)") +
  theme_paper()
fig7 <- (p7a / p7b) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("(a)", "(b)"))) &
  theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(0, "mm"),
        plot.tag.position = "top",
        plot.tag = element_text(size = 8, family = "serif", face = "bold", hjust = 0, margin = margin(0, 0, 2, 0)))
save_fig(fig7, "fig07_out_of_time", W1, 3.8)

write_numbers()
cat("\nDone: Figs. 2-7 in", FIG_DIR, "\n")
