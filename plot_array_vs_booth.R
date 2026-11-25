# ==============================================================================
# Array vs Booth Architecture - Power & Energy Analysis Plots
# Data source: Array_power_summary.csv & Booth_power_summary.csv (30 batches)
# ==============================================================================

# --- Display / Export Settings ---
# Set save_png <- TRUE to save wide, high-res (300 DPI) 3-panel PNG files.
# When save_png <- FALSE (for online runners), one_plot_per_page <- TRUE
# prevents the online runner from squishing 3 plots into a tiny narrow box.
save_png <- FALSE
one_plot_per_page <- TRUE

if (exists("options")) {
  options(repr.plot.width = 16, repr.plot.height = 5.5)
}

start_fig <- function(filename, width_in = 16, height_in = 5.5) {
  if (save_png) {
    png(filename, width = width_in, height = height_in, units = "in", res = 300)
    par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3.2, 1.5), cex.main = 1.05)
  } else if (one_plot_per_page) {
    par(mfrow = c(1, 1), mar = c(4.8, 4.8, 3.5, 1.5), cex.main = 1.15, cex.lab = 1.05)
  } else {
    par(mfrow = c(1, 3), mar = c(4.5, 4.2, 3.0, 1.2), cex.main = 0.95)
  }
}

end_fig <- function() {
  if (save_png) dev.off()
}

# ==============================================================================
# 1. Dataset: Array vs Booth (30 batches)
# ==============================================================================
batch <- 1:30

# Note: Batches 1-6 are 0.0 in the repo CSVs (change 0 -> 1 if matching reference image)
array_dyn_power  <- c(rep(0, 6), rep(4, 6), rep(8, 6), rep(6, 6), rep(9, 6))
booth_dyn_power  <- c(rep(0, 6), rep(3, 6), rep(7, 6), rep(5, 6), rep(8, 6))

array_tot_power  <- c(rep(103, 6), rep(107, 6), rep(111, 6), rep(109, 6), rep(112, 6))
booth_tot_power  <- c(rep(103, 6), rep(106, 6), rep(110, 6), rep(108, 6), rep(111, 6))

array_dyn_energy <- c(rep(0, 6), rep(28.62, 6), rep(57.24, 6), rep(42.93, 6), rep(64.395, 6))
booth_dyn_energy <- c(rep(0, 6), rep(16.131, 6), rep(37.639, 6), rep(26.885, 6), rep(43.016, 6))

col_array <- "#2b6cb0"
col_booth <- "#e6842a"

# ==============================================================================
# FIGURE 1: Boxplot, Dynamic Energy Histogram + KDE, Batch-Series Profile
# ==============================================================================
start_fig("figure1_power_energy.png")

# (a) Dynamic Power Distribution (Boxplot)
boxplot(
  list(Array = array_dyn_power, Booth = booth_dyn_power),
  col = c(col_array, col_booth),
  ylab = "Dynamic Power (mW)",
  main = "(a) Dynamic Power Distribution (Boxplot)",
  ylim = c(0, 9.5),
  las = 1
)
grid(nx = NA, ny = NULL, lty = 2, col = "gray80")
stripchart(
  list(Array = array_dyn_power, Booth = booth_dyn_power),
  vertical = TRUE, method = "jitter", jitter = 0.08,
  pch = 16, col = rgb(0, 0, 0, 0.7), add = TRUE
)

# (b) Dynamic Energy Histogram + KDE
breaks <- seq(0, 65, length.out = 6)
bin_width <- diff(breaks)[1]
h_arr <- hist(array_dyn_energy, breaks = breaks, plot = FALSE)
h_boo <- hist(booth_dyn_energy, breaks = breaks, plot = FALSE)
ymax <- max(h_arr$counts, h_boo$counts) * 1.20

hist(
  array_dyn_energy, breaks = breaks, ylim = c(0, ymax),
  col = rgb(0.17, 0.42, 0.69, 0.35), border = col_array,
  xlab = "Dynamic Energy (pJ)", ylab = "Batch Frequency",
  main = "(b) Dynamic Energy Histogram", las = 1
)
grid(nx = NA, ny = NULL, lty = 2, col = "gray80")
hist(
  booth_dyn_energy, breaks = breaks, add = TRUE,
  col = rgb(0.90, 0.52, 0.16, 0.35), border = col_booth
)

d_arr <- density(array_dyn_energy, from = 0, to = 65)
d_boo <- density(booth_dyn_energy, from = 0, to = 65)
lines(d_arr$x, d_arr$y * length(array_dyn_energy) * bin_width, col = col_array, lwd = 2)
lines(d_boo$x, d_boo$y * length(booth_dyn_energy) * bin_width, col = col_booth, lwd = 2)

legend(
  "topright", title = "Architecture",
  legend = c("Array", "Booth"),
  fill = c(rgb(0.17, 0.42, 0.69, 0.35), rgb(0.90, 0.52, 0.16, 0.35)),
  border = c(col_array, col_booth), bg = "white", cex = 0.9
)

# (c) Batch-Series Power Profile (5 Clusters)
plot(
  batch, array_dyn_power, type = "o", pch = 16, col = col_array, lwd = 2,
  ylim = c(0, 9.5), xlab = "Test Batch Index (1 to 30)",
  ylab = "Dynamic Power (mW)",
  main = "(c) Batch-Series Power Profile (5 Clusters)", las = 1
)
grid(lty = 2, col = "gray80")
lines(batch, booth_dyn_power, type = "o", pch = 15, col = col_booth, lwd = 2)
legend(
  "topleft", legend = c("Array", "Booth"),
  col = c(col_array, col_booth), pch = c(16, 15),
  lty = 1, lwd = 2, bg = "white", cex = 0.9
)

end_fig()

# ==============================================================================
# FIGURE 2: Normal Q-Q Plots of Paired Differences (Array - Booth)
# ==============================================================================
diff_dyn_power  <- array_dyn_power  - booth_dyn_power
diff_tot_power  <- array_tot_power  - booth_tot_power
diff_dyn_energy <- array_dyn_energy - booth_dyn_energy

plot_qq_paired <- function(x, title_text) {
  q <- qqnorm(x, plot.it = FALSE)
  plot(
    q$x, q$y,
    pch = 21, bg = "#2e7d32", col = "black", cex = 1.2,
    xlab = "Theoretical quantiles", ylab = "Ordered Values",
    main = title_text, las = 1
  )
  grid(lty = 2, col = "gray80")
  # OLS reference line (matches scipy.stats.probplot)
  abline(lm(q$y ~ q$x), col = "#b71c1c", lwd = 2)
  points(q$x, q$y, pch = 21, bg = "#2e7d32", col = "black", cex = 1.2)
}

start_fig("figure2_qq_plots.png")
plot_qq_paired(diff_dyn_power,  "Paired Diff: Dynamic Power (mW)")
plot_qq_paired(diff_tot_power,  "Paired Diff: Total Power (mW)")
plot_qq_paired(diff_dyn_energy, "Paired Diff: Dynamic Energy (pJ)")
end_fig()

# ==============================================================================
# FIGURE 3: Shapiro-Wilk Test Null Distribution & Observed W0
# ==============================================================================
set.seed(42)
w_null <- replicate(10000, unname(shapiro.test(rnorm(30))$statistic))
dens_w <- density(w_null, from = 0.80, to = 1.00, n = 512)
w_crit <- unname(quantile(w_null, 0.05))

plot_shapiro_panel <- function(x, title_expr) {
  sw <- shapiro.test(x)
  w_obs <- unname(sw$statistic)
  p_val <- sw$p.value
  p_str <- ifelse(p_val < 0.001, "P < 0.001", sprintf("P = %.4f", p_val))

  xmin <- min(w_obs - 0.03, 0.80)
  span <- 1.00 - xmin
  ymax <- max(dens_w$y) * 1.12

  plot(
    dens_w$x, dens_w$y, type = "n",
    xlim = c(xmin, 1.00), ylim = c(0, ymax),
    xlab = "Shapiro-Wilk Statistic (W)", ylab = expression(italic(f)(W)),
    main = title_expr, las = 1, bty = "l"
  )
  grid(nx = NA, ny = NULL, lty = 2, col = "gray80")

  # Shade critical region (W <= w_crit)
  idx_crit <- dens_w$x <= w_crit
  polygon(
    c(dens_w$x[idx_crit], w_crit),
    c(dens_w$y[idx_crit], 0),
    col = rgb(0.27, 0.51, 0.71, 0.35), border = NA
  )

  # Null density curve & critical threshold
  lines(dens_w$x, dens_w$y, col = "#005f73", lwd = 2)
  abline(v = w_crit, col = "gray30", lwd = 1.2)

  # Observed W0 vertical line + point
  abline(v = w_obs, col = "#b22222", lty = 2, lwd = 2)
  points(w_obs, 0, pch = 16, col = "#b22222", cex = 1.6)

  # Annotations (spaced cleanly to avoid overlapping lines)
  text_x_obs <- w_obs + span * 0.22
  text(
    text_x_obs, ymax * 0.76,
    labels = sprintf("W0 = %.4f\n(%s)", w_obs, p_str),
    col = "#b22222", font = 2, cex = 0.9
  )
  arrows(
    text_x_obs - span * 0.04, ymax * 0.67, w_obs + span * 0.005, ymax * 0.12,
    col = "#b22222", length = 0.08, lwd = 1.2
  )

  text(
    w_crit - span * 0.22, ymax * 0.45,
    labels = "Critical region\n(alpha = 0.05)", cex = 0.85
  )
  arrows(
    w_crit - span * 0.16, ymax * 0.36, w_crit - 0.004, ymax * 0.02,
    length = 0.08, lwd = 1
  )

  text(
    (w_crit + 1.00) / 2, ymax * 0.20,
    labels = "Acceptance\nregion\n(1 - alpha)",
    col = "#003049", font = 2, cex = 0.8
  )
}

start_fig("figure3_shapiro_wilk.png")
plot_shapiro_panel(diff_dyn_power,  expression(bold("(a) ") * italic(D)[P*",dyn"] * bold(" (Dynamic Power)")))
plot_shapiro_panel(diff_tot_power,  expression(bold("(b) ") * italic(D)[P*",tot"] * bold(" (Total Power)")))
plot_shapiro_panel(diff_dyn_energy, expression(bold("(c) ") * italic(D)[E*",dyn"] * bold(" (Dynamic Energy)")))
end_fig()
