# Wine — Variable Relationships, PCA & Outlier Analysis (ggplot2)
# Output: QC_out/wine_eda_report.pdf

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(cowplot)
  library(MASS)
  library(factoextra)
  library(FactoMineR)
  library(scales)
  library(grid)
  library(gridExtra)
})

DATA_PATH <- "data/wine_cleaned.csv"
OUT_PDF   <- "QC_out/wine_eda_report.pdf"
dir.create("QC_out", showWarnings = FALSE)

# ── Data ──────────────────────────────────────────────────────────────────────
wine <- read.csv(DATA_PATH, row.names = 1, stringsAsFactors = FALSE)
wine$color <- factor(wine$color, levels = c("red", "white", "rose", "sparkling"))

numeric_cols <- c("alcohol", "acidity", "residual_sugar", "ph", "price_eur", "rating")
num_data     <- wine[, numeric_cols]

# Types with >= 2 wines (density/violin need at least two points; rose has one)
wine_multi <- wine[wine$color %in% names(which(table(wine$color) >= 2)), ]

COLOR_PAL <- c(red = "#C0392B", white = "#B7950B", rose = "#E91E8C", sparkling = "#1A8C6E")

# Short axis labels
short_lab <- c(alcohol = "Alcohol (%)", acidity = "Acidity (g/L)",
               residual_sugar = "Res. Sugar (g/L)", ph = "pH",
               price_eur = "Price (EUR)", rating = "Rating (pts)")
short_tag <- c(alcohol = "Alc", acidity = "Acid", residual_sugar = "RS",
               ph = "pH", price_eur = "€", rating = "Rtg")

# ── Helpers ───────────────────────────────────────────────────────────────────
iqr_out <- function(x) {
  q1 <- quantile(x, .25); q3 <- quantile(x, .75)
  which(x < q1 - 1.5*(q3-q1) | x > q3 + 1.5*(q3-q1))
}

theme_clean <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold", size = 10.5),
        plot.subtitle = element_text(size = 8.5, colour = "grey45"),
        legend.title  = element_text(size = 8.5),
        legend.text   = element_text(size = 8))

section_page <- function(txt) {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.55, label = txt,
             size = 10, fontface = "bold", colour = "grey25", hjust = 0.5) +
    annotate("segment", x = 0.15, xend = 0.85, y = 0.44, yend = 0.44,
             linewidth = 1.2, colour = "grey60") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void()
  print(p)
}

# ── Open PDF ──────────────────────────────────────────────────────────────────
cairo_pdf(OUT_PDF, width = 13, height = 9, onefile = TRUE)  # Cairo renders Unicode (—, €, ≥, χ²)

# =============================================================================
# SECTION 1 — DISTRIBUTIONS
# =============================================================================
section_page("Section 1 — Univariate Distributions")

# ── 1a. Histograms with density overlay ──────────────────────────────────────
hist_plots <- lapply(numeric_cols, function(col) {
  bw <- (max(wine[[col]]) - min(wine[[col]])) / 9

  ggplot(wine, aes(x = .data[[col]], fill = color, colour = color)) +
    geom_histogram(binwidth = bw, alpha = 0.55, position = "identity", linewidth = 0.3) +
    geom_density(data = wine_multi, aes(y = after_stat(count) * bw),
                 linewidth = 0.7, fill = NA) +
    scale_fill_manual(values   = COLOR_PAL, name = "Type") +
    scale_colour_manual(values = COLOR_PAL, name = "Type") +
    labs(x = short_lab[col], y = "Count",
         title = short_lab[col]) +
    theme_clean +
    theme(legend.position = "none")
})

# Shared legend
leg_p <- ggplot(wine, aes(x = alcohol, fill = color)) +
  geom_histogram(bins = 5) +
  scale_fill_manual(values = COLOR_PAL, name = "Wine type") +
  theme_void() +
  theme(legend.position = "right")
shared_leg <- get_legend(leg_p)

hist_grid <- wrap_plots(hist_plots, ncol = 3) +
  plot_annotation(title = "Histograms — Numeric Variables  (colour = wine type)",
                  theme = theme(plot.title = element_text(size = 13, face = "bold")))

print(hist_grid)

# ── 1b. Violin + jitter by wine type ─────────────────────────────────────────
vio_plots <- lapply(numeric_cols, function(col) {
  ggplot(wine, aes(x = color, y = .data[[col]], fill = color, colour = color)) +
    geom_violin(data = wine_multi, alpha = 0.35, linewidth = 0.5, trim = FALSE) +
    geom_jitter(width = 0.12, size = 2.2, alpha = 0.85) +
    scale_fill_manual(values   = COLOR_PAL) +
    scale_colour_manual(values = COLOR_PAL) +
    labs(x = NULL, y = short_lab[col], title = short_lab[col]) +
    theme_clean +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
})

vio_grid <- wrap_plots(vio_plots, ncol = 3) +
  plot_annotation(title = "Distributions by Wine Type",
                  theme = theme(plot.title = element_text(size = 13, face = "bold")))

print(vio_grid)

# =============================================================================
# SECTION 2 — BIVARIATE RELATIONSHIPS
# =============================================================================
section_page("Section 2 — Bivariate Relationships")

# ── 2a. Scatter matrix (ggplot2 panels via patchwork) ─────────────────────────
n_v <- length(numeric_cols)
mat_panels <- vector("list", n_v * n_v)

for (i in seq_len(n_v)) {
  for (j in seq_len(n_v)) {
    idx <- (i - 1) * n_v + j
    xi  <- numeric_cols[j]
    yi  <- numeric_cols[i]

    if (i == j) {
      # Diagonal — histogram
      mat_panels[[idx]] <-
        ggplot(wine, aes(x = .data[[xi]], fill = color)) +
        geom_histogram(bins = 8, alpha = 0.65, position = "identity", linewidth = 0.2,
                       colour = "white") +
        scale_fill_manual(values = COLOR_PAL) +
        labs(x = NULL, y = NULL, title = short_tag[xi]) +
        theme_void(base_size = 7) +
        theme(legend.position = "none",
              plot.title = element_text(size = 7, face = "bold", hjust = 0.5,
                                        margin = margin(b = 2)),
              plot.background = element_rect(fill = "grey95", colour = NA))

    } else if (i > j) {
      # Lower triangle — scatter + loess
      mat_panels[[idx]] <-
        ggplot(wine, aes(x = .data[[xi]], y = .data[[yi]], colour = color)) +
        geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, colour = "grey70",
                    linewidth = 0.5, linetype = "dashed") +
        geom_point(size = 1.5, alpha = 0.85) +
        scale_colour_manual(values = COLOR_PAL) +
        theme_void(base_size = 7) +
        theme(legend.position = "none")

    } else {
      # Upper triangle — correlation value
      r   <- cor(wine[[xi]], wine[[yi]])
      col <- if (abs(r) >= 0.6) "#C0392B" else if (abs(r) >= 0.3) "#E67E22" else "grey55"
      mat_panels[[idx]] <-
        ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = sprintf("r=%.2f", r),
                 size = 3.2, colour = col,
                 fontface = if (abs(r) >= 0.6) "bold" else "plain") +
        theme_void() +
        theme(plot.background = element_rect(fill = "grey97", colour = NA))
    }
  }
}

mat_p <- wrap_plots(mat_panels, ncol = n_v) +
  plot_annotation(
    title    = "Scatter Matrix  (diagonal = histogram, lower = scatter+lm, upper = r)",
    subtitle = "Red bold r ≥ 0.6  |  Orange r ≥ 0.3  |  colour = wine type",
    theme    = theme(plot.title    = element_text(size = 12, face = "bold"),
                     plot.subtitle = element_text(size = 9,  colour = "grey45"))
  )

print(mat_p)

# ── 2b. Key scatter plots (2×3) ───────────────────────────────────────────────
scatter_specs <- list(
  list(x = "price_eur",  y = "rating",         xl = "Price (EUR)",    yl = "Rating"),
  list(x = "alcohol",    y = "rating",          xl = "Alcohol (%)",   yl = "Rating"),
  list(x = "alcohol",    y = "residual_sugar",  xl = "Alcohol (%)",   yl = "Res. Sugar (g/L)"),
  list(x = "acidity",    y = "ph",              xl = "Acidity (g/L)", yl = "pH"),
  list(x = "price_eur",  y = "alcohol",         xl = "Price (EUR)",   yl = "Alcohol (%)"),
  list(x = "vintage",    y = "rating",          xl = "Vintage",       yl = "Rating")
)

make_key_scatter <- function(sp) {
  df <- wine
  df$lbl <- ""
  flag <- union(iqr_out(df[[sp$x]]), iqr_out(df[[sp$y]]))
  df$lbl[flag] <- df$name[flag]
  r <- round(cor(df[[sp$x]], df[[sp$y]]), 2)

  ggplot(df, aes(x = .data[[sp$x]], y = .data[[sp$y]], colour = color)) +
    geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = TRUE,
                colour = "grey60", fill = "grey88", linewidth = 0.7) +
    geom_point(size = 3, alpha = 0.85) +
    geom_text_repel(aes(label = lbl), size = 2.7, colour = "grey20",
                    box.padding = 0.35, max.overlaps = 15) +
    scale_colour_manual(values = COLOR_PAL, name = "Type") +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("r = %+.2f", r),
             hjust = -0.15, vjust = 1.5, size = 3.4,
             colour = if (abs(r) >= 0.6) "#C0392B" else "grey45",
             fontface = if (abs(r) >= 0.6) "bold" else "plain") +
    labs(x = sp$xl, y = sp$yl,
         title = paste(sp$yl, "vs", sp$xl)) +
    theme_clean +
    theme(legend.position = "none",
          plot.title = element_text(size = 9.5))
}

key_plots  <- lapply(scatter_specs, make_key_scatter)
leg_right  <- get_legend(
  key_plots[[1]] + theme(legend.position = "right",
                          legend.title = element_text(size = 9)))

key_grid <- wrap_plots(key_plots, ncol = 3) +
  plot_annotation(
    title    = "Key Variable Relationships  (IQR outliers labelled)",
    subtitle = "Shaded band = 95% CI of linear fit",
    theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 9, colour = "grey45"))
  )

print(key_grid)

# =============================================================================
# SECTION 3 — PCA
# =============================================================================
section_page("Section 3 — Principal Component Analysis")

# Log-transform residual_sugar (heavily right-skewed)
pca_data <- num_data
pca_data$residual_sugar <- log1p(pca_data$residual_sugar)
rownames(pca_data) <- wine$name

pca_res <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)
eig     <- as.data.frame(get_eigenvalue(pca_res))  # matrix in factoextra >= 2.x

# ── 3a. Scree plot ────────────────────────────────────────────────────────────
eig_df <- data.frame(
  PC  = factor(paste0("PC", seq_len(nrow(eig))), levels = paste0("PC", seq_len(nrow(eig)))),
  var = eig$variance.percent,
  cum = eig$cumulative.variance.percent
)

scree_p <- ggplot(eig_df, aes(x = PC, y = var)) +
  geom_col(fill = "#2980B9", alpha = 0.75, width = 0.6) +
  geom_line(aes(y = cum, group = 1), colour = "#C0392B", linewidth = 1.1) +
  geom_point(aes(y = cum), colour = "#C0392B", size = 2.8) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "grey55") +
  annotate("text", x = 5.6, y = 82, label = "80%", size = 3, colour = "grey45") +
  scale_y_continuous(limits = c(0, 107),
                     sec.axis = sec_axis(~., name = "Cumulative (%)")) +
  labs(x = NULL, y = "Variance explained (%)",
       title = "Scree Plot",
       subtitle = "Bars = individual; red line = cumulative") +
  theme_clean

# ── 3b. Variable contributions ────────────────────────────────────────────────
contrib_df <- as.data.frame(pca_res$var$contrib)
contrib_df$variable <- rownames(contrib_df)
ref_line <- 100 / length(numeric_cols)

c1 <- ggplot(contrib_df,
             aes(x = reorder(variable, Dim.1), y = Dim.1,
                 fill = Dim.1 > ref_line)) +
  geom_col(alpha = 0.80) +
  geom_hline(yintercept = ref_line, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("TRUE" = "#2980B9", "FALSE" = "grey70"),
                    guide = "none") +
  coord_flip() +
  labs(x = NULL, y = "Contribution (%)",
       title = "PC1 contributions",
       subtitle = "Dashed = uniform reference") +
  theme_clean

c2 <- ggplot(contrib_df,
             aes(x = reorder(variable, Dim.2), y = Dim.2,
                 fill = Dim.2 > ref_line)) +
  geom_col(alpha = 0.80) +
  geom_hline(yintercept = ref_line, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("TRUE" = "#8E44AD", "FALSE" = "grey70"),
                    guide = "none") +
  coord_flip() +
  labs(x = NULL, y = "Contribution (%)",
       title = "PC2 contributions",
       subtitle = "Dashed = uniform reference") +
  theme_clean

print((scree_p | c1 | c2) +
  plot_annotation(title = "PCA — Variance Explained & Variable Contributions",
                  theme = theme(plot.title = element_text(size = 13, face = "bold"))))

# ── 3c. Biplot PC1 vs PC2 ────────────────────────────────────────────────────
print(
  fviz_pca_biplot(
    pca_res,
    repel        = TRUE,
    col.ind      = wine$color,
    palette      = unname(COLOR_PAL),
    col.var      = "#2C3E50",
    label        = "all",
    labelsize    = 3.2,
    pointsize    = 2.8,
    arrowsize    = 0.65,
    title        = "PCA Biplot — PC1 vs PC2  (residual_sugar log-transformed)",
    legend.title = "Wine type",
    ggtheme      = theme_minimal(base_size = 11)
  ) + theme(panel.grid.minor = element_blank())
)

# ── 3d. Variable correlation circle ──────────────────────────────────────────
print(
  fviz_pca_var(
    pca_res,
    col.var       = "contrib",
    gradient.cols = c("#3498DB", "#E67E22", "#C0392B"),
    repel         = TRUE,
    title         = "Variable Correlation Circle  (colour = contribution %)",
    ggtheme       = theme_minimal(base_size = 11)
  ) + theme(panel.grid.minor = element_blank())
)

# ── 3e. Individual map coloured by wine type ──────────────────────────────────
ind_coords        <- as.data.frame(pca_res$ind$coord)
ind_coords$wine   <- wine$name
ind_coords$color  <- wine$color

pc1_var <- round(eig$variance.percent[1], 1)
pc2_var <- round(eig$variance.percent[2], 1)

ind_p <- ggplot(ind_coords, aes(x = Dim.1, y = Dim.2,
                                  colour = color, label = wine)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_text_repel(size = 2.9, max.overlaps = 20, box.padding = 0.35) +
  scale_colour_manual(values = COLOR_PAL, name = "Wine type") +
  labs(x = sprintf("PC1 (%.1f%%)", pc1_var),
       y = sprintf("PC2 (%.1f%%)", pc2_var),
       title = "PCA — Individual Map",
       subtitle = "Each point = one wine; coloured by type") +
  theme_clean

print(ind_p)

# =============================================================================
# SECTION 4 — OUTLIER DETECTION
# =============================================================================
section_page("Section 4 — Outlier Detection")

# ── 4a. Z-score heatmap ───────────────────────────────────────────────────────
z_mat  <- scale(num_data)
z_df   <- as.data.frame(z_mat)
z_df$wine <- wine$name

z_long <- tidyr::pivot_longer(z_df, cols = all_of(numeric_cols),
                               names_to = "variable", values_to = "z")
z_long$variable <- factor(z_long$variable, levels = rev(numeric_cols))
z_long$wine     <- factor(z_long$wine,     levels = rev(wine$name))

heat_p <- ggplot(z_long, aes(x = wine, y = variable, fill = z)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", z)),
            size = 2.6,
            colour = ifelse(abs(z_long$z) > 1.5, "white", "grey20")) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#C0392B",
                       midpoint = 0, limits = c(-3.5, 3.5), oob = squish,
                       name = "Z-score") +
  labs(x = NULL, y = NULL,
       title = "Z-score Heatmap — All Numeric Variables",
       subtitle = "|z| > 1.5 highlights potential outliers; values capped at ±3.5") +
  theme_clean +
  theme(axis.text.x  = element_text(angle = 40, hjust = 1, size = 8),
        panel.grid   = element_blank())

print(heat_p)

# ── 4b. IQR outlier strip charts ─────────────────────────────────────────────
strip_plots <- lapply(numeric_cols, function(col) {
  x    <- wine[[col]]
  q1   <- quantile(x, .25); q3 <- quantile(x, .75); iqr <- q3 - q1
  lo   <- q1 - 1.5*iqr;    hi  <- q3 + 1.5*iqr
  df   <- data.frame(val = x, wine = wine$name, color = wine$color,
                     out = x < lo | x > hi)

  ggplot(df, aes(x = val, y = 0, colour = color, label = ifelse(out, wine, ""))) +
    annotate("rect", xmin = max(lo, min(x)), xmax = min(hi, max(x)),
             ymin = -0.4, ymax = 0.4, fill = "#27AE6022", colour = NA) +
    geom_vline(xintercept = c(max(lo, min(x)), min(hi, max(x))),
               linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    geom_jitter(height = 0.25, size = 2.5, alpha = 0.85) +
    geom_text_repel(direction = "y", nudge_y = 0.45, size = 2.4,
                    colour = "#C0392B", box.padding = 0.2, max.overlaps = 10) +
    scale_colour_manual(values = COLOR_PAL) +
    scale_y_continuous(limits = c(-0.8, 1)) +
    labs(x = short_lab[col], y = NULL, title = short_lab[col]) +
    theme_clean +
    theme(legend.position = "none",
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
})

strip_grid <- wrap_plots(strip_plots, ncol = 2) +
  plot_annotation(
    title    = "IQR Strip Charts  (green band = IQR ×1.5 fence; red labels = outliers)",
    theme    = theme(plot.title = element_text(size = 12, face = "bold"))
  )

print(strip_grid)

# ── 4c. Mahalanobis distance (robust MCD) ────────────────────────────────────
rob_cov  <- cov.rob(num_data, method = "mcd")
mah_dist <- mahalanobis(num_data, rob_cov$center, rob_cov$cov)
chi_thr  <- qchisq(0.975, df = length(numeric_cols))

mah_df <- data.frame(
  wine    = wine$name,
  dist    = mah_dist,
  color   = wine$color,
  outlier = mah_dist > chi_thr
)

mah_bar <- ggplot(mah_df, aes(x = reorder(wine, dist), y = dist, fill = color)) +
  geom_col(aes(alpha = outlier), width = 0.7) +
  geom_hline(yintercept = chi_thr, linetype = "dashed",
             colour = "#C0392B", linewidth = 0.9) +
  annotate("text", x = 1.2, y = chi_thr + max(mah_dist)*0.03,
           label = sprintf("χ²₀.₉₇₅ (df=6) = %.1f", chi_thr),
           hjust = 0, size = 3, colour = "#C0392B") +
  scale_fill_manual(values = COLOR_PAL, name = "Type") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.55), guide = "none") +
  coord_flip() +
  labs(x = NULL, y = "Mahalanobis distance (robust MCD)",
       title = "Multivariate Outlier Detection — Mahalanobis Distance",
       subtitle = "Fully opaque bars exceed the 97.5% chi-squared threshold") +
  theme_clean +
  theme(panel.grid.major.y = element_blank())

# ── 4d. Chi-sq QQ plot ───────────────────────────────────────────────────────
n <- nrow(num_data)
p <- length(numeric_cols)
qq_df <- data.frame(
  theoretical = qchisq(ppoints(n), df = p),
  observed    = sort(mah_dist),
  wine        = wine$name[order(mah_dist)],
  outlier     = sort(mah_dist) > chi_thr
)

qq_p <- ggplot(qq_df, aes(x = theoretical, y = observed,
                            colour = outlier, label = ifelse(outlier, wine, ""))) +
  geom_abline(slope = 1, intercept = 0, colour = "grey65", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text_repel(size = 3, colour = "grey20", box.padding = 0.4, max.overlaps = 10) +
  scale_colour_manual(values = c("TRUE" = "#C0392B", "FALSE" = "#2980B9"),
                      labels = c("Normal", "Outlier"), name = NULL) +
  labs(x = "Theoretical χ² quantiles (df=6)",
       y = "Observed Mahalanobis distance",
       title = "Chi-squared Q–Q Plot",
       subtitle = "Points above the diagonal = heavier tails than multivariate normal") +
  theme_clean

print((mah_bar | qq_p) +
  plot_annotation(theme = theme(plot.title = element_blank())))

# ── 4e. PCA space — point size = Mahalanobis distance ────────────────────────
ind_coords$mah     <- mah_dist
ind_coords$outlier <- mah_dist > chi_thr

pca_out_p <- ggplot(ind_coords,
                     aes(x = Dim.1, y = Dim.2,
                         colour = color, size = mah, label = wine)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(alpha = 0.78) +
  geom_text_repel(
    aes(fontface = ifelse(outlier, "bold", "plain")),
    size = 2.9, colour = "grey20",
    max.overlaps = 20, box.padding = 0.4
  ) +
  scale_colour_manual(values = COLOR_PAL, name = "Wine type") +
  scale_size_continuous(name = "Mahal.\ndistance", range = c(2, 9)) +
  labs(x = sprintf("PC1 (%.1f%%)", pc1_var),
       y = sprintf("PC2 (%.1f%%)", pc2_var),
       title = "PCA Space — Point Size = Mahalanobis Distance",
       subtitle = "Bold labels = multivariate outliers beyond χ²₀.₉₇₅ threshold") +
  theme_clean

print(pca_out_p)

dev.off()
cat(sprintf("\nEDA report saved to: %s\n", OUT_PDF))
