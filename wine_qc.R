# Wine Dataset QC Script
# Dataset: data/wine_cleaned.csv
# Output:  QC_out/wine_qc_report.pdf

library(grid)

DATA_PATH    <- "data/wine_cleaned.csv"
OUT_DIR      <- "QC_out"
OUT_PDF      <- file.path(OUT_DIR, "wine_qc_report.pdf")
CURRENT_YEAR <- 2026

dir.create(OUT_DIR, showWarnings = FALSE)

# ── Helper: write text pages to current PDF device ───────────────────────────
write_text_page <- function(lines, title = NULL, mono_size = 8) {
  grid.newpage()
  y <- 0.97
  if (!is.null(title)) {
    grid.text(title, x = 0.5, y = y, just = c("centre", "top"),
              gp = gpar(fontsize = 13, fontface = "bold"))
    y <- y - 0.04
    grid.lines(x = c(0.02, 0.98), y = c(y, y), gp = gpar(lwd = 1))
    y <- y - 0.015
  }
  line_h <- (mono_size + 1) / (8.5 * 72)  # approx line height in npc
  for (ln in lines) {
    if (y < 0.02) {
      grid.newpage()
      y <- 0.97
    }
    grid.text(ln, x = 0.03, y = y, just = c("left", "top"),
              gp = gpar(fontfamily = "mono", fontsize = mono_size))
    y <- y - line_h
  }
}

# ── Load data ─────────────────────────────────────────────────────────────────
wine <- read.csv(DATA_PATH, row.names = 1, stringsAsFactors = FALSE)

# ── Capture QC text output ────────────────────────────────────────────────────
qc_lines <- capture.output({

  cat("========================================\n")
  cat("  WINE DATASET QUALITY CONTROL REPORT  \n")
  cat("========================================\n\n")

  # 1. Structure
  cat("── 1. STRUCTURE ─────────────────────────────────────\n")
  cat(sprintf("Rows: %d  |  Columns: %d\n", nrow(wine), ncol(wine)))
  cat("Column names & types:\n")
  for (col in colnames(wine)) {
    cat(sprintf("  %-20s %s\n", col, class(wine[[col]])))
  }
  cat("\n")

  # 2. Missing values
  cat("── 2. MISSING VALUES ────────────────────────────────\n")
  na_counts    <- colSums(is.na(wine))
  blank_counts <- sapply(wine, function(x) sum(x == "" | x == " ", na.rm = TRUE))
  total_missing <- na_counts + blank_counts
  if (all(total_missing == 0)) {
    cat("  PASS: No missing or blank values found.\n\n")
  } else {
    cat("  FAIL: Missing / blank values detected:\n")
    miss <- total_missing[total_missing > 0]
    for (nm in names(miss)) cat(sprintf("    %-20s %d\n", nm, miss[[nm]]))
    cat("\n")
  }

  # 3. Duplicates
  cat("── 3. DUPLICATE ROWS ────────────────────────────────\n")
  dup_rows  <- wine[duplicated(wine) | duplicated(wine, fromLast = TRUE), ]
  dup_names <- wine$name[duplicated(wine$name)]
  if (nrow(dup_rows) == 0) {
    cat("  PASS: No duplicate rows.\n")
  } else {
    cat(sprintf("  FAIL: %d duplicate row(s) found.\n", nrow(dup_rows)))
  }
  if (length(dup_names) == 0) {
    cat("  PASS: All wine names are unique.\n\n")
  } else {
    cat(sprintf("  WARN: Duplicate names: %s\n\n", paste(dup_names, collapse = ", ")))
  }

  # 4. Categorical checks
  cat("── 4. CATEGORICAL VALUES ────────────────────────────\n")
  valid_colors <- c("red", "white", "rose", "sparkling", "dessert", "orange")
  bad_color    <- wine[!wine$color %in% valid_colors, ]
  if (nrow(bad_color) == 0) {
    cat("  PASS: All color values valid.\n")
  } else {
    cat("  FAIL: Unexpected color values:\n")
    for (i in seq_len(nrow(bad_color))) {
      cat(sprintf("    %-25s -> '%s'\n", bad_color$name[i], bad_color$color[i]))
    }
  }
  tbl <- table(wine$color)
  cat(sprintf("  Color distribution: %s\n",
      paste(names(tbl), tbl, sep = "=", collapse = "  ")))
  cat(sprintf("  Countries (%d): %s\n",
      length(unique(wine$country)), paste(sort(unique(wine$country)), collapse = ", ")))
  cat(sprintf("  Grapes   (%d): %s\n\n",
      length(unique(wine$grape)),   paste(sort(unique(wine$grape)),   collapse = ", ")))

  # 5. Numeric range checks
  cat("── 5. NUMERIC RANGE CHECKS ──────────────────────────\n")
  range_rules <- list(
    alcohol        = list(min = 5,    max = 22),
    acidity        = list(min = 3,    max = 12),
    residual_sugar = list(min = 0,    max = 400),
    ph             = list(min = 2.5,  max = 4.5),
    price_eur      = list(min = 0.01, max = 10000),
    rating         = list(min = 50,   max = 100),
    vintage        = list(min = 1900, max = CURRENT_YEAR)
  )
  all_ok <- TRUE
  for (var in names(range_rules)) {
    rule <- range_rules[[var]]
    vals <- wine[[var]]
    bad  <- which(vals < rule$min | vals > rule$max)
    if (length(bad) == 0) {
      cat(sprintf("  PASS  %-18s [%.1f – %.1f]\n", var, rule$min, rule$max))
    } else {
      all_ok <- FALSE
      cat(sprintf("  FAIL  %-18s %d out-of-range:\n", var, length(bad)))
      for (i in bad) cat(sprintf("          Row %2d  %-24s = %g\n", i, wine$name[i], vals[i]))
    }
  }
  if (all_ok) cat("  All numeric ranges OK.\n")
  cat("\n")

  # 6. Outliers (IQR)
  cat("── 6. OUTLIER DETECTION (IQR × 1.5) ────────────────\n")
  numeric_cols <- c("alcohol", "acidity", "residual_sugar", "ph", "price_eur", "rating")
  any_out <- FALSE
  for (col in numeric_cols) {
    x   <- wine[[col]]
    q1  <- quantile(x, 0.25)
    q3  <- quantile(x, 0.75)
    iqr <- q3 - q1
    lo  <- q1 - 1.5 * iqr
    hi  <- q3 + 1.5 * iqr
    out <- which(x < lo | x > hi)
    if (length(out) > 0) {
      any_out <- TRUE
      cat(sprintf("  WARN  %-18s fence [%.2f, %.2f]\n", col, lo, hi))
      for (i in out) {
        dir <- if (x[i] > hi) "HIGH" else "LOW"
        cat(sprintf("          Row %2d  %-24s = %g  (%s)\n", i, wine$name[i], x[i], dir))
      }
    }
  }
  if (!any_out) cat("  No IQR outliers detected.\n")
  cat("\n")

  # 7. Domain consistency
  cat("── 7. DOMAIN CONSISTENCY CHECKS ─────────────────────\n")

  dessert_idx <- which(wine$residual_sugar > 45)
  if (length(dessert_idx) > 0) {
    cat(sprintf("  INFO  %d dessert-style wine(s) (residual sugar >45 g/L):\n", length(dessert_idx)))
    for (i in dessert_idx) {
      cat(sprintf("          %-25s  sugar=%.0f g/L  color=%s\n",
                  wine$name[i], wine$residual_sugar[i], wine$color[i]))
    }
  }

  low_alc_red <- wine[wine$color == "red" & wine$alcohol < 11, ]
  if (nrow(low_alc_red) > 0) {
    cat("  WARN  Low alcohol (<11%) in red wine(s):\n")
    for (i in seq_len(nrow(low_alc_red))) {
      cat(sprintf("          %-25s  alc=%.1f%%\n", low_alc_red$name[i], low_alc_red$alcohol[i]))
    }
  } else {
    cat("  PASS  Red wine alcohol levels all >=11%.\n")
  }

  high_p_low_r <- wine[wine$price_eur > 40 & wine$rating < 88, ]
  if (nrow(high_p_low_r) > 0) {
    cat("  WARN  High price (>40 EUR) with rating <88:\n")
    for (i in seq_len(nrow(high_p_low_r))) {
      cat(sprintf("          %-25s  price=%.0f  rating=%d\n",
                  high_p_low_r$name[i], high_p_low_r$price_eur[i], high_p_low_r$rating[i]))
    }
  } else {
    cat("  PASS  No high-price / low-rating inconsistencies.\n")
  }
  cat("\n")

  # 8. Summary statistics
  cat("── 8. SUMMARY STATISTICS ─────────────────────────────\n")
  num_data <- wine[, numeric_cols]
  fmt <- "  %-18s  min=%7.2f  Q1=%7.2f  median=%7.2f  mean=%7.2f  Q3=%7.2f  max=%7.2f  sd=%6.2f\n"
  for (col in numeric_cols) {
    x <- num_data[[col]]
    cat(sprintf(fmt, col,
                min(x), quantile(x,.25), median(x), mean(x),
                quantile(x,.75), max(x), sd(x)))
  }
  cat("\n")

  # 9. Correlation
  cat("── 9. CORRELATION MATRIX ─────────────────────────────\n")
  cor_mat <- round(cor(wine[, numeric_cols]), 2)
  cat(capture.output(print(cor_mat)), sep = "\n")
  cat("\n  Notable correlations (|r| > 0.6):\n")
  found <- FALSE
  for (i in seq_len(nrow(cor_mat))) {
    for (j in seq_len(ncol(cor_mat))) {
      if (i < j && abs(cor_mat[i,j]) > 0.6) {
        found <- TRUE
        cat(sprintf("    %-18s ~ %-18s  r = %.2f\n",
                    rownames(cor_mat)[i], colnames(cor_mat)[j], cor_mat[i,j]))
      }
    }
  }
  if (!found) cat("    None above threshold.\n")
  cat("\n")

  # 10. Per-row flag summary
  cat("── 10. PER-ROW FLAG SUMMARY ─────────────────────────\n")
  flags <- rep("", nrow(wine))
  for (col in numeric_cols) {
    x  <- wine[[col]]
    q1 <- quantile(x,.25); q3 <- quantile(x,.75)
    lo <- q1 - 1.5*(q3-q1); hi <- q3 + 1.5*(q3-q1)
    for (i in which(x < lo | x > hi)) {
      flags[i] <- paste0(flags[i], ifelse(nchar(flags[i])>0, "; ", ""), col, "_outlier")
    }
  }
  for (i in which(wine$residual_sugar > 45)) {
    if (!grepl("residual_sugar_outlier", flags[i])) {
      flags[i] <- paste0(flags[i], ifelse(nchar(flags[i])>0, "; ", ""), "high_sugar")
    }
  }
  n_clean   <- sum(nchar(flags) == 0)
  n_flagged <- sum(nchar(flags) >  0)
  cat(sprintf("  Clean rows  : %d / %d\n", n_clean,   nrow(wine)))
  cat(sprintf("  Flagged rows: %d / %d\n", n_flagged, nrow(wine)))
  if (n_flagged > 0) {
    cat("\n  Flagged wines:\n")
    for (i in which(nchar(flags) > 0)) {
      cat(sprintf("    Row %2d  %-25s  [%s]\n", i, wine$name[i], flags[i]))
    }
  }
  cat("\n")
  cat("========================================\n")
  cat("          QC REPORT COMPLETE            \n")
  cat("========================================\n")
})

# ── Open PDF and write pages ──────────────────────────────────────────────────
pdf(OUT_PDF, width = 14, height = 10, paper = "special")

# Page 1+: text report
write_text_page(qc_lines, title = "Wine Dataset — QC Report", mono_size = 7.5)

# ── Plot page: distributions ──────────────────────────────────────────────────
numeric_cols <- c("alcohol", "acidity", "residual_sugar", "ph", "price_eur", "rating")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3)))

for (k in seq_along(numeric_cols)) {
  col <- numeric_cols[k]
  row <- ceiling(k / 3); col_pos <- ((k - 1) %% 3) + 1
  pushViewport(viewport(layout.pos.row = row, layout.pos.col = col_pos))
  x  <- wine[[col]]
  q1 <- quantile(x,.25); q3 <- quantile(x,.75)
  lo <- q1 - 1.5*(q3-q1); hi <- q3 + 1.5*(q3-q1)

  grid.rect(gp = gpar(fill = "grey97", col = "grey80"))
  grid.text(col, x = 0.5, y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))

  # Simple dot strip + IQR box
  xr <- range(x)
  xn <- (x - xr[1]) / diff(xr)
  grid.points(xn, rep(0.55, length(xn)), pch = 16, size = unit(3, "mm"),
              gp = gpar(col = "#2166AC88"))
  # IQR fence lines
  fn_lo <- (max(lo, xr[1]) - xr[1]) / diff(xr)
  fn_hi <- (min(hi, xr[2]) - xr[1]) / diff(xr)
  grid.rect(x = (fn_lo + fn_hi)/2, y = 0.55, width = fn_hi - fn_lo, height = 0.12,
            gp = gpar(fill = "#4DAF4A33", col = "#4DAF4A"), just = "centre")
  grid.text(sprintf("min %.2g", xr[1]), x = 0.05, y = 0.25, just = "left",
            gp = gpar(fontsize = 7, col = "grey40"))
  grid.text(sprintf("max %.2g", xr[2]), x = 0.95, y = 0.25, just = "right",
            gp = gpar(fontsize = 7, col = "grey40"))
  grid.text(sprintf("mean %.2g", mean(x)), x = 0.5, y = 0.18, just = "centre",
            gp = gpar(fontsize = 7.5))

  # Mark outliers in red
  out_idx <- which(x < lo | x > hi)
  if (length(out_idx) > 0) {
    grid.points(xn[out_idx], rep(0.55, length(out_idx)), pch = 4, size = unit(4, "mm"),
                gp = gpar(col = "red", lwd = 2))
    grid.text(paste0(length(out_idx), " outlier(s)"), x = 0.5, y = 0.08,
              gp = gpar(fontsize = 7, col = "red"))
  }
  popViewport()
}

# ── Plot page: correlation heatmap ───────────────────────────────────────────
cor_mat <- cor(wine[, numeric_cols])
n <- length(numeric_cols)

grid.newpage()
grid.text("Correlation Matrix", x = 0.5, y = 0.97,
          gp = gpar(fontsize = 14, fontface = "bold"))

cell_w <- 0.12; cell_h <- 0.12
x_start <- 0.22; y_start <- 0.82

for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    r   <- cor_mat[i, j]
    clr <- if (r > 0) rgb(r, 0, 0, abs(r) * 0.8 + 0.1) else rgb(0, 0, -r, abs(r) * 0.8 + 0.1)
    cx  <- x_start + (j - 1) * cell_w
    cy  <- y_start - (i - 1) * cell_h
    grid.rect(x = cx, y = cy, width = cell_w * 0.9, height = cell_h * 0.9,
              gp = gpar(fill = clr, col = "white"), just = "centre")
    grid.text(sprintf("%.2f", r), x = cx, y = cy,
              gp = gpar(fontsize = 8, col = "white", fontface = if (abs(r) > 0.6) "bold" else "plain"))
  }
  # Row labels
  grid.text(numeric_cols[i], x = x_start - 0.01, y = y_start - (i - 1) * cell_h,
            just = "right", gp = gpar(fontsize = 8))
  # Col labels
  grid.text(numeric_cols[i], x = x_start + (i - 1) * cell_w,
            y = y_start + cell_h * 0.6, rot = 45, just = "left",
            gp = gpar(fontsize = 8))
}

# Legend
grid.text("Red = positive  |  Blue = negative  |  Bold = |r| > 0.6",
          x = 0.5, y = 0.06, gp = gpar(fontsize = 8, col = "grey40"))

dev.off()

cat(sprintf("\nQC report saved to: %s\n", OUT_PDF))
