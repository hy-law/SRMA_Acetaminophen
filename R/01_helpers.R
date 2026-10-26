# =============================================================================
# 01_helpers.R — shared forest / meta helpers (style only; methods locked)
# =============================================================================
# Statistical defaults match the manuscript runners:
#   metacont / metabin: common=FALSE, random=TRUE,
#                       method.tau="REML", method.random.ci="HK"
#   prediction interval when k >= 3
#   Arm labels: Intravenous / Oral
#   Two-line smlab titles via "\n" in label strings
# =============================================================================

if (!exists("OUTDIR")) {
  stop("Source R/00_setup.R before R/01_helpers.R", call. = FALSE)
}

# Default forest layout used across primary and subgroup plots
.forest_layout <- list(
  plotwidth = grid::unit(8.5, "cm"),
  colgap.forest.left = grid::unit(0.8, "cm"),
  colgap.forest.right = grid::unit(0.5, "cm"),
  fs.smlab = 10,
  label.e = "Intravenous",
  label.c = "Oral"
)

.leftlabs_cont <- c(
  "Study", "Total (IV)", "Mean (IV)", "SD (IV)",
  "Total (PO)", "Mean (PO)", "SD (PO)"
)
.leftlabs_bin <- c(
  "Study", "Events (IV)", "Total (IV)", "Events (PO)", "Total (PO)"
)

# Scale 0–100 VAS to 0–10 (divide mean and SD by 10)
scale_pain <- function(dat, mean_iv, sd_iv, n_iv, mean_po, sd_po, n_po) {
  dat %>%
    dplyr::transmute(
      StudyID,
      PainScale,
      IV_n = .data[[n_iv]], IV_mean = .data[[mean_iv]], IV_sd = .data[[sd_iv]],
      PO_n = .data[[n_po]], PO_mean = .data[[mean_po]], PO_sd = .data[[sd_po]]
    ) %>%
    dplyr::filter(
      !is.na(IV_n), !is.na(PO_n), !is.na(IV_mean), !is.na(PO_mean),
      !is.na(IV_sd), !is.na(PO_sd)
    ) %>%
    dplyr::mutate(
      scale = dplyr::if_else(stringr::str_detect(PainScale, "0-100|100"), 10, 1),
      IV_mean = IV_mean / scale, IV_sd = IV_sd / scale,
      PO_mean = PO_mean / scale, PO_sd = PO_sd / scale
    )
}

save_forest_dual <- function(file_stem, width, height, outdir = OUTDIR, ...) {
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = width, height = height)
  meta::forest(...)
  dev.off()
  png(
    file.path(outdir, paste0(file_stem, ".png")),
    width = width * 150, height = height * 150, res = 150
  )
  meta::forest(...)
  dev.off()
  invisible(TRUE)
}

# Continuous primary pool + forest (MD, REML+HK)
# min_k=1 used only for QoR (single-study narrative forest in Add5)
run_cont <- function(dat, label, file_stem,
                     xlab = "MD (IV - PO); lower favours IV",
                     min_k = 2,
                     outdir = OUTDIR,
                     width = 16) {
  if (nrow(dat) < min_k) {
    cat("SKIP", label, "k<", min_k, "\n")
    return(NULL)
  }
  m <- meta::metacont(
    n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
    n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
    studlab = StudyID, data = dat, sm = "MD",
    common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK",
    prediction = (nrow(dat) >= 3), title = label
  )
  cat(sprintf(
    "%s | k=%d N=%d | MD %.3f [%.3f, %.3f] | I2=%.1f%%\n",
    label, m$k, sum(m$n.e) + sum(m$n.c),
    m$TE.random, m$lower.random, m$upper.random, 100 * m$I2
  ))
  forest_args <- c(
    list(
      m, smlab = label, comb.random = TRUE, prediction = (m$k >= 3),
      xlab = xlab, leftlabs = .leftlabs_cont
    ),
    .forest_layout
  )
  h <- max(6, 0.45 * m$k + 4)
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = width, height = h)
  do.call(meta::forest, forest_args)
  dev.off()
  png(
    file.path(outdir, paste0(file_stem, ".png")),
    width = width * 150, height = h * 150, res = 150
  )
  do.call(meta::forest, forest_args)
  dev.off()
  invisible(m)
}

# Binary primary pool + forest (RR, REML+HK)
run_bin <- function(dat, label, file_stem, outdir = OUTDIR, width = 14) {
  if (nrow(dat) < 2) {
    cat("SKIP", label, "\n")
    return(NULL)
  }
  m <- meta::metabin(
    event.e = IV_events, n.e = IV_total,
    event.c = PO_events, n.c = PO_total,
    studlab = StudyID, data = dat, sm = "RR",
    common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK",
    prediction = (nrow(dat) >= 3), title = label
  )
  cat(sprintf(
    "%s | k=%d N=%d | RR %.3f [%.3f, %.3f] | I2=%.1f%%\n",
    label, m$k, sum(m$n.e) + sum(m$n.c),
    exp(m$TE.random), exp(m$lower.random), exp(m$upper.random), 100 * m$I2
  ))
  forest_args <- c(
    list(
      m, smlab = label, comb.random = TRUE, prediction = (m$k >= 3),
      xlab = "RR (IV vs PO)", leftlabs = .leftlabs_bin
    ),
    .forest_layout
  )
  h <- max(6, 0.45 * m$k + 4)
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = width, height = h)
  do.call(meta::forest, forest_args)
  dev.off()
  png(
    file.path(outdir, paste0(file_stem, ".png")),
    width = width * 150, height = h * 150, res = 150
  )
  do.call(meta::forest, forest_args)
  dev.off()
  invisible(m)
}

# Continuous subgroup forest.
# Allow single-level subgroups so the forest still prints a heading
# (needed for Add6B move 6-24h / >24h when only Postop PO + Postop IV remains).
run_sub <- function(dat, subgroup_var, label, file_stem,
                    outdir = OUTDIR, width = 16) {
  if (nrow(dat) < 2 || !subgroup_var %in% names(dat)) return(NULL)
  d <- dat %>%
    dplyr::filter(!is.na(.data[[subgroup_var]])) %>%
    dplyr::mutate(subgroup = .data[[subgroup_var]])
  if (nrow(d) < 2 || dplyr::n_distinct(d$subgroup) < 1) {
    cat("SKIP subgroup", label, subgroup_var, "\n")
    return(NULL)
  }
  m <- meta::metacont(
    n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
    n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
    studlab = StudyID, data = d, sm = "MD",
    common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK",
    subgroup = subgroup, test.subgroup = TRUE, title = label
  )
  forest_args <- c(
    list(
      m, smlab = label, comb.random = TRUE, xlab = "MD (IV - PO)",
      leftlabs = .leftlabs_cont
    ),
    .forest_layout
  )
  h <- max(7, 0.5 * m$k + 5)
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = width, height = h)
  do.call(meta::forest, forest_args)
  dev.off()
  png(
    file.path(outdir, paste0(file_stem, ".png")),
    width = width * 150, height = h * 150, res = 150
  )
  do.call(meta::forest, forest_args)
  dev.off()
  cat("Wrote subgroup", file_stem, "\n")
  invisible(m)
}

# Binary subgroup (same single-level rule as run_sub)
run_sub_bin <- function(dat, subgroup_var, label, file_stem,
                        outdir = OUTDIR, width = 14) {
  d <- dat %>%
    dplyr::filter(!is.na(.data[[subgroup_var]])) %>%
    dplyr::mutate(subgroup = .data[[subgroup_var]])
  if (nrow(d) < 2 || dplyr::n_distinct(d$subgroup) < 1) {
    cat("SKIP", file_stem, "\n")
    return(NULL)
  }
  m <- meta::metabin(
    event.e = IV_events, n.e = IV_total,
    event.c = PO_events, n.c = PO_total,
    studlab = StudyID, data = d, sm = "RR",
    common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK",
    subgroup = subgroup, test.subgroup = TRUE
  )
  forest_args <- c(
    list(
      m, smlab = label, comb.random = TRUE, xlab = "RR (IV vs PO)",
      leftlabs = .leftlabs_bin
    ),
    .forest_layout
  )
  h <- max(7, 0.5 * m$k + 5)
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = width, height = h)
  do.call(meta::forest, forest_args)
  dev.off()
  png(
    file.path(outdir, paste0(file_stem, ".png")),
    width = width * 150, height = h * 150, res = 150
  )
  do.call(meta::forest, forest_args)
  dev.off()
  cat("Wrote", file_stem, " RR=", round(exp(m$TE.random), 3), "\n")
  invisible(m)
}

cat("01_helpers.R loaded\n")
