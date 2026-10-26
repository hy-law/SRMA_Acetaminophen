# =============================================================================
# export_TSA_opioid_sensitivities.R
# =============================================================================
# Purpose: TSA opioid 24-h sensitivities (MCID 5 / 15 mg; 90% power)
# Same RE DL+HKSJ / esOF settings as primary Fig5a (except mc and/or beta).
# Standalone; also callable from export_Fig5_TSA.R / run_R1_full_package.R.
# Manuscript: BMC Anesthesiology
# Outputs: figures_Fig5/TSA_opioid_24h_* + sensitivity_summary.csv|.md
#
# How to run: source("R/export_TSA_opioid_sensitivities.R")
# =============================================================================

if (!exists("PROJECT_ROOT") || !exists("FIG5_DIR")) {
  cands <- c(Sys.getenv("SRMA_PROJECT_ROOT", ""), getwd(), file.path(getwd(), ".."))
  setup_path <- NULL
  for (cand in cands) {
    if (!nzchar(cand)) next
    p <- file.path(cand, "R", "00_setup.R")
    if (file.exists(p)) { setup_path <- normalizePath(p); break }
    p2 <- file.path(cand, "00_setup.R")
    if (file.exists(p2)) { setup_path <- normalizePath(p2); break }
  }
  if (is.null(setup_path)) {
    here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
    if (file.exists(file.path(here, "00_setup.R"))) {
      setup_path <- normalizePath(file.path(here, "00_setup.R"))
    }
  }
  if (is.null(setup_path)) stop("Cannot find R/00_setup.R")
  source(setup_path)
}
load_r1_packages()
ensure_outdir(FIG5_DIR)

xlsx <- XLSX_PATH
dat_list <- load_r1_workbook(xlsx)
char <- dat_list[["Study_Characteristics"]]
if (!"Publication Year" %in% names(char)) char$`Publication Year` <- seq_len(nrow(char))

outdir <- FIG5_DIR

opioid_24h <- dat_list[["Opioid_Consumption_MME"]] %>%
  filter(TimeWindow_Hours == "0-24") %>%
  left_join(char %>% select(StudyID, `Publication Year`), by = "StudyID") %>%
  arrange(`Publication Year`) %>%
  mutate(mI = IV_mean_MME, sdI = IV_sd_MME, nI = IV_n,
         mC = PO_mean_MME, sdC = PO_sd_MME, nC = PO_n, study = StudyID) %>%
  select(study, mI, sdI, nI, mC, sdC, nC) %>% drop_na()

accrued_n <- sum(opioid_24h$nI + opioid_24h$nC)
cat("Opioid 0-24h studies:", nrow(opioid_24h), " accrued N=", accrued_n, "\n")
print(opioid_24h)

# Caption helper: never leave RTSA's "Fixed-effect" footer (tau2=0 makes RTSA flip settings$fixed)
make_caption <- function(beta, mc) {
  power_pct <- round((1 - beta) * 100)
  beta_pct  <- round(beta * 100)
  paste0(
    "Retrospective TSA; alpha 5%, beta ", beta_pct, "% (", power_pct, "% power), ",
    "O'Brien-Fleming (esOF); MCID ", mc, " mg oral MME.\n",
    "Methods: Random-effects DL+HKSJ only (fixed = FALSE). Futility boundaries: none."
  )
}

summarize_tsa <- function(obj, scenario, mc, beta) {
  df <- obj$results$results_df
  # Monitoring crossed if any |z_random| exceeds |upper/lower| when boundary defined
  mon_cross <- FALSE
  for (i in seq_len(nrow(df))) {
    z <- df$z_random[i]; u <- df$upper[i]; l <- df$lower[i]
    if (!is.na(z) && !is.na(u) && !is.na(l) && (z >= u || z <= l)) mon_cross <- TRUE
  }
  # Futility: RTSA futility="none" → never constructed
  fut_defined <- !is.null(obj$bounds$futility) && !identical(obj$bounds$futility, "none")
  fut_cross <- FALSE
  if (fut_defined) {
    for (i in seq_len(nrow(df))) {
      z <- df$z_random[i]; fu <- df$fut_upper[i]; fl <- df$fut_lower[i]
      if (!is.na(z) && !is.na(fu) && !is.na(fl) && z <= fu && z >= fl) fut_cross <- TRUE
    }
  }
  meta <- obj$results$metaanalysis$meta_results
  re <- meta[meta$type == "Random", , drop = FALSE]
  if (nrow(re) == 0) re <- meta[1, , drop = FALSE]
  md <- as.numeric(re$MD[1])
  # Prefer final non-NA TSA-adj random CI; else naive RE CI
  tsa_l <- df$TSAadjCIrandom_lower
  tsa_u <- df$TSAadjCIrandom_upper
  idx <- max(which(!is.na(tsa_l) & !is.na(tsa_u)), 0)
  if (idx > 0) {
    ci_l <- tsa_l[idx]; ci_u <- tsa_u[idx]; ci_src <- "TSA-adj"
  } else {
    ci_l <- as.numeric(re$lower.95CI[1]); ci_u <- as.numeric(re$upper.95CI[1]); ci_src <- "naive"
  }
  # Conclusion wording (neutral; reviewers: RIS reached ≠ futility)
  ris <- as.numeric(obj$results$SMA_RIS)
  ais <- as.numeric(obj$results$AIS)
  ris_reached <- ais >= ris
  if (mon_cross) {
    concl <- "Z-curve crossed O'Brien-Fleming superiority monitoring boundary."
  } else if (ris_reached && !fut_defined) {
    concl <- paste0(
      "RIS reached (AIS ", round(ais), " >= RIS ", round(ris),
      "); Z-curve remained within superiority monitoring boundaries. ",
      "Futility boundaries were not constructed (futility='none'); ",
      "exclusion of a clinically important IV benefit rests on the conventional ",
      sprintf("95%% CI relative to MCID %g mg (MD %.2f, %s CI %.2f to %.2f).",
              mc, md, ci_src, ci_l, ci_u)
    )
  } else if (!ris_reached) {
    concl <- sprintf(
      "RIS not reached (AIS %s < RIS %s); evidence inconclusive for MCID %g mg.",
      round(ais), round(ris), mc
    )
  } else {
    concl <- "See print.txt."
  }
  data.frame(
    scenario = scenario,
    mc_mg = mc,
    beta = beta,
    power_pct = round((1 - beta) * 100),
    RIS = round(ris),
    accrued_N = round(ais),
    n_studies = nrow(opioid_24h),
    monitoring_crossed = mon_cross,
    futility_defined = fut_defined,
    futility_crossed = fut_cross,
    MD = round(md, 2),
    CI_lower = round(ci_l, 2),
    CI_upper = round(ci_u, 2),
    CI_source = ci_src,
    conclusion = concl,
    stringsAsFactors = FALSE
  )
}

save_tsa <- function(obj, stem, title, mc, beta) {
  p <- plot(obj, model = "random", type = "classic", theme = "classic")
  p$labels$caption <- make_caption(beta, mc)
  p <- p + labs(title = title) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          plot.caption = element_text(size = 7.5, hjust = 0),
          plot.tag = element_text(size = 8, hjust = 0))
  ggsave(file.path(outdir, paste0(stem, ".png")), p, width = 8, height = 6.2, dpi = 300)
  ggsave(file.path(outdir, paste0(stem, ".pdf")), p, width = 8, height = 6.2)
  sink(file.path(outdir, paste0(stem, "_print.txt"))); print(obj); sink()
  ris <- as.numeric(obj$results$SMA_RIS)
  ais <- as.numeric(obj$results$AIS)
  writeLines(
    sprintf("RIS=%s; accrued N=%s; RE DL+HKSJ; fixed=FALSE; mc=%s; beta=%s; futility=none",
            round(ris), round(ais), mc, beta),
    file.path(outdir, paste0(stem, "_RIS.txt"))
  )
  cat("Wrote", stem, " RIS=", round(ris), " AIS=", round(ais), "\n")
}

run_one <- function(stem, title, mc, beta, scenario) {
  obj <- RTSA(
    type = "analysis", data = opioid_24h, outcome = "MD",
    mc = mc, sd_mc = 40,
    fixed = FALSE, re_method = "DL_HKSJ",
    alpha = 0.05, beta = beta, side = 2, es_alpha = "esOF"
  )
  save_tsa(obj, stem, title, mc, beta)
  summarize_tsa(obj, scenario, mc, beta)
}

specs <- list(
  list(stem = "Fig5a_TSA_opioid_24h",
       title = "Fig 5a. TSA - 24-h opioid (MCID 10 mg oral MME)",
       mc = 10, beta = 0.20,
       scenario = "Primary MCID 10 mg / 80% power",
       save_plot = FALSE),  # primary already from export_Fig5_TSA.R; still summarize
  list(stem = "TSA_opioid_24h_MCID5mg",
       title = "TSA sensitivity - 24-h opioid (MCID 5 mg oral MME, 80% power)",
       mc = 5, beta = 0.20,
       scenario = "Sensitivity MCID 5 mg / 80% power",
       save_plot = TRUE),
  list(stem = "TSA_opioid_24h_MCID15mg",
       title = "TSA sensitivity - 24-h opioid (MCID 15 mg oral MME, 80% power)",
       mc = 15, beta = 0.20,
       scenario = "Sensitivity MCID 15 mg / 80% power",
       save_plot = TRUE),
  list(stem = "TSA_opioid_24h_90power",
       title = "TSA sensitivity - 24-h opioid (MCID 10 mg oral MME, 90% power)",
       mc = 10, beta = 0.10,
       scenario = "Sensitivity MCID 10 mg / 90% power",
       save_plot = TRUE)
)

rows <- list()
for (sp in specs) {
  obj <- RTSA(
    type = "analysis", data = opioid_24h, outcome = "MD",
    mc = sp$mc, sd_mc = 40,
    fixed = FALSE, re_method = "DL_HKSJ",
    alpha = 0.05, beta = sp$beta, side = 2, es_alpha = "esOF"
  )
  if (isTRUE(sp$save_plot)) {
    save_tsa(obj, sp$stem, sp$title, sp$mc, sp$beta)
  } else {
    # Refresh primary RIS sidecar for consistency
    ris <- as.numeric(obj$results$SMA_RIS)
    ais <- as.numeric(obj$results$AIS)
    writeLines(
      sprintf("RIS=%s; accrued N=%s; RE DL+HKSJ; fixed=FALSE; mc=%s; beta=%s; futility=none",
              round(ris), round(ais), sp$mc, sp$beta),
      file.path(outdir, paste0(sp$stem, "_RIS.txt"))
    )
    cat("Primary summary only (plot not overwritten):", sp$stem,
        " RIS=", round(ris), " AIS=", round(ais), "\n")
  }
  rows[[length(rows) + 1]] <- summarize_tsa(obj, sp$scenario, sp$mc, sp$beta)
}

tab <- dplyr::bind_rows(rows)
write.csv(tab, file.path(outdir, "TSA_opioid_24h_sensitivity_summary.csv"), row.names = FALSE)

# Markdown one-pager
md <- c(
  "# TSA 24-h opioid: primary + sensitivities",
  "",
  paste0("- Workbook: `", xlsx, "`"),
  "- Model: RE DL+HKSJ (`fixed=FALSE`), O'Brien-Fleming esOF, alpha 5%, sd_mc=40",
  paste0("- Accrued N (AIS): ", accrued_n, " from ", nrow(opioid_24h), " trials (chronological by Publication Year)"),
  "- Futility: **none** (not constructed in RTSA settings) — reaching RIS without crossing a superiority monitoring boundary does **not** equal formal futility.",
  "",
  "| scenario | RIS | accrued N | monitoring crossed? | futility crossed? | MD [CI] | conclusion |",
  "|---|---:|---:|---|---|---|---|"
)
for (i in seq_len(nrow(tab))) {
  r <- tab[i, ]
  md <- c(md, sprintf(
    "| %s | %s | %s | %s | %s (defined=%s) | %.2f [%.2f, %.2f] (%s) | %s |",
    r$scenario, r$RIS, r$accrued_N,
    ifelse(r$monitoring_crossed, "yes", "no"),
    ifelse(r$futility_crossed, "yes", "no"),
    ifelse(r$futility_defined, "yes", "no"),
    r$MD, r$CI_lower, r$CI_upper, r$CI_source,
    gsub("\\|", "/", r$conclusion)
  ))
}
writeLines(md, file.path(outdir, "TSA_opioid_24h_sensitivity_summary.md"))
cat("\nSUMMARY:\n"); print(tab[, c("scenario","RIS","accrued_N","monitoring_crossed","futility_crossed","MD","CI_lower","CI_upper")])
cat("\nDONE opioid TSA sensitivities\n")
