# =============================================================================
# export_Fig5_TSA.R
# =============================================================================
# Purpose: Standalone Fig 5a–e TSA export (RE DL+HKSJ only; no fixed panels)
# Manuscript: BMC Anesthesiology
# TSA settings: fixed=FALSE, re_method="DL_HKSJ", alpha=0.05, beta=0.20,
#               es_alpha="esOF", futility none
# MCID: opioid 10 mg oral MME (sd_mc=40); pain 1.0 on 0–10 scale (sd_mc=2.5)
# Inputs:  revision workbook under PROJECT_ROOT
# Outputs: figures_Fig5/Fig5*.png|pdf + _print.txt + _RIS.txt
# Also sources export_TSA_opioid_sensitivities.R (MCID 5/15; 90% power)
#
# How to run: source("R/export_Fig5_TSA.R")
# =============================================================================

.root_setup <- function() {
  cands <- c(Sys.getenv("SRMA_PROJECT_ROOT", ""), getwd(), file.path(getwd(), ".."))
  for (cand in cands) {
    if (!nzchar(cand)) next
    p <- file.path(cand, "R", "00_setup.R")
    if (file.exists(p)) return(normalizePath(p))
    p2 <- file.path(cand, "00_setup.R")
    if (file.exists(p2)) return(normalizePath(p2))
  }
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
  p <- file.path(here, "00_setup.R")
  if (file.exists(p)) return(normalizePath(p))
  stop("Cannot find R/00_setup.R")
}
setup_path <- .root_setup()
source(setup_path)
load_r1_packages()
ensure_outdir(FIG5_DIR)

dat_list <- load_r1_workbook()
char <- dat_list[["Study_Characteristics"]]
if (!"Publication Year" %in% names(char)) {
  char$`Publication Year` <- seq_len(nrow(char))
}
outdir <- FIG5_DIR

# ---- Helpers -----------------------------------------------------------------
prep_pain <- function(iv_m, iv_sd, iv_n, po_m, po_sd, po_n) {
  dat_list[["Pain_Scores_Rest"]] %>%
    dplyr::filter(!is.na(.data[[iv_m]]) & !is.na(.data[[po_m]])) %>%
    dplyr::left_join(char %>% dplyr::select(StudyID, `Publication Year`), by = "StudyID") %>%
    dplyr::arrange(`Publication Year`) %>%
    dplyr::mutate(
      scale_factor = ifelse(
        grepl("100", PainScale) | grepl("VAS 0-100", PainScale, ignore.case = TRUE),
        10, 1
      ),
      mI = .data[[iv_m]] / scale_factor, sdI = .data[[iv_sd]] / scale_factor, nI = .data[[iv_n]],
      mC = .data[[po_m]] / scale_factor, sdC = .data[[po_sd]] / scale_factor, nC = .data[[po_n]],
      study = StudyID
    ) %>%
    dplyr::select(study, mI, sdI, nI, mC, sdC, nC) %>%
    tidyr::drop_na()
}

# Caption replaces RTSA's Fixed-effect footer (RTSA may flip settings$fixed when tau2==0)
save_tsa <- function(obj, stem, title, beta = 0.20, mc = NULL) {
  p <- plot(obj, model = "random", type = "classic", theme = "classic")
  power_pct <- round((1 - beta) * 100)
  beta_pct  <- round(beta * 100)
  mc_bit <- if (is.null(mc)) "" else paste0("; MCID ", mc)
  p$labels$caption <- paste0(
    "Retrospective TSA; alpha 5%, beta ", beta_pct, "% (", power_pct, "% power), ",
    "O'Brien-Fleming (esOF)", mc_bit, ".\n",
    "Methods: Random-effects DL+HKSJ only (fixed = FALSE). Futility boundaries: none."
  )
  p <- p + ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      plot.tag = ggplot2::element_text(size = 8, hjust = 0)
    )
  ggplot2::ggsave(file.path(outdir, paste0(stem, ".png")), p, width = 8, height = 6.2, dpi = 300)
  ggplot2::ggsave(file.path(outdir, paste0(stem, ".pdf")), p, width = 8, height = 6.2)
  sink(file.path(outdir, paste0(stem, "_print.txt")))
  print(obj)
  sink()
  ris <- as.numeric(obj$results$SMA_RIS)
  ais <- as.numeric(obj$results$AIS)
  writeLines(
    sprintf(
      "RIS=%s; accrued N=%s; RE DL+HKSJ; fixed=FALSE; beta=%s; futility=none",
      round(ris), round(ais), beta
    ),
    file.path(outdir, paste0(stem, "_RIS.txt"))
  )
  cat("Wrote", stem, " RIS=", round(ris), " AIS=", round(ais), "\n")
}

# ---- Opioid primary (Fig 5a) -------------------------------------------------
opioid_24h <- dat_list[["Opioid_Consumption_MME"]] %>%
  dplyr::filter(TimeWindow_Hours == "0-24") %>%
  dplyr::left_join(char %>% dplyr::select(StudyID, `Publication Year`), by = "StudyID") %>%
  dplyr::arrange(`Publication Year`) %>%
  dplyr::mutate(
    mI = IV_mean_MME, sdI = IV_sd_MME, nI = IV_n,
    mC = PO_mean_MME, sdC = PO_sd_MME, nC = PO_n, study = StudyID
  ) %>%
  dplyr::select(study, mI, sdI, nI, mC, sdC, nC) %>%
  tidyr::drop_na()

tsa_opioid <- RTSA(
  type = "analysis", data = opioid_24h, outcome = "MD",
  mc = 10, sd_mc = 40,
  fixed = FALSE, re_method = "DL_HKSJ",
  alpha = 0.05, beta = 0.20, side = 2, es_alpha = "esOF"
)
save_tsa(
  tsa_opioid, "Fig5a_TSA_opioid_24h",
  "Fig 5a. TSA - 24-h opioid (MCID 10 mg oral MME)",
  beta = 0.20, mc = 10
)

# ---- Pain windows (Fig 5b–e) -------------------------------------------------
specs <- list(
  list(
    "Fig5b_TSA_pain_0to2h", "Fig 5b. TSA - Pain at rest 0-2 h (MCID 1.0)",
    c("Pain_0to2h_IV_mean", "Pain_0to2h_IV_sd", "Pain_0to2h_IV_n",
      "Pain_0to2h_PO_mean", "Pain_0to2h_PO_sd", "Pain_0to2h_PO_n")
  ),
  list(
    "Fig5c_TSA_pain_2to6h", "Fig 5c. TSA - Pain at rest 2-6 h (MCID 1.0)",
    c("Pain_2to6h_IV_mean", "Pain_2to6h_IV_sd", "Pain_2to6h_IV_n",
      "Pain_2to6h_PO_mean", "Pain_2to6h_PO_sd", "Pain_2to6h_PO_n")
  ),
  list(
    "Fig5d_TSA_pain_6to24h", "Fig 5d. TSA - Pain at rest 6-24 h (MCID 1.0)",
    c("Pain_6to24h_IV_mean", "Pain_6to24h_IV_sd", "Pain_6to24h_IV_n",
      "Pain_6to24h_PO_mean", "Pain_6to24h_PO_sd", "Pain_6to24h_PO_n")
  ),
  list(
    "Fig5e_TSA_pain_gt24h", "Fig 5e. TSA - Pain at rest >24 h (MCID 1.0)",
    c("Pain_gt24h_IV_mean", "Pain_gt24h_IV_sd", "Pain_gt24h_IV_n",
      "Pain_gt24h_PO_mean", "Pain_gt24h_PO_sd", "Pain_gt24h_PO_n")
  )
)
for (ps in specs) {
  d <- do.call(prep_pain, as.list(ps[[3]]))
  obj <- RTSA(
    type = "analysis", data = d, outcome = "MD",
    mc = 1.0, sd_mc = 2.5,
    fixed = FALSE, re_method = "DL_HKSJ",
    alpha = 0.05, beta = 0.20, side = 2, es_alpha = "esOF"
  )
  save_tsa(obj, ps[[1]], ps[[2]], beta = 0.20, mc = 1.0)
}
cat("DONE primary Fig5\n")

# Opioid MCID/power sensitivities (Reviewer 4)
cat("\n===== Opioid TSA sensitivities =====\n")
sens <- file.path(dirname(setup_path), "export_TSA_opioid_sensitivities.R")
if (file.exists(sens)) source(sens, local = TRUE)
cat("DONE\n")
