# =============================================================================
# run_R1_full_package.R
# =============================================================================
# Purpose: Main R1 pipeline — primary forests, subgroups, funnels; hooks TSA.
# Manuscript: BMC Anesthesiology (IV vs oral peri-op acetaminophen)
# Methods: REML + Hartung–Knapp; forests via meta; arm labels Intravenous/Oral
# Inputs:  Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx (PROJECT_ROOT)
# Outputs: R1_outputs/*.pdf|png, R1_primary_models.rds; may call TSA exporters
#
# How to run (repository root, workbook beside R/):
#   source("R/run_R1_full_package.R")
# Or: Sys.setenv(SRMA_PROJECT_ROOT = "/path/to/project"); source(...)
# =============================================================================

root_candidates <- c(
  if (exists("PROJECT_ROOT")) PROJECT_ROOT else "",
  Sys.getenv("SRMA_PROJECT_ROOT", unset = ""),
  getwd(),
  file.path(getwd(), "..")
)
setup_path <- NULL
for (cand in root_candidates) {
  if (!nzchar(cand)) next
  p <- file.path(cand, "R", "00_setup.R")
  if (file.exists(p)) { setup_path <- normalizePath(p); break }
  p2 <- file.path(cand, "00_setup.R")
  if (file.exists(p2)) { setup_path <- normalizePath(p2); break }
}
if (is.null(setup_path)) {
  # When this file lives in R/, sibling 00_setup.R
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  if (!is.null(here) && file.exists(file.path(here, "00_setup.R"))) {
    setup_path <- normalizePath(file.path(here, "00_setup.R"))
  }
}
if (is.null(setup_path)) stop("Cannot find R/00_setup.R — set SRMA_PROJECT_ROOT")
source(setup_path)
source(file.path(dirname(setup_path), "01_helpers.R"))

load_r1_packages()
ensure_outdir(OUTDIR, FIG5_DIR, AUDIT_DIR)

# ---- Data --------------------------------------------------------------------
dat_list   <- load_r1_workbook()
study_char <- annotate_study_char(dat_list[["Study_Characteristics"]])
pain_rest  <- dat_list[["Pain_Scores_Rest"]]
pain_move  <- dat_list[["Pain_Scores_Movement"]]
opioid_mme <- dat_list[["Opioid_Consumption_MME"]]
ponv       <- dat_list[["PONV"]]
other_sec  <- dat_list[["Other_Secondaries"]]

results <- list()

# ---- Pain at rest ------------------------------------------------------------
cat("\n===== PAIN AT REST =====\n")
pain_specs <- list(
  list("0to2h", "Pain at Rest 0-2 h\nIV vs PO", "Pain at Rest 0-2 h", "Forest_PainRest_0to2h_MD",
       "Pain_0to2h_IV_mean", "Pain_0to2h_IV_sd", "Pain_0to2h_IV_n",
       "Pain_0to2h_PO_mean", "Pain_0to2h_PO_sd", "Pain_0to2h_PO_n"),
  list("2to6h", "Pain at Rest 2-6 h\nIV vs PO", "Pain at Rest 2-6 h", "Forest_PainRest_2to6h_MD",
       "Pain_2to6h_IV_mean", "Pain_2to6h_IV_sd", "Pain_2to6h_IV_n",
       "Pain_2to6h_PO_mean", "Pain_2to6h_PO_sd", "Pain_2to6h_PO_n"),
  list("6to24h", "Pain at Rest 6-24 h\nIV vs PO", "Pain at Rest 6-24 h", "Forest_PainRest_6to24h_MD",
       "Pain_6to24h_IV_mean", "Pain_6to24h_IV_sd", "Pain_6to24h_IV_n",
       "Pain_6to24h_PO_mean", "Pain_6to24h_PO_sd", "Pain_6to24h_PO_n"),
  list("gt24h", "Pain at Rest >24 h\nIV vs PO", "Pain at Rest >24 h", "Forest_PainRest_gt24h_MD",
       "Pain_gt24h_IV_mean", "Pain_gt24h_IV_sd", "Pain_gt24h_IV_n",
       "Pain_gt24h_PO_mean", "Pain_gt24h_PO_sd", "Pain_gt24h_PO_n")
)
for (ps in pain_specs) {
  d <- do.call(scale_pain, c(list(pain_rest), as.list(ps[5:10])))
  d <- d %>%
    dplyr::left_join(
      study_char %>% dplyr::select(StudyID, Timing_Combined, SurgerySpecialty, ModeOfAnesthesia),
      by = "StudyID"
    )
  m <- run_cont(d, ps[[2]], ps[[4]])
  results[[ps[[1]]]] <- m
  if (!is.null(m)) {
    run_sub(d, "Timing_Combined", paste0(ps[[3]], "\nby timing"),
            paste0("Subgroup_PainRest_", ps[[1]], "_by_TimingCombined"))
    run_sub(d, "SurgerySpecialty", paste0(ps[[3]], "\nby surgical specialty"),
            paste0("Subgroup_PainRest_", ps[[1]], "_by_SurgeryType"))
    run_sub(d, "ModeOfAnesthesia", paste0(ps[[3]], "\nby anesthesia"),
            paste0("Subgroup_PainRest_", ps[[1]], "_by_ModeOfAnesthesia"))
  }
}

# ---- Pain on movement --------------------------------------------------------
cat("\n===== PAIN ON MOVEMENT =====\n")
move_specs <- list(
  list("0to2h", "Pain on Movement 0-2 h\nIV vs PO", "Pain on Movement 0-2 h", "Forest_PainMove_0to2h_MD",
       "Pain_0to2h_IV_mean", "Pain_0to2h_IV_sd", "Pain_0to2h_IV_n",
       "Pain_0to2h_PO_mean", "Pain_0to2h_PO_sd", "Pain_0to2h_PO_n"),
  list("6to24h", "Pain on Movement 6-24 h\nIV vs PO", "Pain on Movement 6-24 h", "Forest_PainMove_6to24h_MD",
       "Pain_6to24h_IV_mean", "Pain_6to24h_IV_sd", "Pain_6to24h_IV_n",
       "Pain_6to24h_PO_mean", "Pain_6to24h_PO_sd", "Pain_6to24h_PO_n"),
  list("gt24h", "Pain on Movement >24 h\nIV vs PO", "Pain on Movement >24 h", "Forest_PainMove_gt24h_MD",
       "Pain_gt24h_IV_mean", "Pain_gt24h_IV_sd", "Pain_gt24h_IV_n",
       "Pain_gt24h_PO_mean", "Pain_gt24h_PO_sd", "Pain_gt24h_PO_n")
)
for (ps in move_specs) {
  need <- unlist(ps[5:10])
  if (!all(need %in% names(pain_move))) {
    cat("SKIP move", ps[[1]], "cols missing\n")
    next
  }
  d <- do.call(scale_pain, c(list(pain_move), as.list(ps[5:10])))
  d <- d %>%
    dplyr::left_join(study_char %>% dplyr::select(StudyID, Timing_Combined), by = "StudyID")
  m <- run_cont(d, ps[[2]], ps[[4]])
  if (!is.null(m)) {
    run_sub(d, "Timing_Combined", paste0(ps[[3]], "\nby timing"),
            paste0("Subgroup_PainMove_", ps[[1]], "_by_Timing"))
  }
}

# ---- Opioid 0–24 h -----------------------------------------------------------
cat("\n===== OPIOID 0-24 =====\n")
opioid <- opioid_mme %>%
  dplyr::filter(stringr::str_detect(as.character(TimeWindow_Hours), "0-24")) %>%
  dplyr::transmute(
    StudyID, IV_n, IV_mean = IV_mean_MME, IV_sd = IV_sd_MME,
    PO_n, PO_mean = PO_mean_MME, PO_sd = PO_sd_MME
  ) %>%
  dplyr::filter(!is.na(IV_n), !is.na(PO_n)) %>%
  dplyr::distinct(StudyID, .keep_all = TRUE) %>%
  dplyr::left_join(
    study_char %>% dplyr::select(StudyID, Timing_Combined, SurgerySpecialty, ModeOfAnesthesia),
    by = "StudyID"
  )
m_op <- run_cont(opioid, "24-h Opioid (MME)\nIV vs PO", "Forest_Opioid_MME_0-24h",
                 xlab = "MD MME (IV - PO)")
results$opioid <- m_op
if (!is.null(m_op)) {
  run_sub(opioid, "Timing_Combined", "24-h Opioid (MME)\nby timing",
          "Subgroup_Opioid_MME_0-24h_by_TimingCombined")
  run_sub(opioid, "SurgerySpecialty", "24-h Opioid (MME)\nby surgical specialty",
          "Subgroup_Opioid_MME_0-24h_by_SurgeryType")
  run_sub(opioid, "ModeOfAnesthesia", "24-h Opioid (MME)\nby anesthesia",
          "Subgroup_Opioid_MME_0-24h_by_ModeOfAnesthesia")
}

# ---- PONV --------------------------------------------------------------------
cat("\n===== PONV =====\n")
ponv_c <- ponv %>%
  dplyr::filter(stringr::str_detect(as.character(TimeWindow_Hours), "0-24|PACU")) %>%
  dplyr::filter(!is.na(IV_events), !is.na(PO_events)) %>%
  dplyr::mutate(PONV_Definition = tidyr::replace_na(PONV_Definition, "Unspecified"))
nausea <- ponv_c %>% dplyr::filter(stringr::str_detect(PONV_Definition, "Nausea|Unspecified"))
vomiting <- ponv_c %>% dplyr::filter(stringr::str_detect(PONV_Definition, "Vomiting"))
results$nausea <- run_bin(nausea, "Nausea 0-24 h\nIV vs PO", "Forest_PONV_Nausea_0-24h")
results$vomiting <- run_bin(vomiting, "Vomiting 0-24 h\nIV vs PO", "Forest_PONV_Vomiting_0-24h")

# ---- Other secondaries -------------------------------------------------------
cat("\n===== OTHER SECONDARIES =====\n")
sec_specs <- list(
  list("PACU_LOS", "PACU LOS (min)\nIV vs PO", "PACU LOS", "Forest_PACU_LOS",
       "PACU_LOS_min_IV_n", "PACU_LOS_min_IV_mean", "PACU_LOS_min_IV_sd",
       "PACU_LOS_min_PO_n", "PACU_LOS_min_PO_mean", "PACU_LOS_min_PO_sd"),
  list("Hosp_LOS", "Hospital LOS (days)\nIV vs PO", "Hospital LOS", "Forest_Hospital_LOS",
       "Hospital_LOS_days_IV_n", "Hospital_LOS_days_IV_mean", "Hospital_LOS_days_IV_sd",
       "Hospital_LOS_days_PO_n", "Hospital_LOS_days_PO_mean", "Hospital_LOS_days_PO_sd"),
  list("Sat", "Satisfaction\nIV vs PO", "Satisfaction", "Forest_Satisfaction",
       "Satisfaction_IV_n", "Satisfaction_IV_mean", "Satisfaction_IV_sd",
       "Satisfaction_PO_n", "Satisfaction_PO_mean", "Satisfaction_PO_sd"),
  list("Rescue", "Time to Rescue (min)\nIV vs PO", "Time to Rescue", "Forest_TimeToRescue",
       "TimeToRescue_min_IV_n", "TimeToRescue_min_IV_mean", "TimeToRescue_min_IV_sd",
       "TimeToRescue_min_PO_n", "TimeToRescue_min_PO_mean", "TimeToRescue_min_PO_sd"),
  list("QoR", "QoR Score 24-48 h\nIV vs PO", "QoR Score", "Forest_QoR_Score",
       "QoR_Score_24to48h_IV_n", "QoR_Score_24to48h_IV_mean", "QoR_Score_24to48h_IV_sd",
       "QoR_Score_24to48h_PO_n", "QoR_Score_24to48h_PO_mean", "QoR_Score_24to48h_PO_sd")
)
for (ss in sec_specs) {
  cols <- unlist(ss[5:10])
  if (!all(cols %in% names(other_sec))) {
    cat("SKIP", ss[[1]], "\n")
    next
  }
  d <- other_sec %>%
    dplyr::transmute(
      StudyID,
      IV_n = .data[[cols[1]]], IV_mean = .data[[cols[2]]], IV_sd = .data[[cols[3]]],
      PO_n = .data[[cols[4]]], PO_mean = .data[[cols[5]]], PO_sd = .data[[cols[6]]]
    ) %>%
    dplyr::filter(!is.na(IV_n), !is.na(PO_n), !is.na(IV_mean), !is.na(PO_mean)) %>%
    dplyr::left_join(study_char %>% dplyr::select(StudyID, Timing_Combined), by = "StudyID")
  # QoR: only one trial contributes → allow min_k=1 for a single-study forest (Add5)
  min_k <- if (identical(ss[[1]], "QoR")) 1L else 2L
  m <- run_cont(d, ss[[2]], ss[[4]], min_k = min_k)
  if (!is.null(m) && nrow(d) >= 2) {
    run_sub(d, "Timing_Combined", paste0(ss[[3]], "\nby timing"),
            paste0("Subgroup_", ss[[1]], "_by_Timing"))
  }
}

# Rescue proportion (binary)
if (all(c(
  "RescueProportion_IV_events", "RescueProportion_IV_total",
  "RescueProportion_PO_events", "RescueProportion_PO_total"
) %in% names(other_sec))) {
  rp <- other_sec %>%
    dplyr::transmute(
      StudyID,
      IV_events = RescueProportion_IV_events, IV_total = RescueProportion_IV_total,
      PO_events = RescueProportion_PO_events, PO_total = RescueProportion_PO_total
    ) %>%
    dplyr::filter(!is.na(IV_events), !is.na(PO_events))
  run_bin(rp, "Rescue Analgesia\nIV vs PO", "Forest_RescueProportion")
}

# ---- Funnels (primary pain 0–2 h and opioid) ---------------------------------
if (!is.null(results[["0to2h"]])) {
  pdf(file.path(OUTDIR, "Funnel_PainRest_0to2h.pdf"), width = 8, height = 7)
  funnel(results[["0to2h"]])
  title("Funnel: Pain Score at Rest 0-2 Hours")
  dev.off()
  png(file.path(OUTDIR, "Funnel_PainRest_0to2h.png"), width = 8 * 150, height = 7 * 150, res = 150)
  funnel(results[["0to2h"]])
  title("Funnel: Pain Score at Rest 0-2 Hours")
  dev.off()
}
if (!is.null(results$opioid)) {
  pdf(file.path(OUTDIR, "Funnel_Opioid_MME_0-24h.pdf"), width = 8, height = 7)
  funnel(results$opioid)
  title("Funnel: 24-Hour Opioid Consumption (MME)")
  dev.off()
  png(file.path(OUTDIR, "Funnel_Opioid_MME_0-24h.png"), width = 8 * 150, height = 7 * 150, res = 150)
  funnel(results$opioid)
  title("Funnel: 24-Hour Opioid Consumption (MME)")
  dev.off()
}

saveRDS(results, file.path(OUTDIR, "R1_primary_models.rds"))
cat("\nPRIMARY FORESTS/SUBGROUPS DONE\n")

# ---- Optional TSA opioid sensitivities (standalone exporter) -----------------
tsa_sens <- file.path(dirname(setup_path), "export_TSA_opioid_sensitivities.R")
if (file.exists(tsa_sens)) {
  cat("\n===== FIG5 TSA opioid sensitivities =====\n")
  source(tsa_sens)
}

cat("\n----- sessionInfo -----\n")
print(sessionInfo())
