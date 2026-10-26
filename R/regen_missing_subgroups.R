# =============================================================================
# regen_missing_subgroups.R
# =============================================================================
# Purpose: Fill gap subgroup forests for Add 6D / 7B / 7D / 8B / 8D composites
#          (movement by specialty/anesthesia; PONV by timing/specialty/anesthesia;
#           continuous secondaries by specialty/anesthesia)
# Manuscript: BMC Anesthesiology
# Methods: REML + HK; single-level Timing_Combined subgroups allowed (Add6)
#
# How to run: source("R/regen_missing_subgroups.R")
# =============================================================================

.r1_find_setup <- function() {
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
  stop("Cannot find R/00_setup.R — set SRMA_PROJECT_ROOT")
}
setup_path <- .r1_find_setup()
source(setup_path)
source(file.path(dirname(setup_path), "01_helpers.R"))

load_r1_packages()
ensure_outdir(OUTDIR)

dat_list   <- load_r1_workbook()
study_char <- annotate_study_char(dat_list[["Study_Characteristics"]])
pain_move  <- dat_list[["Pain_Scores_Movement"]]
ponv       <- dat_list[["PONV"]]
other_sec  <- dat_list[["Other_Secondaries"]]
outdir     <- OUTDIR

# Local aliases matching historical stems (SurgeryType stem, SurgerySpecialty var)
run_sub_cont <- function(dat, subgroup_var, label, file_stem) {
  run_sub(dat, subgroup_var, label, file_stem, outdir = outdir)
}

# ---- Movement by Surgery / Anesthesia / Timing -------------------------------
move_specs <- list(
  list("0to2h", "0-2h",
       "Pain_0to2h_IV_mean", "Pain_0to2h_IV_sd", "Pain_0to2h_IV_n",
       "Pain_0to2h_PO_mean", "Pain_0to2h_PO_sd", "Pain_0to2h_PO_n"),
  list("6to24h", "6-24h",
       "Pain_6to24h_IV_mean", "Pain_6to24h_IV_sd", "Pain_6to24h_IV_n",
       "Pain_6to24h_PO_mean", "Pain_6to24h_PO_sd", "Pain_6to24h_PO_n"),
  list("gt24h", ">24h",
       "Pain_gt24h_IV_mean", "Pain_gt24h_IV_sd", "Pain_gt24h_IV_n",
       "Pain_gt24h_PO_mean", "Pain_gt24h_PO_sd", "Pain_gt24h_PO_n")
)
for (ps in move_specs) {
  need <- unlist(ps[3:8])
  if (!all(need %in% names(pain_move))) next
  d <- do.call(scale_pain, c(list(pain_move), as.list(ps[3:8]))) %>%
    dplyr::left_join(
      study_char %>% dplyr::select(StudyID, Timing_Combined, SurgerySpecialty, ModeOfAnesthesia),
      by = "StudyID"
    )
  run_sub_cont(
    d, "SurgerySpecialty",
    paste0("Pain on Movement ", ps[[2]], "\nby surgical specialty"),
    paste0("Subgroup_PainMove_", ps[[1]], "_by_SurgeryType")
  )
  run_sub_cont(
    d, "ModeOfAnesthesia",
    paste0("Pain on Movement ", ps[[2]], "\nby anesthesia"),
    paste0("Subgroup_PainMove_", ps[[1]], "_by_ModeOfAnesthesia")
  )
  run_sub_cont(
    d, "Timing_Combined",
    paste0("Pain on Movement ", ps[[2]], "\nby timing"),
    paste0("Subgroup_PainMove_", ps[[1]], "_by_Timing")
  )
}

# ---- PONV by Timing / Surgery / Anesthesia -----------------------------------
ponv_c <- ponv %>%
  dplyr::filter(stringr::str_detect(as.character(TimeWindow_Hours), "0-24|PACU")) %>%
  dplyr::filter(!is.na(IV_events), !is.na(PO_events)) %>%
  dplyr::mutate(PONV_Definition = tidyr::replace_na(PONV_Definition, "Unspecified")) %>%
  dplyr::left_join(
    study_char %>% dplyr::select(StudyID, Timing_Combined, SurgerySpecialty, ModeOfAnesthesia),
    by = "StudyID"
  )
nausea <- ponv_c %>%
  dplyr::filter(stringr::str_detect(PONV_Definition, "Nausea|Unspecified")) %>%
  dplyr::transmute(StudyID, IV_events, IV_total, PO_events, PO_total,
                    Timing_Combined, SurgerySpecialty, ModeOfAnesthesia)
vomiting <- ponv_c %>%
  dplyr::filter(stringr::str_detect(PONV_Definition, "Vomiting")) %>%
  dplyr::transmute(StudyID, IV_events, IV_total, PO_events, PO_total,
                    Timing_Combined, SurgerySpecialty, ModeOfAnesthesia)

# Keep output stems *_by_SurgeryType while stratifying on SurgerySpecialty
sv_specs <- list(
  list(var = "Timing_Combined", lab = "timing", stem = "Timing_Combined"),
  list(var = "SurgerySpecialty", lab = "surgical specialty", stem = "SurgeryType"),
  list(var = "ModeOfAnesthesia", lab = "anesthesia", stem = "ModeOfAnesthesia")
)
for (sp in sv_specs) {
  run_sub_bin(
    nausea, sp$var, paste0("Nausea 0-24 h\nby ", sp$lab),
    paste0("Subgroup_PONV_Nausea_by_", sp$stem)
  )
  run_sub_bin(
    vomiting, sp$var, paste0("Vomiting 0-24 h\nby ", sp$lab),
    paste0("Subgroup_PONV_Vomiting_by_", sp$stem)
  )
}

# ---- Continuous secondaries by Surgery / Anesthesia --------------------------
sec_specs <- list(
  list("PACU_LOS", "PACU LOS",
       "PACU_LOS_min_IV_n", "PACU_LOS_min_IV_mean", "PACU_LOS_min_IV_sd",
       "PACU_LOS_min_PO_n", "PACU_LOS_min_PO_mean", "PACU_LOS_min_PO_sd"),
  list("Hosp_LOS", "Hospital LOS",
       "Hospital_LOS_days_IV_n", "Hospital_LOS_days_IV_mean", "Hospital_LOS_days_IV_sd",
       "Hospital_LOS_days_PO_n", "Hospital_LOS_days_PO_mean", "Hospital_LOS_days_PO_sd"),
  list("Sat", "Satisfaction",
       "Satisfaction_IV_n", "Satisfaction_IV_mean", "Satisfaction_IV_sd",
       "Satisfaction_PO_n", "Satisfaction_PO_mean", "Satisfaction_PO_sd"),
  list("Rescue", "Time to Rescue",
       "TimeToRescue_min_IV_n", "TimeToRescue_min_IV_mean", "TimeToRescue_min_IV_sd",
       "TimeToRescue_min_PO_n", "TimeToRescue_min_PO_mean", "TimeToRescue_min_PO_sd")
)
for (ss in sec_specs) {
  cols <- unlist(ss[3:8])
  if (!all(cols %in% names(other_sec))) next
  d <- other_sec %>%
    dplyr::transmute(
      StudyID,
      IV_n = .data[[cols[1]]], IV_mean = .data[[cols[2]]], IV_sd = .data[[cols[3]]],
      PO_n = .data[[cols[4]]], PO_mean = .data[[cols[5]]], PO_sd = .data[[cols[6]]]
    ) %>%
    dplyr::filter(!is.na(IV_n), !is.na(PO_n), !is.na(IV_mean), !is.na(PO_mean)) %>%
    dplyr::left_join(
      study_char %>% dplyr::select(StudyID, SurgerySpecialty, ModeOfAnesthesia),
      by = "StudyID"
    )
  run_sub_cont(
    d, "SurgerySpecialty", paste0(ss[[2]], "\nby surgical specialty"),
    paste0("Subgroup_", ss[[1]], "_by_SurgeryType")
  )
  run_sub_cont(
    d, "ModeOfAnesthesia", paste0(ss[[2]], "\nby anesthesia"),
    paste0("Subgroup_", ss[[1]], "_by_ModeOfAnesthesia")
  )
}
cat("MISSING SUBGROUPS DONE\n")
