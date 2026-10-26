# =============================================================================
# 00_setup.R — paths, packages, locked maps (BMC Anesthesiology)
# =============================================================================
# Manuscript: IV vs oral peri-op acetaminophen SRMA (R1 revision)
# Primary MA: REML + Hartung–Knapp (meta:: method.tau="REML", method.random.ci="HK")
# TSA:       RTSA RE DL+HKSJ, fixed=FALSE, esOF, no futility
#
# How to run (from repository root, with workbook present):
#   source("R/00_setup.R")
# Or set env: Sys.setenv(SRMA_PROJECT_ROOT = "/path/to/project")
# =============================================================================

# ---- Project root ------------------------------------------------------------
# Override any time with: PROJECT_ROOT <- "/path/to/folder-with-xlsx"
# or: Sys.setenv(SRMA_PROJECT_ROOT = "...")
.resolve_project_root <- function() {
  env <- Sys.getenv("SRMA_PROJECT_ROOT", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }
  # Walk up from getwd() looking for the revision workbook
  candidates <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "../.."),
    # Box default used during R1 (override via env / PROJECT_ROOT)
    "/workspace/SRMA_Acetaminophen_revision"
  )
  xlsx_name <- "Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx"
  for (cand in candidates) {
    if (is.na(cand) || !nzchar(cand)) next
    if (file.exists(file.path(cand, xlsx_name))) {
      return(normalizePath(cand, winslash = "/", mustWork = TRUE))
    }
  }
  # Fall back to cwd so scripts still run; loaders will stop if xlsx missing
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

if (!exists("PROJECT_ROOT", inherits = FALSE) ||
    is.null(PROJECT_ROOT) || !nzchar(as.character(PROJECT_ROOT)[1])) {
  PROJECT_ROOT <- .resolve_project_root()
}
PROJECT_ROOT <- normalizePath(as.character(PROJECT_ROOT)[1], winslash = "/", mustWork = FALSE)

XLSX_NAME <- "Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx"
XLSX_PATH <- file.path(PROJECT_ROOT, XLSX_NAME)
OUTDIR    <- file.path(PROJECT_ROOT, "R1_outputs")
FIG5_DIR  <- file.path(PROJECT_ROOT, "figures_Fig5")
AUDIT_DIR <- file.path(PROJECT_ROOT, "audit_output")

# Optional personal library (harmless if absent)
.libPaths(unique(c(path.expand("~/R/library"), .libPaths())))

# ---- Packages ----------------------------------------------------------------
# Versions observed on the R1 analysis box (informational; install current CRAN OK):
#   meta, metafor, RTSA, readxl, dplyr, tidyr, stringr, ggplot2
# grid is base.
.require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste(sprintf('\"%s\"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_r1_packages <- function(extra = character()) {
  core <- c("meta", "metafor", "readxl", "dplyr", "tidyr", "stringr", "ggplot2", "RTSA")
  .require_pkgs(unique(c(core, extra)))
  suppressPackageStartupMessages({
    library(meta)
    library(metafor)
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
    library(grid)
    library(RTSA)
  })
  invisible(TRUE)
}

ensure_outdir <- function(...) {
  paths <- c(...)
  for (p in paths) dir.create(p, showWarnings = FALSE, recursive = TRUE)
  invisible(paths)
}

# ---- Locked stratification maps (manuscript Additional files 6–8) ------------
# Exact RCT SurgeryType strings → specialty used in Add7 forests.
SPECIALTY_MAP <- c(
  "Laparoscopic cholecystectomy" = "General surgery",
  "Laparoscopic inguinal hernia repair" = "General surgery",
  "Cesarean Delivery" = "Obstetrics",
  "Cesarean section" = "Obstetrics",
  "Primary unilateral THA" = "Orthopedics",
  "Unilateral TKA" = "Orthopedics",
  "Hip and knee arthroplasties" = "Orthopedics",
  "THA or TKA" = "Orthopedics",
  "Day case arthroscopy of the knee" = "Orthopedics",
  "Ambulatory lumbar spine surgery (microdiscectomy or single-level decompression)" = "Orthopedics",
  "FESS" = "ENT",
  "Robotic-assisted laparoscopic hysterectomy for benign indications" = "Gynecology",
  "CABG surgery with a median sternotomy, with the internal thoracic arteries grafts and saphenous vein harvesting" = "Cardiac",
  "Coronary artery bypass graft surgery with cardiopulmonary bypass (CPB)" = "Cardiac",
  "Lower third molar extraction" = "Dental",
  "Outpatient surgeries: arthroscopy; ear, nose, and throat; gynecology; laparoscopy; ophthalmology; orthopaedic total joint replacement; plastics; and podiatry." = "Mixed outpatient"
)

# ModeOfAnesthesia → GA / Neuraxial / NR (Add8).
# Neuraxial bundles spinal, epidural, and combined/continuous spinal–epidural (CSE).
ANAESTHESIA_MAP <- c(
  "General anaesthesia" = "General anaesthesia",
  "Spinal anaesthesia" = "Neuraxial anaesthesia",
  "Epidural anaesthesia" = "Neuraxial anaesthesia",
  "Combined spinal epidural anaesthesia" = "Neuraxial anaesthesia",
  "Combined spinal-epidural anaesthesia" = "Neuraxial anaesthesia",
  "Continuous spinal epidural anaesthesia" = "Neuraxial anaesthesia",
  "Continuous spinal-epidural anaesthesia" = "Neuraxial anaesthesia",
  "Not reported" = "Not reported"
)

ANAESTHESIA_LEVELS <- c(
  "General anaesthesia",
  "Neuraxial anaesthesia",
  "Not reported"
)

# ---- Workbook loader ---------------------------------------------------------
load_r1_workbook <- function(xlsx = XLSX_PATH) {
  if (!file.exists(xlsx)) {
    stop(
      "Workbook not found: ", xlsx,
      "\nSet PROJECT_ROOT or SRMA_PROJECT_ROOT to the folder containing ",
      XLSX_NAME, call. = FALSE
    )
  }
  sheets <- readxl::excel_sheets(xlsx)
  dat_list <- stats::setNames(
    lapply(sheets, function(s) readxl::read_excel(xlsx, sheet = s)),
    sheets
  )
  needed <- c(
    "Study_Characteristics", "Pain_Scores_Rest", "Pain_Scores_Movement",
    "Opioid_Consumption_MME", "PONV", "Other_Secondaries", "Risk_of_Bias"
  )
  miss <- setdiff(needed, names(dat_list))
  if (length(miss)) {
    stop("Workbook missing sheet(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  dat_list
}

# Attach SurgerySpecialty + remapped ModeOfAnesthesia to Study_Characteristics
annotate_study_char <- function(study_char, as_factor_moa = TRUE) {
  if (!"Timing_Combined" %in% names(study_char)) {
    stop("Timing_Combined missing from Study_Characteristics", call. = FALSE)
  }
  out <- study_char %>%
    dplyr::mutate(
      SurgerySpecialty = unname(SPECIALTY_MAP[as.character(SurgeryType)])
    )
  unmapped <- out %>%
    dplyr::filter(is.na(SurgerySpecialty)) %>%
    dplyr::select(StudyID, SurgeryType)
  if (nrow(unmapped) > 0) {
    print(unmapped)
    stop(
      sprintf("SurgerySpecialty map incomplete: %d StudyID(s) unmapped", nrow(unmapped)),
      call. = FALSE
    )
  }
  moa <- unname(ANAESTHESIA_MAP[as.character(out$ModeOfAnesthesia)])
  if (as_factor_moa) {
    out$ModeOfAnesthesia <- factor(moa, levels = ANAESTHESIA_LEVELS)
  } else {
    out$ModeOfAnesthesia <- moa
  }
  unmapped_moa <- out %>%
    dplyr::filter(is.na(ModeOfAnesthesia)) %>%
    dplyr::select(StudyID)
  if (nrow(unmapped_moa) > 0) {
    print(unmapped_moa)
    stop(
      sprintf("ModeOfAnesthesia map incomplete: %d StudyID(s) unmapped", nrow(unmapped_moa)),
      call. = FALSE
    )
  }
  cat(sprintf(
    "SurgerySpecialty map: %d/%d studies mapped\n",
    sum(!is.na(out$SurgerySpecialty)), nrow(out)
  ))
  cat(sprintf(
    "ModeOfAnesthesia map: %d/%d studies mapped | levels: %s\n",
    sum(!is.na(out$ModeOfAnesthesia)), nrow(out),
    paste(ANAESTHESIA_LEVELS, collapse = ", ")
  ))
  out
}

cat("00_setup.R loaded | PROJECT_ROOT =", PROJECT_ROOT, "\n")
