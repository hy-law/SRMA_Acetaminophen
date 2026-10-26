# =============================================================================
# regen_Add8_ModeOfAnesthesia.R
# =============================================================================
# Purpose: Focused regen of ModeOfAnesthesia forests only (Additional file 8).
#          Does not overwrite Timing/Surgery plots.
# Manuscript: BMC Anesthesiology
# Mapping: GA / Neuraxial (spinal+epidural+CSE) / Not reported — see ANAESTHESIA_MAP
# Methods: REML + HK; display order GA → Neuraxial → NR
#
# How to run: source("R/regen_Add8_ModeOfAnesthesia.R")
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
# as_factor_moa=FALSE so we can write the raw→mapped CSV then factor for forests
study_char_raw <- dat_list[["Study_Characteristics"]]
raw_moa <- study_char_raw %>%
  dplyr::transmute(StudyID, ModeOfAnesthesia_raw = as.character(ModeOfAnesthesia))
study_char <- annotate_study_char(study_char_raw, as_factor_moa = FALSE)
map_tbl <- raw_moa %>%
  dplyr::left_join(study_char %>% dplyr::select(StudyID, ModeOfAnesthesia), by = "StudyID")
write.csv(map_tbl, file.path(OUTDIR, "ModeOfAnesthesia_GA_NA_NR_map.csv"), row.names = FALSE)
cat(sprintf(
  "ModeOfAnesthesia map: %d/%d | %s\n",
  nrow(map_tbl), nrow(map_tbl),
  paste(names(table(map_tbl$ModeOfAnesthesia)), table(map_tbl$ModeOfAnesthesia),
        sep = "=", collapse = "; ")
))
print(map_tbl)

pain_rest  <- dat_list[["Pain_Scores_Rest"]]
pain_move  <- dat_list[["Pain_Scores_Movement"]]
opioid_mme <- dat_list[["Opioid_Consumption_MME"]]
ponv       <- dat_list[["PONV"]]
other_sec  <- dat_list[["Other_Secondaries"]]
outdir     <- OUTDIR

# Collect subgroup pools
pool_rows <- list()
record_pools <- function(m, outcome, sm) {
  if (is.null(m) || is.null(m$bylevs)) return(invisible(NULL))
  for (i in seq_along(m$bylevs)) {
    te <- m$TE.random.w[i]; lo <- m$lower.random.w[i]; up <- m$upper.random.w[i]
    if (sm == "RR") { te <- exp(te); lo <- exp(lo); up <- exp(up) }
    pool_rows[[length(pool_rows)+1]] <<- data.frame(
      outcome=outcome, subgroup=as.character(m$bylevs[i]), k=m$k.w[i],
      estimate=te, lower=lo, upper=up,
      I2=100*m$I2.w[i], tau2=m$tau2.w[i],
      subgroup_p=if (!is.null(m$pval.Q.b.random)) m$pval.Q.b.random else NA_real_,
      sm=sm, stringsAsFactors=FALSE)
  }
}

run_sub_cont <- function(dat, label, file_stem, outcome_key) {
  d <- dat %>% filter(!is.na(ModeOfAnesthesia)) %>% mutate(subgroup=ModeOfAnesthesia)
  if (nrow(d)<2 || dplyr::n_distinct(d$subgroup)<1) { cat("SKIP", file_stem, "\n"); return(NULL) }
  # Enforce display order
  d$subgroup <- factor(d$subgroup, levels=c("General anaesthesia","Neuraxial anaesthesia","Not reported"))
  m <- metacont(n.e=IV_n, mean.e=IV_mean, sd.e=IV_sd, n.c=PO_n, mean.c=PO_mean, sd.c=PO_sd,
                studlab=StudyID, data=d, sm="MD", common=FALSE, random=TRUE,
                method.tau="REML", method.random.ci="HK", subgroup=subgroup, test.subgroup=TRUE)
  forest_args <- list(m, smlab=label, comb.random=TRUE, xlab="MD (IV - PO)",
                      label.e="Intravenous", label.c="Oral",
                      leftlabs=c("Study", "Total (IV)", "Mean (IV)", "SD (IV)",
                                 "Total (PO)", "Mean (PO)", "SD (PO)"),
                      plotwidth=grid::unit(8.5, "cm"),
                      colgap.forest.left=grid::unit(0.8, "cm"),
                      colgap.forest.right=grid::unit(0.5, "cm"),
                      fs.smlab=10)
  h <- max(7, 0.5*m$k+5)
  pdf(file.path(outdir, paste0(file_stem,".pdf")), width=16, height=h)
  do.call(meta::forest, forest_args); dev.off()
  png(file.path(outdir, paste0(file_stem,".png")), width=16*150, height=h*150, res=150)
  do.call(meta::forest, forest_args); dev.off()
  record_pools(m, outcome_key, "MD")
  cat("Wrote", file_stem, " levels=", paste(levels(droplevels(d$subgroup)), collapse="|"), "\n")
  invisible(m)
}

run_sub_bin <- function(dat, label, file_stem, outcome_key) {
  d <- dat %>% filter(!is.na(ModeOfAnesthesia)) %>% mutate(subgroup=ModeOfAnesthesia)
  if (nrow(d)<2 || dplyr::n_distinct(d$subgroup)<1) { cat("SKIP", file_stem, "\n"); return(NULL) }
  d$subgroup <- factor(d$subgroup, levels=c("General anaesthesia","Neuraxial anaesthesia","Not reported"))
  m <- metabin(event.e=IV_events, n.e=IV_total, event.c=PO_events, n.c=PO_total,
               studlab=StudyID, data=d, sm="RR", common=FALSE, random=TRUE,
               method.tau="REML", method.random.ci="HK", subgroup=subgroup, test.subgroup=TRUE)
  forest_args <- list(m, smlab=label, comb.random=TRUE, xlab="RR (IV vs PO)",
                      label.e="Intravenous", label.c="Oral",
                      leftlabs=c("Study", "Events (IV)", "Total (IV)", "Events (PO)", "Total (PO)"),
                      plotwidth=grid::unit(8.5, "cm"),
                      colgap.forest.left=grid::unit(0.8, "cm"),
                      colgap.forest.right=grid::unit(0.5, "cm"),
                      fs.smlab=10)
  h <- max(7, 0.5*m$k+5)
  pdf(file.path(outdir, paste0(file_stem,".pdf")), width=14, height=h)
  do.call(meta::forest, forest_args); dev.off()
  png(file.path(outdir, paste0(file_stem,".png")), width=14*150, height=h*150, res=150)
  do.call(meta::forest, forest_args); dev.off()
  record_pools(m, outcome_key, "RR")
  cat("Wrote", file_stem, "\n"); invisible(m)
}

# --- Pain rest ---
pain_specs <- list(
  list("0to2h","0-2 h","Pain_0to2h_IV_mean","Pain_0to2h_IV_sd","Pain_0to2h_IV_n","Pain_0to2h_PO_mean","Pain_0to2h_PO_sd","Pain_0to2h_PO_n"),
  list("2to6h","2-6 h","Pain_2to6h_IV_mean","Pain_2to6h_IV_sd","Pain_2to6h_IV_n","Pain_2to6h_PO_mean","Pain_2to6h_PO_sd","Pain_2to6h_PO_n"),
  list("6to24h","6-24 h","Pain_6to24h_IV_mean","Pain_6to24h_IV_sd","Pain_6to24h_IV_n","Pain_6to24h_PO_mean","Pain_6to24h_PO_sd","Pain_6to24h_PO_n"),
  list("gt24h",">24 h","Pain_gt24h_IV_mean","Pain_gt24h_IV_sd","Pain_gt24h_IV_n","Pain_gt24h_PO_mean","Pain_gt24h_PO_sd","Pain_gt24h_PO_n")
)
for (ps in pain_specs) {
  d <- do.call(scale_pain, c(list(pain_rest), as.list(ps[3:8]))) %>%
    left_join(study_char %>% select(StudyID, ModeOfAnesthesia), by="StudyID")
  run_sub_cont(d, paste0("Pain at Rest ", ps[[2]], "\nby anesthesia"),
               paste0("Subgroup_PainRest_",ps[[1]],"_by_ModeOfAnesthesia"),
               paste0("PainRest_",ps[[1]]))
}

# --- Pain move ---
move_specs <- list(
  list("0to2h","0-2h","Pain_0to2h_IV_mean","Pain_0to2h_IV_sd","Pain_0to2h_IV_n","Pain_0to2h_PO_mean","Pain_0to2h_PO_sd","Pain_0to2h_PO_n"),
  list("6to24h","6-24h","Pain_6to24h_IV_mean","Pain_6to24h_IV_sd","Pain_6to24h_IV_n","Pain_6to24h_PO_mean","Pain_6to24h_PO_sd","Pain_6to24h_PO_n"),
  list("gt24h",">24h","Pain_gt24h_IV_mean","Pain_gt24h_IV_sd","Pain_gt24h_IV_n","Pain_gt24h_PO_mean","Pain_gt24h_PO_sd","Pain_gt24h_PO_n")
)
for (ps in move_specs) {
  need <- unlist(ps[3:8])
  if (!all(need %in% names(pain_move))) next
  d <- do.call(scale_pain, c(list(pain_move), as.list(ps[3:8]))) %>%
    left_join(study_char %>% select(StudyID, ModeOfAnesthesia), by="StudyID")
  run_sub_cont(d, paste0("Pain on Movement ", ps[[2]], "\nby anesthesia"),
               paste0("Subgroup_PainMove_",ps[[1]],"_by_ModeOfAnesthesia"),
               paste0("PainMove_",ps[[1]]))
}

# --- Opioid ---
opioid <- opioid_mme %>%
  filter(str_detect(as.character(TimeWindow_Hours), "0-24")) %>%
  transmute(StudyID, IV_n, IV_mean=IV_mean_MME, IV_sd=IV_sd_MME,
            PO_n, PO_mean=PO_mean_MME, PO_sd=PO_sd_MME) %>%
  filter(!is.na(IV_n), !is.na(PO_n)) %>% distinct(StudyID, .keep_all=TRUE) %>%
  left_join(study_char %>% select(StudyID, ModeOfAnesthesia), by="StudyID")
run_sub_cont(opioid, "24-h Opioid (MME)\nby anesthesia",
             "Subgroup_Opioid_MME_0-24h_by_ModeOfAnesthesia", "Opioid_0-24h")

# --- PONV ---
ponv_c <- ponv %>%
  filter(str_detect(as.character(TimeWindow_Hours), "0-24|PACU")) %>%
  filter(!is.na(IV_events), !is.na(PO_events)) %>%
  mutate(PONV_Definition = replace_na(PONV_Definition, "Unspecified")) %>%
  left_join(study_char %>% select(StudyID, ModeOfAnesthesia), by="StudyID")
nausea <- ponv_c %>% filter(str_detect(PONV_Definition, "Nausea|Unspecified")) %>%
  transmute(StudyID, IV_events, IV_total, PO_events, PO_total, ModeOfAnesthesia)
vomiting <- ponv_c %>% filter(str_detect(PONV_Definition, "Vomiting")) %>%
  transmute(StudyID, IV_events, IV_total, PO_events, PO_total, ModeOfAnesthesia)
run_sub_bin(nausea, "Nausea 0-24 h\nby anesthesia", "Subgroup_PONV_Nausea_by_ModeOfAnesthesia", "Nausea")
run_sub_bin(vomiting, "Vomiting 0-24 h\nby anesthesia", "Subgroup_PONV_Vomiting_by_ModeOfAnesthesia", "Vomiting")

# --- Secondaries ---
sec_specs <- list(
  list("PACU_LOS","PACU LOS","PACU_LOS_min_IV_n","PACU_LOS_min_IV_mean","PACU_LOS_min_IV_sd","PACU_LOS_min_PO_n","PACU_LOS_min_PO_mean","PACU_LOS_min_PO_sd"),
  list("Hosp_LOS","Hospital LOS","Hospital_LOS_days_IV_n","Hospital_LOS_days_IV_mean","Hospital_LOS_days_IV_sd","Hospital_LOS_days_PO_n","Hospital_LOS_days_PO_mean","Hospital_LOS_days_PO_sd"),
  list("Sat","Satisfaction","Satisfaction_IV_n","Satisfaction_IV_mean","Satisfaction_IV_sd","Satisfaction_PO_n","Satisfaction_PO_mean","Satisfaction_PO_sd"),
  list("Rescue","Time to Rescue","TimeToRescue_min_IV_n","TimeToRescue_min_IV_mean","TimeToRescue_min_IV_sd","TimeToRescue_min_PO_n","TimeToRescue_min_PO_mean","TimeToRescue_min_PO_sd")
)
for (ss in sec_specs) {
  cols <- unlist(ss[3:8])
  if (!all(cols %in% names(other_sec))) next
  d <- other_sec %>%
    transmute(StudyID, IV_n=.data[[cols[1]]], IV_mean=.data[[cols[2]]], IV_sd=.data[[cols[3]]],
              PO_n=.data[[cols[4]]], PO_mean=.data[[cols[5]]], PO_sd=.data[[cols[6]]]) %>%
    filter(!is.na(IV_n), !is.na(PO_n), !is.na(IV_mean), !is.na(PO_mean)) %>%
    left_join(study_char %>% select(StudyID, ModeOfAnesthesia), by="StudyID")
  run_sub_cont(d, paste0(ss[[2]], "\nby anesthesia"),
               paste0("Subgroup_",ss[[1]],"_by_ModeOfAnesthesia"), ss[[1]])
}

pools <- bind_rows(pool_rows)
write.csv(pools, file.path(outdir, "ModeOfAnesthesia_GA_NA_NR_pools.csv"), row.names=FALSE)
cat("\n===== POOLS =====\n")
print(pools, digits=4, row.names=FALSE)
cat("ADD8 MODEOFANESTHESIA REGEN DONE\n")
