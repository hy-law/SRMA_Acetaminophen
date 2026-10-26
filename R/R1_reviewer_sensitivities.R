# =============================================================================
# R1_reviewer_sensitivities.R
# =============================================================================
# Purpose: Reviewer-locked sensitivities (same-timing, MME leave-uncertain,
#          Lombardi as-labeled SD / drop-Lombardi, PONV with Wilson IPD)
# Manuscript: BMC Anesthesiology
# Estimator: REML + Hartung–Knapp (meta::metacont / metabin)
# Why Lombardi: Table 2 ± treated as SD in sensitivity only; primary sheet uses SEM→SD
# Why Wilson: author-supplied IPD event counts for nausea/vomiting primary pool
# Inputs:  revision workbook; Outputs: R1_outputs/, audit_output/
#
# How to run: source("R/R1_reviewer_sensitivities.R")
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
ensure_outdir(OUTDIR, AUDIT_DIR)

xlsx <- XLSX_PATH
stopifnot(file.exists(xlsx))
outdir <- OUTDIR

fmt_pool <- function(m, label) {
  k <- m$k
  n <- sum(m$n.e, na.rm = TRUE) + sum(m$n.c, na.rm = TRUE)
  md <- m$TE.random
  lo <- m$lower.random
  hi <- m$upper.random
  i2 <- m$I2
  sprintf("%s | k=%d N=%d | MD %.2f [%.2f, %.2f] | I2=%.1f%%",
          label, k, n, md, lo, hi, 100 * i2)
}

run_md <- function(dat, label) {
  m <- metacont(
    n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
    n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
    studlab = StudyID, data = dat, sm = "MD",
    common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK",
    prediction = (nrow(dat) >= 3),
    title = label
  )
  cat(fmt_pool(m, label), "\n")
  m
}

# --- Pain rest prep (0–10 scale) ---
pain_rest <- read_excel(xlsx, sheet = "Pain_Scores_Rest")
prep_pain <- function(prefix) {
  pain_rest %>%
    transmute(
      StudyID,
      PainScale,
      IV_n = .data[[paste0("Pain_", prefix, "_IV_n")]],
      IV_mean = .data[[paste0("Pain_", prefix, "_IV_mean")]],
      IV_sd = .data[[paste0("Pain_", prefix, "_IV_sd")]],
      PO_n = .data[[paste0("Pain_", prefix, "_PO_n")]],
      PO_mean = .data[[paste0("Pain_", prefix, "_PO_mean")]],
      PO_sd = .data[[paste0("Pain_", prefix, "_PO_sd")]]
    ) %>%
    filter(!is.na(IV_n), !is.na(PO_n), !is.na(IV_mean), !is.na(PO_mean)) %>%
    mutate(
      scale = if_else(str_detect(PainScale, "0-100|100"), 10, 1),
      IV_mean = IV_mean / scale,
      IV_sd = IV_sd / scale,
      PO_mean = PO_mean / scale,
      PO_sd = PO_sd / scale
    )
}

cat("\n========== PRIMARY PAIN 0–2 h ==========\n")
p02 <- prep_pain("0to2h")
m_p02 <- run_md(p02, "Pain rest 0–2 h PRIMARY")

# Same-timing preoperative (cleanest route contrast) — locked study set
same_pre <- c("Plunkett2017", "O'Neal2017", "Politi2017", "Pelzer2021")
mismatch <- c("Patel2020", "Bhoja2020", "Fenlon2013", "Lombardi2019", "Brett2012", "Schwenk2025")

cat("\n========== SAME-TIMING / MISMATCH (pain 0–2 h) ==========\n")
m_same <- run_md(filter(p02, StudyID %in% same_pre), "Same-timing preoperative")
m_mis <- run_md(filter(p02, StudyID %in% mismatch), "PO-pre vs IV-intra/peri (ERAS-like)")

# --- Opioid 0–24 ---
opioid <- read_excel(xlsx, sheet = "Opioid_Consumption_MME") %>%
  filter(str_detect(as.character(TimeWindow_Hours), "0-24")) %>%
  transmute(StudyID, IV_n, IV_mean = IV_mean_MME, IV_sd = IV_sd_MME,
            PO_n, PO_mean = PO_mean_MME, PO_sd = PO_sd_MME) %>%
  filter(!is.na(IV_n), !is.na(PO_n)) %>%
  distinct(StudyID, .keep_all = TRUE)

cat("\n========== PRIMARY OPIOID 0–24 h ==========\n")
m_op <- run_md(opioid, "Opioid 24 h PRIMARY")

uncertain <- c("Plunkett2017", "Patel2020", "Politi2017")
cat("\n========== MME LEAVE-UNCERTAIN / PATEL LOO ==========\n")
m_excl <- run_md(filter(opioid, !StudyID %in% uncertain), "Exclude Plunkett+Patel+Politi")
m_patel <- run_md(filter(opioid, StudyID != "Patel2020"), "Omit Patel2020 only (HK LOO)")
m_plunk <- run_md(filter(opioid, StudyID != "Plunkett2017"), "Omit Plunkett2017 only")
m_pol <- run_md(filter(opioid, StudyID != "Politi2017"), "Omit Politi2017 only")

# Other pain windows (primary only)
cat("\n========== OTHER PAIN WINDOWS ==========\n")
for (pfx in c("2to6h", "6to24h", "gt24h")) {
  d <- prep_pain(pfx)
  if (nrow(d) >= 2) run_md(d, paste("Pain rest", pfx))
}

# Write summary
sink(file.path(outdir, "R1_locked_pools_verify.txt"))
cat("Generated:", format(Sys.time(), tz = "Asia/Hong_Kong"), "HKT\n")
cat("Workbook:", xlsx, "\n\n")
cat(fmt_pool(m_p02, "Pain 0–2 h PRIMARY"), "\n")
cat(fmt_pool(m_same, "Same-timing preop"), "\n")
cat(fmt_pool(m_mis, "Mismatch ERAS-like"), "\n")
cat(fmt_pool(m_op, "Opioid PRIMARY"), "\n")
cat(fmt_pool(m_excl, "Opioid excl uncertain"), "\n")
cat(fmt_pool(m_patel, "Opioid omit Patel"), "\n")
sink()

# Save RDS for manuscript wiring
saveRDS(list(
  pain_0to2h = m_p02, same_timing = m_same, mismatch = m_mis,
  opioid = m_op, opioid_excl_uncertain = m_excl, opioid_omit_Patel = m_patel
), file.path(outdir, "R1_locked_models.rds"))

# Forest for same-timing + opioid primary
save_forest_dual <- function(file_stem, w, h, ...) {
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = w, height = h)
  meta::forest(...)
  dev.off()
  png(file.path(outdir, paste0(file_stem, ".png")), width = w*150, height = h*150, res = 150)
  meta::forest(...)
  dev.off()
}
save_forest_dual("Forest_SameTiming_Pain0to2h", 16, 7,
  m_same, smlab = "Same-timing Rest Pain 0-2 h\nIV vs PO",
  comb.random = TRUE, prediction = (m_same$k >= 3),
  xlab = "MD (IV - PO); lower favours IV",
  label.e = "Intravenous", label.c = "Oral",
  leftlabs = c("Study", "Total (IV)", "Mean (IV)", "SD (IV)",
               "Total (PO)", "Mean (PO)", "SD (PO)"),
  plotwidth = grid::unit(8.5, "cm"),
  colgap.forest.left = grid::unit(0.8, "cm"),
  colgap.forest.right = grid::unit(0.5, "cm"),
  fs.smlab = 10)
save_forest_dual("Forest_Opioid_excl_uncertain", 16, 7,
  m_excl, smlab = "24-h Opioid (MME), excl. uncertain\nIV vs PO",
  comb.random = TRUE, prediction = (m_excl$k >= 3),
  xlab = "MD MME (IV - PO)",
  label.e = "Intravenous", label.c = "Oral",
  leftlabs = c("Study", "Total (IV)", "Mean (IV)", "SD (IV)",
               "Total (PO)", "Mean (PO)", "SD (PO)"),
  plotwidth = grid::unit(8.5, "cm"),
  colgap.forest.left = grid::unit(0.8, "cm"),
  colgap.forest.right = grid::unit(0.5, "cm"),
  fs.smlab = 10)

cat("\nDONE. Wrote", normalizePath(outdir), "\n")

# =============================================================================
# Lombardi as-labeled SD sensitivity + drop-Lombardi + PONV (Wilson IPD primary)
# =============================================================================
cat("\n========== LOMBARDI AS-LABELED SD / DROP-LOMBARDI ==========\n")

# Printed Table 2 ± (treated as SD in this sensitivity only); SEM primary stays in sheet
lombardi_as_labeled <- list(
  `0to2h`  = list(IV_sd = 1.9, PO_sd = 2.7),
  `2to6h`  = list(IV_sd = 2.3, PO_sd = 2.2),
  `6to24h` = list(IV_sd = 1.7, PO_sd = 1.8)
)

fmt_list <- function(m) list(
  MD = unname(m$TE.random), lo = unname(m$lower.random), hi = unname(m$upper.random),
  k = m$k, N = sum(m$n.e) + sum(m$n.c), IV_n = sum(m$n.e), PO_n = sum(m$n.c),
  I2 = unname(m$I2), tau2 = unname(m$tau2), studies = m$studlab
)

lombardi_results <- list()
for (pfx in names(lombardi_as_labeled)) {
  d_pri <- prep_pain(pfx)
  # Primary already SEM->SD in sheet
  m_pri <- run_md(d_pri, paste0("pain_rest_", pfx, "__primary_SEM_to_SD"))
  lombardi_results[[paste0("pain_rest_", pfx, "__primary_SEM_to_SD")]] <- c(list(label = paste0("pain_rest_", pfx, "__primary_SEM_to_SD")), fmt_list(m_pri))

  # As-labeled: replace Lombardi SDs with printed ± /10 (0-10 scale)
  d_lab <- d_pri
  idx <- which(d_lab$StudyID == "Lombardi2019")
  if (length(idx) == 1) {
    d_lab$IV_sd[idx] <- lombardi_as_labeled[[pfx]]$IV_sd / 10
    d_lab$PO_sd[idx] <- lombardi_as_labeled[[pfx]]$PO_sd / 10
    m_lab <- run_md(d_lab, paste0("pain_rest_", pfx, "__as_labeled_SD"))
    lombardi_results[[paste0("pain_rest_", pfx, "__as_labeled_SD")]] <- c(list(label = paste0("pain_rest_", pfx, "__as_labeled_SD")), fmt_list(m_lab))
  }
  d_drop <- filter(d_pri, StudyID != "Lombardi2019")
  if (nrow(d_drop) >= 2) {
    m_drop <- run_md(d_drop, paste0("pain_rest_", pfx, "__drop_Lombardi"))
    lombardi_results[[paste0("pain_rest_", pfx, "__drop_Lombardi")]] <- c(list(label = paste0("pain_rest_", pfx, "__drop_Lombardi")), fmt_list(m_drop))
  }
}
saveRDS(lombardi_results, file.path(AUDIT_DIR, "Lombardi_as_labeled_sens_results.rds"))

cat("\n========== PONV NAUSEA / VOMITING (Wilson IPD primary) ==========\n")
ponv <- read_excel(xlsx, sheet = "PONV") %>%
  filter(str_detect(as.character(TimeWindow_Hours), "0-24|PACU")) %>%
  filter(!is.na(IV_events), !is.na(PO_events))

run_rr <- function(dat, label) {
  m <- metabin(event.e = IV_events, n.e = IV_total, event.c = PO_events, n.c = PO_total,
               studlab = StudyID, data = dat, sm = "RR",
               common = FALSE, random = TRUE, method.tau = "REML", method.random.ci = "HK",
               title = label)
  cat(sprintf("%s | k=%d N=%d | RR %.3f [%.3f, %.3f] | I2=%.1f%%\n",
              label, m$k, sum(m$n.e)+sum(m$n.c), exp(m$TE.random),
              exp(m$lower.random), exp(m$upper.random), 100*m$I2))
  # Also print Wilson row
  w <- dat %>% filter(StudyID == "Wilson2019")
  if (nrow(w)) cat("  Wilson row:", w$IV_events, "/", w$IV_total, "vs", w$PO_events, "/", w$PO_total, "\n")
  m
}

nausea <- ponv %>% filter(str_detect(PONV_Definition, "Nausea|Unspecified"))
vomiting <- ponv %>% filter(str_detect(PONV_Definition, "Vomiting"))
m_naus <- run_rr(nausea, "Nausea 0-24/PACU PRIMARY (Wilson IPD 9/15)")
m_vom <- run_rr(vomiting, "Vomiting 0-24/PACU PRIMARY (Wilson IPD 6/4)")

ponv_out <- list(
  nausea_IPD = list(k=m_naus$k, N=sum(m_naus$n.e)+sum(m_naus$n.c),
    RR=exp(m_naus$TE.random), lo=exp(m_naus$lower.random), hi=exp(m_naus$upper.random),
    I2=m_naus$I2, tau2=m_naus$tau2, studies=m_naus$studlab,
    IV_n=sum(m_naus$n.e), PO_n=sum(m_naus$n.c),
    study_counts=as.data.frame(nausea %>% select(StudyID, IV_e=IV_events, IV_n=IV_total, PO_e=PO_events, PO_n=PO_total))),
  vomiting_IPD = list(k=m_vom$k, N=sum(m_vom$n.e)+sum(m_vom$n.c),
    RR=exp(m_vom$TE.random), lo=exp(m_vom$lower.random), hi=exp(m_vom$upper.random),
    I2=m_vom$I2, tau2=m_vom$tau2, studies=m_vom$studlab,
    IV_n=sum(m_vom$n.e), PO_n=sum(m_vom$n.c),
    study_counts=as.data.frame(vomiting %>% select(StudyID, IV_e=IV_events, IV_n=IV_total, PO_e=PO_events, PO_n=PO_total)))
)
saveRDS(ponv_out, file.path(AUDIT_DIR, "Wilson_PONV_repool_results.rds"))

# Forests
if (!exists("save_forest_dual")) {
  save_forest_dual <- function(file_stem, w, h, ...) {
    pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = w, height = h)
    meta::forest(...)
    dev.off()
    png(file.path(outdir, paste0(file_stem, ".png")), width = w*150, height = h*150, res = 150)
    meta::forest(...)
    dev.off()
  }
}
save_forest_dual("Forest_PONV_Nausea_0-24h", 14, 7,
  m_naus, smlab = "Nausea 0-24 h\nIV vs PO",
  comb.random = TRUE, prediction = (m_naus$k >= 3),
  xlab = "RR (IV vs PO)",
  label.e = "Intravenous", label.c = "Oral",
  leftlabs = c("Study", "Events (IV)", "Total (IV)", "Events (PO)", "Total (PO)"),
  plotwidth = grid::unit(8.5, "cm"),
  colgap.forest.left = grid::unit(0.8, "cm"),
  colgap.forest.right = grid::unit(0.5, "cm"),
  fs.smlab = 10)
save_forest_dual("Forest_PONV_Vomiting_0-24h", 14, 7,
  m_vom, smlab = "Vomiting 0-24 h\nIV vs PO",
  comb.random = TRUE, prediction = (m_vom$k >= 3),
  xlab = "RR (IV vs PO)",
  label.e = "Intravenous", label.c = "Oral",
  leftlabs = c("Study", "Events (IV)", "Total (IV)", "Events (PO)", "Total (PO)"),
  plotwidth = grid::unit(8.5, "cm"),
  colgap.forest.left = grid::unit(0.8, "cm"),
  colgap.forest.right = grid::unit(0.5, "cm"),
  fs.smlab = 10)

# Append PONV + Lombardi lines to verify txt
sink(file.path(outdir, "R1_locked_pools_verify.txt"), append = TRUE)
cat("\n--- PONV (Wilson IPD) ---\n")
cat(sprintf("Nausea RR %.3f [%.3f, %.3f] k=%d I2=%.1f%%\n",
    ponv_out$nausea_IPD$RR, ponv_out$nausea_IPD$lo, ponv_out$nausea_IPD$hi,
    ponv_out$nausea_IPD$k, 100*ponv_out$nausea_IPD$I2))
cat(sprintf("Vomiting RR %.3f [%.3f, %.3f] k=%d I2=%.1f%%\n",
    ponv_out$vomiting_IPD$RR, ponv_out$vomiting_IPD$lo, ponv_out$vomiting_IPD$hi,
    ponv_out$vomiting_IPD$k, 100*ponv_out$vomiting_IPD$I2))
cat("\n--- Lombardi as-labeled / drop (see RDS) ---\n")
for (nm in names(lombardi_results)) {
  x <- lombardi_results[[nm]]
  cat(sprintf("%s | MD %.3f [%.3f, %.3f] k=%d I2=%.1f%%\n", nm, x$MD, x$lo, x$hi, x$k, 100*x$I2))
}
sink()

cat("\nSENSITIVITIES+PONV DONE\n")
