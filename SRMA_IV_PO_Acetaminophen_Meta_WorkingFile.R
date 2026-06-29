# =============================================================================
# Intravenous versus oral perioperative acetaminophen for postoperative acute pain management in adults
# A systematic review, meta-analysis, subgroup analysis and trial sequential analysis of randomized controlled trials
#
# Date: [2026-06-30]
# R version: 4.5.2
#
# License: MIT (see LICENSE file in repository root)
#
# DISCLAIMER
# This code is provided "AS IS" for research, educational, and reproducibility
# purposes ONLY. It was developed specifically to reproduce the analyses
# reported in our systematic review and meta-analysis of IV vs PO acetaminophen
# (including random-effects models, subgroup analyses by mode of anesthesia/
# surgery type/timing, influence analyses, and funnel plots).
#
# THIS CODE IS NOT INTENDED FOR CLINICAL DECISION-MAKING, PATIENT CARE,
# OR ANY MEDICAL APPLICATION WHATSOEVER.
#
# The authors provide NO WARRANTIES, express or implied, regarding accuracy,
# completeness, reliability, or fitness for any particular purpose. Users
# assume ALL risk and full responsibility for any use, adaptation, or
# interpretation of this code and its outputs. Results must be independently
# verified against the source data (study-level aggregate data from the
# accompanying Excel extraction file) and primary publications.
#
# When using or adapting this code, please cite:
#   1. The associated peer-reviewed publication (when available).
#   2. This GitHub repository (with commit hash or release tag).
#
# For questions or collaboration: henrylawheiyeung@gmail.com
# =============================================================================

install.packages("meta")
install.packages("metafor")
install.packages("metaviz")
install.packages("forestploter")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("robvis")
install.packages("magick")
install.packages("pdftools")
install.packages("RTSA")
install.packages("tidyverse")

library(meta)
library(metafor)
library(metaviz)
library(forestploter)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(grid)
library(robvis)
library(magick)
library(pdftools)
library(RTSA)
library(tidyverse)

# Confirm all sheets
excel_sheets("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx")

# Import all sheets
dat_list <- lapply(excel_sheets("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx"), 
                   function(sheet) read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", sheet = sheet))

names(dat_list) <- excel_sheets("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx")

# Assign to individual data frames
study_char   <- dat_list[[1]]
rob          <- dat_list[[2]]
pain_rest    <- dat_list[[3]]
pain_move    <- dat_list[[4]]
opioid_mme   <- dat_list[[5]]
ponv         <- dat_list[[6]]
other_sec    <- dat_list[[7]]

# ==========================
# ===== 1.PAIN AT REST =====
# ==========================

# FUNCTION TO ANALYSE ANY TIME POINT
analyze_rest_pain <- function(time_prefix, time_label) {
  
  # 1. Extract columns for this time window
  dat <- pain_rest %>%
    select(StudyID, PainScale,
           IV_n   = paste0("Pain_", time_prefix, "_IV_n"),
           IV_mean = paste0("Pain_", time_prefix, "_IV_mean"),
           IV_sd   = paste0("Pain_", time_prefix, "_IV_sd"),
           PO_n   = paste0("Pain_", time_prefix, "_PO_n"),
           PO_mean = paste0("Pain_", time_prefix, "_PO_mean"),
           PO_sd   = paste0("Pain_", time_prefix, "_PO_sd")) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%   # complete cases only
    mutate(
      Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
      IV_mean_10 = if_else(Scale_Type == "0-100", IV_mean / 10, IV_mean),
      IV_sd_10   = if_else(Scale_Type == "0-100", IV_sd / 10, IV_sd),
      PO_mean_10 = if_else(Scale_Type == "0-100", PO_mean / 10, PO_mean),
      PO_sd_10   = if_else(Scale_Type == "0-100", PO_sd / 10, PO_sd)
    )
  
  cat("\n=== ", time_label, "===\n")
  cat("Number of studies:", nrow(dat), "\n")
  
  # 2. Run meta-analysis (identical settings to primary)
  m <- metacont(n.e   = IV_n,   mean.e = IV_mean_10, sd.e = IV_sd_10,
                n.c   = PO_n,   mean.c = PO_mean_10, sd.c = PO_sd_10,
                studlab = StudyID,
                data    = dat,
                sm      = "MD",
                fixed   = FALSE,
                random  = TRUE,
                method.tau = "REML",
                hakn    = TRUE,
                title   = paste("Pain at Rest", time_label, ": IV vs PO Acetaminophen"))
  
  print(summary(m))
  
  # 3. Forest plot with heading
  pdf(paste0("Forest_PainRest_", time_prefix, "_MD.pdf"), width = 15.5, height = 9.8)
  meta::forest(m,
               smlab = paste("Pain Score at Rest", time_label, ":\nIV vs PO Acetaminophen"),
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               label.e = "Intraveous",
               label.c = "Oral",
               xlab = "Mean Difference (IV minus PO) — lower values favour IV",
               col.square = "#4575b4",
               col.diamond = "#d73027",
               col.predict = "darkred",
               comb.random = TRUE,
               prediction = TRUE,
               print.tau2 = TRUE,
               print.I2 = TRUE,
               print.Q = TRUE,
               print.pval.Q = TRUE,
               fs.main = 13.5,
               fs.study = 10.5,
               fs.study.label = 11,
               fontsize = 10,
               rows.gr = 2.2)
  dev.off()
  
  return(m)   # save the model object for later use
}

# === RUN FOR ALL TIME POINTS ===
m_0to2h   <- analyze_rest_pain("0to2h",   "0–2 Hours")
m_2to6h   <- analyze_rest_pain("2to6h",   "2–6 Hours")
m_6to24h  <- analyze_rest_pain("6to24h",  "6–24 Hours")
m_gt24h   <- analyze_rest_pain("gt24h",   ">24 Hours")

# === Capture each forest plot ===
p1 <- grid.grabExpr({
  meta::forest(m_0to2h,
               smlab = "A. 0–2 Hours",
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4", col.diamond = "#d73027",
               prediction = TRUE, comb.random = TRUE,
               print.tau2 = TRUE, print.I2 = TRUE)
})

p2 <- grid.grabExpr({
  meta::forest(m_2to6h,
               smlab = "B. 2–6 Hours",
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4", col.diamond = "#d73027",
               prediction = TRUE, comb.random = TRUE,
               print.tau2 = TRUE, print.I2 = TRUE)
})

p3 <- grid.grabExpr({
  meta::forest(m_6to24h,
               smlab = "C. 6–24 Hours",
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4", col.diamond = "#d73027",
               prediction = TRUE, comb.random = TRUE,
               print.tau2 = TRUE, print.I2 = TRUE)
})

p4 <- grid.grabExpr({
  meta::forest(m_gt24h,
               smlab = "D. >24 Hours",
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4", col.diamond = "#d73027",
               prediction = TRUE, comb.random = TRUE,
               print.tau2 = TRUE, print.I2 = TRUE)
})

# === Combine the four captured plots into ONE figure ===
combined <- 
  wrap_elements(full = p1) / 
  wrap_elements(full = p2) / 
  wrap_elements(full = p3) / 
  wrap_elements(full = p4) +
  plot_annotation(
    title = "Pain Scores at Rest: Intravenous vs Oral Acetaminophen",
    subtitle = "All postoperative time points (standardised to 0–10 VAS/NRS scale)",
    caption = "Positive MD = higher pain with IV acetaminophen.\nRandom-effects model (REML + Hartung–Knapp). All analyses followed Harrer et al. (2021), Chapter 9.",
    tag_levels = "A",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 13, hjust = 0.5))
  )

# === Save as high-resolution PDF ===
ggsave("Figure1_Pain_at_Rest_All_Time_Points_PUBLICATION.pdf", 
       combined, width = 15.5, height = 23, dpi = 800, bg = "white")


# ==================================================================
# ===== 2. INFLUENCE / LEAVE-ONE-OUT ANALYSIS FOR PAIN AT REST =====
# ==================================================================

# Function to run influence analysis and save plot
run_influence <- function(model, time_label) {
  cat("\n=== Influence Analysis:", time_label, "===\n")
  
  # Leave-one-out meta-analysis
  inf <- metainf(model)
  print(inf)
  
  # Influence forest plot (saved as PDF)
  pdf(paste0("Influence_PainRest_", time_label, ".pdf"), 
      width = 14, height = 9)
  meta::forest(inf,
               smlab = paste("Influence Analysis: ", time_label),
               xlab = "Mean Difference (IV minus PO)",
               leftcols = c("studlab", "TE", "lower", "upper"),
               leftlabs = c("Omitted Study", "MD", "Lower", "Upper"),
               col.square = "#4575b4",
               col.diamond = "#d73027",
               prediction = FALSE,
               print.tau2 = TRUE,
               print.I2 = TRUE,
               fs.smlab = 12)
  dev.off()
  
  return(inf)
}

# Run for the three time points with moderate heterogeneity
inf_0to2h   <- run_influence(m_0to2h,   "0–2hrs")
inf_2to6h   <- run_influence(m_2to6h,   "2–6hrs")
inf_6to24h  <- run_influence(m_6to24h,  "6–24hrs")

# ==============================================
# ===== 3. OPIOID CONSUMPTION (0–24 h MME) =====
# ==============================================

# Load the opioid sheet
opioid_mme <- read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", 
                         sheet = 5, col_names = TRUE)

# 1. Clean to 0–24 h window (handles multiple rows per study)
opioid_24h <- opioid_mme %>%
  filter(str_detect(TimeWindow_Hours, "0-24")) %>%   # selects all 0-24 h rows
  select(StudyID, 
         IV_n = IV_n,
         IV_mean = IV_mean_MME,
         IV_sd   = IV_sd_MME,
         PO_n = PO_n,
         PO_mean = PO_mean_MME,
         PO_sd   = PO_sd_MME) %>%
  filter(!is.na(IV_n) & !is.na(PO_n)) %>%   # complete cases only
  distinct(StudyID, .keep_all = TRUE)       # keeps only the first 0-24 h row per study

# 2. Run meta-analysis (same settings as pain scores)
m_opioid <- metacont(n.e   = IV_n,   mean.e = IV_mean, sd.e = IV_sd,
                     n.c   = PO_n,   mean.c = PO_mean, sd.c = PO_sd,
                     studlab = StudyID,
                     data    = opioid_24h,
                     sm      = "MD",                  # mean difference in MME
                     fixed   = FALSE,
                     random  = TRUE,
                     method.tau = "REML",
                     hakn    = TRUE,
                     title   = "24-h Opioid Consumption (MME): IV vs PO Acetaminophen")

# 3. Publication-ready forest plot with heading
pdf("Forest_Opioid_MME_0-24h.pdf", width = 15.5, height = 9.8)
meta::forest(m_opioid,
             smlab = "24h Postop Opioid (MME):\nIV vs PO Acetaminophen",
             leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
             leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
             xlab = "Mean Difference in MME (IV minus PO)",
             col.square = "#4575b4",
             col.diamond = "#d73027",
             col.predict = "darkred",
             comb.random = TRUE,
             prediction = TRUE,
             print.tau2 = TRUE,
             print.I2 = TRUE,
             print.Q = TRUE,
             fs.smlab = 12,
             fs.study = 10,
             fontsize = 10)
dev.off()

# =========================================================================
# ===== 4. INFLUENCE / LEAVE-ONE-OUT ANALYSIS FOR OPIOID MME (0–24 h) =====
# =========================================================================

# Run leave-one-out meta-analysis
inf_opioid <- metainf(m_opioid)

# Save influence forest plot
pdf("Influence_Opioid_MME_0-24h.pdf", width = 14, height = 8.5)
meta::forest(inf_opioid,
             smlab = "Influence Analysis:\n24h Postop Opioid (MME)",
             xlab = "Mean Difference in MME (IV minus PO)",
             leftcols = c("studlab", "TE", "lower", "upper"),
             leftlabs = c("Omitted Study", "MD", "Lower", "Upper"),
             col.square = "#4575b4",
             col.diamond = "#d73027",
             print.tau2 = TRUE,
             print.I2 = TRUE,
             fs.smlab = 13)
dev.off()

# ==================================
# ===== 5.PAIN DURING MOVEMENT =====
# ==================================

# Import pain-on-movement sheet
pain_move <- read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", 
                        sheet = 4, col_names = TRUE)

# === FUNCTION FOR PAIN ON MOVEMENT ===
analyze_move_pain <- function(time_prefix, time_label) {
  
  dat <- pain_move %>%
    select(StudyID, PainScale,
           IV_n   = paste0("Pain_", time_prefix, "_IV_n"),
           IV_mean = paste0("Pain_", time_prefix, "_IV_mean"),
           IV_sd   = paste0("Pain_", time_prefix, "_IV_sd"),
           PO_n   = paste0("Pain_", time_prefix, "_PO_n"),
           PO_mean = paste0("Pain_", time_prefix, "_PO_mean"),
           PO_sd   = paste0("Pain_", time_prefix, "_PO_sd")) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    mutate(
      Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
      IV_mean_10 = if_else(Scale_Type == "0-100", IV_mean / 10, IV_mean),
      IV_sd_10   = if_else(Scale_Type == "0-100", IV_sd / 10, IV_sd),
      PO_mean_10 = if_else(Scale_Type == "0-100", PO_mean / 10, PO_mean),
      PO_sd_10   = if_else(Scale_Type == "0-100", PO_sd / 10, PO_sd)
    )
  
  cat("\n=== Pain on Movement —", time_label, "===\n")
  cat("Number of studies:", nrow(dat), "\n")
  
  if(nrow(dat) < 2) {
    cat("→ Insufficient studies for meta-analysis (k < 2)\n")
    return(NULL)
  }
  
  m <- metacont(n.e   = IV_n,   mean.e = IV_mean_10, sd.e = IV_sd_10,
                n.c   = PO_n,   mean.c = PO_mean_10, sd.c = PO_sd_10,
                studlab = StudyID,
                data    = dat,
                sm      = "MD",
                random  = TRUE,
                method.tau = "REML",
                hakn    = TRUE,
                title   = paste("Pain on Movement", time_label))
  
  print(summary(m))
  
  # Publication forest plot
  pdf(paste0("Forest_PainMove_", time_prefix, "_MD.pdf"), width = 15.5, height = 8)
  meta::forest(m,
               smlab = paste("Pain Score (Movement)", time_label, ":\nIV vs PO Acetaminophen"),
               leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
               leftlabs = c("Study", "N (IV)", "Mean (IV)", "SD (IV)", "N (PO)", "Mean (PO)", "SD (PO)"),
               xlab = "Mean Difference (IV minus PO) — lower = favours IV",
               col.square = "#4575b4",
               col.diamond = "#d73027",
               prediction = TRUE,
               print.tau2 = TRUE,
               print.I2 = TRUE)
  dev.off()
  
  return(m)
}

# === Run all time windows ===
m_move_0to2h  <- analyze_move_pain("0to2h",  "0–2hrs")
m_move_2to6h  <- analyze_move_pain("2to6h",  "2–6hrs")
m_move_6to24h <- analyze_move_pain("6to24h", "6–24hrs")
m_move_gt24h  <- analyze_move_pain("gt24h",  ">24hrs")


# ===================
# ===== 6. PONV =====
# ===================

# Import PONV sheet
ponv <- read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", 
                   sheet = 6, col_names = TRUE)

# 1. Clean PONV data (focus on 0-24 h and PACU windows)
ponv_clean <- ponv %>%
  filter(str_detect(TimeWindow_Hours, "0-24|PACU")) %>%
  select(StudyID, 
         PONV_Definition,
         events.e = IV_events, 
         n.e     = IV_total,
         events.c = PO_events, 
         n.c     = PO_total) %>%
  filter(!is.na(events.e) & !is.na(n.e) & !is.na(events.c) & !is.na(n.c)) %>%
  mutate(PONV_Definition = replace_na(PONV_Definition, "Unspecified"))

# Split into Nausea and Vomiting
ponv_nausea <- ponv_clean %>% 
  filter(str_detect(PONV_Definition, "Nausea|Unspecified"))

ponv_vomiting <- ponv_clean %>% 
  filter(str_detect(PONV_Definition, "Vomiting"))

cat("Nausea 0–24 h studies:", nrow(ponv_nausea), "\n")
cat("Vomiting 0–24 h studies:", nrow(ponv_vomiting), "\n")

# 2. Meta-analysis: Nausea (most important)
m_nausea <- metabin(data      = ponv_nausea,
                    event.e  = events.e,
                    n.e       = n.e,
                    event.c  = events.c,
                    n.c       = n.c,
                    studlab   = StudyID,
                    sm        = "RR",           # Risk Ratio (standard for PONV)
                    random    = TRUE,
                    method.tau = "REML",
                    method.random.ci = "HK",    # modern Hartung-Knapp
                    title     = "PONV Nausea 0–24 h")

summary(m_nausea)

# Forest plot: Nausea
pdf("Forest_PONV_Nausea_0-24h.pdf", width = 14.5, height = 8)
meta::forest(m_nausea,
             smlab = "Postop Nausea 0–24h:\nIV vs PO Acetaminophen",
             leftcols = c("studlab", "events.e", "n.e", "events.c", "n.c"),
             leftlabs = c("Study", "Events (IV)", "N (IV)", "Events (PO)", "N (PO)"),
             xlab = "Risk Ratio (IV vs PO)",
             col.square = "#4575b4",
             col.diamond = "#d73027",
             prediction = TRUE,
             print.tau2 = TRUE,
             print.I2 = TRUE)
dev.off()

# 3. Meta-analysis: Vomiting
m_vomiting <- metabin(data      = ponv_vomiting,
                      event.e  = events.e,
                      n.e       = n.e,
                      event.c  = events.c,
                      n.c       = n.c,
                      studlab   = StudyID,
                      sm        = "RR",
                      random    = TRUE,
                      method.tau = "REML",
                      method.random.ci = "HK",
                      title     = "PONV Vomiting 0–24 h")

summary(m_vomiting)

# Forest plot: Vomiting
pdf("Forest_PONV_Vomiting_0-24h.pdf", width = 14.5, height = 8)
meta::forest(m_vomiting,
             smlab = "Postop Vomiting 0–24h:\nIV vs PO Acetaminophen",
             leftcols = c("studlab", "events.e", "n.e", "events.c", "n.c"),
             leftlabs = c("Study", "Events (IV)", "N (IV)", "Events (PO)", "N (PO)"),
             xlab = "Risk Ratio (IV vs PO)",
             col.square = "#4575b4",
             col.diamond = "#d73027",
             prediction = TRUE,
             print.tau2 = TRUE,
             print.I2 = TRUE)
dev.off()

# =================================================
# ===== 7. ALL OTHER MAJOR SECONDARY OUTCOMES =====
# =================================================

# Import Sheet 7 (all other secondary outcomes)
other_sec <- read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", 
                        sheet = 7, col_names = TRUE)

# 1. PACU Length of Stay (minutes) - continuous
pacu <- other_sec %>%
  filter(!is.na(PACU_LOS_min_IV_n)) %>%
  select(StudyID,
         IV_n = PACU_LOS_min_IV_n,
         IV_mean = PACU_LOS_min_IV_mean,
         IV_sd = PACU_LOS_min_IV_sd,
         PO_n = PACU_LOS_min_PO_n,
         PO_mean = PACU_LOS_min_PO_mean,
         PO_sd = PACU_LOS_min_PO_sd)

m_pacu <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                   n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                   studlab = StudyID,
                   data = pacu,
                   sm = "MD",
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE)

summary(m_pacu)
pdf("Forest_PACU_LOS.pdf", width = 15, height = 8)
meta::forest(m_pacu, smlab = "PACU Length of Stay (minutes):\nIV vs PO Acetaminophen")
dev.off()

# 2. Hospital Length of Stay (days) - continuous
hosp <- other_sec %>%
  filter(!is.na(Hospital_LOS_days_IV_n)) %>%
  select(StudyID,
         IV_n = Hospital_LOS_days_IV_n,
         IV_mean = Hospital_LOS_days_IV_mean,
         IV_sd = Hospital_LOS_days_IV_sd,
         PO_n = Hospital_LOS_days_PO_n,
         PO_mean = Hospital_LOS_days_PO_mean,
         PO_sd = Hospital_LOS_days_PO_sd)

m_hosp <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                   n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                   studlab = StudyID,
                   data = hosp,
                   sm = "MD",
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE)

summary(m_hosp)
pdf("Forest_Hospital_LOS.pdf", width = 15, height = 8)
meta::forest(m_hosp, smlab = "Hospital Length of Stay (days):\nIV vs PO Acetaminophen")
dev.off()

# 3. Patient Satisfaction score - continuous
sat <- other_sec %>%
  filter(!is.na(Satisfaction_IV_n)) %>%
  select(StudyID,
         IV_n = Satisfaction_IV_n,
         IV_mean = Satisfaction_IV_mean,
         IV_sd = Satisfaction_IV_sd,
         PO_n = Satisfaction_PO_n,
         PO_mean = Satisfaction_PO_mean,
         PO_sd = Satisfaction_PO_sd)

m_sat <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                  n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                  studlab = StudyID,
                  data = sat,
                  sm = "MD",
                  random = TRUE,
                  method.tau = "REML",
                  hakn = TRUE)

summary(m_sat)
pdf("Forest_Satisfaction.pdf", width = 15, height = 8)
meta::forest(m_sat, smlab = "Patient Satisfaction Score:\nIV vs PO Acetaminophen")
dev.off()

# 4. Time to Rescue Analgesia (minutes) - continuous
rescue <- other_sec %>%
  filter(!is.na(TimeToRescue_min_IV_n)) %>%
  select(StudyID,
         IV_n = TimeToRescue_min_IV_n,
         IV_mean = TimeToRescue_min_IV_mean,
         IV_sd = TimeToRescue_min_IV_sd,
         PO_n = TimeToRescue_min_PO_n,
         PO_mean = TimeToRescue_min_PO_mean,
         PO_sd = TimeToRescue_min_PO_sd)

m_rescue <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                     n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                     studlab = StudyID,
                     data = rescue,
                     sm = "MD",
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE)

summary(m_rescue)
pdf("Forest_TimeToRescue.pdf", width = 15, height = 8)
meta::forest(m_rescue, smlab = "Time to Rescue Analgesia(Min):\nIV vs PO Acetaminophen")
dev.off()

# 5. QoR Score (24–48 h) - continuous
qor <- other_sec %>%
  filter(!is.na(QoR_Score_24to48h_IV_n)) %>%
  select(StudyID,
         IV_n = QoR_Score_24to48h_IV_n,
         IV_mean = QoR_Score_24to48h_IV_mean,
         IV_sd = QoR_Score_24to48h_IV_sd,
         PO_n = QoR_Score_24to48h_PO_n,
         PO_mean = QoR_Score_24to48h_PO_mean,
         PO_sd = QoR_Score_24to48h_PO_sd)

m_qor <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                  n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                  studlab = StudyID,
                  data = qor,
                  sm = "MD",
                  random = TRUE,
                  method.tau = "REML",
                  hakn = TRUE)

summary(m_qor)
pdf("Forest_QoR_Score.pdf", width = 15, height = 8)
meta::forest(m_qor, smlab = "QoR Score (24–48 h):\nIV vs PO Acetaminophen")
dev.off()

# 6. Rescue Analgesia Proportion - binary
rescue_prop <- other_sec %>%
  filter(!is.na(RescueProportion_IV_events)) %>%
  select(StudyID,
         events.e = RescueProportion_IV_events,
         n.e = RescueProportion_IV_total,
         events.c = RescueProportion_PO_events,
         n.c = RescueProportion_PO_total)

m_rescue_prop <- metabin(event.e = events.e, n.e = n.e,
                         event.c = events.c, n.c = n.c,
                         studlab = StudyID,
                         data = rescue_prop,
                         sm = "RR",
                         random = TRUE,
                         method.tau = "REML",
                         hakn = TRUE)

summary(m_rescue_prop)
pdf("Forest_RescueProportion.pdf", width = 15, height = 8)
meta::forest(m_rescue_prop, smlab = "Need for Rescue Analgesia:\nIV vs PO Acetaminophen")
dev.off()

# =========================================================================
# ===== 8. SUBGROUP ANALYSIS - TIMING OF ACETAMINOPHEN ADMINISTRATION =====
# =========================================================================

# Load study characteristics
study_char <- read_excel("Acetaminophen_IV_vs_PO_DataExtraction_final.xlsx", 
                         sheet = 1, col_names = TRUE)

# Focus on the three subgroup variables
subgroup_cols <- study_char %>%
  select(StudyID, 
         any_of(c("SurgeryType", "Surgery_Type", "Surgery", 
                  "ModeOfAnesthesia", "Anesthesia_Mode", "Anesthesia",
                  "IV_Acetaminophen_1stDoseTiming", "IV_DoseTiming", "IV_Timing",
                  "PO_Acetaminophen_1stDoseTiming", "PO_DoseTiming", "PO_Timing")))

# Re-create Timing_Combined
study_char <- study_char %>%
  mutate(
    Timing_Combined = case_when(
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Intraoperative" ~ "Preop PO + Intraop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Postoperative" ~ "Preop PO + Postop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Postoperative" & 
        IV_Acetaminophen_1stDoseTiming == "Postoperative" ~ "Postop PO + Postop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Preoparative" ~ "Preop PO + Preop IV",
      
      TRUE ~ "Other / Mixed"
    )
  )

# Merge with pain_rest (keeps PainScale for standardisation)
pain_sub <- pain_rest %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID")

# Meta-analysis for all groups of different combinations of acetaminophen administration timing

run_timing_subgroup <- function(time_prefix, time_label) {
  
  dat <- pain_sub %>%
    select(StudyID, Timing_Combined, PainScale,
           IV_n   = paste0("Pain_", time_prefix, "_IV_n"),
           IV_mean = paste0("Pain_", time_prefix, "_IV_mean"),
           IV_sd   = paste0("Pain_", time_prefix, "_IV_sd"),
           PO_n   = paste0("Pain_", time_prefix, "_PO_n"),
           PO_mean = paste0("Pain_", time_prefix, "_PO_mean"),
           PO_sd   = paste0("Pain_", time_prefix, "_PO_sd")) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    mutate(
      Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
      IV_mean_10 = if_else(Scale_Type == "0-100", IV_mean / 10, IV_mean),
      IV_sd_10   = if_else(Scale_Type == "0-100", IV_sd / 10, IV_sd),
      PO_mean_10 = if_else(Scale_Type == "0-100", PO_mean / 10, PO_mean),
      PO_sd_10   = if_else(Scale_Type == "0-100", PO_sd / 10, PO_sd)
    )
  
  cat("\n=== Subgroup Analysis:", time_label, "(k =", nrow(dat), ") ===\n")
  
  if(nrow(dat) < 2) {
    cat("→ Insufficient studies for subgroup analysis\n")
    return(NULL)
  }
  
  m_sub <- metacont(n.e = IV_n, mean.e = IV_mean_10, sd.e = IV_sd_10,
                    n.c = PO_n, mean.c = PO_mean_10, sd.c = PO_sd_10,
                    studlab = StudyID,
                    data = dat,
                    sm = "MD",
                    random = TRUE,
                    method.tau = "REML",
                    hakn = TRUE,
                    subgroup = Timing_Combined,
                    test.subgroup = TRUE)
  
  print(summary(m_sub))
  
  # Subgroup forest plot
  pdf(paste0("Subgroup_PainRest_", time_prefix, "_by_TimingCombined.pdf"), 
      width = 16, height = 10)
  meta::forest(m_sub,
               smlab = paste("Pain at Rest", time_label, "\nby Timing"),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
  
  invisible(m_sub)
}

# === Run for ALL time periods ===
run_timing_subgroup("0to2h",   "0–2 Hours")
run_timing_subgroup("2to6h",   "2–6 Hours")
run_timing_subgroup("6to24h",  "6–24 Hours")
run_timing_subgroup("gt24h",   ">24 Hours")


# Re-create Timing_Combined (safe, in case session was reset)
study_char <- study_char %>%
  mutate(
    Timing_Combined = case_when(
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Intraoperative" ~ "Preop PO + Intraop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Postoperative" ~ "Preop PO + Postop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Postoperative" & 
        IV_Acetaminophen_1stDoseTiming == "Postoperative" ~ "Postop PO + Postop IV",
      
      PO_Acetaminophen_1stDoseTiming == "Preoparative" & 
        IV_Acetaminophen_1stDoseTiming == "Preoparative" ~ "Preop PO + Preop IV",
      
      TRUE ~ "Other / Mixed"
    )
  )

# Merge with opioid 0–24 h data
opioid_sub <- opioid_24h %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID")

# Subgroup analysis function for opioid MME
run_opioid_timing_subgroup <- function() {
  cat("=== Subgroup Analysis: Opioid MME 0–24 h by Timing Combined ===\n")
  
  m_sub <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                    n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                    studlab = StudyID,
                    data = opioid_sub,
                    sm = "MD",
                    random = TRUE,
                    method.tau = "REML",
                    hakn = TRUE,
                    subgroup = Timing_Combined,
                    test.subgroup = TRUE)
  
  print(summary(m_sub))
  
  # Subgroup forest plot
  pdf("Subgroup_Opioid_MME_0-24h_by_TimingCombined.pdf", 
      width = 16, height = 9)
  meta::forest(m_sub,
               smlab = "24h Postop Opioid (MME) \nby Timing Combined",
               xlab = "MD in MME (IV minus PO)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
  
  invisible(m_sub)
}

m_opioid_timing <- run_opioid_timing_subgroup()

# Continuous outcomes
run_continuous_subgroup <- function(data, outcome_name) {
  cat("\n=== Subgroup Analysis:", outcome_name, "by Timing Combined ===\n")
  
  m_sub <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                    n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                    studlab = StudyID,
                    data = data,
                    sm = "MD",
                    random = TRUE,
                    method.tau = "REML",
                    hakn = TRUE,
                    subgroup = Timing_Combined,
                    test.subgroup = TRUE)
  
  print(summary(m_sub))
  
  pdf(paste0("Subgroup_", gsub(" ", "_", outcome_name), "_by_Timing.pdf"), 
      width = 16, height = 9)
  meta::forest(m_sub,
               smlab = paste(outcome_name, "by Timing Combined"),
               xlab = "MD",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

# Pain on Movement – 0–2 h
dat_move_02 <- pain_move %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID") %>%
  filter(!is.na(Pain_0to2h_IV_n)) %>%
  mutate(IV_n = Pain_0to2h_IV_n, IV_mean = Pain_0to2h_IV_mean, IV_sd = Pain_0to2h_IV_sd,
         PO_n = Pain_0to2h_PO_n, PO_mean = Pain_0to2h_PO_mean, PO_sd = Pain_0to2h_PO_sd,
         Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
         IV_mean = if_else(Scale_Type == "0-100", IV_mean/10, IV_mean),
         IV_sd   = if_else(Scale_Type == "0-100", IV_sd/10, IV_sd),
         PO_mean = if_else(Scale_Type == "0-100", PO_mean/10, PO_mean),
         PO_sd   = if_else(Scale_Type == "0-100", PO_sd/10, PO_sd))
run_continuous_subgroup(dat_move_02, "Pain on Movement 0-2h")

# Pain on Movement – 6–24 h
dat_move_624 <- pain_move %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID") %>%
  filter(!is.na(Pain_6to24h_IV_n)) %>%
  mutate(IV_n = Pain_6to24h_IV_n, IV_mean = Pain_6to24h_IV_mean, IV_sd = Pain_6to24h_IV_sd,
         PO_n = Pain_6to24h_PO_n, PO_mean = Pain_6to24h_PO_mean, PO_sd = Pain_6to24h_PO_sd,
         Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
         IV_mean = if_else(Scale_Type == "0-100", IV_mean/10, IV_mean),
         IV_sd   = if_else(Scale_Type == "0-100", IV_sd/10, IV_sd),
         PO_mean = if_else(Scale_Type == "0-100", PO_mean/10, PO_mean),
         PO_sd   = if_else(Scale_Type == "0-100", PO_sd/10, PO_sd))
run_continuous_subgroup(dat_move_624, "Pain on Movement 6-24h")

# Pain on Movement – >24 h
dat_move_gt <- pain_move %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID") %>%
  filter(!is.na(Pain_gt24h_IV_n)) %>%
  mutate(IV_n = Pain_gt24h_IV_n, IV_mean = Pain_gt24h_IV_mean, IV_sd = Pain_gt24h_IV_sd,
         PO_n = Pain_gt24h_PO_n, PO_mean = Pain_gt24h_PO_mean, PO_sd = Pain_gt24h_PO_sd,
         Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
         IV_mean = if_else(Scale_Type == "0-100", IV_mean/10, IV_mean),
         IV_sd   = if_else(Scale_Type == "0-100", IV_sd/10, IV_sd),
         PO_mean = if_else(Scale_Type == "0-100", PO_mean/10, PO_mean),
         PO_sd   = if_else(Scale_Type == "0-100", PO_sd/10, PO_sd))
run_continuous_subgroup(dat_move_gt, "Pain on Movement >24h")

# PACU LOS
run_continuous_subgroup(m_pacu$data %>% left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID"), "PACU LOS")

# Hospital LOS
run_continuous_subgroup(m_hosp$data %>% left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID"), "Hospital LOS")

# Patient Satisfaction
run_continuous_subgroup(m_sat$data %>% left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID"), "Patient Satisfaction")

# Time to Rescue Analgesia
run_continuous_subgroup(m_rescue$data %>% left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID"), "Time to Rescue")

# Binary outcome: PONV
run_binary_subgroup <- function(data, outcome_name) {
  cat("\n=== Subgroup Analysis:", outcome_name, "by Timing Combined ===\n")
  
  m_sub <- metabin(event.e = events.e, n.e = n.e,
                   event.c = events.c, n.c = n.c,
                   studlab = StudyID,
                   data = data,
                   sm = "RR",
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   subgroup = Timing_Combined,
                   test.subgroup = TRUE)
  
  print(summary(m_sub))
  
  pdf(paste0("Subgroup_", gsub(" ", "_", outcome_name), "_by_Timing.pdf"), 
      width = 16, height = 9)
  meta::forest(m_sub,
               smlab = paste(outcome_name, "by Timing Combined"),
               xlab = "Risk Ratio (IV vs PO)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

# PONV Nausea
ponv_nausea_sub <- ponv_nausea %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID")
run_binary_subgroup(ponv_nausea_sub, "PONV Nausea")

# PONV Vomiting
ponv_vomiting_sub <- ponv_vomiting %>%
  left_join(study_char %>% select(StudyID, Timing_Combined), by = "StudyID")
run_binary_subgroup(ponv_vomiting_sub, "PONV Vomiting")

# =======================================================================
# ===== 9. SUBGROUP ANALYSIS - MODE OF ANAESTHESIA, TYPE OF SURGERY =====
# =======================================================================

### Pain at rest ###

# Merge subgroup variables from Sheet 1
pain_sub <- pain_rest %>%
  left_join(study_char %>% select(StudyID, SurgeryType, ModeOfAnesthesia), 
            by = "StudyID")

run_pain_subgroup <- function(time_prefix, time_label, subgroup_var) {
  
  # Rename the subgroup column to a fixed name "subgroup"
  dat <- pain_sub %>%
    select(StudyID, PainScale, subgroup = !!sym(subgroup_var),
           IV_n   = paste0("Pain_", time_prefix, "_IV_n"),
           IV_mean = paste0("Pain_", time_prefix, "_IV_mean"),
           IV_sd   = paste0("Pain_", time_prefix, "_IV_sd"),
           PO_n   = paste0("Pain_", time_prefix, "_PO_n"),
           PO_mean = paste0("Pain_", time_prefix, "_PO_mean"),
           PO_sd   = paste0("Pain_", time_prefix, "_PO_sd")) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    filter(!is.na(subgroup)) %>%
    mutate(
      Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
      IV_mean_10 = if_else(Scale_Type == "0-100", IV_mean / 10, IV_mean),
      IV_sd_10   = if_else(Scale_Type == "0-100", IV_sd / 10, IV_sd),
      PO_mean_10 = if_else(Scale_Type == "0-100", PO_mean / 10, PO_mean),
      PO_sd_10   = if_else(Scale_Type == "0-100", PO_sd / 10, PO_sd)
    )
  
  cat("\n=== Pain at Rest", time_label, "by", subgroup_var, "(k =", nrow(dat), ") ===\n")
  
  m <- metacont(n.e = IV_n, mean.e = IV_mean_10, sd.e = IV_sd_10,
                n.c = PO_n, mean.c = PO_mean_10, sd.c = PO_sd_10,
                studlab = StudyID,
                data = dat,
                sm = "MD",
                random = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                subgroup = subgroup,     # fixed name
                test.subgroup = TRUE)
  
  print(summary(m))
  
  pdf(paste0("Subgroup_PainRest_", time_prefix, "_by_", subgroup_var, ".pdf"), 
      width = 16, height = 10)
  meta::forest(m,
               smlab = paste("Pain at Rest", time_label, "\nby", subgroup_var),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

# Function calls

# 0–2 Hours
run_pain_subgroup("0to2h", "0–2 Hours", "SurgeryType")
run_pain_subgroup("0to2h", "0–2 Hours", "ModeOfAnesthesia")

# 2–6 Hours
run_pain_subgroup("2to6h", "2–6 Hours", "SurgeryType")
run_pain_subgroup("2to6h", "2–6 Hours", "ModeOfAnesthesia")

# 6–24 Hours
run_pain_subgroup("6to24h", "6–24 Hours", "SurgeryType")
run_pain_subgroup("6to24h", "6–24 Hours", "ModeOfAnesthesia")

# >24 Hours
run_pain_subgroup("gt24h", ">24 Hours", "SurgeryType")
run_pain_subgroup("gt24h", ">24 Hours", "ModeOfAnesthesia")


### Pain on movement ###

# Merge subgroup variables from Sheet 1
pain_move_sub <- pain_move %>%
  left_join(study_char %>% select(StudyID, SurgeryType, ModeOfAnesthesia), 
            by = "StudyID")

# Same function that worked for Timing_Combined
run_pain_move_subgroup <- function(time_prefix, time_label, subgroup_var) {
  
  dat <- pain_move_sub %>%
    select(StudyID, PainScale, subgroup = !!sym(subgroup_var),
           IV_n   = paste0("Pain_", time_prefix, "_IV_n"),
           IV_mean = paste0("Pain_", time_prefix, "_IV_mean"),
           IV_sd   = paste0("Pain_", time_prefix, "_IV_sd"),
           PO_n   = paste0("Pain_", time_prefix, "_PO_n"),
           PO_mean = paste0("Pain_", time_prefix, "_PO_mean"),
           PO_sd   = paste0("Pain_", time_prefix, "_PO_sd")) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    filter(!is.na(subgroup)) %>%
    mutate(
      Scale_Type = if_else(str_detect(PainScale, "0-100"), "0-100", "0-10"),
      IV_mean_10 = if_else(Scale_Type == "0-100", IV_mean / 10, IV_mean),
      IV_sd_10   = if_else(Scale_Type == "0-100", IV_sd / 10, IV_sd),
      PO_mean_10 = if_else(Scale_Type == "0-100", PO_mean / 10, PO_mean),
      PO_sd_10   = if_else(Scale_Type == "0-100", PO_sd / 10, PO_sd)
    )
  
  cat("\n=== Pain on Movement", time_label, "by", subgroup_var, "(k =", nrow(dat), ") ===\n")
  
  m <- metacont(n.e = IV_n, mean.e = IV_mean_10, sd.e = IV_sd_10,
                n.c = PO_n, mean.c = PO_mean_10, sd.c = PO_sd_10,
                studlab = StudyID,
                data = dat,
                sm = "MD",
                random = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                subgroup = subgroup,
                test.subgroup = TRUE)
  
  print(summary(m))
  
  pdf(paste0("Subgroup_PainMove_", time_prefix, "_by_", subgroup_var, ".pdf"), 
      width = 16, height = 10)
  meta::forest(m,
               smlab = paste("Pain on Movement", time_label, "\nby", subgroup_var),
               xlab = "MD (0-10 VAS/NRS)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

# Function calls

# 0–2 Hours
run_pain_move_subgroup("0to2h", "0–2 Hours", "SurgeryType")
run_pain_move_subgroup("0to2h", "0–2 Hours", "ModeOfAnesthesia")

# 6–24 Hours
run_pain_move_subgroup("6to24h", "6–24 Hours", "SurgeryType")
run_pain_move_subgroup("6to24h", "6–24 Hours", "ModeOfAnesthesia")

# >24 Hours
run_pain_move_subgroup("gt24h", ">24 Hours", "SurgeryType")
run_pain_move_subgroup("gt24h", ">24 Hours", "ModeOfAnesthesia")

### Opioid MME ###

# Merge subgroup variables from Sheet 1
opioid_sub <- opioid_24h %>%
  left_join(study_char %>% select(StudyID, SurgeryType, ModeOfAnesthesia), 
            by = "StudyID")

# Same function that worked for pain at rest
run_opioid_subgroup <- function(subgroup_var) {
  
  dat <- opioid_sub %>%
    select(StudyID, subgroup = !!sym(subgroup_var),
           IV_n = IV_n, IV_mean = IV_mean, IV_sd = IV_sd,
           PO_n = PO_n, PO_mean = PO_mean, PO_sd = PO_sd) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    filter(!is.na(subgroup))
  
  cat("\n=== 24-h Opioid MME by", subgroup_var, "(k =", nrow(dat), ") ===\n")
  
  m <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                studlab = StudyID,
                data = dat,
                sm = "MD",
                random = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                subgroup = subgroup,
                test.subgroup = TRUE)
  
  print(summary(m))
  
  pdf(paste0("Subgroup_Opioid_MME_0-24h_by_", subgroup_var, ".pdf"), 
      width = 16, height = 9)
  meta::forest(m,
               smlab = paste("24h Postop Opioid (MME)\nby", subgroup_var),
               xlab = "MD in MME (IV minus PO)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

run_opioid_subgroup("SurgeryType")
run_opioid_subgroup("ModeOfAnesthesia")

### Continuous variables ###

run_subgroup_cont <- function(data, outcome_name, subgroup_var) {
  dat <- data %>%
    select(StudyID, subgroup = !!sym(subgroup_var),
           IV_n = IV_n, IV_mean = IV_mean, IV_sd = IV_sd,
           PO_n = PO_n, PO_mean = PO_mean, PO_sd = PO_sd) %>%
    filter(!is.na(IV_n) & !is.na(PO_n)) %>%
    filter(!is.na(subgroup))
  
  cat("\n=== ", outcome_name, "by", subgroup_var, "(k =", nrow(dat), ") ===\n")
  
  m <- metacont(n.e = IV_n, mean.e = IV_mean, sd.e = IV_sd,
                n.c = PO_n, mean.c = PO_mean, sd.c = PO_sd,
                studlab = StudyID,
                data = dat,
                sm = "MD",
                random = TRUE,
                method.tau = "REML",
                hakn = TRUE,
                subgroup = subgroup,
                test.subgroup = TRUE)
  
  print(summary(m))
  
  pdf(paste0("Subgroup_", gsub(" ", "_", outcome_name), "_by_", subgroup_var, ".pdf"), 
      width = 16, height = 10)
  meta::forest(m,
               smlab = paste(outcome_name, "\nby", subgroup_var),
               xlab = "MD",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

### Binary variables ###

run_subgroup_bin <- function(data, outcome_name, subgroup_var) {
  dat <- data %>%
    select(StudyID, subgroup = !!sym(subgroup_var),
           events.e, n.e, events.c, n.c) %>%
    filter(!is.na(events.e) & !is.na(n.e) & !is.na(events.c) & !is.na(n.c)) %>%
    filter(!is.na(subgroup))
  
  cat("\n=== ", outcome_name, "by", subgroup_var, "(k =", nrow(dat), ") ===\n")
  
  m <- metabin(event.e = events.e, n.e = n.e,
               event.c = events.c, n.c = n.c,
               studlab = StudyID,
               data = dat,
               sm = "RR",
               random = TRUE,
               method.tau = "REML",
               hakn = TRUE,
               subgroup = subgroup,
               test.subgroup = TRUE)
  
  print(summary(m))
  
  pdf(paste0("Subgroup_", gsub(" ", "_", outcome_name), "_by_", subgroup_var, ".pdf"), 
      width = 16, height = 10)
  meta::forest(m,
               smlab = paste(outcome_name, "\nby", subgroup_var),
               xlab = "RR (IV vs PO)",
               col.square = "#4575b4",
               col.diamond = "#d73027")
  dev.off()
}

# PONV Nausea (binary)
run_subgroup_bin(ponv_nausea %>% left_join(subgroup_data, by = "StudyID"), "PONV Nausea", "SurgeryType")
run_subgroup_bin(ponv_nausea %>% left_join(subgroup_data, by = "StudyID"), "PONV Nausea", "ModeOfAnesthesia")

# PONV Vomiting (binary)
run_subgroup_bin(ponv_vomiting %>% left_join(subgroup_data, by = "StudyID"), "PONV Vomiting", "SurgeryType")
run_subgroup_bin(ponv_vomiting %>% left_join(subgroup_data, by = "StudyID"), "PONV Vomiting", "ModeOfAnesthesia")

# PACU LOS (continuous)
run_subgroup_cont(m_pacu$data %>% left_join(subgroup_data, by = "StudyID"), "PACU LOS", "SurgeryType")
run_subgroup_cont(m_pacu$data %>% left_join(subgroup_data, by = "StudyID"), "PACU LOS", "ModeOfAnesthesia")

# Hospital LOS (continuous)
run_subgroup_cont(m_hosp$data %>% left_join(subgroup_data, by = "StudyID"), "Hospital LOS", "SurgeryType")
run_subgroup_cont(m_hosp$data %>% left_join(subgroup_data, by = "StudyID"), "Hospital LOS", "ModeOfAnesthesia")

# Patient Satisfaction (continuous)
run_subgroup_cont(m_sat$data %>% left_join(subgroup_data, by = "StudyID"), "Patient Satisfaction", "SurgeryType")
run_subgroup_cont(m_sat$data %>% left_join(subgroup_data, by = "StudyID"), "Patient Satisfaction", "ModeOfAnesthesia")

# Time to First Rescue Analgesia (continuous)
run_subgroup_cont(m_rescue$data %>% left_join(subgroup_data, by = "StudyID"), "Time to Rescue", "SurgeryType")
run_subgroup_cont(m_rescue$data %>% left_join(subgroup_data, by = "StudyID"), "Time to Rescue", "ModeOfAnesthesia")

# ===========================================
# ===== 10. PUBLICATION BIAS ASSESSMENT =====
# ===========================================

assess_pub_bias <- function(model, name) {
  cat("\n=== Publication Bias:", name, "(k =", model$k, ") ===\n")
  
  # Funnel plot
  funnel(model, main = paste("Funnel Plot -", name))
  
  # Egger’s test (only if k >= 3)
  if (model$k >= 3) {
    egger <- metabias(model, method.bias = "Egger")
    print(egger)
  } else {
    cat("→ Egger's test not performed (k < 3)\n")
  }
  
  # Save PDF
  pdf(paste0("Funnel_", gsub(" ", "_", name), ".pdf"), width = 8, height = 7)
  funnel(model, main = paste("Funnel Plot -", name))
  dev.off()
}

assess_pub_bias(m_0to2h,   "Pain Rest 0-2h")
assess_pub_bias(m_2to6h,   "Pain Rest 2-6h")
assess_pub_bias(m_6to24h,  "Pain Rest 6-24h")
assess_pub_bias(m_gt24h,   "Pain Rest >24h")
assess_pub_bias(m_opioid,  "Opioid MME 0-24h")

# =========================================
# ===== 11. TRIAL SEQUENTIAL ANALYSIS =====
# =========================================

# Prepare the 0-24 h opioid dataset for TSA

opioid_raw <- dat_list[["Opioid_Consumption_MME"]]
char       <- dat_list[["Study_Characteristics"]]

opioid_24h <- opioid_raw %>%
  filter(TimeWindow_Hours == "0-24") %>%
  left_join(
    char %>% select(StudyID, `Publication Year`),
    by = "StudyID"
  ) %>%
  arrange(`Publication Year`) %>%           # chronological order for cumulative analysis
  mutate(
    mI   = IV_mean_MME,      # IV acetaminophen = intervention
    sdI  = IV_sd_MME,
    nI   = IV_n,
    mC   = PO_mean_MME,      # PO acetaminophen = control
    sdC  = PO_sd_MME,
    nC   = PO_n,
    study = StudyID
  ) %>%
  select(study, mI, sdI, nI, mC, sdC, nC) %>%
  drop_na()

# Check how many studies contribute to 0-24 h opioid data
cat("Number of studies with complete 0-24 h opioid data:", nrow(opioid_24h), "\n")
print(opioid_24h)

# TSA parameters (pre-specified & clinically justified)

alpha     <- 0.05
beta      <- 0.20          # 80% power
mc        <- 10            # MCID: 10 mg oral MME reduction over 24 h
sd_mc     <- 40            # Anticipated SD (based on observed variability in your data)
fixed     <- FALSE
re_method <- "DL_HKSJ"     # Hartung-Knapp-Sidik-Jonkman (preferred with few studies)
es_alpha  <- "esOF"        # O’Brien-Fleming spending function
side      <- 2

# Run Trial Sequential Analysis

tsa_opioid <- RTSA(
  type      = "analysis",
  data      = opioid_24h,
  outcome   = "MD",
  mc        = mc,
  sd_mc     = sd_mc,
  fixed     = fixed,
  re_method = re_method,
  alpha     = alpha,
  beta      = beta,
  side      = side,
  es_alpha  = es_alpha
)

# View results
print(tsa_opioid)

# TSA boundary plot
plot(tsa_opioid)

# =====================================================
# SENSITIVITY ANALYSES - Opioid MME 0-24h TSA
# =====================================================

# ---- Sensitivity 1: MCID = 5 mg (smaller effect, larger RIS) ----
tsa_sens1 <- RTSA(
  type      = "analysis",
  data      = opioid_24h,
  outcome   = "MD",
  mc        = 5,
  sd_mc     = 40,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)
cat("\n=== SENSITIVITY 1: MCID = 5 mg, 80% power ===\n")
print(tsa_sens1)

# ---- Sensitivity 2: MCID = 15 mg (larger effect, smaller RIS) ----
tsa_sens2 <- RTSA(
  type      = "analysis",
  data      = opioid_24h,
  outcome   = "MD",
  mc        = 15,
  sd_mc     = 40,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)
cat("\n=== SENSITIVITY 2: MCID = 15 mg, 80% power ===\n")
print(tsa_sens2)

# ---- Sensitivity 3: 90% power (beta = 0.10), original MCID = 10 mg ----
tsa_sens3 <- RTSA(
  type      = "analysis",
  data      = opioid_24h,
  outcome   = "MD",
  mc        = 10,
  sd_mc     = 40,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.10,
  side      = 2,
  es_alpha  = "esOF"
)
cat("\n=== SENSITIVITY 3: MCID = 10 mg, 90% power ===\n")
print(tsa_sens3)


# Prepare Pain at Rest 0-2 h data (with scale standardization)

pain_rest_0to2h <- dat_list[["Pain_Scores_Rest"]] %>%
  filter(!is.na(Pain_0to2h_IV_mean) & !is.na(Pain_0to2h_PO_mean)) %>%
  left_join(
    dat_list[["Study_Characteristics"]] %>% 
      select(StudyID, `Publication Year`),
    by = "StudyID"
  ) %>%
  arrange(`Publication Year`) %>%
  mutate(
    # Standardize to 0-10 scale
    scale_factor = ifelse(grepl("100", PainScale) | grepl("VAS 0-100", PainScale, ignore.case = TRUE), 10, 1),
    
    mI  = Pain_0to2h_IV_mean / scale_factor,
    sdI = Pain_0to2h_IV_sd   / scale_factor,
    nI  = Pain_0to2h_IV_n,
    
    mC  = Pain_0to2h_PO_mean / scale_factor,
    sdC = Pain_0to2h_PO_sd   / scale_factor,
    nC  = Pain_0to2h_PO_n,
    
    study = StudyID
  ) %>%
  select(study, mI, sdI, nI, mC, sdC, nC, PainScale, `Publication Year`) %>%
  drop_na()

cat("Number of studies with complete 0-2 h pain at rest data:", nrow(pain_rest_0to2h), "\n")
print(pain_rest_0to2h %>% select(study, PainScale, mI, mC))


# Main TSA: MCID = 1.0 point on 0-10 scale, 80% power

tsa_pain_0to2h <- RTSA(
  type      = "analysis",
  data      = pain_rest_0to2h,
  outcome   = "MD",
  mc        = 1.0,           # MCID = 1.0 point on 0-10 scale
  sd_mc     = 2.5,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)

cat("\n=== MAIN TSA: Pain Rest 0-2 h, MCID = 1.0, 80% power ===\n")
print(tsa_pain_0to2h)

# TSA boundary plot
plot(tsa_pain_0to2h)

# Prepare Pain at Rest 2-6 h data (with scale standardization)

pain_rest_2to6h <- dat_list[["Pain_Scores_Rest"]] %>%
  filter(!is.na(Pain_2to6h_IV_mean) & !is.na(Pain_2to6h_PO_mean)) %>%
  left_join(
    dat_list[["Study_Characteristics"]] %>% 
      select(StudyID, `Publication Year`),
    by = "StudyID"
  ) %>%
  arrange(`Publication Year`) %>%
  mutate(
    # Standardize to 0-10 scale (handles any VAS 0-100 studies)
    scale_factor = ifelse(grepl("100", PainScale) | grepl("VAS 0-100", PainScale, ignore.case = TRUE), 10, 1),
    
    mI  = Pain_2to6h_IV_mean / scale_factor,
    sdI = Pain_2to6h_IV_sd   / scale_factor,
    nI  = Pain_2to6h_IV_n,
    
    mC  = Pain_2to6h_PO_mean / scale_factor,
    sdC = Pain_2to6h_PO_sd   / scale_factor,
    nC  = Pain_2to6h_PO_n,
    
    study = StudyID
  ) %>%
  select(study, mI, sdI, nI, mC, sdC, nC, PainScale, `Publication Year`) %>%
  drop_na()

cat("Number of studies with complete 2-6 h pain at rest data:", nrow(pain_rest_2to6h), "\n")
print(pain_rest_2to6h %>% select(study, PainScale, mI, mC))


# Main TSA: MCID = 1.0 point on 0-10 scale, 80% power

tsa_pain_2to6h <- RTSA(
  type      = "analysis",
  data      = pain_rest_2to6h,
  outcome   = "MD",
  mc        = 1.0,           # MCID = 1.0 point on 0-10 scale
  sd_mc     = 2.5,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)

cat("\n=== MAIN TSA: Pain Rest 2-6 h, MCID = 1.0, 80% power ===\n")
print(tsa_pain_2to6h)

# TSA boundary plot
plot(tsa_pain_2to6h)

# Prepare Pain at Rest 6-24 h data (with scale standardization)

pain_rest_6to24h <- dat_list[["Pain_Scores_Rest"]] %>%
  filter(!is.na(Pain_6to24h_IV_mean) & !is.na(Pain_6to24h_PO_mean)) %>%
  left_join(
    dat_list[["Study_Characteristics"]] %>% 
      select(StudyID, `Publication Year`),
    by = "StudyID"
  ) %>%
  arrange(`Publication Year`) %>%
  mutate(
    # Standardize to 0-10 scale
    scale_factor = ifelse(grepl("100", PainScale) | grepl("VAS 0-100", PainScale, ignore.case = TRUE), 10, 1),
    
    mI  = Pain_6to24h_IV_mean / scale_factor,
    sdI = Pain_6to24h_IV_sd   / scale_factor,
    nI  = Pain_6to24h_IV_n,
    
    mC  = Pain_6to24h_PO_mean / scale_factor,
    sdC = Pain_6to24h_PO_sd   / scale_factor,
    nC  = Pain_6to24h_PO_n,
    
    study = StudyID
  ) %>%
  select(study, mI, sdI, nI, mC, sdC, nC, PainScale, `Publication Year`) %>%
  drop_na()

cat("Number of studies with complete 6-24 h pain at rest data:", nrow(pain_rest_6to24h), "\n")
print(pain_rest_6to24h %>% select(study, PainScale, mI, mC))

# Main TSA: MCID = 1.0 point on 0-10 scale, 80% power

tsa_pain_6to24h <- RTSA(
  type      = "analysis",
  data      = pain_rest_6to24h,
  outcome   = "MD",
  mc        = 1.0,           # MCID = 1.0 point on 0-10 scale
  sd_mc     = 2.5,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)

cat("\n=== MAIN TSA: Pain Rest 6-24 h, MCID = 1.0, 80% power ===\n")
print(tsa_pain_6to24h)

# TSA boundary plot
plot(tsa_pain_6to24h)

# Prepare Pain at Rest >24 h data (with scale standardization)

pain_rest_gt24h <- dat_list[["Pain_Scores_Rest"]] %>%
  filter(!is.na(Pain_gt24h_IV_mean) & !is.na(Pain_gt24h_PO_mean)) %>%
  left_join(
    dat_list[["Study_Characteristics"]] %>% 
      select(StudyID, `Publication Year`),
    by = "StudyID"
  ) %>%
  arrange(`Publication Year`) %>%
  mutate(
    # Standardize to 0-10 scale
    scale_factor = ifelse(grepl("100", PainScale) | grepl("VAS 0-100", PainScale, ignore.case = TRUE), 10, 1),
    
    mI  = Pain_gt24h_IV_mean / scale_factor,
    sdI = Pain_gt24h_IV_sd   / scale_factor,
    nI  = Pain_gt24h_IV_n,
    
    mC  = Pain_gt24h_PO_mean / scale_factor,
    sdC = Pain_gt24h_PO_sd   / scale_factor,
    nC  = Pain_gt24h_PO_n,
    
    study = StudyID
  ) %>%
  select(study, mI, sdI, nI, mC, sdC, nC, PainScale, `Publication Year`) %>%
  drop_na()

cat("Number of studies with complete >24 h pain at rest data:", nrow(pain_rest_gt24h), "\n")
print(pain_rest_gt24h %>% select(study, PainScale, mI, mC))

# Main TSA: MCID = 1.0 point on 0-10 scale, 80% power

tsa_pain_gt24h <- RTSA(
  type      = "analysis",
  data      = pain_rest_gt24h,
  outcome   = "MD",
  mc        = 1.0,           # MCID = 1.0 point on 0-10 scale
  sd_mc     = 2.5,
  fixed     = FALSE,
  re_method = "DL_HKSJ",
  alpha     = 0.05,
  beta      = 0.20,
  side      = 2,
  es_alpha  = "esOF"
)

cat("\n=== MAIN TSA: Pain Rest >24 h, MCID = 1.0, 80% power ===\n")
print(tsa_pain_gt24h)

# TSA boundary plot
plot(tsa_pain_gt24h)
