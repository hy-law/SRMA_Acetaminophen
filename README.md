# SRMA Acetaminophen — R analysis code (BMC Anesthesiology)

Supporting dataset for the systematic review, meta-analysis, subgroup analysis and trial sequential analysis of randomised trials of intravenous versus oral perioperative acetaminophen for postoperative acute pain in adults.

| | |
|---|---|
| PROSPERO | [CRD420251157230](https://www.crd.york.ac.uk/prospero/display_record.php?ID=CRD420251157230) |
| Dataset file | [`Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx`](Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx) |
| Version | 1.3 (20 Oct 2026) |
| Licence | MIT (this extraction and any analysis code). The 16 source RCT publications remain copyright of their publishers. |
| Corresponding author | Hei Yeung Law, Department of Anaesthesiology, Perioperative and Pain Medicine, Queen Elizabeth Hospital, Hong Kong SAR. `LHY624@ha.org.hk` |

This is the **minimal dataset** needed to reproduce the pooled estimates. It is trial-level (one or more rows per RCT), not individual-participant data, except where Wilson 2019 summaries were computed from author-supplied individual participant data and then stored as arm-level means/SDs or event counts.

These files are released to support reproducibility of a systematic review. They are **not** a clinical guideline, product label, or dosing reference. Do not use them to treat patients.

## Files in this folder

| File | Role |
|---|---|
| `Acetaminophen_IV_vs_PO_DataExtraction_revision.xlsx` | Machine-readable extraction (sheets listed below) |
| `README.md` | This file |
| `LICENSE` | MIT licence for the extraction |

Do **not** treat the workbook as a substitute for the source papers. Always cite the original RCT.

## Workbook sheets

1. **README** — dataset metadata, direction of effects, imputation rules.
2. **Study_Characteristics** — 16 RCTs; surgery, anaesthesia, n, regimen, funding, conflicts.
3. **Risk_of_Bias** — Cochrane RoB 2 domain judgements and signalling notes.
4. **Pain_Scores_Rest** — arm n / mean / SD by postoperative window (0–2 h, 2–6 h, 6–24 h, >24 h).
5. **Pain_Scores_Movement** — same structure, movement / dynamic pain.
6. **Opioid_Consumption_MME** — cumulative opioid consumption converted to **oral** morphine milligram equivalents.
7. **PONV** — dichotomous nausea / vomiting / unspecified PONV (events / total).
8. **Other_Secondaries** — PACU stay, hospital stay, time to rescue, rescue proportion, satisfaction, QoR-15.
9. **Data_Dictionary** — column definitions, units, coding.

## Statistical defaults (do not change when reproducing)

| Layer | Settings |
|-------|----------|
| Meta-analysis | `meta::metacont` / `metabin`: `common=FALSE`, `random=TRUE`, `method.tau="REML"`, `method.random.ci="HK"` |
| Prediction interval | When \(k \ge 3\) |
| Arm labels | Intravenous / Oral |
| TSA (Fig 5) | `RTSA`: `fixed=FALSE`, `re_method="DL_HKSJ"`, `alpha=0.05`, `beta=0.20`, `es_alpha="esOF"`, futility none |
| TSA MCID | Pain 1.0 (0–10 scale, `sd_mc=2.5`); opioid 10 mg oral MME (`sd_mc=40`) |
| Opioid TSA sensitivities | MCID 5 mg, 15 mg; 90% power (`beta=0.10`) |

## How to run (recommended order)

From the repository root in R / RStudio:

```r
Sys.setenv(SRMA_PROJECT_ROOT = normalizePath("."))  # if needed

source("R/run_R1_full_package.R")          # primary forests + subgroups + funnels
source("R/R1_reviewer_sensitivities.R")    # same-timing, MME, Lombardi, PONV
source("R/regen_missing_subgroups.R")      # gap subgroups for Add 6/7/8 composites
source("R/regen_Add8_ModeOfAnesthesia.R")  # focused Add8 regen + pool CSV
source("R/regen_Add3_LOO.R")               # needs R1_primary_models.rds
source("R/export_Fig5_TSA.R")              # Fig 5a–e + opioid TSA sensitivities
# or only: source("R/export_TSA_opioid_sensitivities.R")
```

Interactive companion (writes many figures to `PROJECT_ROOT` cwd):

```r
source("R/SRMA_IV_PO_Acetaminophen_Meta_WorkingFile_R1.R")
```

Prefer the modular `R/run_*.R` / `R/export_*.R` / `R/regen_*.R` scripts to reproduce the R1 submission package.

## Script map

| File | Purpose |
|------|---------|
| `R/00_setup.R` | Paths, packages, locked specialty / anaesthesia maps, workbook loader |
| `R/01_helpers.R` | Shared `scale_pain`, forest layout, `run_cont` / `run_bin` / `run_sub` (REML+HK) |
| `R/run_R1_full_package.R` | Main R1 pipeline: primary forests, subgroups, funnels |
| `R/R1_reviewer_sensitivities.R` | Same-timing, MME leave-uncertain, Lombardi SD, Wilson PONV |
| `R/regen_missing_subgroups.R` | Gap subgroups for Add6/7/8 composites |
| `R/regen_Add3_LOO.R` | Additional file 3 leave-one-out influence forests |
| `R/regen_Add8_ModeOfAnesthesia.R` | ModeOfAnesthesia-only forests + mapping/pool CSVs |
| `R/export_Fig5_TSA.R` | Fig 5a–e TSA (RE DL+HKSJ) |
| `R/export_TSA_opioid_sensitivities.R` | Opioid TSA MCID 5/15 mg and 90% power |
| `R/SRMA_IV_PO_Acetaminophen_Meta_WorkingFile_R1.R` | Full interactive R1 working script |
| `archive/SRMA_IV_PO_Acetaminophen_Meta_WorkingFile.R` | Pre-R1 working file (superseded) |
| `scripts/python/rebuild_composites_nowrap.py` | Optional Additional File composite assembly |

## Disclaimer

Research / reproducibility only — not for clinical decision-making. Verify outputs against the extraction workbook and primary publications.
