# SRMA Acetaminophen — R analysis code (BMC Anesthesiology)

| | |
|---|---|
| PROSPERO | [CRD420251157230](https://www.crd.york.ac.uk/prospero/display_record.php?ID=CRD420251157230) |
| Licence | MIT (see `LICENSE`) — analysis code only; RCT publications remain copyright of their publishers |
| Corresponding author | Hei Yeung Law (`LHY624@ha.org.hk`) |

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
