# Intravenous versus oral perioperative acetaminophen — extracted trial-level data

Supporting dataset for the systematic review, meta-analysis, subgroup analysis and trial sequential analysis of randomised trials of **intravenous versus oral perioperative acetaminophen** for postoperative acute pain in adults.

| | |
|---|---|
| Review | BMC *Anesthesiology* submission |
| PROSPERO | [CRD420251157230](https://www.crd.york.ac.uk/prospero/display_record.php?ID=CRD420251157230) |
| Dataset file | [`Acetaminophen_IV_vs_PO_DataExtraction.xlsx`](Acetaminophen_IV_vs_PO_DataExtraction.xlsx) |
| Version | 1.2 (1 July 2026) |
| Licence | MIT (this extraction and any analysis code). The 16 source RCT publications remain copyright of their publishers. |
| Corresponding author | Hei Yeung Law, Department of Anaesthesiology, Perioperative and Pain Medicine, Queen Elizabeth Hospital, Hong Kong SAR. `LHY624@ha.org.hk` |

This is the **minimal dataset** needed to reproduce the pooled estimates. It is trial-level (one or more rows per RCT), not individual-participant data, except where Wilson 2019 summaries were computed from author-supplied IPD and then stored as arm-level means/SDs or event counts.

These files are released to support reproducibility of a systematic review. They are **not** a clinical guideline, product label, or dosing reference. Do not use them to treat patients.

## Files in this folder

| File | Role |
|---|---|
| `Acetaminophen_IV_vs_PO_DataExtraction.xlsx` | Machine-readable extraction (sheets listed below) |
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

Blank cells mean the outcome was not reported in an extractable form for that window. Orange-highlighted notes flag source-paper caveats (standard error stored as SD, IPD vs published counts, time-window mapping).

## Studies

16 parallel-group RCTs; 1,955 analysed participants (intravenous 972, oral 983).

Brett 2012; Fenlon 2013; Pettersson 2005; Plunkett 2017; O'Neal 2017; Politi 2017; Hickman 2018; Lombardi 2019; Westrich 2019; Wilson 2019; Bhoja 2020; Patel 2020; Yarahmadi 2020; Pelzer 2021; Mahajan 2017; Schwenk 2025.

## Conventions used in the review

- **Direction of effects.** Mean difference = intravenous minus oral. Negative values favour intravenous acetaminophen.
- **Pain.** 0–10 NRS/VAS as reported. Scores originally on a 0–100 mm scale (Wilson 2019, Lombardi 2019, Brett 2012) are stored on 0–100 and converted to 0–10 in the analysis scripts, not in this file.
- **Time windows.** Where a trial reported several time points inside a window, the **latest** point in that window was taken.
- **Opioid metric.** Converted to oral MME using CDC *Clinical Practice Guideline for Prescribing Opioids for Pain — United States, 2022* (IV morphine ×3; IV hydromorphone ×20; ketobemidone 1:1 with IV morphine then ×3). Only rows labelled `0-24` were pooled for the primary 24-hour opioid outcome.
- **Imputation.** Median (IQR) → mean/SD by Wan et al. *BMC Med Res Methodol* 2014; median (range) → Hozo et al. 2005; SE → SD as SD = SE × √n.
- **Wilson 2019.** Opioid consumption, hospital stay, time to rescue, satisfaction and PONV counts were calculated from author-supplied IPD. Published Table 3 pain parentheticals and Table 4 PONV counts differ from IPD / behave as SEs.

## Licence (MIT)

Copyright (c) 2026 Hei Yeung Law and co-authors.

Permission is hereby granted, free of charge, to any person obtaining a copy of this dataset and associated documentation files (the “Data”), to deal in the Data without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Data, and to permit persons to whom the Data is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Data.

THE DATA ARE PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE DATA OR THE USE OR OTHER DEALINGS IN THE DATA.

The 16 included RCT publications are **not** covered by this licence.
