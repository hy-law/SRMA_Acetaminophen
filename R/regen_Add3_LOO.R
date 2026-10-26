# =============================================================================
# regen_Add3_LOO.R
# =============================================================================
# Purpose: Additional file 3 leave-one-out influence forests from locked RDS
# Manuscript: BMC Anesthesiology
# Requires: R1_outputs/R1_primary_models.rds (from run_R1_full_package.R)
# Methods: meta::metainf on REML+HK primary models
#
# How to run: source("R/regen_Add3_LOO.R")
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

suppressPackageStartupMessages({ library(meta); library(grid) })
ensure_outdir(file.path(OUTDIR, "loo"))
outdir <- file.path(OUTDIR, "loo")

m <- readRDS(file.path(OUTDIR, "R1_primary_models.rds"))
stopifnot(all(c("0to2h", "2to6h", "6to24h", "opioid") %in% names(m)))

# Sanity vs locked manuscript pools
cat(sprintf("Pain 0-2h MD %.3f (expect ~-0.28)\n", m[["0to2h"]]$TE.random))
cat(sprintf("Opioid MD %.3f (expect ~-1.75)\n", m$opioid$TE.random))

run_loo <- function(model, label, file_stem, xlab) {
  inf <- metainf(model)
  forest_call <- function() {
    meta::forest(
      inf,
      smlab = paste0("Influence analysis\n", label),
      xlab = xlab,
      leftcols = c("studlab", "TE", "lower", "upper"),
      leftlabs = c("Omitted Study", "MD", "Lower", "Upper"),
      col.square = "#4575b4",
      col.diamond = "#d73027",
      print.tau2 = TRUE,
      print.I2 = TRUE,
      plotwidth = grid::unit(8.5, "cm"),
      colgap.forest.left = grid::unit(0.8, "cm"),
      colgap.forest.right = grid::unit(0.5, "cm"),
      fs.smlab = 10
    )
  }
  h <- max(7, 0.42 * (model$k + 2) + 3)
  pdf(file.path(outdir, paste0(file_stem, ".pdf")), width = 16, height = h)
  forest_call(); dev.off()
  png(file.path(outdir, paste0(file_stem, ".png")), width = 16 * 150, height = h * 150, res = 150)
  forest_call(); dev.off()
  cat("Wrote", file_stem, "overall TE=", round(model$TE.random, 4), "\n")
  invisible(inf)
}

run_loo(m$opioid, "24-h Opioid (MME)", "Influence_Opioid_MME_0-24h",
        "Mean Difference in MME (IV minus PO)")
run_loo(m[["0to2h"]], "Pain at Rest 0-2 h", "Influence_PainRest_0to2h",
        "Mean Difference (IV minus PO)")
run_loo(m[["2to6h"]], "Pain at Rest 2-6 h", "Influence_PainRest_2to6h",
        "Mean Difference (IV minus PO)")
run_loo(m[["6to24h"]], "Pain at Rest 6-24 h", "Influence_PainRest_6to24h",
        "Mean Difference (IV minus PO)")
cat("LOO DONE\n")
