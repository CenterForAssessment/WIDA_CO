#################################################################################
###                                                                           ###
###       WIDA Colorado Learning Loss Analyses -- Create Baseline Matrices    ###
###                                                                           ###
#################################################################################

### Load necessary packages
require(SGP)

###   Load data from the 'official' 2020 SGP analyses
load("Data/WIDA_CO_SGP_LONG_Data.Rdata")

###   Create a smaller subset of the LONG data to work with.
WIDA_CO_Baseline_Data <- data.table::data.table(WIDA_CO_SGP_LONG_Data[, c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "VALID_CASE"),])

### NULL out existing baseline matrices (that never get used anyway)
SGPstateData[["WIDA_CO"]][["Baseline_splineMatrix"]] <- NULL

###   Read in Baseline SGP Configuration Scripts and Combine
source("SGP_CONFIG/2020/BASELINE/Matrices/READING.R")

WIDA_CO_BASELINE_CONFIG <- READING_BASELINE.config


###   Create Baseline Matrices

WIDA_CO_SGP <- prepareSGP(WIDA_CO_Baseline_Data, create.additional.variables=FALSE)

WIDA_CO_Baseline_Matrices <- baselineSGP(
				WIDA_CO_SGP,
				sgp.baseline.config=WIDA_CO_BASELINE_CONFIG,
				return.matrices.only=TRUE,
				calculate.baseline.sgps=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config = list(
					BACKEND="PARALLEL", WORKERS=list(TAUS=4))
)

###   Save results
save(WIDA_CO_Baseline_Matrices, file="Data/WIDA_CO_Baseline_Matrices.Rdata")
