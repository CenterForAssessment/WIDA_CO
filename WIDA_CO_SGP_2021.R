################################################################################
###                                                                          ###
###           Script for calculating SGPs for 2021 for WIDA-ACCESS           ###
###                                                                          ###
################################################################################

### Load SGP package
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_CO_SGP.Rdata")
load("Data/WIDA_CO_Data_LONG_2021.Rdata")

### Add baseline matrices
SGPstateData <- SGPmatrices::addBaselineMatrices("WIDA_CO", "2021")

###   Run updateSGP to produce cohort referrenced SGPs, etc.
WIDA_CO_SGP <- updateSGP(
		WIDA_CO_SGP,
		WIDA_CO_Data_LONG_2021,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=TRUE,
		sgp.projections.baseline=TRUE,
		sgp.projections.lagged.baseline=TRUE,
		sgp.percentiles.equated=FALSE,
		# sgp.target.scale.scores=TRUE, #  Run below
		save.intermediate.results=FALSE,
		parallel.config=list(
			BACKEND="PARALLEL",
			WORKERS=list(
				PERCENTILES=8,
				PROJECTIONS=4,
				LAGGED_PROJECTIONS=2,
				SGP_SCALE_SCORE_TARGETS=2,
				SUMMARY=4)))


###  Interim Target Levels for 2021 NOT RUN

###   Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")


######
###
###   Create Individual Student Reports
###
######

###   No ISRs created in 2021
