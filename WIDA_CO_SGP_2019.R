##########################################################################################
###
### Script for calculating SGPs for 2018-2019 for WIDA-ACCESS
###
##########################################################################################

### Load SGP package

require(SGP)
require(data.table)

### Load Data

load("Data/WIDA_CO_SGP_LONG_Data.Rdata")
load("Data/WIDA_CO_Data_LONG_2019.Rdata")

WIDA_CO_Data_LONG <- rbindlist(list(WIDA_CO_SGP_LONG_Data[YEAR %in% c("2017", "2018") & CONTENT_AREA == "READING"], WIDA_CO_Data_LONG_2019), fill = TRUE)

### Run analyses

WIDA_CO_SGP <- abcSGP(
		WIDA_CO_Data_LONG,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=FALSE,
		sgp.projections.baseline=FALSE,
		sgp.projections.lagged.baseline=FALSE,
		sgp.percentiles.equated=FALSE,
		sgp.target.scale.scores=TRUE,
		save.intermediate.results=FALSE,
		parallel.config=list(
			BACKEND="PARALLEL",
			WORKERS=list(
				PERCENTILES=13,
				PROJECTIONS=13,
				LAGGED_PROJECTIONS=13,
				SGP_SCALE_SCORE_TARGETS=13,
				SUMMARY=20)))


###   Re-run combineSGP with multiple year TARGET_SCALE_SCORES (Added 5/14/2020)

WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2019.TARGET_SCALE_SCORES"]] <- NULL
WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2019.LAGGED.TARGET_SCALE_SCORES"]] <- NULL

WIDA_CO_SGP <- combineSGP(
		WIDA_CO_SGP,
		years = 2019,
		sgp.target.scale.scores=TRUE,
		max.sgp.target.years.forward=1:6
)

### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")


###   Create Student Reports

visualizeSGP(
		WIDA_CO_SGP,
		plot.types="studentGrowthPlot",
		sgPlot.years='2019',
		sgPlot.content_areas="READING",
		# sgPlot.year.span=3, # Now in SGPstateData
		# sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SG_PLOTS=20)))
