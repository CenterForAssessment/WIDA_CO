##########################################################################################
###
### Script for calculating SGPs for 2016-2017 for WIDA-ACCESS
###
##########################################################################################

### Load SGP package

require(SGP)


### Load Data

load("Data/WIDA_CO_SGP.Rdata")
load("Data/WIDA_CO_Data_LONG_2018.Rdata")

# WIDA_CO_SGP@Data <- WIDA_CO_SGP@Data[CONTENT_AREA == "READING"]

### Run analyses

WIDA_CO_SGP <- updateSGP(
		what_sgp_object = WIDA_CO_SGP,
		with_sgp_data_LONG = WIDA_CO_Data_LONG_2018,
		content_areas="READING",
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
		sgPlot.demo.report=TRUE,
		parallel.config=list(
			BACKEND="PARALLEL",
			WORKERS=list(
				PERCENTILES=6,
				PROJECTIONS=6,
				LAGGED_PROJECTIONS=6,
				SGP_SCALE_SCORE_TARGETS=6,
				SUMMARY=6)))


### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")


###   Create Student Reports

visualizeSGP(
		WIDA_CO_SGP,
		plot.types="studentGrowthPlot",
		sgPlot.years='2018',
		sgPlot.content_areas="READING",
		# sgPlot.year.span=3, # Now in SGPstateData
		sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SG_PLOTS=6)))
