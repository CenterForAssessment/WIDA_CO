##########################################################################################
###
### Script for calculating SGPs for 2016-2017 for WIDA-ACCESS
###
##########################################################################################

### Load SGP package

require(SGP)


### Load Data

load("Data/WIDA_CO_Data_LONG.Rdata")


### Run analyses

WIDA_CO_SGP <- abcSGP(
		WIDA_CO_Data_LONG,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "visualizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=FALSE,
		sgp.projections.baseline=FALSE,
		sgp.projections.lagged.baseline=FALSE,
		sgp.percentiles.equated=FALSE,
		sgp.target.scale.scores=TRUE,
		save.intermediate.results=TRUE,
		sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SGP_SCALE_SCORE_TARGETS=4)))


### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")
