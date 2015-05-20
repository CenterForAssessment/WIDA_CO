########################################################################
###
### Script for Running ACCESS (Colorado WIDA) SGP calculations
###
########################################################################

### Load SGP Package

require(SGP)
require(data.table)
setwd("/media/Data/Dropbox/SGP/ACCESS")

###  Load ACCESS Data provided by Marie
WIDA_CO_Data_LONG <- fread('Data/Base_Files/ACCESS_Data_12-15_LONG readin.csv', colClasses=rep("character", 23))

###  Run abcSGP to produce 2015 percentiles and projections
WIDA_CO_SGP <- abcSGP(
	sgp_object = WIDA_CO_Data_LONG, state="WIDA_CO",
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "outputSGP"),
	years=c("2014", "2015"),
	sgp.percentiles.baseline = FALSE,
	sgp.projections.baseline = FALSE,
	sgp.projections.lagged.baseline = FALSE,
	sgp.target.scale.scores=TRUE,
	simulate.sgps=FALSE,
	outputSGP.output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data"),
	parallel.config=list(BACKEND="PARALLEL", 
		WORKERS=list(
			PERCENTILES=12, PROJECTIONS=12, LAGGED_PROJECTIONS=12, 
			SGP_SCALE_SCORE_TARGETS=12, SUMMARY=12)))

### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")

###  Produce custom ISR's

visualizeSGP(
	WIDA_CO_SGP,# state="WIDA_CO",
	plot.types="studentGrowthPlot",
	sgPlot.years='2015',
	sgPlot.year.span=3,
	sgPlot.demo.report=TRUE,
	parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SG_PLOTS=23)))
