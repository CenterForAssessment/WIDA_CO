############################################################################################
###
### Script for formatting ACCESS (Colorado WIDA) Data and Running SGP calculations
###
############################################################################################

### Load SGP Package

require(SGP)
require(data.table)

setwd("WIDA_CO")

###  Format ACCESS Data as provided by Marie
WIDA_CO_Data_LONG <- data.table(read.csv(file= 'Data/Base_Files/ACCESS_Data_LONG.csv', stringsAsFactors=FALSE))

#  Make all content area "READING" to conform with other WIDA state members
WIDA_CO_Data_LONG[, CONTENT_AREA := "READING"]

#  Make all ACHIEVEMENT_LEVEL values conform to same format ('L1', 'L2', ... 'L6')
WIDA_CO_Data_LONG[, ACHIEVEMENT_LEVEL := gsub("CELA ", "", WIDA_CO_Data_LONG$ACHIEVEMENT_LEVEL)]
WIDA_CO_Data_LONG[, ACHIEVEMENT_LEVEL := gsub("ACCESS ", "", WIDA_CO_Data_LONG$ACHIEVEMENT_LEVEL)]
WIDA_CO_Data_LONG[, ACHIEVEMENT_LEVEL := ordered(ACHIEVEMENT_LEVEL, labels=c('L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'NO SCORE'))]

###  Run abcSGP to produce 2014 percentiles and projections

WIDA_CO_SGP <- abcSGP(
	sgp_object = WIDA_CO_Data_LONG, state="WIDA_CO",
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "outputSGP"),
	years="2014",
	sgp.percentiles.baseline = FALSE,
	sgp.projections.baseline = FALSE,
	sgp.projections.lagged.baseline = FALSE,
	sgp.target.scale.scores=TRUE,
	simulate.sgps=FALSE,
	save.intermediate.results=TRUE,
	sgPlot.demo.report=TRUE,
	outputSGP.output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data"),
	parallel.config=list(BACKEND="PARALLEL", 
		WORKERS=list(PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SGP_SCALE_SCORE_TARGETS=4, SUMMARY=4)))

### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")

###  Produce sample reports ("boiler plate" reports from the SGP package)

visualizeSGP(
	WIDA_CO_SGP,# state="WIDA_CO",
	plot.types="studentGrowthPlot",
	sgPlot.years='2014',
	sgPlot.save.sgPlot.data=TRUE,
	sgPlot.year.span=2,
	sgPlot.demo.report=TRUE
)
