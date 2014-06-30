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
WIDA_CO_Data_LONG <- data.table(read.csv(file= 'Data/Base_Files/ACCESS_Data_LONG_2014_06_02_2014.csv', stringsAsFactors=FALSE))

###  Only include CELA data from 2011-13.  Only 3 priors per Marie 6/2/14 phone call.
###  Control through SGPstateData - SGP_Configuration instead of subsetting data - per Damian 6/3/14
# WIDA_CO_Data_LONG <- WIDA_CO_Data_LONG[YEAR %in% 2011:2014]

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
	outputSGP.output.type=c("LONG_Data", "LONG_FINAL_YEAR_Data"),
	parallel.config=list(BACKEND="PARALLEL", 
		WORKERS=list(PERCENTILES=12, PROJECTIONS=12, LAGGED_PROJECTIONS=12, SGP_SCALE_SCORE_TARGETS=12, SUMMARY=12)))

### Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")

###  Produce Individual Student Reports

visualizeSGP(
	WIDA_CO_SGP,# state="WIDA_CO",
	plot.types="studentGrowthPlot",
	sgPlot.years='2014',
	sgPlot.year.span=2,
	# sgPlot.demo.report=TRUE # Use this argument to produce sample catalogue only
)
