################################################################################
###                                                                          ###
###           Script for calculating SGPs for 2020 for WIDA-ACCESS           ###
###                                                                          ###
################################################################################

### Load SGP package

require(SGP)
require(data.table)

###   Load Data

load("Data/WIDA_CO_Data_LONG_2020.Rdata")
load("Data/WIDA_CO_SGP.Rdata")  #  NOTE:  Saved with new OTG data 6/11/2020 (See WIDA_CO_OTG_2019.R)

###   Run updateSGP to produce cohort referrenced SGPs, etc.

WIDA_CO_SGP <- updateSGP(
		WIDA_CO_SGP,
		WIDA_CO_Data_LONG_2020,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=FALSE,
		sgp.projections.baseline=FALSE,
		sgp.projections.lagged.baseline=FALSE,
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


###  Re-run combineSGP for Interim Target Levels (for both 2019 and 2020)

###   Clean Up old
WIDA_CO_SGP@Data[, grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE) := NULL]

###   Remove CUKU status vars tied to L3/L4 except original (3 Year)
WIDA_CO_SGP@Data[, grep("UP_STATUS", names(WIDA_CO_SGP@Data), value=TRUE)[-3] := NULL]
setnames(WIDA_CO_SGP@Data, "CATCH_UP_KEEP_UP_STATUS_3_YEAR", "CUKU_ORIG")

###   Remove Target SS calcs tied to L3/L4 (6 Year horizon)
WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2019.TARGET_SCALE_SCORES"]] <- NULL
WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2020.TARGET_SCALE_SCORES"]] <- NULL
WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2019.LAGGED.TARGET_SCALE_SCORES"]] <- NULL
WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2020.LAGGED.TARGET_SCALE_SCORES"]] <- NULL


###  L1/L2
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Proficient","Proficient","Proficient","Proficient","Proficient",NA)

WIDA_CO_SGP <- combineSGP(
		WIDA_CO_SGP,
		years = c("2019", "2020"),
		sgp.target.scale.scores=TRUE,
		sgp.target.scale.scores.merge="all_years_lagged_current",
		max.sgp.target.years.forward=0:1
)

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_1_TARGET", tmp.target.names))
setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L1L2_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep('^(?!.*2018).*SCALE_SCORES', names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L1L2", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L2/L3
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Proficient","Proficient","Proficient","Proficient",NA)

WIDA_CO_SGP <- combineSGP(
		WIDA_CO_SGP,
		years = c("2019", "2020"),
		sgp.target.scale.scores=TRUE,
		sgp.target.scale.scores.merge="all_years_lagged_current",
		max.sgp.target.years.forward=0:2
)

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_2_TARGET", tmp.target.names))
setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L2L3_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep("^(?!.*2018).*SCALE_SCORES", names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L2L3", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L4/L5 (Stay at above L5 - do before Official/L4)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Not Proficient","Not Proficient","Proficient","Proficient",NA)

WIDA_CO_SGP <- combineSGP(
		WIDA_CO_SGP,
		years = c("2019", "2020"),
		sgp.target.scale.scores=TRUE,
		sgp.target.scale.scores.merge="all_years_lagged_current",
		max.sgp.target.years.forward=0:1
)

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_4_TARGET", tmp.target.names))
setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L4L5_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep("^(?!.*2018).*SCALE_SCORES", names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L4L5", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L3/L4 (Official)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Not Proficient","Proficient","Proficient","Proficient",NA)

WIDA_CO_SGP <- combineSGP(
		WIDA_CO_SGP,
		years = c("2019", "2020"),
		sgp.target.scale.scores=TRUE,
		sgp.target.scale.scores.merge="all_years_lagged_current",
		max.sgp.target.years.forward=0:3
)

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_3_TARGET", tmp.target.names))
setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L3L4_CUKU", names(WIDA_CO_SGP@Data)))


###   Rename generic "CATCH_UP_KEEP_UP_STATUS" for all students as "Official"
setnames(WIDA_CO_SGP@Data, "CUKU_ORIG", "CATCH_UP_KEEP_UP_STATUS")


###   Save results

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")



###   Create Student Reports

visualizeSGP(
		WIDA_CO_SGP,
		plot.types="studentGrowthPlot",
		sgPlot.years='2020',
		sgPlot.content_areas="READING",
		# sgPlot.year.span=3, # Now in SGPstateData
		# sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SG_PLOTS=20)))
