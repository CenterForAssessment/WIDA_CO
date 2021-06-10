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


###  Re-run combineSGP for Interim Target Levels (for both 2020 and 2021)

###   Clean Up old
#WIDA_CO_SGP@Data[, grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE) := NULL]

###   Remove CUKU status vars tied to L3/L4 except original (3 Year)
#WIDA_CO_SGP@Data[, grep("UP_STATUS", names(WIDA_CO_SGP@Data), value=TRUE)[-3] := NULL]
#setnames(WIDA_CO_SGP@Data, "CATCH_UP_KEEP_UP_STATUS_3_YEAR", "CUKU_ORIG")

###   Remove Target SS calcs tied to L3/L4 (6 Year horizon)
#WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2020.TARGET_SCALE_SCORES"]] <- NULL
#WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2021.TARGET_SCALE_SCORES"]] <- NULL
#WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2020.LAGGED.TARGET_SCALE_SCORES"]] <- NULL
#WIDA_CO_SGP@SGP[["SGProjections"]][["READING.2021.LAGGED.TARGET_SCALE_SCORES"]] <- NULL


###  L1/L2
#SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Proficient","Proficient","Proficient","Proficient","Proficient",NA)

#WIDA_CO_SGP <- combineSGP(
#		WIDA_CO_SGP,
#		years = c("2020", "2021"),
#		sgp.target.scale.scores=TRUE,
#		sgp.target.scale.scores.merge="all_years_lagged_current",
#		max.sgp.target.years.forward=0:1
#)

#tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
#setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_1_TARGET", tmp.target.names))
#setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L1L2_CUKU", names(WIDA_CO_SGP@Data)))

#target.ss.index <- grep('^(?!.*2019).*SCALE_SCORES', names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
#names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L1L2", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L2/L3
#SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Proficient","Proficient","Proficient","Proficient",NA)

#WIDA_CO_SGP <- combineSGP(
#		WIDA_CO_SGP,
#		years = c("2020", "2021"),
#		sgp.target.scale.scores=TRUE,
#		sgp.target.scale.scores.merge="all_years_lagged_current",
#		max.sgp.target.years.forward=0:2
#)

#tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
#setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_2_TARGET", tmp.target.names))
#setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L2L3_CUKU", names(WIDA_CO_SGP@Data)))

#target.ss.index <- grep("^(?!.*2019).*SCALE_SCORES", names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
#names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L2L3", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L4/L5 (Stay at above L5 - do before Official/L4)
#SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Not Proficient","Not Proficient","Proficient","Proficient",NA)

#WIDA_CO_SGP <- combineSGP(
#		WIDA_CO_SGP,
#		years = c("2020", "2021"),
#		sgp.target.scale.scores=TRUE,
#		sgp.target.scale.scores.merge="all_years_lagged_current",
#		max.sgp.target.years.forward=0:1
#)

#tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
#setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_4_TARGET", tmp.target.names))
#setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L4L5_CUKU", names(WIDA_CO_SGP@Data)))

#target.ss.index <- grep("^(?!.*2019).*SCALE_SCORES", names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl=TRUE)
#names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <- gsub("SCALE_SCORES", "SS_L4L5", names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


###  L3/L4 (Official)
#SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <- c("Not Proficient","Not Proficient","Not Proficient","Proficient","Proficient","Proficient",NA)

#WIDA_CO_SGP <- combineSGP(
#		WIDA_CO_SGP,
#		years = c("2020", "2021"),
#		sgp.target.scale.scores=TRUE,
#		sgp.target.scale.scores.merge="all_years_lagged_current",
#		max.sgp.target.years.forward=0:3
#)

#tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value=TRUE)
#setnames(WIDA_CO_SGP@Data, tmp.target.names, gsub("SGP_TARGET", "SGP_LEVEL_3_TARGET", tmp.target.names))
#setnames(WIDA_CO_SGP@Data, gsub("CATCH_UP_KEEP_UP", "L3L4_CUKU", names(WIDA_CO_SGP@Data)))


###   Rename generic "CATCH_UP_KEEP_UP_STATUS" for all students as "Official"
#setnames(WIDA_CO_SGP@Data, "CUKU_ORIG", "CATCH_UP_KEEP_UP_STATUS")


###   Save results

#save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")


######
###
###   Create Individual Student Reports
###
######

#require(data.table)

###   Clean up SCHOOL_NAME and DISTRICT_NAME
###   Check levels first to confirm special.words - Clean Well for ISRs

#   Schools
#grep("Ece", levels(WIDA_CO_SGP@Data$SCHOOL_NAME), value=T)

#new.sch.levs <- toupper(levels(WIDA_CO_SGP@Data$SCHOOL_NAME))
#new.sch.levs <- gsub("/", " / ", new.sch.levs)

#new.sch.levs <- sapply(new.sch.levs, SGP::capwords, special.words = c('AIM', 'APS', 'AXIS', 'AXL', 'CCH', 'CEC', 'CMS', 'COVA', 'CUBE', 'DC', 'DCIS', 'DSST', 'DSST:', 'ECE-8', 'GES', 'GOAL', 'GVR', 'IB', 'KIPP', 'PK', 'PK-8', 'PK-12', 'PSD', 'LEAP', 'MHCD', 'MS', 'SHS', 'STEM', 'TCA', 'VSSA'), USE.NAMES=FALSE)
#new.sch.levs <- gsub(" / ", "/", new.sch.levs)
#new.sch.levs <- gsub("[']S", "'s", new.sch.levs)
#new.sch.levs <- gsub("Prek", "PreK", new.sch.levs)

#sort(grep("Mc", new.sch.levs, value=T))
#new.sch.levs <- gsub("Mc Auliffe", "McAuliffe", new.sch.levs)
#new.sch.levs <- gsub("Mcauliffe", "McAuliffe", new.sch.levs)
#new.sch.levs <- gsub("Mc Clave", "McClave", new.sch.levs)
#new.sch.levs <- gsub("Mcclave", "McClave", new.sch.levs)
#new.sch.levs <- gsub("Mc Elwain", "McElwain", new.sch.levs)
#new.sch.levs <- gsub("Mcelwain", "McElwain", new.sch.levs)
#new.sch.levs <- gsub("Mc Ginnis", "McGinnis", new.sch.levs)
#new.sch.levs <- gsub("Mcginnis", "McGinnis", new.sch.levs)
#new.sch.levs <- gsub("Mc Glone", "McGlone", new.sch.levs)
#new.sch.levs <- gsub("Mcglone", "McGlone", new.sch.levs)
#new.sch.levs <- gsub("Mc Graw", "McGraw", new.sch.levs)
#new.sch.levs <- gsub("Mcgraw", "McGraw", new.sch.levs)
#new.sch.levs <- gsub("Mc Kinley", "McKinley", new.sch.levs)
#new.sch.levs <- gsub("Mckinley", "McKinley", new.sch.levs)
#new.sch.levs <- gsub("Mc Lain", "McLain", new.sch.levs)
#new.sch.levs <- gsub("Mclain", "McLain", new.sch.levs)
#new.sch.levs <- gsub("Mc Meen", "McMeen", new.sch.levs)
#new.sch.levs <- gsub("Mcmeen", "McMeen", new.sch.levs)
#sort(grep("Mc", new.sch.levs, value=T))

#new.sch.levs <- gsub("Achieve Online", "ACHIEVE Online", new.sch.levs)
#new.sch.levs <- gsub("Hope Online", "HOPE Online", new.sch.levs)
#new.sch.levs <- gsub("Reach Charter", "REACH Charter", new.sch.levs)
#new.sch.levs <- gsub("Soar A", "SOAR A", new.sch.levs)
#new.sch.levs <- gsub("Strive Prep", "STRIVE Prep", new.sch.levs)
#new.sch.levs <- gsub("Edcsd", "eDCSD", new.sch.levs)

#setattr(WIDA_CO_SGP@Data$SCHOOL_NAME, "levels", new.sch.levs)


###   Districts

#WIDA_CO_SGP@Data[, DISTRICT_NAME := as.factor(DISTRICT_NAME)]
#grep("J", levels(WIDA_CO_SGP@Data$DISTRICT_NAME), value=T)
#new.dst.levs <- toupper(levels(WIDA_CO_SGP@Data$DISTRICT_NAME))
#new.dst.levs <- gsub("/", " / ", new.dst.levs)
#new.dst.levs <- gsub("[-]", " - ", new.dst.levs)

#new.dst.levs <- sapply(new.dst.levs, SGP::capwords, special.words = c('1J', '2J', '3J', '4J', '5J', '6J', '10J', '10JT', '13JT', '11J', '22J', '27J', '28J', '29J', '31J', '33J', '50J', '50JT', '60JT', '100J', 'JT', '32J', 'RJ', '26J', '49JT', '4A', 'RD', 'RE', 'RE1J'), USE.NAMES=FALSE)
#new.dst.levs <- gsub(" / ", "/", new.dst.levs)
#new.dst.levs <- gsub(" - ", "-", new.dst.levs)
#new.dst.levs <- gsub("Mc Clave", "McClave", new.dst.levs)
#new.dst.levs <- gsub("Mcclave", "McClave", new.dst.levs)
#grep("Mc", new.dst.levs, value=T) # Should only leave * Conejos
#grep("j", new.dst.levs, value=T) # Should only leave * Conejos

#setattr(WIDA_CO_SGP@Data$DISTRICT_NAME, "levels", new.dst.levs)


###
###   Produce ISRs using visualizeSGP function
###

#visualizeSGP(
#		WIDA_CO_SGP,
#		plot.types="studentGrowthPlot",
#		sgPlot.years='2021',
#		sgPlot.content_areas="READING",
#		# sgPlot.demo.report=TRUE,
#		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SG_PLOTS=20)))


###
###   Post-Hoc checks for missing schools/districs
###

#dist <- system("ls /home/ec2-user/ACCESS_2021_ISRs/Visualizations/studentGrowthPlots/School/2021", intern=TRUE)
#dat.dist <- unique(WIDA_CO_SGP@Data[YEAR=='2021' & !is.na(SGP)]$DISTRICT_NUMBER)
#miss <- setdiff(as.numeric(dat.dist), as.numeric(dist))
#m <- WIDA_CO_SGP@Data[!is.na(SGP) & as.numeric(DISTRICT_NUMBER) %in% miss]
#table(m[, GRADE, CONTENT_AREA]) #  0

#problem.districts <- list()
#for (d in as.numeric(dist)) {
#	data.schools <- unique(WIDA_CO_SGP@Data[as.numeric(DISTRICT_NUMBER) == d, as.numeric(SCHOOL_NUMBER)])
#	file.schools <- system(paste0("ls /home/ec2-user/ACCESS_2021_ISRs/Visualizations/studentGrowthPlots/School/2021/", d), intern=TRUE)
#	file.schools <- gsub("[.]zip", "", file.schools)
#	if (!(all(file.schools %in% data.schools) | all(data.schools %in% file.schools))) {
#		missing.schools <- setdiff(data.schools, file.schools)
#		problem.districts[[d]] <- missing.schools
#	}
#}

#  No Problem Schools within Districts :-)
