###############################################################################
###
###     Data prep and cleaning for WIDA Colorado  -  2018
###
###############################################################################

### Load packages

require(data.table)
require(SGP)


### Load data
tmp.dir <- getwd()
setwd(tempdir())
system(paste0("unzip '", file.path(tmp.dir, "Data/Base_Files/ACCESS18_SGP_LongFile_Readin_06.08.18.zip"), "'"))
WIDA_CO_Data_LONG_2018 <- fread("ACCESS18_SGP_LongFile_Readin_06.08.18.csv", colClasses=rep("character", 23))
unlink("ACCESS18_SGP_LongFile_Readin.csv")
setwd(tmp.dir)


### Tidy up data

WIDA_CO_Data_LONG_2018[, FIRST_NAME := as.factor(FIRST_NAME)]
WIDA_CO_Data_LONG_2018[, LAST_NAME := as.factor(LAST_NAME)]
levels(WIDA_CO_Data_LONG_2018$FIRST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2018$FIRST_NAME), capwords)
levels(WIDA_CO_Data_LONG_2018$LAST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2018$LAST_NAME), capwords)

WIDA_CO_Data_LONG_2018[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

# WIDA_CO_Data_LONG_2018[ACHIEVEMENT_LEVEL == "", ACHIEVEMENT_LEVEL := NA] # table(WIDA_CO_Data_LONG_2018[, is.na(SCALE_SCORE), ACHIEVEMENT_LEVEL])
# tbl <- WIDA_CO_Data_LONG_2018[CONTENT_AREA=="READING" & !is.na(SCALE_SCORE), as.list(summary(SCALE_SCORE)), keyby=c("ACHIEVEMENT_LEVEL", "GRADE")]
# setkey(tbl, GRADE);tbl
WIDA_CO_Data_LONG_2018[, ACHIEVEMENT_LEVEL := as.factor(ACHIEVEMENT_LEVEL)]
levels(WIDA_CO_Data_LONG_2018$ACHIEVEMENT_LEVEL) <- c("NO SCORE", "L1", "L2", "L3", "L4", "L5", "L6")

WIDA_CO_Data_LONG_2018[GENDER=="MISSING", GENDER := NA]
WIDA_CO_Data_LONG_2018[, GENDER := as.factor(GENDER)]
levels(WIDA_CO_Data_LONG_2018$GENDER) <- sapply(levels(WIDA_CO_Data_LONG_2018$GENDER), capwords)

WIDA_CO_Data_LONG_2018[ETHNICITY=="MISSING", ETHNICITY := NA]
WIDA_CO_Data_LONG_2018[, ETHNICITY := as.factor(ETHNICITY)]
levels(WIDA_CO_Data_LONG_2018$ETHNICITY) <- sapply(levels(WIDA_CO_Data_LONG_2018$ETHNICITY), capwords)

WIDA_CO_Data_LONG_2018[FREE_REDUCED_LUNCH_STATUS == "MISSING", FREE_REDUCED_LUNCH_STATUS := NA]
WIDA_CO_Data_LONG_2018[, FREE_REDUCED_LUNCH_STATUS := as.factor(FREE_REDUCED_LUNCH_STATUS)]

WIDA_CO_Data_LONG_2018[ELL_STATUS == "MISSING", ELL_STATUS := NA]
WIDA_CO_Data_LONG_2018[, ELL_STATUS := as.factor(ELL_STATUS)]

WIDA_CO_Data_LONG_2018[IEP_STATUS == "MISSING", IEP_STATUS := NA]
WIDA_CO_Data_LONG_2018[, IEP_STATUS := as.factor(IEP_STATUS)]

WIDA_CO_Data_LONG_2018[GIFTED_TALENTED_PROGRAM_STATUS == "MISSING", GIFTED_TALENTED_PROGRAM_STATUS := NA]
WIDA_CO_Data_LONG_2018[, GIFTED_TALENTED_PROGRAM_STATUS := as.factor(GIFTED_TALENTED_PROGRAM_STATUS)]

WIDA_CO_Data_LONG_2018[, SCHOOL_NAME := as.factor(SCHOOL_NAME)]
levels(WIDA_CO_Data_LONG_2018$SCHOOL_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2018$SCHOOL_NAME), capwords)

WIDA_CO_Data_LONG_2018[, SCHOOL_ENROLLMENT_STATUS := as.factor(SCHOOL_ENROLLMENT_STATUS)]
WIDA_CO_Data_LONG_2018[, DISTRICT_ENROLLMENT_STATUS := as.factor(DISTRICT_ENROLLMENT_STATUS)]


### Setkey and invalidate duplicate cases

setkey(WIDA_CO_Data_LONG_2018, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG_2018, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG_2018[which(duplicated(WIDA_CO_Data_LONG_2018, by=key(WIDA_CO_Data_LONG_2018)))-1, VALID_CASE:="INVALID_CASE"]


### Invalidate cases with missing SCALE_SCOREs and IDs

WIDA_CO_Data_LONG_2018[is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
WIDA_CO_Data_LONG_2018[is.na(ID), VALID_CASE := "INVALID_CASE"]


### Save results

save(WIDA_CO_Data_LONG_2018, file="Data/WIDA_CO_Data_LONG_2018.Rdata")
