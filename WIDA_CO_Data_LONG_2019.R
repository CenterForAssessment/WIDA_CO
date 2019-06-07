###############################################################################
###
###     Data prep and cleaning for WIDA Colorado  -  2019
###
###############################################################################

### Load packages

require(data.table)
require(SGP)


### Load data
tmp.dir <- getwd()
setwd(tempdir())
system(paste0("unzip '", file.path(tmp.dir, "Data/Base_Files/ACCESS19_SGP_LongFile_Readin.zip"), "'"))
WIDA_CO_Data_LONG_2019 <- fread("ACCESS19_SGP_LongFile_Readin.csv", colClasses=rep("character", 23))
unlink("ACCESS19_SGP_LongFile_Readin.csv")
setwd(tmp.dir)


### Tidy up data

WIDA_CO_Data_LONG_2019[, FIRST_NAME := as.factor(FIRST_NAME)]
WIDA_CO_Data_LONG_2019[, LAST_NAME := as.factor(LAST_NAME)]
levels(WIDA_CO_Data_LONG_2019$FIRST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2019$FIRST_NAME), capwords)
levels(WIDA_CO_Data_LONG_2019$LAST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2019$LAST_NAME), capwords)

WIDA_CO_Data_LONG_2019[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

# WIDA_CO_Data_LONG_2019[ACHIEVEMENT_LEVEL == "", ACHIEVEMENT_LEVEL := NA] # table(WIDA_CO_Data_LONG_2019[, is.na(SCALE_SCORE), ACHIEVEMENT_LEVEL])
WIDA_CO_Data_LONG_2019[, ACHIEVEMENT_LEVEL := as.factor(ACHIEVEMENT_LEVEL)]
levels(WIDA_CO_Data_LONG_2019$ACHIEVEMENT_LEVEL) <- c("NO SCORE", "L1", "L2", "L3", "L4", "L5", "L6")

WIDA_CO_Data_LONG_2019[GENDER=="MISSING", GENDER := NA]
WIDA_CO_Data_LONG_2019[, GENDER := as.factor(GENDER)]
levels(WIDA_CO_Data_LONG_2019$GENDER) <- sapply(levels(WIDA_CO_Data_LONG_2019$GENDER), capwords)

WIDA_CO_Data_LONG_2019[ETHNICITY=="MISSING", ETHNICITY := NA]
WIDA_CO_Data_LONG_2019[, ETHNICITY := as.factor(ETHNICITY)]
levels(WIDA_CO_Data_LONG_2019$ETHNICITY) <- sapply(levels(WIDA_CO_Data_LONG_2019$ETHNICITY), capwords)

WIDA_CO_Data_LONG_2019[FREE_REDUCED_LUNCH_STATUS == "MISSING", FREE_REDUCED_LUNCH_STATUS := NA]
WIDA_CO_Data_LONG_2019[, FREE_REDUCED_LUNCH_STATUS := as.factor(FREE_REDUCED_LUNCH_STATUS)]

WIDA_CO_Data_LONG_2019[ELL_STATUS == "MISSING", ELL_STATUS := NA]
WIDA_CO_Data_LONG_2019[, ELL_STATUS := as.factor(ELL_STATUS)]

WIDA_CO_Data_LONG_2019[IEP_STATUS == "MISSING", IEP_STATUS := NA]
WIDA_CO_Data_LONG_2019[, IEP_STATUS := as.factor(IEP_STATUS)]

WIDA_CO_Data_LONG_2019[GIFTED_TALENTED_PROGRAM_STATUS == "MISSING", GIFTED_TALENTED_PROGRAM_STATUS := NA]
WIDA_CO_Data_LONG_2019[, GIFTED_TALENTED_PROGRAM_STATUS := as.factor(GIFTED_TALENTED_PROGRAM_STATUS)]


WIDA_CO_Data_LONG_2019[, SCHOOL_NAME := as.factor(SCHOOL_NAME)]
levels(WIDA_CO_Data_LONG_2019$SCHOOL_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2019$SCHOOL_NAME), capwords)

WIDA_CO_Data_LONG_2019[, SCHOOL_ENROLLMENT_STATUS := as.factor(SCHOOL_ENROLLMENT_STATUS)]
WIDA_CO_Data_LONG_2019[, DISTRICT_ENROLLMENT_STATUS := as.factor(DISTRICT_ENROLLMENT_STATUS)]


### Setkey and invalidate duplicate cases

setkey(WIDA_CO_Data_LONG_2019, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG_2019, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG_2019[which(duplicated(WIDA_CO_Data_LONG_2019, by=key(WIDA_CO_Data_LONG_2019)))-1, VALID_CASE:="INVALID_CASE"]


### Invalidate cases with missing SCALE_SCOREs and IDs

WIDA_CO_Data_LONG_2019[is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
WIDA_CO_Data_LONG_2019[is.na(ID), VALID_CASE := "INVALID_CASE"]


### Save results

save(WIDA_CO_Data_LONG_2019, file="Data/WIDA_CO_Data_LONG_2019.Rdata")
