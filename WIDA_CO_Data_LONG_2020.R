###############################################################################
###
###     Data prep and cleaning for WIDA Colorado  -  2020
###
###############################################################################

### Load packages

require(data.table)
require(SGP)


### Load data
WIDA_CO_Data_LONG_2020 <- fread("Data/Base_Files/NCIEA (Marie Huchton)/WIDA_2020_Readin_6.2.2020.csv", colClasses=rep("character", 31))


### Tidy up data

WIDA_CO_Data_LONG_2020[, FIRST_NAME := as.factor(FIRST_NAME)]
WIDA_CO_Data_LONG_2020[, LAST_NAME := as.factor(LAST_NAME)]
levels(WIDA_CO_Data_LONG_2020$FIRST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2020$FIRST_NAME), capwords)
levels(WIDA_CO_Data_LONG_2020$LAST_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2020$LAST_NAME), capwords)

WIDA_CO_Data_LONG_2020[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

# WIDA_CO_Data_LONG_2020[ACHIEVEMENT_LEVEL == "", ACHIEVEMENT_LEVEL := NA] # table(WIDA_CO_Data_LONG_2020[, is.na(SCALE_SCORE), ACHIEVEMENT_LEVEL])
# WIDA_CO_Data_LONG_2020[, ACHIEVEMENT_LEVEL := as.factor(ACHIEVEMENT_LEVEL)]
# levels(WIDA_CO_Data_LONG_2020$ACHIEVEMENT_LEVEL) <- c("L1", "L2", "L3", "L4", "L5", "L6", "NO SCORE")
# WIDA_CO_Data_LONG_2020[, ACHIEVEMENT_LEVEL := as.character(ACHIEVEMENT_LEVEL)]

WIDA_CO_Data_LONG_2020[GENDER=="MISSING", GENDER := NA]
WIDA_CO_Data_LONG_2020[, GENDER := as.factor(GENDER)]
levels(WIDA_CO_Data_LONG_2020$GENDER) <- sapply(levels(WIDA_CO_Data_LONG_2020$GENDER), capwords)

WIDA_CO_Data_LONG_2020[ETHNICITY=="MISSING", ETHNICITY := NA]
WIDA_CO_Data_LONG_2020[, ETHNICITY := as.factor(ETHNICITY)]
levels(WIDA_CO_Data_LONG_2020$ETHNICITY) <- sapply(levels(WIDA_CO_Data_LONG_2020$ETHNICITY), capwords)

WIDA_CO_Data_LONG_2020[FREE_REDUCED_LUNCH_STATUS == "MISSING", FREE_REDUCED_LUNCH_STATUS := NA]
WIDA_CO_Data_LONG_2020[, FREE_REDUCED_LUNCH_STATUS := as.factor(FREE_REDUCED_LUNCH_STATUS)]

WIDA_CO_Data_LONG_2020[ELL_STATUS == "Missing", ELL_STATUS := NA] # NOTE change in 2019 to camel case
WIDA_CO_Data_LONG_2020[, ELL_STATUS := as.factor(ELL_STATUS)]

WIDA_CO_Data_LONG_2020[IEP_STATUS == "MISSING", IEP_STATUS := NA]
WIDA_CO_Data_LONG_2020[, IEP_STATUS := as.factor(IEP_STATUS)]

WIDA_CO_Data_LONG_2020[GIFTED_TALENTED_PROGRAM_STATUS == "MISSING", GIFTED_TALENTED_PROGRAM_STATUS := NA]
WIDA_CO_Data_LONG_2020[, GIFTED_TALENTED_PROGRAM_STATUS := as.factor(GIFTED_TALENTED_PROGRAM_STATUS)]


WIDA_CO_Data_LONG_2020[, SCHOOL_NAME := as.factor(SCHOOL_NAME)]
levels(WIDA_CO_Data_LONG_2020$SCHOOL_NAME) <- sapply(levels(WIDA_CO_Data_LONG_2020$SCHOOL_NAME), capwords)

WIDA_CO_Data_LONG_2020[, SCHOOL_ENROLLMENT_STATUS := as.factor(SCHOOL_ENROLLMENT_STATUS)]
WIDA_CO_Data_LONG_2020[, DISTRICT_ENROLLMENT_STATUS := as.factor(DISTRICT_ENROLLMENT_STATUS)]


### Setkey and invalidate duplicate cases (0 new invalidated in 2020 - 1772 already done by Marie)

table(WIDA_CO_Data_LONG_2020[, VALID_CASE])
setkey(WIDA_CO_Data_LONG_2020, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG_2020, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG_2020[which(duplicated(WIDA_CO_Data_LONG_2020, by=key(WIDA_CO_Data_LONG_2020)))-1, VALID_CASE:="INVALID_CASE"]


### Invalidate cases with missing SCALE_SCOREs and IDs (0 new invalidated in 2020)

WIDA_CO_Data_LONG_2020[is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
WIDA_CO_Data_LONG_2020[is.na(ID), VALID_CASE := "INVALID_CASE"]


### Save results

save(WIDA_CO_Data_LONG_2020, file="Data/WIDA_CO_Data_LONG_2020.Rdata")
