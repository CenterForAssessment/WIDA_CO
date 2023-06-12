###############################################################################
###                                                                         ###
###        Data prep and cleaning for Colorado WIDA/ACCESS  -  2023         ###
###                                                                         ###
###############################################################################

###   Load packages
require(data.table)

###   Load data
WIDA_CO_Data_LONG_2023 <- fread(
    "Data/Base_Files/WIDA_ACCESS_GRO_READIN_2023_FINAL_20230530.txt",
    colClasses = rep("character", 34), quote = "'")

###   Tidy up data
WIDA_CO_Data_LONG_2023[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

##    Student Names
WIDA_CO_Data_LONG_2023[, FIRST_NAME := as.factor(FIRST_NAME)]
setattr(
    WIDA_CO_Data_LONG_2023$FIRST_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2023$FIRST_NAME), SGP::capwords)
)

WIDA_CO_Data_LONG_2023[, LAST_NAME := as.factor(LAST_NAME)]
setattr(
    WIDA_CO_Data_LONG_2023$LAST_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2023$LAST_NAME), SGP::capwords)
)

##    Student Demographics
WIDA_CO_Data_LONG_2023[
    GENDER == "MISSING", GENDER := NA
][, GENDER := as.factor(GENDER)]
setattr(
    WIDA_CO_Data_LONG_2023$GENDER, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2023$GENDER), SGP::capwords)
)

WIDA_CO_Data_LONG_2023[
    ETHNICITY == "MISSING", ETHNICITY := NA
][, ETHNICITY := as.factor(ETHNICITY)]
setattr(
    WIDA_CO_Data_LONG_2023$ETHNICITY, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2023$ETHNICITY), SGP::capwords)
)

WIDA_CO_Data_LONG_2023[
    FREE_REDUCED_LUNCH_STATUS == "MISSING", FREE_REDUCED_LUNCH_STATUS := NA
][, FREE_REDUCED_LUNCH_STATUS := as.factor(FREE_REDUCED_LUNCH_STATUS)
]
WIDA_CO_Data_LONG_2023[
    ELL_STATUS == "Missing", ELL_STATUS := NA
][, ELL_STATUS := as.factor(ELL_STATUS)
]

WIDA_CO_Data_LONG_2023[IEP_STATUS == "MISSING", IEP_STATUS := NA]
WIDA_CO_Data_LONG_2023[, IEP_STATUS := as.factor(IEP_STATUS)]

WIDA_CO_Data_LONG_2023[
    GIFTED_TALENTED_PROGRAM_STATUS == "MISSING",
        GIFTED_TALENTED_PROGRAM_STATUS := NA
][, GIFTED_TALENTED_PROGRAM_STATUS :=
        as.factor(GIFTED_TALENTED_PROGRAM_STATUS)]

##    School and District Info
WIDA_CO_Data_LONG_2023[, SCHOOL_NAME := as.factor(SCHOOL_NAME)]
setattr(
    WIDA_CO_Data_LONG_2023$SCHOOL_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2023$SCHOOL_NAME), SGP::capwords)
)

WIDA_CO_Data_LONG_2023[,
    SCHOOL_ENROLLMENT_STATUS := as.factor(SCHOOL_ENROLLMENT_STATUS)
][, DISTRICT_ENROLLMENT_STATUS := as.factor(DISTRICT_ENROLLMENT_STATUS)
]


###   Setkey and invalidate duplicate cases
table(WIDA_CO_Data_LONG_2023[, VALID_CASE])
setkey(WIDA_CO_Data_LONG_2023,
       VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG_2023, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG_2023[
    which(duplicated(WIDA_CO_Data_LONG_2023, by = key(WIDA_CO_Data_LONG_2023)))-1,
    VALID_CASE := "INVALID_CASE"]


###   Invalidate cases with missing SCALE_SCOREs and IDs
WIDA_CO_Data_LONG_2023[is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
WIDA_CO_Data_LONG_2023[is.na(ID), VALID_CASE := "INVALID_CASE"]


###   Save results
save(WIDA_CO_Data_LONG_2023, file = "Data/WIDA_CO_Data_LONG_2023.Rdata")
