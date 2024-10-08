#+ include = FALSE, purl = FALSE
###############################################################################
###                                                                         ###
###        Data prep and cleaning for Colorado WIDA/ACCESS  -  2024         ###
###                                                                         ###
###############################################################################

#' ## Data Preparation
#'
#' The data preparation step involves taking data provided by the CDE and
#' producing a `.Rdata` file that will subsequently be analyzed using the `SGP`
#' software. This process is carried out annually as new data becomes available
#' from the Colorado ACCESS for ELLs assessment program.
#'
#' The data received from CDE is pre-processed and requires minimal formatting
#' prior to running SGP analyses. For the 2024 ACCESS data preparation and
#' cleaning, we ensure that all data fields have been read in as the correct
#' type (e.g., scale scores are `numeric` values) and format demographic and
#' student information to match values used in the historical data set. All
#' variable names were confirmed to conform to the `SGP` required package
#' conventions.
#'
#' Invalid records were identified based on the following criteria:
#'
#' * Students with duplicate records. In these instances, a student's highest
#'   scale score is retained as the "valid" case in the analyses.
#' * Cases with missing student identifiers.
#' * Cases with missing scale scores.


#+ include = FALSE, purl = FALSE, eval = FALSE
###   Load packages
require(data.table)

###   Load data
WIDA_CO_Data_LONG_2024 <- fread(
    "Data/Base_Files/WIDA_ACCESS_GRO_READIN_2024_20240528.csv",
    colClasses = rep("character", 34), quote = "'")

###   Tidy up data
WIDA_CO_Data_LONG_2024[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

##    Student Names
WIDA_CO_Data_LONG_2024[, FIRST_NAME := as.factor(FIRST_NAME)]
setattr(
    WIDA_CO_Data_LONG_2024$FIRST_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2024$FIRST_NAME), SGP::capwords)
)

WIDA_CO_Data_LONG_2024[, LAST_NAME := as.factor(LAST_NAME)]
setattr(
    WIDA_CO_Data_LONG_2024$LAST_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2024$LAST_NAME), SGP::capwords)
)

##    Student Demographics
WIDA_CO_Data_LONG_2024[
    GENDER == "MISSING", GENDER := NA
][, GENDER := as.factor(GENDER)]
setattr(
    WIDA_CO_Data_LONG_2024$GENDER, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2024$GENDER), SGP::capwords)
)

WIDA_CO_Data_LONG_2024[
    ETHNICITY == "MISSING", ETHNICITY := NA
][, ETHNICITY := as.factor(ETHNICITY)]
setattr(
    WIDA_CO_Data_LONG_2024$ETHNICITY, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2024$ETHNICITY), SGP::capwords)
)

WIDA_CO_Data_LONG_2024[
    FREE_REDUCED_LUNCH_STATUS == "MISSING", FREE_REDUCED_LUNCH_STATUS := NA
][, FREE_REDUCED_LUNCH_STATUS := as.factor(FREE_REDUCED_LUNCH_STATUS)
][
    ELL_STATUS == "Missing", ELL_STATUS := NA
][, ELL_STATUS := as.factor(ELL_STATUS)
][
    IEP_STATUS == "MISSING", IEP_STATUS := NA
][, IEP_STATUS := as.factor(IEP_STATUS)
][
    GIFTED_TALENTED_PROGRAM_STATUS == "MISSING",
        GIFTED_TALENTED_PROGRAM_STATUS := NA
][, GIFTED_TALENTED_PROGRAM_STATUS :=
        as.factor(GIFTED_TALENTED_PROGRAM_STATUS)
]

##    School and District Info
WIDA_CO_Data_LONG_2024[, SCHOOL_NAME := as.factor(SCHOOL_NAME)]
setattr(
    WIDA_CO_Data_LONG_2024$SCHOOL_NAME, "levels",
    sapply(levels(WIDA_CO_Data_LONG_2024$SCHOOL_NAME), SGP::capwords)
)

WIDA_CO_Data_LONG_2024[,
    SCHOOL_ENROLLMENT_STATUS := as.factor(SCHOOL_ENROLLMENT_STATUS)
][, DISTRICT_ENROLLMENT_STATUS := as.factor(DISTRICT_ENROLLMENT_STATUS)
]


###   Setkey and invalidate duplicate cases
table(WIDA_CO_Data_LONG_2024[, VALID_CASE])
setkey(WIDA_CO_Data_LONG_2024,
       VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG_2024,
       VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG_2024[
    which(duplicated(WIDA_CO_Data_LONG_2024, by = key(WIDA_CO_Data_LONG_2024)))-1,
    VALID_CASE := "INVALID_CASE"]


###   Invalidate cases with missing SCALE_SCOREs and IDs
WIDA_CO_Data_LONG_2024[
  is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"
][is.na(ID), VALID_CASE := "INVALID_CASE"
]

###   Save results
save(WIDA_CO_Data_LONG_2024, file = "Data/WIDA_CO_Data_LONG_2024.Rdata")
