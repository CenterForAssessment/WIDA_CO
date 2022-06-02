################################################################################
###                                                                          ###
###    Calculate 2022 Student Growth Percentiles for Colorado WIDA/ACCESS    ###
###                                                                          ###
################################################################################

###   Load required packages
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_CO_SGP.Rdata")
load("Data/WIDA_CO_Data_LONG_2022.Rdata")

###   Add baseline matrices
SGPstateData <- SGPmatrices::addBaselineMatrices("WIDA_CO", "2021")

###   Run updateSGP to produce cohort referrenced SGPs, etc.
WIDA_CO_SGP <- updateSGP(
    what_sgp_object = WIDA_CO_SGP,
    with_sgp_data_LONG = WIDA_CO_Data_LONG_2022,
    steps = c("prepareSGP", "analyzeSGP", "combineSGP",
              "summarizeSGP", "outputSGP"),
    sgp.percentiles = TRUE,
    sgp.projections = TRUE,
    sgp.projections.lagged = TRUE,
    sgp.percentiles.baseline = TRUE,
    sgp.projections.baseline = TRUE,
    sgp.projections.lagged.baseline = TRUE,
    sgp.percentiles.equated = FALSE,
    sgp.target.scale.scores = FALSE,  #  Run below
    save.intermediate.results = FALSE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(
            PERCENTILES = 8,
            PROJECTIONS = 4,
            LAGGED_PROJECTIONS = 2,
            SUMMARY = 4)))


###  Run combineSGP for Interim Target Levels (for both 2021 and 2022)
`%w/o%` <- function(x, y) x[!x %in% y]

##    Preserve previously calculated "interim" variables
setnames(WIDA_CO_SGP@Data, "CATCH_UP_KEEP_UP_INTERIM_STATUS", "CUPKUP_IS")
interim.targets <- c("SGP_TARGET_INTERIM", "SGP_TARGET_INTERIM_CURRENT")


###   Clean Up pre-combined data
simple.targets <- c("SGP_TARGET_3_YEAR_CURRENT", "SGP_TARGET_3_YEAR", 
  "SGP_TARGET_BASELINE_3_YEAR_CURRENT", "SGP_TARGET_BASELINE_3_YEAR")

WIDA_CO_SGP@Data[, (simple.targets) := NULL]

###   Remove CUKU status vars tied to L3/L4 except original (3 Year)
setnames(WIDA_CO_SGP@Data,
         c("CATCH_UP_KEEP_UP_STATUS_3_YEAR", "CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR"),
         c("CUKU_ORIG", "CUKU_BASELINE_ORIG"))


##    L1/L2
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient",
      "Proficient", "Proficient", "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <- combineSGP(
    WIDA_CO_SGP,
    years = c("2021", "2022"),
    sgp.target.scale.scores = TRUE,
    sgp.target.scale.scores.merge = "all_years_lagged_current",
    max.sgp.target.years.forward = 0:1,
    parallel.config = list(
        BACKEND = "FOREACH", TYPE = "doParallel",
        WORKERS = list(SGP_SCALE_SCORE_TARGETS = 4)))

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
setnames(WIDA_CO_SGP@Data, tmp.target.names,
         gsub("SGP_TARGET", "SGP_LEVEL_1_TARGET", tmp.target.names))

setnames(WIDA_CO_SGP@Data,
         gsub("CATCH_UP_KEEP_UP", "L1L2_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep("^(?!.*2020).*SCALE_SCORES",
                        names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L1L2", 
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L2/L3
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient",
      "Proficient", "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <- combineSGP(
    WIDA_CO_SGP,
    years = c("2021", "2022"),
    sgp.target.scale.scores = TRUE,
    sgp.target.scale.scores.merge = "all_years_lagged_current",
    max.sgp.target.years.forward = 0:2,
    parallel.config = list(
        BACKEND = "FOREACH", TYPE = "doParallel",
        WORKERS = list(SGP_SCALE_SCORE_TARGETS = 4)))

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
setnames(WIDA_CO_SGP@Data, tmp.target.names,
         gsub("SGP_TARGET", "SGP_LEVEL_2_TARGET", tmp.target.names))

setnames(WIDA_CO_SGP@Data,
         gsub("CATCH_UP_KEEP_UP", "L2L3_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep("^(?!.*2020).*SCALE_SCORES",
                        names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L2L3",
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L4/L5 (Stay at above L5 - do before Official/L4)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
      "Proficient", "Proficient", NA)

WIDA_CO_SGP <- combineSGP(
    WIDA_CO_SGP,
    years = c("2021", "2022"),
    sgp.target.scale.scores = TRUE,
    sgp.target.scale.scores.merge = "all_years_lagged_current",
    max.sgp.target.years.forward = 0:1,
    parallel.config = list(
        BACKEND = "FOREACH", TYPE = "doParallel",
        WORKERS = list(SGP_SCALE_SCORE_TARGETS = 4)))

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
setnames(WIDA_CO_SGP@Data, tmp.target.names,
         gsub("SGP_TARGET", "SGP_LEVEL_4_TARGET", tmp.target.names))

setnames(WIDA_CO_SGP@Data,
         gsub("CATCH_UP_KEEP_UP", "L4L5_CUKU", names(WIDA_CO_SGP@Data)))

target.ss.index <- grep("^(?!.*2020).*SCALE_SCORES",
                        names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L4L5",
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L3/L4 (Official)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient", "Not Proficient",
      "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <- combineSGP(
    WIDA_CO_SGP,
    years = c("2021", "2022"),
    sgp.target.scale.scores = TRUE,
    sgp.target.scale.scores.merge = "all_years_lagged_current",
    max.sgp.target.years.forward = 0:3,
    parallel.config = list(
        BACKEND = "FOREACH", TYPE = "doParallel",
        WORKERS = list(SGP_SCALE_SCORE_TARGETS = 4)))

tmp.target.names <- grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
setnames(WIDA_CO_SGP@Data, tmp.target.names,
         gsub("SGP_TARGET", "SGP_LEVEL_3_TARGET", tmp.target.names))

setnames(WIDA_CO_SGP@Data,
         gsub("CATCH_UP_KEEP_UP", "L3L4_CUKU", names(WIDA_CO_SGP@Data)))

##   Rename generic "CATCH_UP_KEEP_UP_STATUS" for all students as "Official"
setnames(WIDA_CO_SGP@Data, c("CUKU_ORIG", "CUPKUP_IS"),
         c("CATCH_UP_KEEP_UP_STATUS", "CATCH_UP_KEEP_UP_INTERIM_STATUS"))


###   Save results
save(WIDA_CO_SGP, file = "Data/WIDA_CO_SGP.Rdata")
