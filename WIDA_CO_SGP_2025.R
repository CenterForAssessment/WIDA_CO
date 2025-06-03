#+ include = FALSE, purl = FALSE, eval = FALSE
###############################################################################
###                                                                         ###
###   Calculate 2025 Student Growth Percentiles for Colorado WIDA/ACCESS    ###
##                                                                          ###
###############################################################################

###   Load required packages
require(SGP)
require(data.table)

###   Load Data
load("Data/WIDA_CO_SGP.Rdata")
load("Data/WIDA_CO_Data_LONG_2025.Rdata")

##    Preserve basic CU/KU variables from previous analyses
setnames(WIDA_CO_SGP@Data,
         c("CATCH_UP_KEEP_UP_STATUS_3_YEAR",
        #    "CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR",
           "CATCH_UP_KEEP_UP_INTERIM_STATUS"),
         c("CUKU_ORIG", "CUPKUP_IS") # , "CUKU_BASELINE_ORIG"
)

###   Run updateSGP to produce cohort referrenced SGPs, etc.
WIDA_CO_SGP <-
    updateSGP(
        what_sgp_object = WIDA_CO_SGP,
        with_sgp_data_LONG = WIDA_CO_Data_LONG_2025,
        steps = c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP"),
        sgp.percentiles = TRUE,
        sgp.projections = TRUE,
        sgp.projections.lagged = TRUE,
        sgp.percentiles.baseline = FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.percentiles.equated = FALSE,
        sgp.target.scale.scores = FALSE,  #  Run below
        save.intermediate.results = FALSE,
        parallel.config = list(
            BACKEND = "PARALLEL",
            WORKERS = list(
                PERCENTILES = 10,
                PROJECTIONS = 10,
                LAGGED_PROJECTIONS = 8
            )
        )
    )


###   Output/Save Results
outputSGP(WIDA_CO_SGP)


######
###    On-Track to Standard growth trajectories/targets calculation
######

###   Move CUPKUP to "ORIG" vars
##    (only current year gets merged due to @SGP$SGProjections renaming (?))
# table(WIDA_CO_SGP@Data[, is.na(CATCH_UP_KEEP_UP_STATUS_3_YEAR), YEAR], exclude = NULL)

WIDA_CO_SGP@Data[
    !is.na(CATCH_UP_KEEP_UP_STATUS_3_YEAR),
    CUKU_ORIG := CATCH_UP_KEEP_UP_STATUS_3_YEAR
][, CATCH_UP_KEEP_UP_STATUS_3_YEAR := NULL
]

# WIDA_CO_SGP@Data[
#     !is.na(CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR),
#     CUKU_BASELINE_ORIG := CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR
# ][, CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR := NULL
# ]

###  Run combineSGP for Interim Target Levels (for both 2024 and 2025)
`%w/o%` <- function(x, y) x[!x %in% y]

##    Preserve previously calculated "interim" variables
# setnames(WIDA_CO_SGP@Data, "CATCH_UP_KEEP_UP_INTERIM_STATUS", "CUPKUP_IS")
interim.targets <- c("SGP_TARGET_INTERIM", "SGP_TARGET_INTERIM_CURRENT")

###   Clean Up pre-combined data
simple.targets <- c("SGP_TARGET_3_YEAR_CURRENT", "SGP_TARGET_3_YEAR")#, 
#   "SGP_TARGET_BASELINE_3_YEAR_CURRENT", "SGP_TARGET_BASELINE_3_YEAR")

WIDA_CO_SGP@Data[, (simple.targets) := NULL]

SGPstateData[["WIDA_CO"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"
SGPstateData[["WIDA_CO"]][["SGP_Configuration"]][['current.year.lagged.target']] <- FALSE


##    L1/L2
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient",
      "Proficient", "Proficient", "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <-
    combineSGP(
        WIDA_CO_SGP,
        years = "2025",
        sgp.percentiles = FALSE,
        sgp.percentiles.baseline = FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.target.scale.scores = TRUE,
        sgp.target.scale.scores.merge = "all_years_lagged_current",
        max.sgp.target.years.forward = 1,
        parallel.config = list(
            BACKEND = "FOREACH",
            TYPE = "doParallel",
            WORKERS = list(SGP_SCALE_SCORE_TARGETS = 8)
        )
    )

tmp.target.names <-
    grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
for (tgt in tmp.target.names) {
    new.tgt <- gsub("SGP_TARGET", "SGP_LEVEL_1_TARGET", tgt)
    WIDA_CO_SGP@Data[
        !is.na(get(tgt)),
        (new.tgt) := get(tgt)
    ][, (tgt) := NULL
    ]
}
for (cuku in grep("CATCH_UP_KEEP_UP", names(WIDA_CO_SGP@Data), value = TRUE)) {
    new.cuku <- gsub("CATCH_UP_KEEP_UP", "L1L2_CUKU", cuku)
    WIDA_CO_SGP@Data[
        !is.na(get(cuku)),
        (new.cuku) := get(cuku)
    ][, (cuku) := NULL
    ]
}

target.ss.index <-
    grep("2025.*SCALE_SCORES",
         names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L1L2", 
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L2/L3
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient",
      "Proficient", "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <-
    combineSGP(
        WIDA_CO_SGP,
        years = "2025",
        sgp.percentiles.baseline=FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.target.scale.scores = TRUE,
        sgp.target.scale.scores.merge = "all_years_lagged_current",
        max.sgp.target.years.forward = 1:2,
        parallel.config = list(
            BACKEND = "FOREACH",
            TYPE = "doParallel",
            WORKERS = list(SGP_SCALE_SCORE_TARGETS = 8)
        )
    )

tmp.target.names <-
    grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
for (tgt in tmp.target.names) {
    new.tgt <- gsub("SGP_TARGET", "SGP_LEVEL_2_TARGET", tgt)
    WIDA_CO_SGP@Data[
        !is.na(get(tgt)),
        (new.tgt) := get(tgt)
    ][, (tgt) := NULL
    ]
}
for (cuku in grep("CATCH_UP_KEEP_UP", names(WIDA_CO_SGP@Data), value = TRUE)) {
    new.cuku <- gsub("CATCH_UP_KEEP_UP", "L2L3_CUKU", cuku)
    WIDA_CO_SGP@Data[
        !is.na(get(cuku)),
        (new.cuku) := get(cuku)
    ][, (cuku) := NULL
    ]
}

target.ss.index <-
    grep("2025.*SCALE_SCORES",
         names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L2L3",
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L4/L5 (Stay at above L5 - do before Official/L4)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
      "Proficient", "Proficient", NA)

WIDA_CO_SGP <-
    combineSGP(
        WIDA_CO_SGP,
        years = "2025",
        sgp.percentiles.baseline=FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.target.scale.scores = TRUE,
        sgp.target.scale.scores.merge = "all_years_lagged_current",
        max.sgp.target.years.forward = 1,
        parallel.config = list(
            BACKEND = "FOREACH",
            TYPE = "doParallel",
            WORKERS = list(SGP_SCALE_SCORE_TARGETS = 8)
        )
    )

tmp.target.names <-
    grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
for (tgt in tmp.target.names) {
    new.tgt <- gsub("SGP_TARGET", "SGP_LEVEL_4_TARGET", tgt)
    WIDA_CO_SGP@Data[
        !is.na(get(tgt)),
        (new.tgt) := get(tgt)
    ][, (tgt) := NULL
    ]
}
for (cuku in grep("CATCH_UP_KEEP_UP", names(WIDA_CO_SGP@Data), value = TRUE)) {
    new.cuku <- gsub("CATCH_UP_KEEP_UP", "L4L5_CUKU", cuku)
    WIDA_CO_SGP@Data[
        !is.na(get(cuku)),
        (new.cuku) := get(cuku)
    ][, (cuku) := NULL
    ]
}

target.ss.index <-
    grep("2025.*SCALE_SCORES",
         names(WIDA_CO_SGP@SGP[["SGProjections"]]), perl = TRUE)
names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index] <-
    gsub("SCALE_SCORES", "SS_L4L5",
         names(WIDA_CO_SGP@SGP[["SGProjections"]])[target.ss.index])


##    L3/L4 (Official)
SGPstateData[["WIDA_CO"]][["Achievement"]][["Levels"]][["Proficient"]] <-
    c("Not Proficient", "Not Proficient", "Not Proficient",
      "Proficient", "Proficient", "Proficient", NA)

WIDA_CO_SGP <-
    combineSGP(
        WIDA_CO_SGP,
        years = "2025",
        sgp.percentiles.baseline=FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        sgp.target.scale.scores = TRUE,
        sgp.target.scale.scores.merge = "all_years_lagged_current",
        max.sgp.target.years.forward = 1:3,
        parallel.config = list(
            BACKEND = "FOREACH",
            TYPE = "doParallel",
            WORKERS = list(SGP_SCALE_SCORE_TARGETS = 8)
        )
    )

tmp.target.names <-
    grep("SGP_TARGET", names(WIDA_CO_SGP@Data), value = TRUE) %w/o% interim.targets
for (tgt in tmp.target.names) {
    new.tgt <- gsub("SGP_TARGET", "SGP_LEVEL_3_TARGET", tgt)
    WIDA_CO_SGP@Data[
        !is.na(get(tgt)),
        (new.tgt) := get(tgt)
    ][, (tgt) := NULL
    ]
}
for (cuku in grep("CATCH_UP_KEEP_UP", names(WIDA_CO_SGP@Data), value = TRUE)) {
    new.cuku <- gsub("CATCH_UP_KEEP_UP", "L3L4_CUKU", cuku)
    WIDA_CO_SGP@Data[
        !is.na(get(cuku)),
        (new.cuku) := get(cuku)
    ][, (cuku) := NULL
    ]
}

##   Rename generic "CATCH_UP_KEEP_UP_STATUS" for all students as "Official"
##   Also rename Interim Status var(s)
setnames(WIDA_CO_SGP@Data,
         c("CUKU_ORIG", "CUPKUP_IS"), # , "CUKU_BASELINE_ORIG"
         c("CATCH_UP_KEEP_UP_STATUS_3_YEAR",
        #    "CATCH_UP_KEEP_UP_STATUS_BASELINE_3_YEAR",
           "CATCH_UP_KEEP_UP_INTERIM_STATUS")
)

#' ### Conduct SGP analyses
#'
#' Colorado uses cohort-referenced SGPs as the official student-level
#' English language proficiency growth metric. All SGPs were calculated
#' concurrently using the [`R` Software Environment](http://www.r-project.org)
#' in conjunction with the [`SGP` package](http://sgp.io). Broadly, the
#' Colorado ACCESS for ELLs analyses were completed in five steps.
#'
#' 1. `prepareSGP`
#' 2. `analyzeSGP`
#' 3. `combineSGP`
#' 4. `outputSGP`
#' 5. `visualizeSGP`
#' 
#' Because these steps are almost always conducted simultaneously, the `SGP`
#' package has "wrapper" functions, `abcSGP` and `updateSGP`, that combine
#' the above steps into a single function call and simplify the source code
#' associated with the data analysis. Documentation for all SGP functions are
#' [available online.](https://cran.r-project.org/web/packages/SGP/SGP.pdf)
#' 
#' We use the [`updateSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/updateSGP)
#' function to ***a)*** do the final preparation and addition of the cleaned and
#' formatted new annual data,
#' ([`prepareSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/prepareSGP)
#' step), ***b)*** calculate SGP estimates
#' ([`analyzeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/analyzeSGP)
#' step), ***c)*** merge the results into the master longitudinal data set
#' ([`combineSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/combineSGP)
#' step) and ***d)*** output a pipe delimited version of the complete long data
#' ([`outputSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/outputSGP)
#' step).
#' 
#' #### Visualize results
#' 
#' Once all analyses were completed via `updateSGP`, individual student growth
#' and English language proficiency reports were produced using the
#' [`visualizeSGP`](https://www.rdocumentation.org/packages/SGP/versions/2.0-1.0/topics/visualizeSGP)
#' function and a custom template designed for Colorado. English and Spanish
#' language versions of these reports were created, and individual reports and
#' school level catalogs were bundled according to CDE specifications.
#'
#' #### Custom *On-Track to Standard* growth
#' 
#' The 2025 Colorado ACCESS for ELLs SGP results data were used in subsequent
#' growth to standard and adequate growth analyses. As part of these analyses,
#' The Center for Assessment calculated additional growth trajectory and target
#' metrics that CDE requires to monitor "On-Track to Standard" status given each
#' student's prior proficiency level and the number of years they have to attain
#' or maintain a particular level of proficiency. Further information on these
#' individualized requirements can be found on
#' the [CDE website](https://www.cde.state.co.us/accountability/access-on-track-growth).


#+ include = FALSE, purl = FALSE, eval = FALSE
###   Add R session Info & Save results (`cfaDocs` version 0.0-1.12 or later)
source(
    system.file(
        "rmarkdown", "shared_resources", "rmd", "R_Session_Info.R",
        package = "cfaDocs"
    )
)
WIDA_CO_SGP@Version$session_platform <- list("2025" = session_platform)
WIDA_CO_SGP@Version$attached_pkgs    <- list("2025" = attached_pkgs)
WIDA_CO_SGP@Version$namespace_pkgs   <- list("2025" = namespace_pkgs)

save(WIDA_CO_SGP, file = "Data/WIDA_CO_SGP.Rdata")
