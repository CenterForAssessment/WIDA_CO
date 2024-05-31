###############################################################################
###                                                                         ###
###     Create 2023 Individual Student Reports for Colorado WIDA/ACCESS     ###
###                                                                         ###
###############################################################################

###   Load required packages
require(SGP)
require(data.table)

###   Load 2023 Data
load("Data/WIDA_CO_SGP.Rdata")

###   Clean up SCHOOL_NAME and DISTRICT_NAME
##    Check levels first to confirm special.words - Clean Well for ISRs

##    Schools
grep("Ece", levels(WIDA_CO_SGP@Data$SCHOOL_NAME), value = T)

new.sch.levs <- toupper(levels(WIDA_CO_SGP@Data$SCHOOL_NAME))
new.sch.levs <- gsub("/", " / ", new.sch.levs)

sch.specials <-
  c("AIM", "APS", "AUL", "AXIS", "AXL", "CCH", "CEC", "CIVICA",
    "CMS", "COVA", "CUBE", "DC", "DCIS", "DSST", "DSST:", "ECE-8",
    "GES", "GOAL", "GVR", "IB", "KIPP", "PK", "PK-8", "PK-12",
    "PSD", "LEAP", "MHCD", "MS", "SHS", "STEM", "TCA", "VSSA")

new.sch.levs <- sapply(X = new.sch.levs, USE.NAMES = FALSE,
                       FUN = SGP::capwords, special.words = sch.specials)
new.sch.levs <- gsub(" / ", "/", new.sch.levs)
new.sch.levs <- gsub("[']S", "'s", new.sch.levs)
new.sch.levs <- gsub("Prek", "PreK", new.sch.levs)

sort(grep("Mc", new.sch.levs, value = TRUE))
new.sch.levs <- gsub("Mc Auliffe", "McAuliffe", new.sch.levs)
new.sch.levs <- gsub("Mcauliffe", "McAuliffe", new.sch.levs)
new.sch.levs <- gsub("Mc Clave", "McClave", new.sch.levs)
new.sch.levs <- gsub("Mcclave", "McClave", new.sch.levs)
new.sch.levs <- gsub("Mc Elwain", "McElwain", new.sch.levs)
new.sch.levs <- gsub("Mcelwain", "McElwain", new.sch.levs)
new.sch.levs <- gsub("Mc Ginnis", "McGinnis", new.sch.levs)
new.sch.levs <- gsub("Mcginnis", "McGinnis", new.sch.levs)
new.sch.levs <- gsub("Mc Glone", "McGlone", new.sch.levs)
new.sch.levs <- gsub("Mcglone", "McGlone", new.sch.levs)
new.sch.levs <- gsub("Mc Graw", "McGraw", new.sch.levs)
new.sch.levs <- gsub("Mcgraw", "McGraw", new.sch.levs)
new.sch.levs <- gsub("Mc Kinley", "McKinley", new.sch.levs)
new.sch.levs <- gsub("Mckinley", "McKinley", new.sch.levs)
new.sch.levs <- gsub("Mc Lain", "McLain", new.sch.levs)
new.sch.levs <- gsub("Mclain", "McLain", new.sch.levs)
new.sch.levs <- gsub("Mc Meen", "McMeen", new.sch.levs)
new.sch.levs <- gsub("Mcmeen", "McMeen", new.sch.levs)
sort(grep("Mc", new.sch.levs, value = TRUE))

new.sch.levs <- gsub("Ace Community", "ACE Community", new.sch.levs)
new.sch.levs <- gsub("Achieve Online", "ACHIEVE Online", new.sch.levs)
new.sch.levs <- gsub("Allies", "ALLIES", new.sch.levs)
new.sch.levs <- gsub("Apex Home", "APEX Home", new.sch.levs)
new.sch.levs <- gsub("Canon", "Ca\u{F1}on", new.sch.levs)
new.sch.levs <- gsub("Hope Online", "HOPE Online", new.sch.levs)
new.sch.levs <- gsub("Reach Charter", "REACH Charter", new.sch.levs)
new.sch.levs <- gsub("Soar A", "SOAR A", new.sch.levs)
new.sch.levs <- gsub("Strive Prep", "STRIVE Prep", new.sch.levs)
new.sch.levs <- gsub("Edcsd", "eDCSD", new.sch.levs)

##  "Error" in 2 rows from 2019
# "Error" -- grep("Error", new.sch.levs, value = TRUE)
# WIDA_CO_SGP@Data[grepl("Error", SCHOOL_NAME), .(SCHOOL_NUMBER, DISTRICT_NUMBER, YEAR)]

grep("''", new.sch.levs, value = TRUE)
new.sch.levs <- gsub("''", "'", new.sch.levs)

# grep("[[:digit:]]", new.sch.levs, value = TRUE)
# grep("[[:digit:]]j", new.sch.levs, value = TRUE)
# new.sch.levs <- gsub("27j", "27J", new.sch.levs)
# new.sch.levs <- gsub("49jt", "49JT", new.sch.levs)

new.sch.levs <- gsub("ADAMS12", "Adams 12", new.sch.levs)


setattr(WIDA_CO_SGP@Data$SCHOOL_NAME, "levels", new.sch.levs)


##    Districts

WIDA_CO_SGP@Data[, DISTRICT_NAME := as.factor(DISTRICT_NAME)]
grep("J", levels(WIDA_CO_SGP@Data$DISTRICT_NAME), value = TRUE)
new.dst.levs <- toupper(levels(WIDA_CO_SGP@Data$DISTRICT_NAME))
new.dst.levs <- gsub("/", " / ", new.dst.levs)
new.dst.levs <- gsub("[-]", " - ", new.dst.levs)

dst.specials <-
    c("1J", "2J", "3J", "4A", "4J", "5J", "6J", "10J", "10JT",
      "11J", "13JT", "22J", "26J", "27J", "28J", "29J", "31J",
      "32J", "33J", "49JT", "50J", "50JT", "60JT", "100J",
      "JT", "RJ", "RD", "RE", "RE1J")

new.dst.levs <- sapply(new.dst.levs, SGP::capwords,
                       special.words = dst.specials, USE.NAMES = FALSE)
new.dst.levs <- gsub(" / ", "/", new.dst.levs)
new.dst.levs <- gsub(" - ", "-", new.dst.levs)
new.dst.levs <- gsub("Mc Clave", "McClave", new.dst.levs)
new.dst.levs <- gsub("Mcclave", "McClave", new.dst.levs)
grep("Mc", new.dst.levs, value = TRUE) # Should only leave * Conejos
grep("j", new.dst.levs, value = TRUE) # Should only leave * Conejos

grep("Canon", new.dst.levs, value = TRUE)
new.dst.levs <- gsub("Canon", "Ca\u{F1}on", new.dst.levs)

grep("Boces", new.dst.levs, value = TRUE)
new.dst.levs <- gsub("Boces", "BOCES", new.dst.levs)


setattr(WIDA_CO_SGP@Data$DISTRICT_NAME, "levels", new.dst.levs)


#####
###   Produce ISRs using visualizeSGP function
#####

###   Patterns in fans use `gridpattern` package
remotes::install_github("trevorld/gridpattern")

SGPstateData[["WIDA_CO_SPANISH"]][["SGP_Configuration"]][["sgPlot.sgp.targets"]] <-
  SGPstateData[["WIDA_CO"]][["SGP_Configuration"]][["sgPlot.sgp.targets"]] <- NULL

visualizeSGP(
    WIDA_CO_SGP,
    plot.types = "studentGrowthPlot",
    sgPlot.years = "2023",
    # sgPlot.demo.report = TRUE,
    parallel.config = list(
        BACKEND = "PARALLEL",
        WORKERS = list(SG_PLOTS = 18)
    )
)


#####
###   Post-Hoc checks for missing schools/districs
#####

# dist <-
#   system(
#     "ls /home/ubuntu/SGP/CO_ISRs_2023/Visualizations/studentGrowthPlots/School/2023",
#     intern = TRUE
#   )
# dat.dist <- unique(WIDA_CO_SGP@Data[YEAR == "2023" & !is.na(SGP)]$DISTRICT_NUMBER)
# miss <- setdiff(dat.dist, dist)
# m <- WIDA_CO_SGP@Data[!is.na(SGP) & DISTRICT_NUMBER %in% miss]
# table(m[, GRADE, CONTENT_AREA]) #  0
# ###   No missing districts :-)

# problem.districts <- list()
# for (d in dat.dist) {
#    data.schools <-
#      unique(WIDA_CO_SGP@Data[YEAR == "2023" & !is.na(SGP) & DISTRICT_NUMBER == d, SCHOOL_NUMBER])
#    file.schools <-
#      system(
#             paste0("ls /home/ubuntu/SGP/CO_ISRs_2023/Visualizations/studentGrowthPlots/School/2023/", d),
#             intern = TRUE)
#    file.schools <- gsub("[.]zip", "", file.schools)
#    if (!(all(file.schools %in% data.schools) || all(data.schools %in% file.schools))) {
#        missing.schools <- setdiff(data.schools, file.schools)
#        problem.districts[[as.character(d)]] <- missing.schools
#    }
# }

# problem.districts[lengths(problem.districts) != 0]
###   No Problem Schools within Districts :-)
