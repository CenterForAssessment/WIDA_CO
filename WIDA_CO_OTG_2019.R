################################################################################
###                                                                          ###
###       Script for calculating 2018-2019 WIDA-ACCESS On-Track-Growth       ###
###                                                                          ###
################################################################################

require(data.table)

load("Data/WIDA_CO_SGP.Rdata")
OTG <- fread("/Users/avi/Dropbox (SGP)/SGP/ACCESS/Data/Base_Files/NCIEA (Marie Huchton)/WIDA_READIN_2017_thru_2019_w_AYFEP.txt", colClasses=rep("character", 31))

###   Merge in EDW_KEY/FELA_KEY for 2018
OTG_Merge <- OTG[YEAR=='2018', list(VALID_CASE, ID, YEAR, EDW_KEY)]

setkey(OTG_Merge, VALID_CASE, ID, YEAR)
setkey(WIDA_CO_SGP@Data, VALID_CASE, ID, YEAR)
tmp_data <- merge(WIDA_CO_SGP@Data, OTG_Merge, all.x = TRUE)

tmp_data[is.na(FELA_KEY), FELA_KEY := EDW_KEY]
tmp_data[, EDW_KEY := NULL]

setnames(OTG, c("EDW_KEY"), c("FELA_KEY"))

OTG_Merge <- OTG[, list(ID, FELA_KEY, YEAR, AYFEP, HIGH_PROF_LEVEL)]
setkey(OTG_Merge, FELA_KEY, ID, YEAR)
setkey(tmp_data, FELA_KEY, ID, YEAR)
tmp_data <- merge(tmp_data, OTG_Merge, all.x = TRUE)

proj_2019 <- copy(WIDA_CO_SGP@SGP$SGProjections$READING.2019.LAGGED)[, YEAR := "2019"]
proj_2019[, grep("LEVEL_5|ACHIEVEMENT_LEVEL_PRIOR|SGP_PROJECTION_GROUP", names(proj_2019), value=TRUE) := NULL]
proj_2019[, VALID_CASE := "VALID_CASE"]  #  Added after 2020 analyses
setkey(proj_2019, VALID_CASE, ID, GRADE, YEAR)
setkey(tmp_data, VALID_CASE, ID, GRADE, YEAR)
tmp_data <- merge(tmp_data, proj_2019, all.x = TRUE)

###   Turn ACHIEVEMENT_LEVEL into numeric
tmp_data[, ACH_LEV := ACHIEVEMENT_LEVEL]
tmp_data[, ACHIEVEMENT_LEVEL := as.numeric(factor(ACHIEVEMENT_LEVEL))]
tmp_data[ACHIEVEMENT_LEVEL==7, ACHIEVEMENT_LEVEL := NA]
table(tmp_data[, ACHIEVEMENT_LEVEL, ACH_LEV], exclude=NULL)


tmp_data[, YEAR := as.numeric(YEAR)]
setkey(tmp_data, ID, YEAR, VALID_CASE)
tmp_data[, AYFEP_PRIOR := tmp_data[SJ(ID, YEAR-1, "VALID_CASE"), mult="last"][, AYFEP]]
tmp_data[, HIGH_PROF_LEVEL_PRIOR := tmp_data[SJ(ID, YEAR-1, "VALID_CASE"), mult="last"][, HIGH_PROF_LEVEL]]
tmp_data[, YEAR := as.character(YEAR)]

###   Check calculations with original data
OTG2 <- fread("/Users/avi/Dropbox (SGP)/SGP/ACCESS/Data/Base_Files/NCIEA (Marie Huchton)/CO_WIDA_ACCESS_AYFEP_INFO_2019.csv", colClasses=rep("character", 13))
setnames(OTG2, c("RECORD_KEY", "SASID", "AYFEP_PRIOR", "HIGH_PROF_LEVEL"), c("FELA_KEY", "ID", "AYFEP_PRIOR_2", "HIGH_PROF_LEVEL_2"))

OTG_Merge <- OTG2[, list(ID, FELA_KEY, YEAR, AYFEP_PRIOR_2, HIGH_PROF_LEVEL_2)]

setkey(OTG_Merge, FELA_KEY, ID, YEAR)
setkey(tmp_data, FELA_KEY, ID, YEAR)
tmp_data <- merge(tmp_data, OTG_Merge, all.x = TRUE)

table(tmp_data[YEAR=='2019', HIGH_PROF_LEVEL_2, HIGH_PROF_LEVEL], exclude=NULL) # checks out

###   Create HIGH_PROF_LEVEL_PRIOR variable
HPL <- tmp_data[YEAR != "2019", max(ACHIEVEMENT_LEVEL), keyby = c("VALID_CASE", "ID")]
HPL[, YEAR := "2019"]
setnames(HPL, "V1", "HIGH_PROF_LEVEL_PRIOR_2")
setkey(HPL, VALID_CASE, ID, YEAR)
setkey(tmp_data, VALID_CASE, ID, YEAR)

tmp_data <- merge(tmp_data, HPL, all.x = TRUE)

table(tmp_data[YEAR=='2019', AYFEP_PRIOR_2, AYFEP_PRIOR], exclude=NULL) # more cases produced for *_2 version
table(tmp_data[YEAR=='2019', HIGH_PROF_LEVEL_PRIOR_2, HIGH_PROF_LEVEL_PRIOR], exclude=NULL) # more cases produced for *_2 version

table(tmp_data[YEAR=='2019' & !is.na(SGP), AYFEP_PRIOR_2, AYFEP_PRIOR], exclude=NULL) # extra cases linked to non-SGP records
table(tmp_data[YEAR=='2019' & !is.na(SGP), HIGH_PROF_LEVEL_PRIOR_2, HIGH_PROF_LEVEL_PRIOR], exclude=NULL) # extra cases linked to non-SGP records

tmp_data[, AYFEP_PRIOR_2 := NULL]
tmp_data[, HIGH_PROF_LEVEL_2 := NULL]

#####
###   **** Catch-Up 1-2-3 Paradigm- countdown of 6 year timeframe to FEP- requires previous year AYFEP ****
#####

###   L1TOL2
tmp_data[, L1TOL2 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==6 & ACHIEVEMENT_LEVEL>=2, L1TOL2 := 1L]
tmp_data[AYFEP_PRIOR==6 & ACHIEVEMENT_LEVEL<2, L1TOL2 := 0L]

###   L2TOL3
tmp_data[, L2TOL3 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL>=3, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & SGP<LEVEL_2_SGP_TARGET_YEAR_2, L2TOL3 := 0L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & SGP>=LEVEL_2_SGP_TARGET_YEAR_2, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & is.na(L2TOL3), L2TOL3 := 0L]

tmp_data[AYFEP_PRIOR==4 & ACHIEVEMENT_LEVEL>=3, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==4 & ACHIEVEMENT_LEVEL<3, L2TOL3 := 0L]

###   L3TOL4
tmp_data[, L3TOL4 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==3 & ACHIEVEMENT_LEVEL>=4, L3TOL4 := 1L]

tmp_data[, TMP_TARG := pmin(LEVEL_3_SGP_TARGET_YEAR_2, LEVEL_3_SGP_TARGET_YEAR_3, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==3 & ACHIEVEMENT_LEVEL<4 & SGP < TMP_TARG, L3TOL4 := 0L]
tmp_data[AYFEP_PRIOR==3 & ACHIEVEMENT_LEVEL<4 & SGP >= TMP_TARG, L3TOL4 := 1L]
tmp_data[, TMP_TARG := NULL]
tmp_data[AYFEP_PRIOR==3 & ACHIEVEMENT_LEVEL<4 & is.na(L3TOL4), L3TOL4 := 0L]

tmp_data[AYFEP_PRIOR==2 & ACHIEVEMENT_LEVEL>=4, L3TOL4 := 1L]
tmp_data[AYFEP_PRIOR==2 & ACHIEVEMENT_LEVEL<4 & SGP<LEVEL_3_SGP_TARGET_YEAR_2, L3TOL4 := 0L]
tmp_data[AYFEP_PRIOR==2 & ACHIEVEMENT_LEVEL<4 & SGP>=LEVEL_3_SGP_TARGET_YEAR_2, L3TOL4 := 1L]
tmp_data[AYFEP_PRIOR==2 & ACHIEVEMENT_LEVEL<4 & is.na(L3TOL4), L3TOL4 := 0L]

tmp_data[AYFEP_PRIOR==1 & ACHIEVEMENT_LEVEL>=4, L3TOL4 := 1L]
tmp_data[AYFEP_PRIOR==1 & ACHIEVEMENT_LEVEL<4, L3TOL4 := 0L]


#####
###   **** Keep-Up 1year Paradigm ****
#####

###   L4TOL4
tmp_data[, L4TOL4 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL>=4, L4TOL4 := 1L]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL<4, L4TOL4 := 0L]

###   L5TOL5
tmp_data[, L5TOL5 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL>=5, L5TOL5 := 1L]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL<5, L5TOL5 := 0L]


#####
###   **** Overall On Track Flag ****
#####

tmp_data[, OnTrack_AnyPathway := as.integer(NA)]
tmp_data[L1TOL2==1 | L2TOL3==1 | L3TOL4==1 | L4TOL4==1 | L5TOL5==1, OnTrack_AnyPathway := 1L]
tmp_data[L1TOL2==0 | L2TOL3==0 | L3TOL4==0 | L4TOL4==0 | L5TOL5==0, OnTrack_AnyPathway := 0L]


#####
###   ***** AGP Calc *****
#####

tmp_data[, AGP := as.numeric(NA)]

tmp_data[AYFEP_PRIOR==6, AGP := LEVEL_1_SGP_TARGET_YEAR_1]

tmp_data[, TMP_TARG := pmin(LEVEL_2_SGP_TARGET_YEAR_1, LEVEL_2_SGP_TARGET_YEAR_2, na.rm=TRUE), by=c("ID", "YEAR")]

tmp_data[AYFEP_PRIOR==5 & LEVEL_2_SGP_TARGET_YEAR_1==TMP_TARG, AGP := LEVEL_2_SGP_TARGET_YEAR_1]
tmp_data[AYFEP_PRIOR==5 & LEVEL_2_SGP_TARGET_YEAR_2==TMP_TARG, AGP := LEVEL_2_SGP_TARGET_YEAR_2]
tmp_data[AYFEP_PRIOR==4, AGP := LEVEL_2_SGP_TARGET_YEAR_1]

tmp_data[, TMP_TARG := pmin(LEVEL_3_SGP_TARGET_YEAR_1, LEVEL_3_SGP_TARGET_YEAR_2, LEVEL_3_SGP_TARGET_YEAR_3, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==3 & LEVEL_3_SGP_TARGET_YEAR_1==TMP_TARG, AGP := LEVEL_3_SGP_TARGET_YEAR_1]
tmp_data[AYFEP_PRIOR==3 & LEVEL_3_SGP_TARGET_YEAR_2==TMP_TARG, AGP := LEVEL_3_SGP_TARGET_YEAR_2]
tmp_data[AYFEP_PRIOR==3 & LEVEL_3_SGP_TARGET_YEAR_3==TMP_TARG, AGP := LEVEL_3_SGP_TARGET_YEAR_3]

tmp_data[, TMP_TARG := pmin(LEVEL_3_SGP_TARGET_YEAR_1, LEVEL_3_SGP_TARGET_YEAR_2, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==2 & LEVEL_3_SGP_TARGET_YEAR_1==TMP_TARG, AGP := LEVEL_3_SGP_TARGET_YEAR_1]
tmp_data[AYFEP_PRIOR==2 & LEVEL_3_SGP_TARGET_YEAR_2==TMP_TARG, AGP := LEVEL_3_SGP_TARGET_YEAR_2]
tmp_data[AYFEP_PRIOR==1, AGP := LEVEL_3_SGP_TARGET_YEAR_1]

tmp_data[, TMP_TARG := NULL]

tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4, AGP := LEVEL_3_SGP_TARGET_YEAR_1]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5, AGP := LEVEL_4_SGP_TARGET_YEAR_1]

tmp_data[, as.list(summary(AGP)), keyby = "AYFEP_PRIOR"]


table(tmp_data[, AYFEP_PRIOR], exclude=NULL)

#####
###   Descriptives
#####

tmp_data_19 <- tmp_data[YEAR=="2019"]
###   Prior ACH L1
###   Need L1L2_TARGET0 for AGP for this group...
table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1", L1TOL2, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L1TOL2), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L1TOL2), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & !is.na(L1TOL2), as.list(summary(AGP)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1", L2TOL3, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
# tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & !is.na(L2TOL3), as.list(summary(AGP)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L2
table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2", L2TOL3, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L3
table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L4
table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L5
table(tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L5", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L5" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_19[ACHIEVEMENT_LEVEL_PRIOR=="L5" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Fix ACHIEVEMENT_LEVEL
tmp_data[, ACHIEVEMENT_LEVEL := ACH_LEV]
tmp_data[, ACH_LEV := NULL]

###  Remove Targets used for AGP
tmp_data[, grep("SGP_TARGET_YEAR|TOL", names(tmp_data), value=TRUE) := NULL]

WIDA_CO_SGP@Data <- tmp_data
setkeyv(WIDA_CO_SGP@Data, SGP:::getKey(WIDA_CO_SGP@Data))

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")

#####
###   Establish TARGET_LEVEL
#####

table(tmp_data[, ACHIEVEMENT_LEVEL, AYFEP])  #  Forward targets
table(tmp_data[, ACHIEVEMENT_LEVEL_PRIOR, AYFEP])  #  Lagged Targets
table(tmp_data[, AYFEP_PRIOR, AYFEP])  #

tmp.ayfep6 <- tmp_data[AYFEP==6 & GRADE == '8' & YEAR == '2019', list(ID, GRADE, AYFEP, YEAR)] # reverse ALevs for diff example
tmp.id <- tmp.ayfep6$ID[82]
tmp_data[ID==tmp.id, list(SCALE_SCORE, SCALE_SCORE_PRIOR, ACHIEVEMENT_LEVEL, ACHIEVEMENT_LEVEL_PRIOR, GRADE, AYFEP, AYFEP_PRIOR, YEAR)]

tmp.exp2 <- tmp_data[ACHIEVEMENT_LEVEL == "L3" & ACHIEVEMENT_LEVEL_PRIOR == "L1" & GRADE == '4' & YEAR == '2019', list(ID, GRADE, AYFEP, YEAR)] # reverse ALevs for diff example
tmp.id <- tmp.exp2$ID[82] # 33/77 also good for ex3
tmp_data[ID==tmp.id, list(SCALE_SCORE, SCALE_SCORE_PRIOR, ACHIEVEMENT_LEVEL, ACHIEVEMENT_LEVEL_PRIOR, GRADE, AYFEP, AYFEP_PRIOR, YEAR)]

tmp.names <- names(WIDA_CO_SGP@SGP$SGProjections$READING.2019.TARGET_SCALE_SCORES)
WIDA_CO_SGP@SGP$SGProjections$READING.2019.TARGET_SCALE_SCORES[ID==tmp.id]
WIDA_CO_SGP@SGP$SGProjections$READING.2019.TARGET_SCALE_SCORES[ID==tmp.id, grep("TARGET_4", tmp.names, value=TRUE), with=FALSE]
