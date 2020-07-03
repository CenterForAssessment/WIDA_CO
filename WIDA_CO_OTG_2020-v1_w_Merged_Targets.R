################################################################################
###                                                                          ###
###       Script for calculating 2019-2020 WIDA-ACCESS On-Track-Growth       ###
###                                                                          ###
################################################################################

require(data.table)

load("Data/WIDA_CO_SGP.Rdata")

###   Merge in Projections for 2019 & 2020

#   Need to re-do 2019 after intial 2020 messed up OnTrack_AnyPathway
proj_2019 <- copy(WIDA_CO_SGP@SGP$SGProjections$READING.2019.LAGGED)[, YEAR := "2019"]
proj_2019[, grep("LEVEL_5|ACHIEVEMENT_LEVEL_PRIOR|SGP_PROJECTION_GROUP", names(proj_2019), value=TRUE) := NULL]
proj_2019[, VALID_CASE := "VALID_CASE"]  #  Added after 2020 analyses
setkey(proj_2019, VALID_CASE, ID, GRADE, YEAR)

proj_2020 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.LAGGED"]])[, YEAR := "2020"]
proj_2020[, grep("LEVEL_5|ACHIEVEMENT_LEVEL_PRIOR|SGP_PROJECTION_GROUP", names(proj_2020), value=TRUE) := NULL]
proj_2020[, VALID_CASE := "VALID_CASE"]  #  Not in 2019 script (YET)
setkey(proj_2020, VALID_CASE, ID, GRADE, YEAR)

proj_1920 <- rbindlist(list(proj_2019, proj_2020))
setkey(proj_1920, VALID_CASE, ID, GRADE, YEAR)

setkey(WIDA_CO_SGP@Data, VALID_CASE, ID, GRADE, YEAR)
tmp_data <- merge(WIDA_CO_SGP@Data, proj_1920, all.x = TRUE)

###   Turn ACHIEVEMENT_LEVEL into numeric
tmp_data[, ACH_LEV := ACHIEVEMENT_LEVEL]
tmp_data[, ACHIEVEMENT_LEVEL := as.numeric(factor(ACHIEVEMENT_LEVEL))]
tmp_data[ACHIEVEMENT_LEVEL==7, ACHIEVEMENT_LEVEL := NA]
table(tmp_data[, ACHIEVEMENT_LEVEL, ACH_LEV], exclude=NULL)

###   Fill in *_PRIOR vars for 2020
tmp_data[, YEAR := as.numeric(YEAR)]
setkey(tmp_data, ID, YEAR, VALID_CASE)
tmp_data[, AYFEP_PRIOR := tmp_data[SJ(ID, YEAR-1, "VALID_CASE"), mult="last"][, AYFEP]]
tmp_data[, HIGH_PROF_LEVEL_PRIOR := tmp_data[SJ(ID, YEAR-1, "VALID_CASE"), mult="last"][, HIGH_PROF_LEVEL]]
tmp_data[, YEAR := as.character(YEAR)]


#####
###   **** Catch-Up 1-2-3 Paradigm- countdown of 6 year timeframe to FEP- requires previous year AYFEP ****
#####

###   L1TOL2
# tmp_data[, L1TOL2 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==6 & ACHIEVEMENT_LEVEL>=2, L1TOL2 := 1L]
tmp_data[AYFEP_PRIOR==6 & ACHIEVEMENT_LEVEL<2, L1TOL2 := 0L]

###   L2TOL3
# tmp_data[, L2TOL3 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL>=3, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & SGP<LEVEL_2_SGP_TARGET_YEAR_2, L2TOL3 := 0L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & SGP>=LEVEL_2_SGP_TARGET_YEAR_2, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==5 & ACHIEVEMENT_LEVEL<3 & is.na(L2TOL3), L2TOL3 := 0L]

tmp_data[AYFEP_PRIOR==4 & ACHIEVEMENT_LEVEL>=3, L2TOL3 := 1L]
tmp_data[AYFEP_PRIOR==4 & ACHIEVEMENT_LEVEL<3, L2TOL3 := 0L]

###   L3TOL4
# tmp_data[, L3TOL4 := as.integer(NA)]
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
# tmp_data[, L4TOL4 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL>=4, L4TOL4 := 1L]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL<4, L4TOL4 := 0L]

###   L5TOL5
# tmp_data[, L5TOL5 := as.integer(NA)]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL>=5, L5TOL5 := 1L]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL<5, L5TOL5 := 0L]


#####
###   **** Overall On Track Flag ****
#####

# tmp_data[, OnTrack_AnyPathway := as.integer(NA)] # Created in 2019 OTG script
tmp_data[(L1TOL2==1 | L2TOL3==1 | L3TOL4==1 | L4TOL4==1 | L5TOL5==1), OnTrack_AnyPathway := 1L]
tmp_data[(L1TOL2==0 | L2TOL3==0 | L3TOL4==0 | L4TOL4==0 | L5TOL5==0), OnTrack_AnyPathway := 0L]
tmp_data[YEAR %in% c("2017", "2018"), OnTrack_AnyPathway := NA]

tmp_data[, INTERIM_LEVEL := as.character(NA)]
tmp_data[!is.na(L1TOL2), INTERIM_LEVEL := paste("L1TOL2", L1TOL2)]
tmp_data[!is.na(L2TOL3), INTERIM_LEVEL := paste("L2TOL3", L2TOL3)]
tmp_data[!is.na(L3TOL4), INTERIM_LEVEL := paste("L3TOL4", L3TOL4)]
tmp_data[!is.na(L4TOL4), INTERIM_LEVEL := paste("L4TOL4", L4TOL4)]
tmp_data[!is.na(L5TOL5), INTERIM_LEVEL := paste("L5TOL5", L5TOL5)]
table(tmp_data[, YEAR, INTERIM_LEVEL])

#####
###   ***** AGP Calc *****
#####

# tmp_data[, AGP := as.numeric(NA)] # Created in 2019 OTG script

tmp_data[AYFEP_PRIOR==6, AGP := LEVEL_1_SGP_TARGET_YEAR_1]

tmp_data[, TMP_TARG := pmin(LEVEL_2_SGP_TARGET_YEAR_1, LEVEL_2_SGP_TARGET_YEAR_2, na.rm=TRUE), by=c("ID", "YEAR")]

# tmp_data[AYFEP_PRIOR==5, AGP := TMP_TARG]  #  Same result (?)
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


#####
###   Descriptives
#####

tmp_data_20 <- tmp_data[YEAR=="2020"]
###   Prior ACH L1
###   Need L1L2_TARGET0 for AGP for this group...
table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1", L1TOL2, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L1TOL2), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L1TOL2), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & !is.na(L1TOL2), as.list(summary(AGP)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1", L2TOL3, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
# tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & !is.na(L2TOL3), as.list(summary(AGP)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L1" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L2
table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2", L2TOL3, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L2TOL3), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L2" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L3
table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3", L3TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L3TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L3" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L4
table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4", L4TOL4, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 0 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 1 & !is.na(L4TOL4), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L4" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]

###   Prior ACH L5
table(tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L5", L5TOL5, ACHIEVEMENT_LEVEL], exclude=NULL)
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L5" & OnTrack_AnyPathway == 0 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]
tmp_data_20[ACHIEVEMENT_LEVEL_PRIOR=="L5" & OnTrack_AnyPathway == 1 & !is.na(L5TOL5), list(N=.N, MSGP = median(SGP), MAGP = median(AGP, na.rm=TRUE)), keyby="ACHIEVEMENT_LEVEL"]


save(tmp_data, file="Data/tmp_data_ss_targ_step1.rda")


#####
###   Establish TARGET_LEVEL
#####

#  tmp_data <- copy(WIDA_CO_SGP@Data)

###   Straight Target Scale Scores
trgt_ss_str_l2 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.TARGET_SS_L1L2"]])[, YEAR := "2020"]
trgt_ss_str_l3 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.TARGET_SS_L2L3"]])[, YEAR := "2020"]
trgt_ss_str_l4 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.TARGET_SCALE_SCORES"]])[, YEAR := "2020"]
trgt_ss_str_l5 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.TARGET_SS_L4L5"]])[, YEAR := "2020"]

target.key <- grep("SCALE_SCORE_SGP_TARGET", names(trgt_ss_str_l4), invert=T, value=TRUE)
setnames(trgt_ss_str_l2, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_1_TARGET", names(trgt_ss_str_l2))); setkeyv(trgt_ss_str_l2, target.key)
setnames(trgt_ss_str_l3, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_2_TARGET", names(trgt_ss_str_l3))); setkeyv(trgt_ss_str_l3, target.key)
setnames(trgt_ss_str_l4, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_3_TARGET", names(trgt_ss_str_l4))); setkeyv(trgt_ss_str_l4, target.key)
setnames(trgt_ss_str_l5, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_4_TARGET", names(trgt_ss_str_l5))); setkeyv(trgt_ss_str_l5, target.key)

trgt_ss_str <- trgt_ss_str_l2[trgt_ss_str_l3][trgt_ss_str_l4][trgt_ss_str_l5][, VALID_CASE := "VALID_CASE"]
trgt_ss_str[, grep("SGP_PROJECTION_GROUP", names(tmp_data), value=TRUE) := NULL]

###   LAGGED Target Scale Scores
trgt_ss_lag_l2 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.LAGGED.TARGET_SS_L1L2"]])[, YEAR := "2020"]
trgt_ss_lag_l3 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.LAGGED.TARGET_SS_L2L3"]])[, YEAR := "2020"]
trgt_ss_lag_l4 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.LAGGED.TARGET_SCALE_SCORES"]])[, YEAR := "2020"]
trgt_ss_lag_l5 <- copy(WIDA_CO_SGP@SGP$SGProjections[["READING.2020.LAGGED.TARGET_SS_L4L5"]])[, YEAR := "2020"]

setnames(trgt_ss_lag_l2, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_1_TARGET", names(trgt_ss_lag_l2))); setkeyv(trgt_ss_lag_l2, target.key)
setnames(trgt_ss_lag_l3, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_2_TARGET", names(trgt_ss_lag_l3))); setkeyv(trgt_ss_lag_l3, target.key)
setnames(trgt_ss_lag_l4, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_3_TARGET", names(trgt_ss_lag_l4))); setkeyv(trgt_ss_lag_l4, target.key)
setnames(trgt_ss_lag_l5, gsub("SCALE_SCORE_SGP_TARGET", "SCALE_SCORE_SGP_LEVEL_4_TARGET", names(trgt_ss_lag_l5))); setkeyv(trgt_ss_lag_l5, target.key)

trgt_ss_lag <- trgt_ss_lag_l2[trgt_ss_lag_l3][trgt_ss_lag_l4][trgt_ss_lag_l5][, VALID_CASE := "VALID_CASE"]
trgt_ss_lag[, grep("SGP_PROJECTION_GROUP", names(tmp_data), value=TRUE) := NULL]


setkey(trgt_ss_str, VALID_CASE, ID, GRADE, YEAR)
setkey(trgt_ss_lag, VALID_CASE, ID, GRADE, YEAR)
setkey(tmp_data, VALID_CASE, ID, GRADE, YEAR)
tmp_data <- merge(tmp_data, trgt_ss_str, all.x = TRUE)
tmp_data <- merge(tmp_data, trgt_ss_lag, all.x = TRUE)


#####
###   ***** Target Scale Score Calc *****
#####

grep("SCALE_SCORE_SGP_LEVEL_", names(tmp_data), value=TRUE)

###   Set up Target SS Variables
tmp_data[, TARGET_SCALE_SCORES_1_YEAR := as.numeric(NA)]
tmp_data[, TARGET_SCALE_SCORES_2_YEAR := as.numeric(NA)]
tmp_data[, TARGET_SCALE_SCORES_3_YEAR := as.numeric(NA)]
tmp_data[, TARGET_SCALE_SCORES_1_YEAR_CURRENT := as.numeric(NA)]
tmp_data[, TARGET_SCALE_SCORES_2_YEAR_CURRENT := as.numeric(NA)]
tmp_data[, TARGET_SCALE_SCORES_3_YEAR_CURRENT := as.numeric(NA)]

###   Fill in variables according to AYFEP values
tmp_data[AYFEP_PRIOR==6L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_1_TARGET_0_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP==6L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_1_TARGET_1_YEAR_PROJ_YEAR_1_CURRENT]

tmp_data[AYFEP_PRIOR==5L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_2_TARGET_1_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP_PRIOR==5L, TARGET_SCALE_SCORES_2_YEAR := SCALE_SCORE_SGP_LEVEL_2_TARGET_1_YEAR_PROJ_YEAR_2]
tmp_data[AYFEP==5L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_2_TARGET_2_YEAR_PROJ_YEAR_1_CURRENT]
tmp_data[AYFEP==5L, TARGET_SCALE_SCORES_2_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_2_TARGET_2_YEAR_PROJ_YEAR_2_CURRENT]

tmp_data[AYFEP_PRIOR==4L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_2_TARGET_0_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP==4L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_2_TARGET_1_YEAR_PROJ_YEAR_1_CURRENT]

tmp_data[AYFEP_PRIOR==3L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_2_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP_PRIOR==3L, TARGET_SCALE_SCORES_2_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_2_YEAR_PROJ_YEAR_2]
tmp_data[AYFEP_PRIOR==3L, TARGET_SCALE_SCORES_3_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_2_YEAR_PROJ_YEAR_3]
tmp_data[AYFEP==3L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_3_YEAR_PROJ_YEAR_1_CURRENT]
tmp_data[AYFEP==3L, TARGET_SCALE_SCORES_2_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_3_YEAR_PROJ_YEAR_2_CURRENT]
tmp_data[AYFEP==3L, TARGET_SCALE_SCORES_3_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_3_YEAR_PROJ_YEAR_3_CURRENT]

table(tmp_data[AYFEP_PRIOR==3L & SCALE_SCORE >= TARGET_SCALE_SCORES_3_YEAR, OnTrack_AnyPathway])

tmp_data[AYFEP_PRIOR==2L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_1_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP_PRIOR==2L, TARGET_SCALE_SCORES_2_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_1_YEAR_PROJ_YEAR_2]
tmp_data[AYFEP==2L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_2_YEAR_PROJ_YEAR_1_CURRENT]
tmp_data[AYFEP==2L, TARGET_SCALE_SCORES_2_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_2_YEAR_PROJ_YEAR_2_CURRENT]

tmp_data[AYFEP_PRIOR==1L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_0_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP==1L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_1_YEAR_PROJ_YEAR_1_CURRENT]

tmp_data[AYFEP_PRIOR==0L & HIGH_PROF_LEVEL_PRIOR==4L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_3_TARGET_0_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP==0L & HIGH_PROF_LEVEL <= 4L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_3_TARGET_1_YEAR_PROJ_YEAR_1_CURRENT]

tmp_data[AYFEP_PRIOR==0L & HIGH_PROF_LEVEL_PRIOR==5L, TARGET_SCALE_SCORES_1_YEAR := SCALE_SCORE_SGP_LEVEL_4_TARGET_0_YEAR_PROJ_YEAR_1]
tmp_data[AYFEP==0L & HIGH_PROF_LEVEL>=5L, TARGET_SCALE_SCORES_1_YEAR_CURRENT := SCALE_SCORE_SGP_LEVEL_4_TARGET_1_YEAR_PROJ_YEAR_1_CURRENT]

tmp_data[, as.list(summary(TARGET_SCALE_SCORES_1_YEAR)), keyby="AYFEP_PRIOR"]
tmp_data[, as.list(summary(TARGET_SCALE_SCORES_3_YEAR_CURRENT)), keyby="AYFEP"]

###  Remove Targets Scale Scores
save(tmp_data, file="Data/tmp_data_ss_targ_step2.rda")


#####
###   ***** Target AGP/SGP Calc (From combineSGP Level Specific Runs) *****
#####

grep("SGP_LEVEL_|SGP_TARGET_", names(tmp_data), value=TRUE)

###   Set up Target SS Variables
tmp_data[, SGP_TARGET_INTERIM := as.numeric(NA)]
tmp_data[, SGP_TARGET_INTERIM_CURRENT := as.numeric(NA)]

###   Fill in variables according to AYFEP values
tmp_data[AYFEP_PRIOR==6, SGP_TARGET_INTERIM := SGP_LEVEL_1_TARGET_0_YEAR]
tmp_data[AYFEP==6, SGP_TARGET_INTERIM_CURRENT := SGP_LEVEL_1_TARGET_1_YEAR_CURRENT]

tmp_data[, TMP_TARG := pmin(SGP_LEVEL_2_TARGET_0_YEAR, SGP_LEVEL_2_TARGET_1_YEAR, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[, TMP_TARG_CURRENT := pmin(SGP_LEVEL_2_TARGET_1_YEAR_CURRENT, SGP_LEVEL_2_TARGET_2_YEAR_CURRENT, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==5, SGP_TARGET_INTERIM := TMP_TARG]
tmp_data[AYFEP==5, SGP_TARGET_INTERIM_CURRENT := TMP_TARG_CURRENT]

tmp_data[AYFEP_PRIOR==4, SGP_TARGET_INTERIM := SGP_LEVEL_2_TARGET_0_YEAR]
tmp_data[AYFEP==4, SGP_TARGET_INTERIM_CURRENT := SGP_LEVEL_2_TARGET_1_YEAR_CURRENT]

tmp_data[, TMP_TARG := pmin(SGP_LEVEL_3_TARGET_0_YEAR, SGP_LEVEL_3_TARGET_1_YEAR, SGP_LEVEL_3_TARGET_2_YEAR, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[, TMP_TARG_CURRENT := pmin(SGP_LEVEL_3_TARGET_1_YEAR_CURRENT, SGP_LEVEL_3_TARGET_2_YEAR_CURRENT, SGP_LEVEL_3_TARGET_3_YEAR_CURRENT, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==3, SGP_TARGET_INTERIM := TMP_TARG]
tmp_data[AYFEP==3, SGP_TARGET_INTERIM_CURRENT := TMP_TARG_CURRENT]

tmp_data[, TMP_TARG := pmin(SGP_LEVEL_3_TARGET_0_YEAR, SGP_LEVEL_3_TARGET_1_YEAR, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[, TMP_TARG_CURRENT := pmin(SGP_LEVEL_3_TARGET_1_YEAR_CURRENT, SGP_LEVEL_3_TARGET_2_YEAR_CURRENT, na.rm=TRUE), by=c("ID", "YEAR")]
tmp_data[AYFEP_PRIOR==2, SGP_TARGET_INTERIM := TMP_TARG]
tmp_data[AYFEP==2, SGP_TARGET_INTERIM_CURRENT := TMP_TARG_CURRENT]

tmp_data[AYFEP_PRIOR==1, SGP_TARGET_INTERIM := SGP_LEVEL_3_TARGET_0_YEAR]
tmp_data[AYFEP==1, SGP_TARGET_INTERIM_CURRENT := SGP_LEVEL_3_TARGET_1_YEAR_CURRENT]

tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR == 4, SGP_TARGET_INTERIM := SGP_LEVEL_3_TARGET_0_YEAR]
tmp_data[AYFEP==0 & HIGH_PROF_LEVEL <= 4, SGP_TARGET_INTERIM_CURRENT := SGP_LEVEL_3_TARGET_1_YEAR_CURRENT]

tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5, SGP_TARGET_INTERIM := SGP_LEVEL_4_TARGET_0_YEAR]
tmp_data[AYFEP==0 & HIGH_PROF_LEVEL>=5, SGP_TARGET_INTERIM_CURRENT := SGP_LEVEL_4_TARGET_1_YEAR_CURRENT]

tmp_data[, DIFF := AGP - SGP_TARGET_INTERIM]
summary(tmp_data[AYFEP_PRIOR==3, DIFF])

tmp_data[, as.list(summary(DIFF)), keyby="AYFEP_PRIOR"]
tmp_data[!is.na(AGP), as.list(summary(AGP)), keyby="AYFEP_PRIOR"] # ?
tmp_data[!is.na(SGP_TARGET_INTERIM), as.list(summary(SGP_TARGET_INTERIM)), keyby="AYFEP_PRIOR"]

tmp_data[YEAR=='2020' & AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==6, as.list(summary(SGP_TARGET_INTERIM)), keyby="GRADE"]
tmp_data[YEAR=='2020' & AYFEP==0, as.list(summary(SGP_TARGET_INTERIM_CURRENT)), keyby="GRADE"]

table(tmp_data[YEAR =='2019', is.na(AGP), is.na(SGP_TARGET_INTERIM)])
# missing <- tmp_data[YEAR =='2019' & is.na(AGP) & !is.na(SGP_TARGET_INTERIM)]


#####
###   *****  Interim CUKU  *****
#####

grep("CUKU", names(tmp_data), value=T)
grep("CATCH_UP_KEEP_UP", names(tmp_data), value=T)

###   Set up Target SS Variables
tmp_data[, CATCH_UP_KEEP_UP_INTERIM_TARGET := as.character(NA)]

###   Fill in CATCH_UP_KEEP_UP_INTERIM_TARGET according to AYFEP values
tmp_data[AYFEP_PRIOR==6, CATCH_UP_KEEP_UP_INTERIM_TARGET := L1L2_CUKU_STATUS_0_YEAR]
tmp_data[AYFEP_PRIOR==5, CATCH_UP_KEEP_UP_INTERIM_TARGET := L2L3_CUKU_STATUS_1_YEAR]
tmp_data[AYFEP_PRIOR==4, CATCH_UP_KEEP_UP_INTERIM_TARGET := L2L3_CUKU_STATUS_0_YEAR]
tmp_data[AYFEP_PRIOR==3, CATCH_UP_KEEP_UP_INTERIM_TARGET := L3L4_CUKU_STATUS_2_YEAR]
tmp_data[AYFEP_PRIOR==2, CATCH_UP_KEEP_UP_INTERIM_TARGET := L3L4_CUKU_STATUS_1_YEAR]
tmp_data[AYFEP_PRIOR==1, CATCH_UP_KEEP_UP_INTERIM_TARGET := L3L4_CUKU_STATUS_0_YEAR]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4, CATCH_UP_KEEP_UP_INTERIM_TARGET := L3L4_CUKU_STATUS_0_YEAR]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5, CATCH_UP_KEEP_UP_INTERIM_TARGET := L4L5_CUKU_STATUS_0_YEAR]
# tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR >= 5, CATCH_UP_KEEP_UP_INTERIM_TARGET := L5_CUKU_STATUS_0_YEAR] #  What about L6 students?

table(tmp_data[, CATCH_UP_KEEP_UP_INTERIM_TARGET, OnTrack_AnyPathway]) # Some disagreement... Manual corrections in combineSGP all depend on AGP/SGP relationship.
table(tmp_data[YEAR=="2020" & CATCH_UP_KEEP_UP_INTERIM_TARGET == "Catch Up: Yes" & OnTrack_AnyPathway==0, HIGH_PROF_LEVEL_PRIOR, AYFEP_PRIOR])
table(tmp_data[YEAR=="2020" & CATCH_UP_KEEP_UP_INTERIM_TARGET == "Keep Up: No" & OnTrack_AnyPathway==1, HIGH_PROF_LEVEL_PRIOR, AYFEP_PRIOR])

yn1 <- tmp_data[CATCH_UP_KEEP_UP_INTERIM_TARGET == "Catch Up: Yes" & OnTrack_AnyPathway==0, ]
table(yn1[, ACHIEVEMENT_LEVEL, AYFEP_PRIOR])
table(yn1[, ACHIEVEMENT_LEVEL, HIGH_PROF_LEVEL_PRIOR])

yn2 <- tmp_data[CATCH_UP_KEEP_UP_INTERIM_TARGET == "Keep Up: No" & OnTrack_AnyPathway==1, ]
table(yn2[, ACHIEVEMENT_LEVEL, AYFEP_PRIOR])
table(yn2[, ACHIEVEMENT_LEVEL, HIGH_PROF_LEVEL_PRIOR])


###  Manual Corrections for all levels that do not depend on SGP/AGP relationship
###   L3TOL4 (1 Year remaining)
tmp_data[AYFEP_PRIOR==1 & ACHIEVEMENT_LEVEL >= 4, CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: Yes"] #  0 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==1 & ACHIEVEMENT_LEVEL < 4,  CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: No"]  #  7 Fixed in 2019/2020

###   L4TOL4
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL >= 4 & ACHIEVEMENT_LEVEL_PRIOR != "L4", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: Yes"] #   0 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL >= 4 & ACHIEVEMENT_LEVEL_PRIOR == "L4", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Keep Up: Yes"]  # 482 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL < 4  & ACHIEVEMENT_LEVEL_PRIOR != "L4", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: No"]  #   2 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==4 & ACHIEVEMENT_LEVEL < 4  & ACHIEVEMENT_LEVEL_PRIOR == "L4", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Keep Up: No"]   #   0 Fixed in 2019/2020

###   L5TOL5
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL >= 5 & ACHIEVEMENT_LEVEL_PRIOR != "L5", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: Yes"] #   0 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL >= 5 & ACHIEVEMENT_LEVEL_PRIOR == "L5", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Keep Up: Yes"]  #  70 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL < 5  & ACHIEVEMENT_LEVEL_PRIOR != "L5", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Catch Up: No"]  #   1 Fixed in 2019/2020
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR==5 & ACHIEVEMENT_LEVEL < 5  & ACHIEVEMENT_LEVEL_PRIOR == "L5", CATCH_UP_KEEP_UP_INTERIM_TARGET := "Keep Up: No"]   #   0 Fixed in 2019/2020

table(tmp_data[, CATCH_UP_KEEP_UP_INTERIM_TARGET, OnTrack_AnyPathway]) # Perfect alignment now.


#####
###   *****  INTERIM_TARGET_LEVEL  *****
#####

tmp_data[, INTERIM_TARGET_LEVEL := as.character(NA)]
tmp_data[, INTERIM_TARGET_LEVEL_CURRENT := as.character(NA)]

tmp_data[AYFEP_PRIOR==6, INTERIM_TARGET_LEVEL := "L2"]
tmp_data[AYFEP==6, INTERIM_TARGET_LEVEL_CURRENT := "L2"]
tmp_data[AYFEP_PRIOR %in% 4:5, INTERIM_TARGET_LEVEL := "L3"]
tmp_data[AYFEP %in% 4:5, INTERIM_TARGET_LEVEL_CURRENT := "L3"]
tmp_data[AYFEP_PRIOR %in% 1:3, INTERIM_TARGET_LEVEL := "L4"]
tmp_data[AYFEP %in% 1:3, INTERIM_TARGET_LEVEL_CURRENT := "L4"]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR >= 4, INTERIM_TARGET_LEVEL := "L4"]
tmp_data[AYFEP==0 & HIGH_PROF_LEVEL <= 4, INTERIM_TARGET_LEVEL_CURRENT := "L4"]
tmp_data[AYFEP_PRIOR==0 & HIGH_PROF_LEVEL_PRIOR >= 5, INTERIM_TARGET_LEVEL := "L5"]
tmp_data[AYFEP==0 & HIGH_PROF_LEVEL >= 5, INTERIM_TARGET_LEVEL_CURRENT := "L5"]

table(tmp_data[, AYFEP_PRIOR, INTERIM_TARGET_LEVEL], exclude=NULL)
table(tmp_data[, AYFEP, INTERIM_TARGET_LEVEL_CURRENT], exclude=NULL)

save(tmp_data, file="Data/tmp_data_ss_targ_step3.rda")

###   Fix ACHIEVEMENT_LEVEL
tmp_data[, ACHIEVEMENT_LEVEL := ACH_LEV]
tmp_data[, ACH_LEV := NULL]

###  Remove Targets Scale Scores, CUKU and other extraneous vars
tmp_data[, grep("SCALE_SCORE_SGP_LEVEL_", names(tmp_data), value=TRUE) := NULL]
tmp_data[, grep("SGP_TARGET_YEAR", names(tmp_data), value=TRUE) := NULL] # SGP_TARGET_YEAR|TOL
tmp_data[, grep("CUKU", names(tmp_data), value=TRUE) := NULL]
tmp_data[, c("DIFF", "TMP_TARG") := NULL]


###   Set up Single Target SS Variable for ISRs

tmp_data[, ISR_INTERIM_DATA := list(list(c(
              LAGGED_TARGET_LEVEL = list(INTERIM_TARGET_LEVEL),
              CURRENT_TARGET_LEVEL = list(INTERIM_TARGET_LEVEL_CURRENT),
              ON_TRACK_STATUS = list(CATCH_UP_KEEP_UP_INTERIM_TARGET),
              LAGGED_AGP = list(SGP_TARGET_INTERIM),
              CURRENT_AGP = list(SGP_TARGET_INTERIM_CURRENT),
              LAGGED_SS_TARGET=list(c(TARGET_SCALE_SCORES_1_YEAR, TARGET_SCALE_SCORES_2_YEAR, TARGET_SCALE_SCORES_3_YEAR)),
              CURRENT_SS_TARGET=list(c(TARGET_SCALE_SCORES_1_YEAR_CURRENT, TARGET_SCALE_SCORES_2_YEAR_CURRENT, TARGET_SCALE_SCORES_3_YEAR_CURRENT))))), by = c("VALID_CASE", "ID", "GRADE", "YEAR")] # list of components for ISRs

head(tmp_data[VALID_CASE=="VALID_CASE" & YEAR=="2020" & AYFEP_PRIOR==3 & AYFEP == 2, ISR_INTERIM_DATA], 10)


###   Save results and insert into WIDA_CO_SGP@Data

assign("WIDA_CO_SGP_LONG_Data", tmp_data)
save(WIDA_CO_SGP_LONG_Data, file="Data/WIDA_CO_SGP_LONG_Data_w_OTG.Rdata")
fwrite(WIDA_CO_SGP_LONG_Data, file="Data/WIDA_CO_SGP_LONG_Data_w_OTG.txt", sep="|")

WIDA_CO_SGP@Data <- tmp_data
setkeyv(WIDA_CO_SGP@Data, SGP:::getKey(WIDA_CO_SGP@Data))

save(WIDA_CO_SGP, file="Data/WIDA_CO_SGP.Rdata")
