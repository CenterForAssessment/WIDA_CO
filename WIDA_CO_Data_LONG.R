###############################################################################
###
### Script for preparing WIDA Colorado Data
###
###############################################################################

### Load packages

require(data.table)
require(SGP)


### Load data

WIDA_CO_Data_LONG_READING <- fread("Data/Base_Files/ACCESS_2016_2017_Growth_Readin_Overall.csv", colClasses=rep("character", 24))
WIDA_CO_Data_LONG_LITERACY <- fread("Data/Base_Files/ACCESS_2016_2017_Growth_Readin_Literacy.csv", colClasses=rep("character", 24))


### Stack data

WIDA_CO_Data_LONG_LITERACY[,CONTENT_AREA:="LITERACY"]
WIDA_CO_Data_LONG <- rbindlist(list(WIDA_CO_Data_LONG_READING, WIDA_CO_Data_LONG_LITERACY))


### Tidy up data

WIDA_CO_Data_LONG[,FIRST_NAME:=as.factor(FIRST_NAME)]
WIDA_CO_Data_LONG[,LAST_NAME:=as.factor(LAST_NAME)]
levels(WIDA_CO_Data_LONG$FIRST_NAME) <- sapply(levels(WIDA_CO_Data_LONG$FIRST_NAME), capwords)
levels(WIDA_CO_Data_LONG$LAST_NAME) <- sapply(levels(WIDA_CO_Data_LONG$LAST_NAME), capwords)

WIDA_CO_Data_LONG[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]

WIDA_CO_Data_LONG[GENDER=="MISSIN",GENDER:=NA]
WIDA_CO_Data_LONG[,GENDER:=as.factor(GENDER)]
levels(WIDA_CO_Data_LONG$GENDER) <- sapply(levels(WIDA_CO_Data_LONG$GENDER), capwords)

WIDA_CO_Data_LONG[ETHNICITY=="MISSING",ETHNICITY:=NA]
WIDA_CO_Data_LONG[,ETHNICITY:=as.factor(ETHNICITY)]
levels(WIDA_CO_Data_LONG$ETHNICITY) <- sapply(levels(WIDA_CO_Data_LONG$ETHNICITY), capwords)

WIDA_CO_Data_LONG[FREE_REDUCED_LUNCH_STATUS=="MISSING",FREE_REDUCED_LUNCH_STATUS:=NA]
WIDA_CO_Data_LONG[,FREE_REDUCED_LUNCH_STATUS:=as.factor(FREE_REDUCED_LUNCH_STATUS)]

WIDA_CO_Data_LONG[ELL_STATUS=="MISSING",ELL_STATUS:=NA]
WIDA_CO_Data_LONG[,ELL_STATUS:=as.factor(ELL_STATUS)]

WIDA_CO_Data_LONG[IEP_STATUS=="MISSING",IEP_STATUS:=NA]
WIDA_CO_Data_LONG[,IEP_STATUS:=as.factor(IEP_STATUS)]

WIDA_CO_Data_LONG[GIFTED_TALENTED_PROGRAM_STATUS=="MISSING",GIFTED_TALENTED_PROGRAM_STATUS:=NA]
WIDA_CO_Data_LONG[,GIFTED_TALENTED_PROGRAM_STATUS:=as.factor(GIFTED_TALENTED_PROGRAM_STATUS)]

WIDA_CO_Data_LONG[EMH_LEVEL=="MISSING",EMH_LEVEL:=NA]
WIDA_CO_Data_LONG[,EMH_LEVEL:=as.factor(EMH_LEVEL)]
levels(WIDA_CO_Data_LONG$EMH_LEVEL) <- sapply(levels(WIDA_CO_Data_LONG$EMH_LEVEL), capwords)

WIDA_CO_Data_LONG[,SCHOOL_NAME:=as.factor(SCHOOL_NAME)]
levels(WIDA_CO_Data_LONG$SCHOOL_NAME) <- sapply(levels(WIDA_CO_Data_LONG$SCHOOL_NAME), capwords)

WIDA_CO_Data_LONG[,SCHOOL_ENROLLMENT_STATUS:=as.factor(SCHOOL_ENROLLMENT_STATUS)]

WIDA_CO_Data_LONG[,DISTRICT_ENROLLMENT_STATUS:=as.factor(DISTRICT_ENROLLMENT_STATUS)]


### Setkey and invalidate duplicate cases

setkey(WIDA_CO_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_CO_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_CO_Data_LONG[which(duplicated(WIDA_CO_Data_LONG, by=key(WIDA_CO_Data_LONG)))-1, VALID_CASE:="INVALID_CASE"]


### Invalidate cases with missing SCALE_SCOREs and IDs

WIDA_CO_Data_LONG[is.na(SCALE_SCORE), VALID_CASE:="INVALID_CASE"]
WIDA_CO_Data_LONG[is.na(ID), VALID_CASE:="INVALID_CASE"]


### Save results

save(WIDA_CO_Data_LONG, file="Data/WIDA_CO_Data_LONG.Rdata")
