rm(list=ls())

path = "E:/CE Survey"
setwd(path)

library(data.table)
library(lubridate)
library(lfe)
library(plyr)
library(stargazer)
library(FinCal)

# MOR ---------------------------------------------------------------------

filelist <- list.files(path=paste(path,"/Data/Raw/mor",sep=""),pattern = "*.csv",full.names = TRUE)
mor_keep_names <- c("NEWID"  , "FIXEDRTE","FRSTPYMO","FRSTPYYR","INTONLY","MRTPMTX","NEWMRRT","OLDMRRT","ORGMRTX","OWNYF","PAYMT1X",
                    "PAYTYPE","PROP_NOF","QBLNCM1X","QMRTTERM","QNEWDATE","QRFINDAT","QYEAR","REFINED","VARRTE")

datamor <- NULL
for(fn in filelist) {
  print(fn)
  temp <- read.csv(fn, stringsAsFactors = FALSE,header = TRUE)
  temp <- temp[,names(temp) %in% mor_keep_names]
  temp['mor_filename'] <- fn
  mor_keep_names <- names(temp)
  missing <- which(!names(datamor) %in% names(temp))
  for(m in missing) {
    temp[names(datamor)[m]] <- NA
  }
  datamor <- rbind(datamor,temp)
}

datamor['CUID'] <- sapply(datamor$NEWID,function(x) substr(as.character(x),1,(nchar(as.character(x))-1)))
datamor['year'] <- floor(datamor$QYEAR/10)

datamor <- datamor[datamor$FIXEDRTE==1,]      # FIXEDRTE == 1 fixed rate
datamor <- datamor[datamor$OWNYF==100,]       # OWNYF == 100 owner occupied
datamor <- datamor[datamor$REFINED != 1 | is.na(datamor$REFINED),]    # REFINED != 1 not refinanced

datamor$OLDMRRT <- as.numeric(datamor$OLDMRRT)
datamor$NEWMRRT <- as.numeric(datamor$NEWMRRT)
datamor$QMRTTERM <- as.numeric(datamor$QMRTTERM)
datamor <- datamor[datamor$QMRTTERM==30 & !is.na(datamor$QMRTTERM),] # 30 year mortgages
datamor <- datamor[is.na(datamor$QNEWDATE),]  # not modified
datamor$ORGMRTX <- as.numeric(datamor$ORGMRTX)

source("C:/Users/dnratnadiwakara/OneDrive/CESurvey/mortgage.R")
datamor['calc_mtg_pmt'] <- mapply(mortgage,datamor$ORGMRTX, datamor$NEWMRRT*100,30)
datamor$MRTPMTX <- as.numeric(datamor$MRTPMTX) # reported mortgage payment

datamor <- datamor[datamor$PROP_NOF==1,]


datamor <- data.table(datamor)
datamor_summary <- datamor[,list(amt = sd(ORGMRTX,na.rm=TRUE)/mean(ORGMRTX,na.rm=TRUE),pmt = sd(MRTPMTX,na.rm=TRUE)/mean(MRTPMTX,na.rm=TRUE),QYEAR=sd(QYEAR,na.rm=TRUE),firstyear=sd(FRSTPYYR,na.rm=TRUE))
                           ,by=list(CUID)]

datamor_summary <- datamor_summary[amt<=0.05 & firstyear==0]

datamor <- datamor[datamor$CUID %in% datamor_summary$CUID,]
datamor <- as.data.frame(datamor)
rm(datamor_summary)

datamor <- datamor[!duplicated(datamor$CUID),]
datamor$FIXEDRTE <- NULL
datamor$QMRTTERM <- NULL
datamor$OWNYF <- NULL
datamor['first_payment_date']<- as.Date(paste(datamor$FRSTPYYR,"-",datamor$FRSTPYMO,"-1",sep=""))
datamor$FRSTPYMO <- NULL
datamor$FRSTPYYR <- NULL
datamor$PAYTYPE <- NULL
datamor$PROP_NOF <- NULL
datamor$QNEWDATE <- NULL
datamor$QRFINDAT <- NULL
datamor$QBLNCM1X <- as.numeric(datamor$QBLNCM1X)
datamor$mor_filename <- NULL

datamor$CUID <- as.numeric(datamor$CUID)

names(datamor) <- c("mtg_pmt_reported_mor","interest_rate_cur","interest_rate_org","mtg_amount","mtg_outstanding_cur","refinanced",
                    "NEWID","QYEAR","CUID","interview_year","mtg_pmt_calc_mor","first_payment_date")
datamor$QYEAR <- NULL
datamor$refinanced <- NULL
rm(temp,filelist,mor_keep_names,m,fn,missing)

datamor['first_payment_date'] <- datamor$first_payment_date %m-% months(1)
datamor['org_year'] <- as.numeric(format(datamor$first_payment_date,"%Y"))

# datamor <- datamor[datamor$interest_rate_org==datamor$interest_rate_cur,]


source("C:/Users/dnratnadiwakara/OneDrive/CESurvey/mortgage.R")
datamor['calc_mtg_pmt'] <- mapply(mortgage,datamor$mtg_amount, datamor$interest_rate_cur*100,30)
datamor <- datamor[datamor$mtg_pmt_calc_mor<=datamor$mtg_pmt_reported_mor,]

mtg30us <- read.csv(file="E:/CE Survey/Data/Raw/MORTGAGE30US.csv",stringsAsFactors = FALSE)
names(mtg30us) <- c('first_payment_date','mtg30us')
mtg30us$first_payment_date <- as.Date(mtg30us$first_payment_date)
mtg30us$first_payment_date <- as.Date(paste(substr(as.character(mtg30us$first_payment_date),1,7),"-01",sep=""))
mtg30us <- ddply(mtg30us,.(first_payment_date),summarise,mtg30us=median(mtg30us,na.rm = TRUE))

datamor<-merge(datamor,mtg30us,by=c("first_payment_date"),all.x = TRUE)
datamor <- datamor[!is.na(datamor$NEWID),]
datamor$mtg30us <- datamor$mtg30us/100


# FMLI --------------------------------------------------------------------

fmli <- list.files(path=paste(path,"/Data/Raw/fmli",sep=""),pattern = "*.csv",full.names = TRUE)
# fmli_keep_names <- c("NEWID"  , "TOTEXPCQ",  "STATE" ,"CUID")
fmli_keep_names <- c("NEWID"  , "TOTEXPCQ",  "STATE" ,"CUID","AGE_REF","BLS_URBN","BEDROOMQ","BUILT","CKBKACTX","CUINCOME","EARNINCX",
                     "ECARTKNC","ECARTKUC","EHOUSNGC","ENTERTCQ","ETOTALC","FAM_SIZE","FAM_TYPE","FDAWAYCQ","FDXMAPPQ","FINCATAX",
                     "FOODCQ","JFDSTMPA", "LOT_SIZE","MAJAPPCQ","NUM_AUTO","NUM_TVAN","ORIGIN1","REF_RACE","SAVACCTX","SMSASTAT")
datafmli <- NULL
for(fn in fmli) {
  print(fn)
  temp <- read.csv(fn, stringsAsFactors = FALSE,header = TRUE)
  temp <- temp[,names(temp) %in% fmli_keep_names]
  # temp['filename'] <- fn
  # fmli_keep_names <- names(temp)
  missing <- which(!names(datafmli) %in% names(temp))
  for(m in missing) {
    temp[names(datafmli)[m]] <- NA
  }
  extra <- which(!names(temp) %in% names(datafmli))
  for(e in extra) {
    datafmli[names(temp)[e]] <- NA
  }
  datafmli <- rbind(datafmli,temp)
}

datafmli <- datafmli[!is.na(datafmli$NEWID),]

# names(datafmli) <- c("NEWID","state","total_exp","CUID")
names(datafmli) <- c("NEWID","origin","state","lot_size","no_of_bedrooms","built_year","savingsaccount","checkingaccount","cu_income",
                     "family_size","family_type","no_of_cars","no_of_trucks","race","age_of_ref","family_earnings","family_earnings_aftertax",
                     "urban","msa","food_stamp_value","appliances","food_away_home","new_vehicle_outlay","used_vehicle_outlay",
                     "food_away_home_all","entertainment","food_exp","total_exp","housing_exp","all_major_outlays","CUID")

datafmli['CUID'] <- ifelse(is.na(datafmli$CUID),substr(as.character(datafmli$NEWID),1,nchar((as.character(datafmli$NEWID)))-1),datafmli$CUID)
datafmli <- datafmli[!duplicated(datafmli$NEWID),]

datafmli <- data.table(datafmli)

datafmli_summary <- datafmli[,list(state=min(state,na.rm = TRUE),
                                   total_exp = mean(total_exp,na.rm = TRUE))
                             ,by=list(CUID)]

datafmli_summary$CUID <- as.numeric(datafmli_summary$CUID)
datafmli_summary <- as.data.frame(datafmli_summary)
datafmli_summary$CUID <- as.numeric(datafmli_summary$CUID)
datafmli <- as.data.frame(datafmli)

datafmli['no_of_vehicles'] <- datafmli$no_of_cars+datafmli$no_of_trucks
datafmli$food_stamp_value <- as.numeric(datafmli$food_stamp_value)
# datafmli <- datafmli[,c("NEWID","CUID","state","total_exp")]

# OPB ---------------------------------------------------------------------
# 
# 
# filelist <- list.files(path=paste(path,"/Data/Raw/opb",sep=""),pattern = "*.csv",full.names = TRUE)
# opb_keep_names <- c("NEWID"  ,"OWNDPMTX", "OWN_PURX","QYEAR","ACQUIRYR","ACQUIRMO","ACQMETH","OWNYB","PROPTYPE","PROPVALX","PROP_NOB")
# 
# dataopb <- NULL
# for(fn in filelist) {
#   print(fn)
#   temp <- read.csv(fn, stringsAsFactors = FALSE,header = TRUE)
#   temp <- temp[,names(temp) %in% opb_keep_names]
#   mor_keep_names <- names(temp)
#   missing <- which(!names(dataopb) %in% names(temp))
#   for(m in missing) {
#     temp[names(dataopb)[m]] <- NA
#   }
#   extra <- which(!names(temp) %in% names(dataopb))
#   for(e in extra) {
#     dataopb[names(temp)[e]] <- NA
#   }
#   dataopb <- rbind(dataopb,temp)
# }
# 
# dataopb <- dataopb[dataopb$OWNYB==100 & dataopb$PROP_NOB==1 & dataopb$PROPTYPE==3,]
# dataopb <- dataopb[!is.na(dataopb$NEWID),]
# 
# dataopb <- data.table(dataopb)
# dataopb_summary <- dataopb[,list(acq_year=sd(ACQUIRYR,na.rm=TRUE)),by=list(NEWID)]
# dataopb_summary <- dataopb_summary[dataopb_summary$acq_year==0,]
# 
# dataopb <- as.data.frame(dataopb)
# dataopb <- dataopb[dataopb$NEWID %in% dataopb_summary$NEWID,]
# rm(dataopb_summary)
# dataopb <- dataopb[!duplicated(dataopb$NEWID),]
# 



# MERGE -------------------------------------------------------------------

merged_data <- datamor[datamor$interview_year-datamor$org_year<=1,]
merged_data['mpay_mtg'] <- merged_data$mtg_pmt_calc_mor/merged_data$mtg_amount
merged_data$CUID <- NULL
# 
# dataopb <- dataopb[dataopb$NEWID %in% merged_data$NEWID,]
# dataopb <- dataopb[,c("NEWID","ACQUIRYR")]
# merged_data <- merge(merged_data,dataopb,by="NEWID",all.x = TRUE)

merged_data <- merge(merged_data,datafmli[,c("NEWID","CUID")],by="NEWID",all.x = TRUE)
merged_data <- merge(merged_data,datafmli,by="CUID",all.x = TRUE)
merged_data['org_state'] <- paste(merged_data$org_year,merged_data$state)
merged_data['interview_state'] <- paste(merged_data$interview_year,merged_data$state)
summary(felm(interest_rate_cur~mtg30us|org_year,data=merged_data))


# REGRESSIONS -------------------------------------------------------------


merged_data['total_exp_less_housing_exp'] <- merged_data$total_exp - merged_data$housing_exp
merged_data['major_outlays_less_housing_exp'] <- merged_data$all_major_outlays-merged_data$housing_exp
# merged_data['food'] <- merged_data$food_exp
# # merged_data['exp'] <- merged_data$entertainment
merged_data <- merged_data[merged_data$interest_rate_cur==merged_data$interest_rate_org,]

merged_data <- merged_data[!is.na(merged_data$NEWID.x),]
# merged_data <- merged_data[merged_data$exp >0,]
# merged_data$exp <- merged_data$exp/merged_data$family_size

dependent_var = "I(log(total_exp_less_housing_exp/family_size))"
endo_var = "mpay_mtg"
instrument = "mtg30us"
cluster_var = "interview_year+state+CUID"

controls <- "I(log(cu_income/family_size))|org_state+interview_state+urban+msa"



regs <- list()
regs[[1]] <- felm(as.formula(paste(dependent_var,"~",endo_var,"+",controls,"|0|",cluster_var,sep="")),data = merged_data[merged_data$total_exp_less_housing_exp>0,])
regs[[2]] <- felm(as.formula(paste(endo_var,"~",instrument,"+",controls,"|0|",cluster_var,sep="")),data = merged_data[merged_data$total_exp_less_housing_exp>0,])
regs[[3]] <- felm(as.formula(paste(dependent_var,"~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = merged_data[merged_data$total_exp_less_housing_exp>0,])
regs[[4]] <- felm(as.formula(paste("I(log(major_outlays_less_housing_exp/family_size))~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = merged_data[merged_data$major_outlays_less_housing_exp>0,])
regs[[5]] <- felm(as.formula(paste("I(log(food_exp/family_size))~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = merged_data[merged_data$food_exp>0,])
regs[[6]] <- felm(as.formula(paste("I(log(entertainment/family_size))~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = merged_data[merged_data$entertainment>0,])


stargazer(regs,type="text",dep.var.labels.include = FALSE,omit.stat = c("f","rsq","ser"),
          add.lines = list(c("Cond. F","","",round(condfstat(regs[[3]])[[1]],2),round(condfstat(regs[[4]])[[1]],2)
                             ,round(condfstat(regs[[5]])[[1]],2),round(condfstat(regs[[6]])[[1]],2))))

