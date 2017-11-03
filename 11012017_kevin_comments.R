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
                    "PAYTYPE","PROP_NOF","QBLNCM1X","QMRTTERM","QNEWDATE","QRFINDAT","QYEAR","REFINED","VARRTE","QADINT1X",
                    "QADINT2X","QADINT3X","QPRINM1X","QPRINM2X","QPRINM3X")

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

# names(datamor) <- c("mtg_pmt_reported_mor","interest_rate_cur","interest_rate_org","mtg_amount","mtg_outstanding_cur","refinanced",
#                     "NEWID","QYEAR","CUID","interview_year","mtg_pmt_calc_mor","first_payment_date")

datamor <- rename(datamor,c("MRTPMTX"="mtg_pmt_reported_mor","NEWMRRT"="interest_rate_cur","OLDMRRT"="interest_rate_org","ORGMRTX"="mtg_amount",
                            "QBLNCM1X"="mtg_outstanding_cur","REFINED"="refinanced","NEWID"="NEWID","QYEAR"="QYEAR","CUID"="CUID","year"="interview_year",
                            "calc_mtg_pmt"="mtg_pmt_calc_mor","first_payment_date"="first_payment_date"))
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
                     "FOODCQ","JFDSTMPA", "LOT_SIZE","MAJAPPCQ","NUM_AUTO","NUM_TVAN","ORIGIN1","REF_RACE","SAVACCTX","SMSASTAT",
                     "BATHRMQ","CARTKNCQ","CARTKUCQ","EMRTPNOC","EOTHENTC","EOWNDWLC","ERANKMTH","ETOTACX4","ETRANPTC",
                     "EVEHPURC","FINCBTAX","HLTHINCQ","HOUSEQCQ","INCLASS","INC_HRS1","INC_HRS2","MAINRPCQ","NO_EARNR","POV_CY","SECESTX",
                     "SMLAPPCQ","TVRDIOCQ","UTILCQ","VEHQ","HEALTHCQ","WOMGRLCQ","CHLDRNCQ","MENBOYCQ",
                     "MISC1CQ","TELEPHCQ","APPARCQ","DOMSRVCQ","EDUCACQ","LIQUIDX","FINCATXM","ALLFULCQ","AS_COMP1","AS_COMP2",
                     "AS_COMP3","AS_COMP4","AS_COMP5","FURNTRCQ","HIGH_EDU","HOUSOPCQ","LIFINSCQ","PETTOYCQ","Region")
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
# names(datafmli) <- c("NEWID","origin","state","lot_size","no_of_bedrooms","built_year","savingsaccount","checkingaccount","cu_income",
#                      "family_size","family_type","no_of_cars","no_of_trucks","race","age_of_ref","family_earnings","family_earnings_aftertax",
#                      "urban","msa","food_stamp_value","appliances","food_away_home","new_vehicle_outlay","used_vehicle_outlay",
#                      "food_away_home_all","entertainment","food_exp","total_exp","housing_exp","all_major_outlays","CUID")

datafmli <- rename(datafmli,c("NEWID"="NEWID","AGE_REF"="age_of_ref","BEDROOMQ"="no_of_bedrooms","BLS_URBN"="urban","BUILT"="built_year",
                              "CKBKACTX"="checkingaccount","FINCBTAX"="earnings_before_tax_family","FAM_SIZE"="family_size","FAM_TYPE"="family_type",
                              "FINCATAX"="earnings_after_tax_family_1","JFDSTMPA"="food_stamp_value","LOT_SIZE"="lot_size","NUM_AUTO"="no_of_cars",
                              "ORIGIN1"="origin","REF_RACE"="race","SAVACCTX"="savingsaccount","SMSASTAT"="msa","TOTEXPCQ"="total_exp",
                              "FOODCQ"="food_exp","FDAWAYCQ"="food_away","FDXMAPPQ"="food_away_x","MAJAPPCQ"="major_appliances","ENTERTCQ"="entertainment_exp",
                              "STATE"="state","CUINCOME"="cu_income","NUM_TVAN"="no_of_trucks","ECARTKNC"="new_vehicle_outlay","ECARTKUC"="used_vehicle_outlay",
                              "EHOUSNGC"="housing_outlays","ETOTALC"="all_major_outlays","ETOTACX4"="all_major_outlays_adj","CUID"="CUID","BATHRMQ"="no_of_baths",
                              "CARTKNCQ"="new_vehicle_outlay2","CARTKUCQ"="used_vehicle_outlay2","EMRTPNOC"="mtg_principal_outlay",
                              "EOTHENTC"="outlays_entertainment_equip","EOWNDWLC"="mtg_ins_proptax_maintainance_etc","ERANKMTH"="tot_exp_outlays",
                              "ETRANPTC"="transport_outlays","EVEHPURC"="vehicle_purchase_outlay","HEALTHCQ"="health_exp",
                              "HOUSEQCQ"="house_furnishings_and_equip","INCLASS"="income_class","INC_HRS1"="hours_worked_ref","INC_HRS2"="hours_worked_spouse",
                              "MAINRPCQ"="maintainance_repairs","NO_EARNR"="no_of_income_earners","POV_CY"="below_poverty_line",
                              "SECESTX"="investments","SMLAPPCQ"="small_appliances","TVRDIOCQ"="tv_radio_etc","UTILCQ"="utility_exp",
                              "VEHQ" = "total_no_of_vehicles","WOMGRLCQ"="clothing_women",
                              "CHLDRNCQ"="clothing_children","MENBOYCQ"="clothing_men","MISC1CQ"="miscellaneous_exp",
                              "TELEPHCQ"="telephone_cost","APPARCQ"="apperel_and_services_exp",
                              "DOMSRVCQ"="domestic_services","EDUCACQ"="education_exp","LIQUIDX"="liquid_assets_2",
                              "FINCATXM"="earnings_after_tax_family_2","ALLFULCQ"="fuel_exp","AS_COMP1"="male_adults","AS_COMP2"="female_adults",
                              "AS_COMP3"="boys","AS_COMP4"="girls","AS_COMP5"="babies","FURNTRCQ"="furniture_exp","HIGH_EDU"="highest_edu_in_cu",
                              "HOUSOPCQ"="household_operations","LIFINSCQ"="life_and_personal_insurance","PETTOYCQ"="pets_toys"
                              ))


datafmli['CUID'] <- ifelse(is.na(datafmli$CUID),substr(as.character(datafmli$NEWID),1,nchar((as.character(datafmli$NEWID)))-1),datafmli$CUID)
datafmli <- datafmli[!duplicated(datafmli$NEWID),]

datafmli <- data.table(datafmli)


# datafmli_summary <- datafmli[,list(state=min(state,na.rm = TRUE),
#                                    total_exp = mean(total_exp,na.rm = TRUE),
#                                    housing_exp = mean(housing_exp,na.rm = TRUE),
#                                    all_major_outlays = mean(all_major_outlays,na.rm = TRUE),
#                                    food_exp = mean(food_exp,na.rm = TRUE),
#                                    entertainment = mean(entertainment,na.rm = TRUE),
#                                    cu_income = mean(cu_income,na.rm = TRUE),
#                                    family_size = mean(family_size,na.rm = TRUE))    
#                              ,by=list(CUID)]
# 
# 
# datafmli_summary$CUID <- as.numeric(datafmli_summary$CUID)
# datafmli_summary <- as.data.frame(datafmli_summary)
# datafmli_summary$CUID <- as.numeric(datafmli_summary$CUID)

datafmli <- as.data.frame(datafmli)

datafmli$food_stamp_value <- as.numeric(datafmli$food_stamp_value)
datafmli$hours_worked_ref <- as.numeric(datafmli$hours_worked_ref)
datafmli$hours_worked_spouse <- as.numeric(datafmli$hours_worked_spouse)
datafmli$no_of_bedrooms <- as.numeric(datafmli$no_of_bedrooms)
datafmli$no_of_baths <- as.numeric(datafmli$no_of_baths)
datafmli$savingsaccount <- as.numeric(datafmli$savingsaccount)
datafmli$checkingaccount <- as.numeric(datafmli$checkingaccount)
datafmli$investments <- as.numeric(datafmli$investments)
datafmli['liquid_assets_temp'] <- datafmli$savingsaccount + datafmli$checkingaccount
datafmli$liquid_assets_2 <- as.numeric(datafmli$liquid_assets_2)
datafmli$liquid_assets <- ifelse(is.na(datafmli$liquid_assets_2),datafmli$liquid_assets_temp,datafmli$liquid_assets_2)
datafmli$liquid_assets_2 <- NULL
datafmli$liquid_assets_temp <- NULL
datafmli$family_income_after_tax <- abs(ifelse(is.na(datafmli$earnings_after_tax_family_1),datafmli$earnings_after_tax_family_2,datafmli$earnings_after_tax_family_1))
datafmli$earnings_after_tax_family_1 <- NULL
datafmli$earnings_after_tax_family_2 <- NULL
# OPI ---------------------------------------------------------------------


filelist <- list.files(path=paste(path,"/Data/Raw/opi",sep=""),pattern = "*.csv",full.names = TRUE)
opi_keep_names <- c("NEWID"  , "PROPVALX","PROP_NOI")

dataopi <- NULL
for(fn in filelist) {
  print(fn)
  temp <- read.csv(fn, stringsAsFactors = FALSE,header = TRUE)
  temp <- temp[,names(temp) %in% opi_keep_names]
  mor_keep_names <- names(temp)
  missing <- which(!names(dataopi) %in% names(temp))
  for(m in missing) {
    temp[names(dataopi)[m]] <- NA
  }
  extra <- which(!names(temp) %in% names(dataopi))
  for(e in extra) {
    dataopi[names(temp)[e]] <- NA
  }
  dataopi <- rbind(dataopi,temp)
}

dataopi['CUID'] <- sapply(dataopi$NEWID,function(x) substr(as.character(x),1,(nchar(as.character(x))-1)))
dataopi <- dataopi[!is.na(dataopi$PROPVALX),]
dataopi <- dataopi[dataopi$PROP_NOI==1,]
dataopi <- dataopi[!is.na(dataopi$NEWID),]
dataopi$PROPVALX <- as.numeric(dataopi$PROPVALX)
dataopi <- dataopi[!is.na(dataopi$PROPVALX),]

dataopi <- data.table(dataopi)
dataopi_summary <- dataopi[,list(self_reported_home_value = mean(PROPVALX,na.rm = TRUE)),by=list(CUID)]

dataopi_summary$CUID <- as.numeric(dataopi_summary$CUID)
dataopi_summary <- as.data.frame(dataopi_summary)



# MERGE -------------------------------------------------------------------

no_of_years  = 2

merged_data <- datamor[(datamor$interview_year-datamor$org_year)<=no_of_years ,]
merged_data['mpay_mtg'] <- merged_data$mtg_pmt_calc_mor/merged_data$mtg_amount

merged_data <- merge(merged_data,dataopi_summary,by="CUID",all.x = TRUE)
merged_data['mpay_value'] <- merged_data$mtg_pmt_calc_mor/merged_data$self_reported_home_value
merged_data$CUID <- NULL



merged_data <- merge(merged_data,datafmli[,c("NEWID","CUID")],by="NEWID",all.x = TRUE)
merged_data <- merge(merged_data,datafmli,by="CUID",all.x = TRUE)
merged_data['org_state'] <- paste(merged_data$org_year,merged_data$state)
merged_data['interview_state'] <- paste(merged_data$interview_year,merged_data$state)

merged_data['int_diff'] <- merged_data$mtg30us - merged_data$interest_rate_cur

merged_data <- merged_data[merged_data$interest_rate_cur==merged_data$interest_rate_org,]

merged_data <- merged_data[!is.na(merged_data$NEWID.x),]

merged_data <- merged_data[merged_data$int_diff>=quantile(merged_data$int_diff,0.01,na.rm = TRUE) & merged_data$int_diff <= quantile(merged_data$int_diff,0.99,na.rm = TRUE),]


merged_data <- merged_data[merged_data$total_exp>100 & !is.na(merged_data$total_exp),]

merged_data <- merged_data[merged_data$total_exp <= quantile(merged_data$total_exp,0.999,na.rm = TRUE) & merged_data$total_exp >= quantile(merged_data$total_exp,0.001,na.rm = TRUE),]



saveRDS(merged_data,file="E:/CE Survey/Data/Processed/merged_data_Nov032017.rds")


# REGRESSIONS Q Level-------------------------------------------------------------


merged_data['housing_exp_calc'] <- merged_data$major_appliances+merged_data$small_appliances + merged_data$maintainance_repairs+merged_data$house_furnishings_and_equip+merged_data$mtg_principal_outlay

merged_data['non_durable'] <- merged_data$food_exp+ merged_data$entertainment_exp + merged_data$utility_exp + merged_data$transport_outlays - merged_data$vehicle_purchase_outlay + merged_data$apperel_and_services_exp+merged_data$miscellaneous_exp 

merged_data['durable'] <- merged_data$major_appliances + merged_data$small_appliances + merged_data$vehicle_purchase_outlay

merged_data['services'] <- merged_data$education_exp + merged_data$domestic_services + merged_data$health_exp


income_medians <- ddply(merged_data,.(state,interview_year),summarise,q1_income = quantile(income_class,0.25,na.rm=TRUE),median_income = median(income_class,na.rm=TRUE),q3_income = quantile(income_class,0.75,na.rm=TRUE))

merged_data <- merge(merged_data,income_medians,by=c("state","interview_year"),all.x = TRUE)

merged_data$mtg30us <- merged_data$mtg30us*100

merged_data$family_income_after_tax <- merged_data$family_income_after_tax+1

endo_var = "mpay_mtg"
instrument = "mtg30us"
cluster_var = "interview_year+CUID"

controls <- "log(family_income_after_tax)+family_size+I(age_of_ref^2)|org_state+interview_state"



dep_vars <- c("non_durable","durable","services","housing_exp_calc","total_exp")#,"food_exp","all_major_outlays","entertainment_exp","transport_outlays","vehicle_purchase_outlay","major_appliances","small_appliances","house_furnishings_and_equip","maintainance_repairs","utility_exp")


for(dep_var in dep_vars) {
  cat(paste("\n\n\n*******************",dep_var,"*********************\n\n\nSummary Statistics\n"))
  reg_data <- merged_data[!is.na(merged_data$state),]
  eval(parse(text=paste("reg_data$",dep_var," <- reg_data$",dep_var,"+1",sep="")))
  eval(parse(text=paste("reg_data$",dep_var," <- ifelse(is.na(reg_data$",dep_var,"),1,reg_data$",dep_var,")",sep="")))
  eval(parse(text=paste("reg_data['",dep_var,"_dummy'] <- ifelse(reg_data$",dep_var,">1,1,0)",sep="")))
  
  log_dep_var = paste("log(",dep_var,")",sep="")
  dummy_dep_var = paste(dep_var,"_dummy",sep="")
  
  if(dep_var %in% c("total_exp_less_mtg")) {
    eval(parse(text=paste("reg_data <- reg_data[reg_data$",dep_var,">1,]",sep="")))
  }
  
  regs <- list()
  regs[[1]] <- felm(as.formula(paste(log_dep_var,"~",endo_var,"+",controls,"|0|",cluster_var,sep="")),data = reg_data)
  regs[[2]] <- felm(as.formula(paste("I(",endo_var,"*1e5)~",instrument,"+",controls,"|0|",cluster_var,sep="")),data = reg_data)
  regs[[3]] <- felm(as.formula(paste(log_dep_var,"~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = reg_data)
  regs[[4]] <- felm(as.formula(paste(dep_var,"~",controls,"|(",endo_var,"~",instrument,")|",cluster_var,sep="")),data = reg_data)

  eval(parse(text=paste("print(summary(reg_data$",dep_var,"-1))",sep="")))
  
  
  stargazer(regs,type="text",dep.var.labels.include = FALSE,omit.stat = c("f","rsq","ser"),
            column.labels = c("Log","First Stage","Log","Level","Log - Low Income", "Log - High Income"),
            add.lines = list(c("Cond. F","","",round(condfstat(regs[[3]])[[1]],2),round(condfstat(regs[[4]])[[1]],2))),title = gsub("_"," ",dep_var))
  
}

