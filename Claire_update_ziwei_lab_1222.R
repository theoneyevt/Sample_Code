###########     SETTING WORKING DIRECTORY   ####################################

# Clean the environment
rm(list = ls())

#set your working directory
# Lab
local = "C:/Users/zzha736/OneDrive - Emory University/Research/Dashboard"
onedrive="C:/Users/zzha736/OneDrive - Emory University/CovidHealthEquityDashboard"

# Apporto
local = "O:/Research/Dashboard"
onedrive = "O:/CovidHealthEquityDashboard"

# mac
local = "/Users/zhangziwei/OneDrive\ -\ Emory\ University/Research/Dashboard"
onedrive="/Users/zhangziwei/OneDrive\ -\ Emory\ University/CovidHealthEquityDashboard"

path_c19dashboard_shared_folder=paste0(onedrive)
############   DOWNLOADING REQUIRED PACKAGES   #################################

if(!("pacman" %in% installed.packages()[,"Package"])) {
  install.packages("pacman", dependencies = TRUE)
}
library(pacman)

pacman::p_load(
  base, tidyverse, plyr, dplyr, tidyr, readr, data.table,
  ggplot2, tibble, lubridate, cdlTools, openintro, gtools,
  rvest, DescTools, readxl, stringr, naniar, zoo, mice, anytime,
  cdcfluview, openxlsx, usmap, haven, purrr, textreadr, data.table,
  ggplot2, openintro, janitor, xts, snakecase, RJSONIO, RCurl, plotly,
  MatchIt, nlme
)

#################  MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES00  ######################
setwd(onedrive)
final_hosptest_ts <- read.csv("./Data/Processed/Hospitalizations and testing/series_hosptest_cleaned.csv")
our_ts <- read.csv("./Data/Upload/covidtimeseries00.csv") 
max(our_ts$date) # check the update date
final_hosptest_ts$date<-anydate(final_hosptest_ts$date)
final_hosptest_ts<-final_hosptest_ts[,-c(6,8,9,10,11)]
final_hosptest_ts[,9]<-""
names(final_hosptest_ts)[9]<-"percent7dayhospDaily"
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$hospDaily[which(final_hosptest_ts$hospDaily  == -1)] = NA
final_hosptest_ts$percent7dayhospDaily=100*(final_hosptest_ts$hospDaily-lag(final_hosptest_ts$hospDaily,7))/lag(final_hosptest_ts$hospDaily,14)
variable<-c("percent7dayhospDaily")
final_hosptest_ts[, variable][is.na(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.infinite(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.nan(final_hosptest_ts[, variable])] <- -999

variable1<-c("hospDaily")
final_hosptest_ts[, variable1][is.na(final_hosptest_ts[, variable1])] <- -1
final_hosptest_ts$hospAdmissionper100beds<-""

# df: community report
df <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports"))
df <- anydate(df)

df <- max(df,na.rm = T)

states_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/states_date_range_clean.RDS"))

states_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/states_df_clean.RDS"))
counties_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/counties_date_range_clean.RDS"))
counties_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/counties_df_clean.RDS"))

states_example_df <- states_df_clean %>% 
summarize(
  file_name = file_name,
  date_of_file = date_of_file,
  S01 = S01,
  S02 = S02,
  state = state,
  county = county,
  S21 = ifelse(is.na(S21),S206,S21),
  S22 = ifelse(is.na(S22),S207,S22),
  S52 = ifelse(is.na(S52),S237,S52),
  S53 = ifelse(is.na(S53),S238,S53),
  S62 = ifelse(is.na(S62),S247,S62),
  S23 = ifelse(is.na(S23),S208,S23)
)%>% 
  left_join(states_daterange_clean %>% 
              dplyr::select(file_name),
            by = "file_name")  %>% 
  dplyr::select(-c("file_name","S02","county")) %>% 
  mutate(S62 = S62*100) %>% 
  rename(date = date_of_file,
         statename = S01,
         percentPositive = S21,
         totaltests= S22,
         hospDaily = S52,
         percent7dayhospDaily = S53,
         hospAdmissionper100beds = S62,
         positivePer100K = S23) %>% 
  mutate(date = anydate(date),
         percentPositive = percentPositive*100)%>%
  filter(date>="2021-03-08" & !statename%in%"Guam"& !statename%in%"United States Virgin Islands"& !statename%in%"Commonwealth of the Northern Mariana Islands"& !statename%in%"American Samoa" & !statename%in%"Puerto Rico") %>% 
  add_column(nation = "",
             county = "")

states_example_df<-states_example_df[
  with(states_example_df, order(statename, date)),
]

counties_example_df <-counties_df_clean%>%
  summarize(
    file_name = file_name,
    date_of_file = date_of_file,
    V01 = V01,
    V02 = V02,
    state = state,
    county = county,
    V33 = ifelse(is.na(V33),V131,V33),
    V34 = ifelse(is.na(V34),V132,V34),
    V56 = ifelse(is.na(V56),V154,V56),
    V57 = ifelse(is.na(V57),V155,V57),
    V66 = ifelse(is.na(V66),V164,V66),
    V35 = ifelse(is.na(V35),V133,V35)
  )%>% 
  left_join(counties_daterange_clean %>% 
              dplyr::select(file_name),
            by = "file_name")%>% 
  dplyr::select(-c("file_name","V02")) %>% 
  rename(date = date_of_file,
         countyname = V01,
         percentPositive = V33,
         totaltests= V34,
         hospDaily = V56,
         percent7dayhospDaily = V57,
         hospAdmissionper100beds = V66,
         positivePer100K = V35) %>% 
  mutate(date = anydate(date),
         hospAdmissionper100beds = hospAdmissionper100beds*100,
         percentPositive = percentPositive*100) %>%
  filter(date>="2021-03-08")

counties_example_df<-counties_example_df[
  with(counties_example_df, order(countyname, date)),
]
counties_example_df[is.na(counties_example_df)]<- -1

final_hosptest_ts[,11] <- ""
names(final_hosptest_ts)[11]<-"positivePer100K"

final_hosptest_ts<-rbind(final_hosptest_ts,states_example_df)
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$percent7dayhospDaily<-round(final_hosptest_ts$percent7dayhospDaily,2)
final_hosptest_ts1<-subset(final_hosptest_ts,date<"2021-03-08")
final_hosptest_ts2<-subset(final_hosptest_ts,date>="2021-03-07")
final_hosptest_ts2$positivetoday=final_hosptest_ts2$totaltests*final_hosptest_ts2$percentPositive/100
detach("package:plyr", unload = TRUE)
final_hosptest_ts3<-final_hosptest_ts2%>%
  group_by(statename)%>%
  mutate(totaltests = cumsum(totaltests))
final_hosptest_ts3<-final_hosptest_ts3%>%
  group_by(statename)%>%
  mutate(positivetoday = cumsum(positivetoday))
final_hosptest_ts3$percentPositive=final_hosptest_ts3$positivetoday/final_hosptest_ts3$totaltests*100
final_hosptest_ts3<-final_hosptest_ts3[!(final_hosptest_ts3$date=="2021-03-07"),]
final_hosptest_ts3<-final_hosptest_ts3[,-c(12)]

final_hosptest_ts4<-rbind(final_hosptest_ts1,final_hosptest_ts3)
final_hosptest_ts4<-final_hosptest_ts4[
  with(final_hosptest_ts4, order(statename, date)),
]

final_hosptest_ts5<-final_hosptest_ts4 %>%
  group_by(statename) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(`statename`)
final_hosptest_ts6<-final_hosptest_ts5%>%
  group_by(statename)%>%
  mutate(state=ifelse(is.na(state),lag(state),state))%>%
  mutate(percentPositive=ifelse(is.na(percentPositive),lag(percentPositive),percentPositive))%>%
  mutate(hospDaily=ifelse(is.na(hospDaily),lag(hospDaily),hospDaily))%>%
  mutate(totaltests=ifelse(is.na(totaltests),lag(totaltests),totaltests))%>%
  mutate(percent7dayhospDaily=ifelse(is.na(percent7dayhospDaily),lag(percent7dayhospDaily),percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds=ifelse(is.na(hospAdmissionper100beds),lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
  mutate(positivePer100K=ifelse(is.na(positivePer100K),lag(positivePer100K),positivePer100K))


counties_example_df1<-counties_example_df%>%
  group_by(countyname) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(`countyname`)

counties_example_df2<-counties_example_df1%>%
  group_by(countyname)%>%
  mutate(state=ifelse(is.na(state),lag(state),state))%>%
  mutate(county=ifelse(is.na(county),lag(county),county))%>%
  mutate(percentPositive=ifelse(is.na(percentPositive),lag(percentPositive),percentPositive))%>%
  mutate(hospDaily=ifelse(is.na(hospDaily),lag(hospDaily),hospDaily))%>%
  mutate(totaltests=ifelse(is.na(totaltests),lag(totaltests),totaltests))%>%
  mutate(percent7dayhospDaily=ifelse(is.na(percent7dayhospDaily),lag(percent7dayhospDaily),percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds=ifelse(is.na(hospAdmissionper100beds),lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
  mutate(positivePer100K=ifelse(is.na(positivePer100K),lag(positivePer100K),positivePer100K))


final_hosptest_ts6$county<-as.integer(final_hosptest_ts6$county)
final_hosptest_ts6$nation<-as.integer(final_hosptest_ts6$nation)
our_ts$date<-anydate(our_ts$date)
merged_covidtimeseries <- left_join(our_ts,final_hosptest_ts6)
counties_example_df2$hospAdmissionper100beds<-as.character(counties_example_df2$hospAdmissionper100beds)

variable<-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K")

merged_covidtimeseries[, variable][is.na(merged_covidtimeseries[, variable])] <- ""
counties_example_df2$percentPositive<-as.character(counties_example_df2$percentPositive)
counties_example_df2$hospDaily<-as.character(counties_example_df2$hospDaily)
counties_example_df2$totaltests<-as.character(counties_example_df2$totaltests)
counties_example_df2$percent7dayhospDaily<-as.character(counties_example_df2$percent7dayhospDaily)
counties_example_df2$hospAdmissionper100beds<-as.character(counties_example_df2$hospAdmissionper100beds)

counties_example_df2$positivePer100K<-as.character(counties_example_df2$positivePer100K)

merged_covidtimeseries$percentPositive<-as.character(merged_covidtimeseries$percentPositive)
merged_covidtimeseries$hospDaily<-as.character(merged_covidtimeseries$hospDaily)
merged_covidtimeseries$totaltests<-as.character(merged_covidtimeseries$totaltests)
merged_covidtimeseries$percent7dayhospDaily<-as.character(merged_covidtimeseries$percent7dayhospDaily)
merged_covidtimeseries$hospAdmissionper100beds<-as.character(merged_covidtimeseries$hospAdmissionper100beds)
merged_covidtimeseries$positivePer100K<-as.character(merged_covidtimeseries$positivePer100K)

merged_covidtimeseries1 <- left_join(merged_covidtimeseries, counties_example_df2, by = c("date", "state", "county"))%>% 
  transmute(
    date = date,
    state = state,
    county = county,
    hospDaily = coalesce(hospDaily.y, hospDaily.x),
    totaltests = coalesce(totaltests.y, totaltests.x),
    percentPositive = coalesce(percentPositive.y, percentPositive.x),
    percent7dayhospDaily=coalesce(percent7dayhospDaily.y,percent7dayhospDaily.x),
    hospAdmissionper100beds=coalesce(hospAdmissionper100beds.y,hospAdmissionper100beds.x),
    positivePer100K=coalesce(positivePer100K.y,positivePer100K.x)
  )

covidtimeseries2 <- merged_covidtimeseries%>%
  dplyr::select(-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K"))
covidtimeseries2<-left_join(merged_covidtimeseries1,covidtimeseries2)

x<-covidtimeseries2[covidtimeseries2$nation==1,] %>% 
  distinct()
x<-x[-1,]
y<-subset(covidtimeseries2,is.na(nation))
covidtimeseries2<-rbind(y,x)

covidtimeseries2$nation[is.na(covidtimeseries2$nation)] <- ""
covidtimeseries2$nation[covidtimeseries2$nation == TRUE] <- "1"
covidtimeseries2$state[is.na(covidtimeseries2$state)] <- ""
covidtimeseries2$county[is.na(covidtimeseries2$county)] <- ""

covidtimeseries2$cases[is.na(covidtimeseries2$cases)] <- -1
covidtimeseries2$deaths[is.na(covidtimeseries2$deaths)] <- -1
covidtimeseries2$caserate[is.na(covidtimeseries2$caserate)] <- -1
covidtimeseries2$covidmortality[is.na(covidtimeseries2$covidmortality)] <- -1
covidtimeseries2$dailycases[is.na(covidtimeseries2$dailycases)] <- -1
covidtimeseries2$dailydeaths[is.na(covidtimeseries2$dailydeaths)] <- -1
covidtimeseries2$caserate7dayfig[is.na(covidtimeseries2$caserate7dayfig)] <- -1
covidtimeseries2$covidmortality7dayfig[is.na(covidtimeseries2$covidmortality7dayfig)] <- -1
covidtimeseries2$mean7daycases[is.na(covidtimeseries2$mean7daycases)] <- -1
covidtimeseries2$mean7daydeaths[is.na(covidtimeseries2$mean7daydeaths)] <- -1

covidtimeseries2$totaltests[is.na(covidtimeseries2$totaltests)] <- -1
covidtimeseries2$hospDaily[is.na(covidtimeseries2$hospDaily)] <- -1
covidtimeseries2$cfr[is.na(covidtimeseries2$cfr)] <- -1
covidtimeseries2$positivePer100K[is.na(covidtimeseries2$positivePer100K)] <- -1
covidtimeseries2$percentPositive[is.na(covidtimeseries2$percentPositive)] <- -1
covidtimeseries2$percent14dayDailyCases[is.na(covidtimeseries2$percent14dayDailyCases)] <- -999
covidtimeseries2$percent14dayDailyDeaths[is.na(covidtimeseries2$percent14dayDailyDeaths)] <- -999
covidtimeseries2$percent7dayhospDaily[is.na(covidtimeseries2$percent7dayhospDaily)] <- -999


###############  REGION DATA TO COVID TIME SERIES  #################

stateNames <- data.frame(stateNames)

setwd(onedrive)
regiondata <- read.csv("./Data/Processed/stateregiondata.csv")

stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  dplyr::select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))
library(plyr)
final_merged_covidtimeseries <- join(covidtimeseries2,stateregions,by="state") %>% dplyr::select(-statename1)
names(final_merged_covidtimeseries)
final_merged_covidtimeseries <- final_merged_covidtimeseries %>% 
  dplyr::select(date,nation,state,statename,county,countyname,region,division,Population,X_013_Urbanization,X_013_Urbanization_Code,urbanrural,everything())


names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization_Code"] <- "_013_Urbanization_Code"

################### National Level Hospitalization Data ###############

#national
final_merged_covidtimeseries1<-subset(final_merged_covidtimeseries,nation==1)
final_merged_covidtimeseries1<-final_merged_covidtimeseries1[!duplicated(final_merged_covidtimeseries1),]
#state level
final_merged_covidtimeseries2<-subset(final_merged_covidtimeseries,!statename=="")

detach("package:plyr", unload = TRUE)
final_merged_covidtimeseries2$percentPositive<-as.numeric(final_merged_covidtimeseries2$percentPositive)
final_merged_covidtimeseries2$hospDaily<-as.numeric(final_merged_covidtimeseries2$hospDaily)
final_merged_covidtimeseries2$totaltests<-as.numeric(final_merged_covidtimeseries2$totaltests)
final_merged_covidtimeseries2$percent7dayhospDaily<-as.numeric(final_merged_covidtimeseries2$percent7dayhospDaily)
final_merged_covidtimeseries2$hospAdmissionper100beds<-as.numeric(final_merged_covidtimeseries2$hospAdmissionper100beds)
final_merged_covidtimeseries2$positivePer100K<-as.numeric(final_merged_covidtimeseries2$positivePer100K)

final_merged_covidtimeseries3<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(percentPositive=mean(percentPositive,na.rm=TRUE))
final_merged_covidtimeseries3<-final_merged_covidtimeseries3[,c(1,15)]
final_merged_covidtimeseries3<-final_merged_covidtimeseries3[!duplicated(final_merged_covidtimeseries3), ]
final_merged_covidtimeseries1$percentPositive=final_merged_covidtimeseries3$percentPositive
final_merged_covidtimeseries4<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(hospDaily=sum(hospDaily,na.rm=TRUE))
final_merged_covidtimeseries4<-final_merged_covidtimeseries4[,c(1,13)]
final_merged_covidtimeseries4<-final_merged_covidtimeseries4[!duplicated(final_merged_covidtimeseries4), ]
final_merged_covidtimeseries1$hospDaily=final_merged_covidtimeseries4$hospDaily
final_merged_covidtimeseries5<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(totaltests=sum(totaltests,na.rm=TRUE))
final_merged_covidtimeseries5<-final_merged_covidtimeseries5[,c(1,14)]
final_merged_covidtimeseries5<-final_merged_covidtimeseries5[!duplicated(final_merged_covidtimeseries5), ]
final_merged_covidtimeseries1$totaltests=final_merged_covidtimeseries5$totaltests
final_merged_covidtimeseries2<-final_merged_covidtimeseries2%>%mutate(percent7dayhospDaily=ifelse(percent7dayhospDaily==-999.00,0,percent7dayhospDaily))
final_merged_covidtimeseries6<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(percent7dayhospDaily=mean(percent7dayhospDaily,na.rm=TRUE))
final_merged_covidtimeseries6<-final_merged_covidtimeseries6[,c(1,16)]
final_merged_covidtimeseries6<-final_merged_covidtimeseries6[!duplicated(final_merged_covidtimeseries6), ]
final_merged_covidtimeseries1$percent7dayhospDaily=final_merged_covidtimeseries6$percent7dayhospDaily
final_merged_covidtimeseries7<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(hospAdmissionper100beds=mean(hospAdmissionper100beds,na.rm=TRUE))
final_merged_covidtimeseries7<-final_merged_covidtimeseries7[,c(1,17)]
final_merged_covidtimeseries7<-final_merged_covidtimeseries7[!duplicated(final_merged_covidtimeseries7), ]
final_merged_covidtimeseries1$hospAdmissionper100beds=final_merged_covidtimeseries7$hospAdmissionper100beds
final_merged_covidtimeseries8<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(positivePer100K=mean(positivePer100K,na.rm=TRUE))
final_merged_covidtimeseries8<-final_merged_covidtimeseries8[,c(1,18)]
final_merged_covidtimeseries8<-final_merged_covidtimeseries8[!duplicated(final_merged_covidtimeseries8), ]
final_merged_covidtimeseries1$positivePer100K=final_merged_covidtimeseries8$positivePer100K


final_merged_covidtimeseries1$percentPositive[is.nan(final_merged_covidtimeseries1$percentPositive)]<--1
final_merged_covidtimeseries1$hospDaily[is.nan(final_merged_covidtimeseries1$hospDaily)]<--1
final_merged_covidtimeseries1$totaltests[is.nan(final_merged_covidtimeseries1$totaltests)]<--1
final_merged_covidtimeseries1$percent7dayhospDaily[is.nan(final_merged_covidtimeseries1$percent7dayhospDaily)]<--1
final_merged_covidtimeseries1$hospAdmissionper100beds[is.nan(final_merged_covidtimeseries1$hospAdmissionper100beds)]<--1
final_merged_covidtimeseries1$positivePer100K[is.nan(final_merged_covidtimeseries1$positivePer100K)]<--1

final_merged_covidtimeseries2<-subset(final_merged_covidtimeseries,nation=="")
final_merged_covidtimeseries<-rbind(final_merged_covidtimeseries2,final_merged_covidtimeseries1)
#x<-subset(final_merged_covidtimeseries,nation==1)

final_merged_covidtimeseries$percentPositive <- ifelse(final_merged_covidtimeseries$percentPositive == "", -1, final_merged_covidtimeseries$percentPositive)
final_merged_covidtimeseries$hospDaily<-ifelse(final_merged_covidtimeseries$hospDaily == "", -1, final_merged_covidtimeseries$hospDaily)
final_merged_covidtimeseries$totaltests<-ifelse(final_merged_covidtimeseries$totaltests == "", -1, final_merged_covidtimeseries$totaltests)
final_merged_covidtimeseries$percent7dayhospDaily<-ifelse(final_merged_covidtimeseries$percent7dayhospDaily == "", -1, final_merged_covidtimeseries$percent7dayhospDaily)
final_merged_covidtimeseries$hospAdmissionper100beds<-ifelse(final_merged_covidtimeseries$hospAdmissionper100beds == "", -1, final_merged_covidtimeseries$hospAdmissionper100beds)
final_merged_covidtimeseries$positivePer100K<-ifelse(final_merged_covidtimeseries$positivePer100K == "", -1, final_merged_covidtimeseries$positivePer100K)

final_merged_covidtimeseries<-subset(final_merged_covidtimeseries,!is.na(date))


county<-subset(final_merged_covidtimeseries,!countyname=="")
rest<-subset(final_merged_covidtimeseries,countyname=="")

#load all the packages again
library(pacman)

pacman::p_load(
  base, tidyverse, plyr, dplyr, tidyr, readr, data.table,
  ggplot2, tibble, lubridate, cdlTools, openintro, gtools,
  rvest, DescTools, readxl, stringr, naniar, zoo, mice, anytime
)

final_merged_covidtimeseries1<-county%>%group_by(countyname,date)%>%
  mutate(percentPositive1=lag(percentPositive))%>%
  mutate(hospDaily1=lag(hospDaily))%>%
  mutate(totaltests1=lag(totaltests))%>%
  mutate(percent7dayhospDaily1=lag(percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds1=lag(hospAdmissionper100beds))%>%
  mutate(positivePer100K1=lag(positivePer100K))

final_merged_covidtimeseries2<-rest%>%group_by(statename,date)%>%
  mutate(percentPositive1=lag(percentPositive))%>%
  mutate(hospDaily1=lag(hospDaily))%>%
  mutate(totaltests1=lag(totaltests))%>%
  mutate(percent7dayhospDaily1=lag(percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds1=lag(hospAdmissionper100beds))%>%
  mutate(positivePer100K1=lag(positivePer100K))

county$percentPositive[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$percentPositive1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$hospDaily[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$hospDaily1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$totaltests[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$totaltests1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$percent7dayhospDaily[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$percent7dayhospDaily1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$hospAdmissionper100beds[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$hospAdmissionper100beds1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$positivePer100K[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$positivePer100K1[final_merged_covidtimeseries1$date=="2021-04-03"]


rest$percentPositive[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$percentPositive1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$hospDaily[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$hospDaily1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$totaltests[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$totaltests1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$percent7dayhospDaily[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$percent7dayhospDaily1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$hospAdmissionper100beds[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$hospAdmissionper100beds1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$positivePer100K[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$positivePer100K1[final_merged_covidtimeseries2$date=="2021-04-03"]



final_merged_covidtimeseries_x<-rbind(rest,county)
nation<-subset(final_merged_covidtimeseries_x,nation==1)
not_nation<-subset(final_merged_covidtimeseries_x,nation=="")

not_nation<-not_nation[
  with(not_nation, order(state,county)),
]
final_merged_covidtimeseries_x<-rbind(not_nation,nation)


final<-function(data){
  final_merged_covidtimeseries_x<-data%>%
    dplyr::group_by(countyname)%>%
    dplyr::mutate(percentPositive=ifelse(percentPositive==-1,lag(percentPositive),percentPositive))%>%
    dplyr::mutate(hospDaily=ifelse(hospDaily==-1,lag(hospDaily),hospDaily))%>%
    dplyr::mutate(totaltests=ifelse(totaltests==-1,lag(totaltests),totaltests))%>%
    dplyr::mutate(percent7dayhospDaily=ifelse(percent7dayhospDaily==-1,lag(percent7dayhospDaily),percent7dayhospDaily))%>%
    dplyr::mutate(hospAdmissionper100beds=ifelse(hospAdmissionper100beds==-1,lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
    dplyr::mutate(positivePer100K=ifelse(positivePer100K==-1,lag(positivePer100K),positivePer100K))
  return(final_merged_covidtimeseries_x)
}


x<-Sys.Date()-1
cpr_date<-regmatches(df, gregexpr("[[:digit:]]+", df))
cpr_date<-ymd(cpr_date)  
y<-as.numeric(x-cpr_date)


m<-Reduce(function(x, ign) final(x), 1:y, init = final_merged_covidtimeseries_x, accumulate = TRUE)
trial<-m[y+1]
trial<-as.data.frame(trial)

#################  MERGING HOSPITALIZATION DATA TO NATIONALRAW  ######################
setwd(onedrive)
our_static <- read.csv("./Data/Upload/nationalraw0.csv")

final_hosptest_ts7<-final_hosptest_ts6%>%
  filter(date==max(final_hosptest_ts6$date)) 

our_static$date<-anydate(our_static$date)
final_hosptest_ts7$percentPositive<-as.character(final_hosptest_ts7$percentPositive)
final_hosptest_ts7$hospDaily<-as.character(final_hosptest_ts7$hospDaily)
final_hosptest_ts7$totaltests<-as.character(final_hosptest_ts7$totaltests)
final_hosptest_ts7$percent7dayhospDaily<-as.character(final_hosptest_ts7$percent7dayhospDaily)


merged_nationalraw <- left_join(our_static,final_hosptest_ts7)
merged_nationalraw$date<-anydate(merged_nationalraw$date)

merged_nationalraw$hospDaily<-as.character(merged_nationalraw$hospDaily)
merged_nationalraw$percentPositive<-as.character(merged_nationalraw$percentPositive)
merged_nationalraw$totaltests<-as.character(merged_nationalraw$totaltests)
merged_nationalraw$percent7dayhospDaily<-as.character(merged_nationalraw$percent7dayhospDaily)
merged_nationalraw$hospAdmissionper100beds<-as.character(merged_nationalraw$hospAdmissionper100beds)
merged_nationalraw$positivePer100K<-as.character(merged_nationalraw$positivePer100K)
variable<-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K")
counties_example_df3<-counties_example_df2%>%
  filter(date==max(counties_example_df2$date))
counties_example_df3<-counties_example_df3[,-c(2)]
merged_nationalraw1 <- left_join(merged_nationalraw, counties_example_df3, by = c("state", "county"))%>% 
  transmute(
    date = date,
    state = state,
    county = county,
    hospDaily = coalesce(hospDaily.y, hospDaily.x),
    totaltests = coalesce(totaltests.y, totaltests.x),
    percentPositive = coalesce(percentPositive.y, percentPositive.x),
    percent7dayhospDaily=coalesce(percent7dayhospDaily.y,percent7dayhospDaily.x),
    hospAdmissionper100beds=coalesce(hospAdmissionper100beds.y,hospAdmissionper100beds.x),
    positivePer100K=coalesce(positivePer100K.y,positivePer100K.x)
  )

merged_nationalraw2 <- merged_nationalraw%>%
  dplyr::select(-variable)

merged_nationalraw2 <- left_join(merged_nationalraw2, merged_nationalraw1, by = c("date", "state", "county"))


merged_nationalraw2[, variable == ""] <- NA
merged_nationalraw2[, variable][is.na(merged_nationalraw2[, variable])] <- -1

rm(our_static)

########## REGION DATA TO NATIONALRAW ############

stateNames <- data.frame(stateNames)

regiondata <- read.csv("./Data/Processed/stateregiondata.csv")

stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  dplyr::select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))

library(plyr)

merge <- join(merged_nationalraw2,stateregions,by="state") %>% dplyr::select(-statename1)

final_merged_nationalraw <- merge %>% 
  dplyr::select(date,nation,state,statename,county,countyname,region,Population,X_2013_Urbanization_Code,everything())

########## Merge CDC Chronic Condition Data to Nationalraw ############
setwd(onedrive)
Chronic_Conditions<-read_excel("./Data/Raw/cdc_90519_DS1.xlsx")

Chronic_Conditions$CNTY_FIPS = str_remove(Chronic_Conditions$CNTY_FIPS, "^0+")
names(Chronic_Conditions)[names(Chronic_Conditions) == "CNTY_FIPS"] <- "county"
names(Chronic_Conditions)[names(Chronic_Conditions) == "STATE_FIPS"] <- "state"
Chronic_Conditions$county<-as.integer(Chronic_Conditions$county)

Chronic_Conditions$state = str_remove(Chronic_Conditions$state, "^0+")

final_merged_nationalraw<-join(final_merged_nationalraw,Chronic_Conditions)


names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18.and.older"] <- "countyPop201818andolder"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Lower 95% CI"] <- "anyconditionLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Upper 95% CI"] <- "anyconditionUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_number"] <- "anyconditionNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_prevalence"] <- "obesityPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Lower 95% CI"] <- "obesityLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Upper 95% CI"] <- "obesityUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_number"] <- "obesityNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_prevalence"] <- "heartdiseasePrevalence"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Lower 95% CI"] <- "heartdiseaseLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Upper 95% CI"] <- "heartdiseaseUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_number"] <- "heartdiseaseNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_prevalence"] <- "copdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Lower 95% CI"] <- "copdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Upper 95% CI"] <- "copdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_number"] <- "copdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_prevalence"] <- "diabetesPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Lower 95% CI"] <- "diabetesLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Upper 95% CI"] <- "diabetesUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_number"] <- "diabetesNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_prevalence"] <- "ckdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Lower 95% CI"] <- "ckdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Upper 95% CI"] <- "ckdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_number"] <- "ckdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18 and older"] <- "countyPop201818andolder"


final_merged_nationalraw<-final_merged_nationalraw %>%
  dplyr::select(- c(COUNTY_NAME, STATE_NAME, FIPS,Urban_rural_code,STAB,anyconditionLower95CI,anyconditionUpper95CI,obesityLower95CI,obesityUpper95CI,heartdiseaseLower95CI,heartdiseaseUpper95CI,copdLower95CI,copdUpper95CI,diabetesLower95CI,diabetesUpper95CI,ckdLower95CI,ckdUpper95CI,diabetes,obesity))


##################  RENAMING URBANIZATION COLUMNS BEFORE EXPORTING  ################  


rm(merged_nationalraw,black,age65over,diabetes,groupquater,hhincome,hispanic,male,merge,minority,natives,obesity,poverty,regiontoNR,stateNames,stateregions,regiondata,outcomes, ourraw)


################### ***UNLOCK TO EXPORT*** ###############
# setwd(onedrive)
# write.csv(final_merged_nationalraw,"./Data/Upload/nationalraw.csv", na="", row.names=F)

# reset directory
setwd(local)
write.csv(final_merged_nationalraw,"./nationalraw.csv", na="", row.names=F)


########## NATIONAL REPORT: Highest 10 BAR CHARTS ###################

## Highest 10 CASES CHANGE 

setwd(local)
highcases <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(caserate7day=round(caserate7day),date=as.Date(date,origin = "1899-12-30")) %>%
  dplyr::select(date,countyname,caserate7day)


Highest10cases <- highcases %>% 
  arrange(desc(caserate7day)) %>% 
  slice(1:10) %>% 
  mutate(variable = "caserate7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = caserate7day)



## Highest 10 DEATHS CHANGE 

setwd(local)
highdeaths <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(covidmortality7day=round(covidmortality7day),date=as.Date(date,origin = "1899-12-30")) %>%
  dplyr::select(date,countyname,covidmortality7day)


Highest10deaths <- highdeaths %>% 
  arrange(desc(covidmortality7day)) %>%
  slice(1:10) %>%
  mutate(variable = "covidmortality7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = covidmortality7day)


# MERGE THE DATASET
merge <- full_join(Highest10cases,Highest10deaths) %>% dplyr::select(date,countyname,variable,measure,rank)



################### ***UNLOCK TO EXPORT*** ###############
setwd(onedrive)
write.csv(merge,"./Data/Upload/Highest10barchart.csv",row.names=FALSE,na="")

# KEEP THE GLOBAL ENVIRONMENT CLEAN
rm(highcases,highdeaths,Highest10cases,Highest10deaths,merge)



########### NATIONAL REPORT: TRENDLINE PLOTS ####################

# CASE RATE 
setwd(local)
raw <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  dplyr::select(countyname,percent14dayDailyCases) %>%
  arrange(desc(percent14dayDailyCases)) %>% slice(1:10) %>% 
  mutate(rank = seq(1:10)) %>% 
  dplyr::select(-percent14dayDailyCases)


#? CHECK--------
data <- trial %>% 
  left_join(raw) %>% 
  filter(!rank=="") %>%
  dplyr::select(date,countyname,rank,caserate7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "caserate7day") %>%
  dplyr::rename(measure = caserate7day)


# DEATH RATE 

setwd(local)
death <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  dplyr::select(countyname,percent14dayDailyDeaths) %>%
  arrange(desc(percent14dayDailyDeaths)) %>% slice(1:5) %>% 
  mutate(rank = seq(1:5)) %>% 
  dplyr::select(-percent14dayDailyDeaths)



deathraw <- trial %>% 
  left_join(death) %>% 
  filter(!rank=="") %>%
  dplyr::select(date,countyname,rank,covidmortality7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "covidmortality7day") %>%
  dplyr::rename(measure = covidmortality7day)


# MERGE DATA 
# ?CHECK------- 
merge <- full_join(data,deathraw) %>% dplyr::select(date,countyname,variable,measure,rank)


################### ***UNLOCK TO EXPORT*** ###############
setwd(onedrive)
write.csv(merge,"./Data/Upload/Highest10trendlines.csv",row.names=FALSE,na="")

######################  MERGING CVI SCORE  ######################
setwd(onedrive)
ccvi_states <- openxlsx::read.xlsx("./Data/Raw/surgo_ccvi_with_raw_public_indicators/ccvi.xlsx",sheet=1) %>%
  dplyr::rename(statename=stateName,state=FIPS) %>%
  mutate(statename=str_to_title(statename), countyname="", countycode=NA)

statecode <- ccvi_states %>% dplyr::select(statename,state)

ccvi_county <- openxlsx::read.xlsx("./Data/Raw/surgo_ccvi_with_raw_public_indicators/ccvi.xlsx",sheet=2) %>%
  dplyr::rename(statename=stateName,countyname=countyName,countycode=FIPS) %>%
  mutate(statename=str_to_title(statename)) %>% join(statecode)

CVI <- full_join(ccvi_county,ccvi_states) %>% dplyr::select(-countyname)


CVI$statename[!is.na(CVI$countycode)] <- ""

setwd(local)
nationalraw <- read.csv("./nationalraw.csv")

CVImerged <- join(nationalraw,CVI)


################## ***UNLOCK TO EXPORT*** ###############
setwd(local)
write.csv(CVImerged,"./nationalraw.csv",row.names=FALSE,na="")


# keep the global environment clean
rm(ccvi_states,statecode,ccvi_county,CVI,nationalraw)

#######################  MERGING RESIDENTIAL SEGREGATION SCORE  ######################
setwd(onedrive)
resseg <- read.csv("./Data/Processed/residSeg.csv") 

setwd(local)
nationalraw <- read.csv("./nationalraw.csv")
# merge with nationalraw
ressegToNR<- left_join(nationalraw,resseg)
ressegToNR$X_2013_Urbanization_Code  <-  factor(ifelse(ressegToNR$X_2013_Urbanization=="",-1,
                                                       ifelse(ressegToNR$X_2013_Urbanization=="Large Central Metro",6,
                                                              ifelse(ressegToNR$X_2013_Urbanization=="Large Fringe Metro",5,
                                                                     ifelse(ressegToNR$X_2013_Urbanization=="Medium Metro",4,
                                                                            ifelse(ressegToNR$X_2013_Urbanization=="Small Metro",3,
                                                                                   ifelse(ressegToNR$X_2013_Urbanization=="Micropolitan (Nonmetro)",2,1)))))))

ressegToNR$region_Code  <- factor(ifelse(ressegToNR$region=="",-1,
                                         ifelse(ressegToNR$region=="South",1,
                                                ifelse(ressegToNR$region=="West",2,
                                                       ifelse(ressegToNR$region=="Northeast",3,4)))))




################### ***UNLOCK TO EXPORT*** ###############

setwd(local)
write.csv(ressegToNR,"./nationalraw.csv",row.names=FALSE,na="")




########### HOW MANY STATES CONTRIBUTED? ############

setwd(local)
raw <- ressegToNR%>% 
  filter(nation=="1"|county==""|!statename=="") %>% 
  dplyr::select(date,state,nation,county,statename,cases,dailycases) %>%
  mutate(date = anydate(date))

raw<-raw%>%
  group_by(statename) %>%
  slice(n()) 

totnationcases <- as.numeric(raw$dailycases[raw$nation=="1"]) 
totalnationdailycases <- totnationcases[!is.na(totnationcases)]

sharedailycases <- raw %>%
  group_by(statename) %>%
  mutate(
    contri = totalnationdailycases*0.5) %>%
  dplyr::select(date,nation,state,statename,dailycases,contri) %>% filter(!nation=="1"|!state=="") %>% arrange(desc(dailycases))

threshold <- max(sharedailycases$contri)

number_of_states <- as.numeric(length(which(cumsum(sharedailycases$dailycases) <= threshold)))

names <- as.data.frame(head(sharedailycases$statename,number_of_states))

contristates <- as_tibble(apply(names,2,paste,collapse=", ")) %>% dplyr::rename(statenames=value)

setwd(onedrive)
jsonlite::write_json(contristates,"./Data/Upload/contristates.json")

rm(number_of_states,names,contristates,sharedailycases,raw,totnationcases,totalnationdailycases,threshold)

############  OUTCOME VS COUNTY CHARACTERISTICS DATA TO NATIONAL RAW #########

setwd(local)
ourraw <- read.csv("./nationalraw.csv")

ourraw$malegroup = quantcut(ourraw$male,5)

ourraw$age65overgroup = quantcut(ourraw$age65over,5)

ourraw$blackgroup = quantcut(ourraw$black,5)

ourraw$povertygroup = quantcut(ourraw$poverty,5)

ourraw$minoritygroup = quantcut(ourraw$minority,5)

ourraw$groupquatergroup = quantcut(ourraw$groupquater,5)

ourraw$hispanicgroup = quantcut(ourraw$hispanic,5)

ourraw$nativesgroup = quantcut(ourraw$natives,5)

ourraw$hhincomegroup = quantcut(ourraw$hhincome,5)

ourraw$cvigroup = quantcut(ourraw$ccvi,5)

ourraw$residseggroup = quantcut(ourraw$RS_blackwhite,5)

ourraw$anyconditiongroup = quantcut(ourraw$anyconditionPrevalence,5)

ourraw$obesity2group = quantcut(ourraw$obesityPrevalence,5)

ourraw$diabetes2group = quantcut(ourraw$diabetesPrevalence,5)

ourraw$heartdiseasegroup = quantcut(ourraw$heartdiseasePrevalence,5)

ourraw$copdgroup = quantcut(ourraw$copdPrevalence,5)

ourraw$ckdgroup = quantcut(ourraw$ckdPrevalence,5)

###############  CASE RATE VS EXPOSURE CHARTS ################

black <- aggregate(cbind(caserate7day,caserate)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male <- aggregate(cbind(caserate7day,caserate)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty <- aggregate(cbind(caserate7day,caserate)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over <- aggregate(cbind(caserate7day,caserate)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority <- aggregate(cbind(caserate7day,caserate)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater <- aggregate(cbind(caserate7day,caserate)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)

hispanic <- aggregate(cbind(caserate7day,caserate)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives <- aggregate(cbind(caserate7day,caserate)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural <- aggregate(cbind(caserate7day,caserate)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)

hhincome <- aggregate(cbind(caserate7day,caserate)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region <- aggregate(cbind(caserate7day,caserate)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi <- aggregate(cbind(caserate7day,caserate)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg <- aggregate(cbind(caserate7day,caserate)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)

anycondition <- aggregate(cbind(caserate7day,caserate)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2 <- aggregate(cbind(caserate7day,caserate)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2 <- aggregate(cbind(caserate7day,caserate)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease <- aggregate(cbind(caserate7day,caserate)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd <- aggregate(cbind(caserate7day,caserate)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd <- aggregate(cbind(caserate7day,caserate)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_pre <- full_join(black,male) %>% full_join(poverty) %>% full_join(age65over) %>%
  full_join(minority) %>% full_join(groupquater)  %>% full_join(hispanic) %>%
  full_join(natives) %>% full_join(urbanrural)  %>% full_join(hhincome) %>% full_join(region) %>%
  full_join(cvi) %>% full_join(residseg) %>% full_join(anycondition) %>% full_join(diabetes2) %>% full_join(obesity2) %>%
  full_join(heartdisease) %>% full_join(copd) %>% full_join(ckd) %>%
  dplyr::select(caserate7day,everything())

outcomes <- gather(outcomes_pre,
                   key="variable",
                   value="measure",
                   c("caserate","caserate7day")) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_pre,black,male,poverty,age65over,minority,groupquater,hispanic,natives,urbanrural,hhincome,region,cvi,residseg)

###############  covidmortality VS EXPOSURE CHARTS ################

black_mort <- aggregate(cbind(covidmortality,covidmortality7day)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male_mort <- aggregate(cbind(covidmortality,covidmortality7day)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty_mort <- aggregate(cbind(covidmortality,covidmortality7day)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over_mort <- aggregate(cbind(covidmortality,covidmortality7day)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority_mort <- aggregate(cbind(covidmortality,covidmortality7day)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater_mort <- aggregate(cbind(covidmortality,covidmortality7day)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)
hispanic_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives_mort <- aggregate(cbind(covidmortality,covidmortality7day)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural_mort <- aggregate(cbind(covidmortality,covidmortality7day)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)
hhincome_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region_mort <- aggregate(cbind(covidmortality,covidmortality7day)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi_mort <- aggregate(cbind(covidmortality,covidmortality7day)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg_mort <- aggregate(cbind(covidmortality,covidmortality7day)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)

anycondition_mort <- aggregate(cbind(covidmortality,covidmortality7day)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease_mort <- aggregate(cbind(covidmortality,covidmortality7day)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_mort_pre <- full_join(black_mort,male_mort) %>% full_join(poverty_mort) %>% full_join(age65over_mort) %>%
  full_join(minority_mort) %>% full_join(groupquater_mort) %>% full_join(hispanic_mort) %>%
  full_join(natives_mort) %>% full_join(urbanrural_mort)  %>% full_join(hhincome_mort) %>% full_join(region_mort) %>% full_join(cvi_mort) %>%
  full_join(residseg_mort) %>% full_join(anycondition_mort) %>% full_join(diabetes2_mort) %>% full_join(obesity2_mort) %>%
  full_join(heartdisease_mort) %>% full_join(copd_mort) %>% full_join(ckd_mort) %>%
  dplyr::select(covidmortality7day,everything())

outcomes_mort <- gather(outcomes_mort_pre,
                        key="variable",
                        value="measure",
                        c("covidmortality","covidmortality7day")) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_mort_pre,ourraw,black_mort,male_mort,poverty_mort,age65over_mort,minority_mort,groupquater_mort,diabetes2_mort,hispanic_mort,natives_mort,urbanrural_mort,obesity_mort,hhincome_mort,region_mort,cvi_mort,residseg_mort)


################### ***JOINING CASERATE AND COVID MORTALITY PLOTS DATA*** ###############

merged <- full_join(outcomes,outcomes_mort)

merged$quintileVar[merged$quintileVa=="CVI"] <- "ccvi"
merged$quintileVar[merged$quintileVa=="any condition"] <- "anycondition"
merged$quintileVar[merged$quintileVa=="diabetes 2018"] <- "diabetes"
merged$quintileVar[merged$quintileVa=="obesity 2018"] <- "obesity"
merged$quintileVar[merged$quintileVa=="heart disease"] <- "heartdisease"
merged$quintileVar[merged$quintileVa=="COPD"] <- "copd"
merged$quintileVar[merged$quintileVa=="CKD"] <- "ckd"

################### ***UNLOCK TO EXPORT*** ###############

setwd(onedrive)
write.csv(merged,"./Data/Upload/lastbarcharts_merged.csv",na="",row.names=FALSE)

rm(outcomes_mort,outcomes)

########### INDICES DATA FROM OXFORD ############
options(timeout=6000)
oxford_indices <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv") %>%
  filter(CountryCode=="USA") %>% 
  transform(date = as.Date(as.character(Date), "%Y%m%d")) %>%
  dplyr::select(date,CountryName,RegionName,StringencyIndex_Average,GovernmentResponseIndex_Average,ContainmentHealthIndex_Average,EconomicSupportIndex)


states_indices <- oxford_indices %>%
  filter(!is.na(StringencyIndex_Average),!is.na(GovernmentResponseIndex_Average),!is.na(ContainmentHealthIndex_Average),!is.na(EconomicSupportIndex)) %>%
  # group_by(RegionName) %>%
  # slice(n()) %>%
  dplyr::rename(statename=RegionName,country=CountryName,stringencyIndex=StringencyIndex_Average,governmentResponseIndex=GovernmentResponseIndex_Average,containmentHealthIndex=ContainmentHealthIndex_Average,economicSupportIndex=EconomicSupportIndex) %>%
  mutate(nation=ifelse(statename=="",1,NA)) %>%
  dplyr::select(-country) %>%
  dplyr::select(date,nation,statename,everything())

setwd(local)
ourraw <- read.csv("./nationalraw.csv")
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_2013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_2013_Urbanization_Code"] <- "_013_Urbanization_Code"

indices_data <- join(ourraw,states_indices)

indices_data<-indices_data %>%
  dplyr::select(-c("urbanrural"))

names(indices_data )[names(indices_data ) == "X_2013_Urbanization"] <- "urbanrural_text"
names(indices_data )[names(indices_data ) == "X_2013_Urbanization_Code"] <- "urbanrural"
names(indices_data )[names(indices_data ) == "RS_blackwhite"] <- "resSeg"
names(indices_data )[names(indices_data ) == "region"] <- "region_text"
names(indices_data )[names(indices_data ) == "region_Code"] <- "region"
names(indices_data )[names(indices_data ) == "diabetesPrevalence"] <- "diabetes"
names(indices_data )[names(indices_data ) == "heartdiseasePrevalence"] <- "heartdisease"
names(indices_data )[names(indices_data ) == "ckdPrevalence"] <- "ckd"
names(indices_data )[names(indices_data ) == "copdPrevalence"] <- "copd"
names(indices_data )[names(indices_data ) == "anyconditionPrevalence"] <- "anycondition"
names(indices_data )[names(indices_data ) == "obesityPrevalence"] <- "obesity"

anycondition_state<-aggregate(anycondition_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
obesity_state<-aggregate(Obesity_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
heartdisease_state<-aggregate(`Heart disease_prevalence` ~ STATE_NAME, Chronic_Conditions, FUN="mean")
COPD_state<-aggregate(COPD_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
diabetes_state<-aggregate(diabetes_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
CKD_state<-aggregate(CKD_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
anycondition_num<-aggregate(anycondition_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
obesity_num<-aggregate(Obesity_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
heartdisease_num<-aggregate(`Heart disease_number` ~ STATE_NAME, Chronic_Conditions, FUN="mean")
COPD_num<-aggregate(COPD_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
diabetes_num<-aggregate(diabetes_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
CKD_num<-aggregate(CKD_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
resSeg<-subset(indices_data,!countyname%in%"")
resSeg_state<-aggregate(resSeg ~ state, resSeg, FUN="mean")
ccvi_state<-aggregate(ccvi ~ state, resSeg, FUN="mean")


chronic_state<-dplyr::left_join(x=anycondition_state,y=obesity_state)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_state)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_state)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_state)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_state)
chronic_state<-dplyr::left_join(x=chronic_state,anycondition_num)
chronic_state<-dplyr::left_join(x=chronic_state,obesity_num)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_num)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_num)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_num)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_num)


names(chronic_state)[names(chronic_state) == "STATE_NAME"] <- "statename"
names(chronic_state)[names(chronic_state) == "diabetes_prevalence"] <- "diabetes"
names(chronic_state)[names(chronic_state) == "Heart disease_prevalence"] <- "heartdisease"
names(chronic_state)[names(chronic_state) == "CKD_prevalence"] <- "ckd"
names(chronic_state )[names(chronic_state) == "COPD_prevalence"] <- "copd"
names(chronic_state)[names(chronic_state) == "anycondition_prevalence"] <- "anycondition"
names(chronic_state)[names(chronic_state) == "Obesity_prevalence"] <- "obesity"

names(chronic_state)[names(chronic_state) == "diabetes_number"] <- "diabetesNumber"
names(chronic_state)[names(chronic_state) == "Heart disease_number"] <- "heartdiseaseNumber"
names(chronic_state)[names(chronic_state) == "CKD_number"] <- "ckdNumber"
names(chronic_state )[names(chronic_state) == "COPD_number"] <- "copdNumber"
names(chronic_state)[names(chronic_state) == "anycondition_number"] <- "anyconditionNumber"
names(chronic_state)[names(chronic_state) == "Obesity_number"] <- "obesityNumber"

chronic_state$state<-fips(chronic_state$statename)
chronic_state$state<-str_remove(chronic_state$state, "^0+")
chronic_state$state<-as.numeric(chronic_state$state)
chronic_state<-dplyr::left_join(x=chronic_state,resSeg_state)
chronic_state<-dplyr::left_join(x=chronic_state,ccvi_state)
chronic_state<-chronic_state[,-14]
variable<-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y","ccvi.y")
indices_data<-indices_data %>% subset(., select=which(!duplicated(names(.)))) 

indices_data$original_order <- 1:nrow(indices_data)

indices_data1 <- merge(indices_data, chronic_state, by = "statename", all = TRUE)

indices_data1<- indices_data1[order(indices_data1$original_order),]
indices_data1<-indices_data1[,c(2:4,1,5:110)]
indices_data=indices_data1

indices_data$anycondition.x <- ifelse(is.na(indices_data$anycondition.x), indices_data$anycondition.y, indices_data$anycondition.x)
names(indices_data)[names(indices_data) == "anycondition.x"] <- "anycondition"
indices_data$anycondition=round(indices_data$anycondition, 2)


indices_data$diabetes.x <- ifelse(is.na(indices_data$diabetes.x), indices_data$diabetes.y, indices_data$diabetes.x)
names(indices_data)[names(indices_data) == "diabetes.x"] <- "diabetes"
indices_data$diabetes=round(indices_data$diabetes, 2)


indices_data$heartdisease.x <- ifelse(is.na(indices_data$heartdisease.x), indices_data$heartdisease.y, indices_data$heartdisease.x)
names(indices_data)[names(indices_data) == "heartdisease.x"] <- "heartdisease"
indices_data$heartdisease=round(indices_data$heartdisease,2)

indices_data$ckd.x <- ifelse(is.na(indices_data$ckd.x), indices_data$ckd.y, indices_data$ckd.x)
names(indices_data)[names(indices_data) == "ckd.x"] <- "ckd"
indices_data$ckd=round(indices_data$ckd,2)

indices_data$copd.x <- ifelse(is.na(indices_data$copd.x), indices_data$copd.y, indices_data$copd.x)
names(indices_data)[names(indices_data) == "copd.x"] <- "copd"
indices_data$copd=round(indices_data$copd,2)

indices_data$obesity.x <- ifelse(is.na(indices_data$obesity.x), indices_data$obesity.y, indices_data$obesity.x)
names(indices_data)[names(indices_data) == "obesity.x"] <- "obesity"
indices_data$obesity=round(indices_data$obesity,2)

indices_data$obesityNumber.x <- ifelse(is.na(indices_data$obesityNumber.x), indices_data$obesityNumber.y, indices_data$obesityNumber.x)
names(indices_data)[names(indices_data) == "obesityNumber.x"] <- "obesityNumber"
indices_data$obesityNumber=round(indices_data$obesityNumber,2)

indices_data$diabetesNumber.x <- ifelse(is.na(indices_data$diabetesNumber.x), indices_data$diabetesNumber.y, indices_data$diabetesNumber.x)
names(indices_data)[names(indices_data) == "diabetesNumber.x"] <- "diabetesNumber"
indices_data$diabetesNumber=round(indices_data$diabetesNumber, 2)

indices_data$heartdiseaseNumber.x <- ifelse(is.na(indices_data$heartdiseaseNumber.x), indices_data$heartdiseaseNumber.y, indices_data$heartdiseaseNumber.x)
names(indices_data)[names(indices_data) == "heartdiseaseNumber.x"] <- "heartdiseaseNumber"
indices_data$heartdiseaseNumber=round(indices_data$heartdiseaseNumber,2)

indices_data$ckdNumber.x <- ifelse(is.na(indices_data$ckdNumber.x), indices_data$ckdNumber.y, indices_data$ckdNumber.x)
names(indices_data)[names(indices_data) == "ckdNumber.x"] <- "ckdNumber"
indices_data$ckdNumber=round(indices_data$ckdNumber,2)

indices_data$copdNumber.x <- ifelse(is.na(indices_data$copdNumber.x), indices_data$copdNumber.y, indices_data$copdNumber.x)
names(indices_data)[names(indices_data) == "copdNumber.x"] <- "copdNumber"
indices_data$copdNumber=round(indices_data$copdNumber,2)

indices_data$anyconditionNumber.x <- ifelse(is.na(indices_data$anyconditionNumber.x), indices_data$anyconditionNumber.y, indices_data$anyconditionNumber.x)
names(indices_data)[names(indices_data) == "anyconditionNumber.x"] <- "anyconditionNumber"
indices_data$anyconditionNumber=round(indices_data$anyconditionNumber, 2)

indices_data$resSeg.x <- ifelse(is.na(indices_data$resSeg.x), indices_data$resSeg.y, indices_data$resSeg.x)
names(indices_data)[names(indices_data) == "resSeg.x"] <- "resSeg"
indices_data$resSeg=round(indices_data$resSeg,2)

names(indices_data)[names(indices_data) == "ccvi.x"] <- "ccvi"
indices_data$ccvi=round(indices_data$ccvi,2)

indices_data<-indices_data%>%dplyr::select(-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y"))

indices_data[which(indices_data$nation == 1),]$anycondition <- mean(chronic_state$anycondition)
indices_data[which(indices_data$nation == 1),]$obesity <- mean(chronic_state$obesity)
indices_data[which(indices_data$nation == 1),]$heartdisease <- mean(chronic_state$heartdisease)
indices_data[which(indices_data$nation == 1),]$copd <- mean(chronic_state$copd)
indices_data[which(indices_data$nation == 1),]$diabetes <- mean(chronic_state$diabetes)
indices_data[which(indices_data$nation == 1),]$ckd <- mean(chronic_state$ckd)
indices_data[which(indices_data$nation == 1),]$anyconditionNumber <- mean(chronic_state$anyconditionNumber )
indices_data[which(indices_data$nation == 1),]$obesityNumber  <- mean(chronic_state$obesityNumber )
indices_data[which(indices_data$nation == 1),]$heartdiseaseNumber  <- mean(chronic_state$heartdiseaseNumber )
indices_data[which(indices_data$nation == 1),]$copdNumber  <- mean(chronic_state$copdNumber )
indices_data[which(indices_data$nation == 1),]$diabetesNumber  <- mean(chronic_state$diabetesNumber )
indices_data[which(indices_data$nation == 1),]$ckdNumber  <- mean(chronic_state$ckdNumber )
indices_data[which(indices_data$nation == 1),]$resSeg  <- mean(resSeg_state$resSeg )
indices_data[which(indices_data$nation == 1),]$ccvi  <- mean(ccvi_state$ccvi )


##########Vaccination Data####################
setwd(local)
VaccineTracker<-read.csv("./VaccineTrackertimeseries1.csv")
vaccination_historic <- read.csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD")

names(vaccination_historic)[names(vaccination_historic) == "Location"] <- "statename"
us <- subset(vaccination_historic, statename == "US")
library(usdata)
vaccination_historic$statename <- abbr2state(vaccination_historic$statename)
vaccination_historic <- subset(vaccination_historic, !is.na(statename))
vaccination_historic <- rbind(vaccination_historic, us)
vaccination_historic$statename[vaccination_historic$statename=="US"] <- "United States"
vaccination_historic<-vaccination_historic[  
  with(vaccination_historic, order(statename, Date)),]
vaccination_historic1 <- vaccination_historic[, (colnames(vaccination_historic) %in% colnames(VaccineTracker))]
vaccination_historic1$FIPS<-fips(vaccination_historic1$statename)
vaccination_historic1<-subset(vaccination_historic1,!statename%in%"American Samoa"& !statename%in%"Bureau of Prisons"& !statename%in%"Dept of Defense"& !statename%in%"Federated States of Micronesia"& !statename%in%"Guam"& !statename%in%"Indian Health Svc"& !statename%in%"Marshall Islands"& !statename%in%"Northern Mariana Islands"& !statename%in%"Puerto Rico"& !statename%in%"Republic of Palau"& !statename%in%"Veterans Health"& !statename%in%"Long Term Care"& !statename%in%"Virgin Islands")

vaccination_historic1<-vaccination_historic1[!duplicated(vaccination_historic1),]
vaccination_historic1 <- vaccination_historic1[,c(1:2,26,3:25)]
# vaccination_historic1 <- vaccination_historic1[,c(1:2,26,3:25)]
# date state fips Distributed_janseen_series_complete_unk

# extract: dose1 recipent, dose1 percent
# census: census2019 doses_dis dose_admi dose2 percent
extract <- vaccination_historic[ ,c("Date", "statename","Administered_Dose1_Recip","Administered_Dose1_Pop_Pct")]
census <- VaccineTracker[,c("Date", "statename", "Census2019", "Doses_Distributed", "Doses_Administered", "percentVaccinatedDose2")]

vaccination_historic1$Date <- as.Date(anytime(vaccination_historic1$Date))

census$Date <- as.Date(anytime(census$Date))
extract$Date <- as.Date(anytime(extract$Date))
census<-census[  
  with(census, order(statename, Date)),]
extract<-extract[  
  with(extract, order(statename, Date)),]
#census <- census[!duplicated(census),]
vaccination_historic2 <- left_join(vaccination_historic1, census)
vaccination_historic2 <- left_join(vaccination_historic2, extract)
names(vaccination_historic2)[32]<-"percentVaccinatedDose1"

names(vaccination_historic2)[31]<-"Administered_Dose1"
#vaccination_historic2$Date <- as.Date(anytime(vaccination_historic2$Date))
a<-names(VaccineTracker)
b<-names(vaccination_historic2)
setdiff(b,a)
setdiff(a,b)

install.packages("RJSONIO")
install.packages("RCurl")
library(RJSONIO)
library(RCurl)
today<-RCurl::getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")
casesdata <- jsonlite::fromJSON(today)[[2]] 
add_today <- casesdata[,c(1,4,6,7,16)]
names(add_today)[2] <- "statename"

names(add_today)[5] <- "percentVaccinatedDose2"
add_today<-subset(add_today,!statename%in%"American Samoa"& !statename%in%"Bureau of Prisons"& !statename%in%"Dept of Defense"& !statename%in%"Federated States of Micronesia"& !statename%in%"Guam"& !statename%in%"Indian Health Svc"& !statename%in%"Marshall Islands"& !statename%in%"Northern Mariana Islands"& !statename%in%"Puerto Rico"& !statename%in%"Republic of Palau"& !statename%in%"Veterans Health"& !statename%in%"Long Term Care"& !statename%in%"Virgin Islands")
add_today<-add_today[!duplicated(add_today),]
vaccination_historic2<-vaccination_historic2[  
  with(vaccination_historic2, order(statename, Date)),]
#vaccination_historic2 <- vaccination_historic2[,-c(31:32)]

# get the week date and the update date 
add <- subset(vaccination_historic2, Date == max(vaccination_historic2$Date))
add$FIPS <- fips(add$statename)
add$FIPS[is.na(add$FIPS)] <- "_nation"
add <- add[,-c(27:30)]
add_today$Date <- as.Date(add_today$Date)

add1 <- left_join(add,add_today)

detach("package:plyr", unload = TRUE)
pop <- VaccineTracker[,c(2,27)]
pop <- pop[!duplicated(pop$statename),]
add1 <- left_join(add1,pop)
a<-names(add1)
b<-names(vaccination_historic2)
setdiff(b,a)
setdiff(a,b)

# check the time---------------
vaccination_historic2 <- subset(vaccination_historic2, Date != max(vaccination_historic2$Date))
vaccination_historic3 <- rbind(vaccination_historic2, add1)
vaccination_historic3<-vaccination_historic3[  
  with(vaccination_historic3, order(statename, Date)),]


vaccination_historic4<-vaccination_historic3%>%group_by(statename) %>%
  mutate(Dist_Per_100K_new = Dist_Per_100K - lag(Dist_Per_100K, default = 0))%>%
  mutate(Dist_Per_100K_new=ifelse (Dist_Per_100K_new<0,0,Dist_Per_100K_new))

vaccination_historic4<-vaccination_historic4[
  with(vaccination_historic4, order(statename, Date)),
]
vaccination_historic4$Dist_Per_100K_new=as.numeric(vaccination_historic4$Dist_Per_100K_new)

vaccination_historic5<-vaccination_historic4%>%group_by(statename) %>%
  mutate(Dist_new = Doses_Distributed - lag(Doses_Distributed, default = 0))%>%
  mutate(Dist_new=ifelse (Dist_new<0,0,Dist_new))

vaccination_historic5<-vaccination_historic5[
  with(vaccination_historic5, order(statename, Date)),
]

vaccination_historic5<-vaccination_historic5%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0|is.na(Dist_new),lag(Date),Date)) %>%
  mutate(distDate = as.Date(distDate, origin ="1970-01-01"))%>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0|is.na(Dist_new),lag(Dist_new),Dist_new))

vaccination_historic5<-vaccination_historic5[!duplicated(vaccination_historic5),]

vaccination_historic5<- vaccination_historic5 %>% 
  distinct(Date,statename, .keep_all = TRUE)

vaccination_historic5$FIPS<-str_remove(vaccination_historic5$FIPS, "^0+")

vaccination_historic5$AdministeredPartial<-vaccination_historic5$Administered_Dose1-vaccination_historic5$Series_Complete_Yes
vaccination_historic5$PercentAdministeredPartial<-vaccination_historic5$AdministeredPartial/vaccination_historic5$Census2019*100
vaccination_historic5$PercentAdministeredPartial<-round(vaccination_historic5$PercentAdministeredPartial,1)
vaccination_historic5$percentReceived<-vaccination_historic5$Doses_Administered/vaccination_historic5$Doses_Distributed
vaccination_historic5$percentReceived<-round(vaccination_historic5$percentReceived,1)
vaccination_historic5$PercentAdministeredPartial<-round(vaccination_historic5$PercentAdministeredPartial,1)
vaccination_historic5$percentReceived<-round(vaccination_historic5$percentReceived,1)

vaccination_historic5$percentVaccinatedDose2[vaccination_historic5$percentVaccinatedDose2>100] <- NA

vaccination_historic6<-vaccination_historic5%>%
  dplyr::group_by(statename)%>%
  dplyr::mutate(percentVaccinatedDose2_avg7 = lag(percentVaccinatedDose2, 7, fill = NA))


vaccination_historic6$percentVaccinatedDose2_avg7<-vaccination_historic6$percentVaccinatedDose2_avg7
vaccination_historic6$percentVaccinatedDose2_avg7<-ifelse(is.na(vaccination_historic6$percentVaccinatedDose2_avg7),-999,vaccination_historic6$percentVaccinatedDose2_avg7)
vaccination_historic6$percentVaccinatedDose2_avg7<-round(vaccination_historic6$percentVaccinatedDose2_avg7,2)
vaccination_historic6$FIPS[is.na(vaccination_historic6$FIPS)] <-"_nation"


#age population by state
data <- read.csv("https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD")

# census
census_pop <- data[,c("Recip_County", "Recip_State", "Census2019",  "Census2019_5PlusPop", "Census2019_12PlusPop", "Census2019_18PlusPop", "Census2019_65PlusPop")]
census_pop <- census_pop[!duplicated(census_pop),]
names(census_pop)[1] <- "countyname"
names(census_pop)[2] <- "statename"

census_pop$statename <- abbr2state(census_pop$statename)
census_pop1<- subset(census_pop, statename%in%VaccineTracker$statename)
census_pop1<-census_pop1[
  with(census_pop1, order(statename, countyname)),
]
census_pop1$Census2019_18PlusPop <- as.numeric(census_pop1$Census2019_18PlusPop)
census_pop1$Census2019_5PlusPop <- census_pop1$Census2019_5PlusPop - census_pop1$Census2019_12PlusPop
census_pop1$Census2019_12PlusPop <- census_pop1$Census2019_12PlusPop - census_pop1$Census2019_18PlusPop
census_pop1$Census2019_18PlusPop <- census_pop1$Census2019_18PlusPop - census_pop1$Census2019_65PlusPop

census_pop2 <- census_pop1%>%
  dplyr::group_by(statename)%>% 
  dplyr::summarise(sum(Census2019_5PlusPop,na.rm=TRUE), sum(Census2019_12PlusPop,na.rm=TRUE), sum(Census2019_18PlusPop,na.rm=TRUE), sum(Census2019_65PlusPop,na.rm=TRUE)) 
names(census_pop2) <- c("statename", "age5to12", "age12to18", "age18to65","age65plus")
census_pop2[52,1] <- "United States"
census_pop2$age5to12[census_pop2$statename == "United States"] <- sum(census_pop2$age5to12[!census_pop2$statename== "United States"])
census_pop2$age12to18[census_pop2$statename == "United States"] <- sum(census_pop2$age12to18[!census_pop2$statename== "United States"])
census_pop2$age18to65[census_pop2$statename == "United States"] <- sum(census_pop2$age18to65[!census_pop2$statename== "United States"])
census_pop2$age65plus[census_pop2$statename == "United States"] <- sum(census_pop2$age65plus[!census_pop2$statename== "United States"])

vaccination_historic7 <- left_join(vaccination_historic6, census_pop2)
vaccination_historic7$Series_Complete_5to12 <- vaccination_historic7$Series_Complete_5Plus - vaccination_historic7$Series_Complete_12Plus
vaccination_historic7$Series_Complete_12to18 <- vaccination_historic7$Series_Complete_12Plus - vaccination_historic7$Series_Complete_18Plus
vaccination_historic7$Series_Complete_18to65 <- vaccination_historic7$Series_Complete_18Plus - vaccination_historic7$Series_Complete_65Plus

vaccination_historic7$pctFullyvac5to12 <- round((vaccination_historic7$Series_Complete_5to12/vaccination_historic7$age5to12)*100,2)
vaccination_historic7$pctFullyvac12to18 <- round((vaccination_historic7$Series_Complete_12to18/vaccination_historic7$age12to18)*100,2)
vaccination_historic7$pctFullyvac18to65 <- round((vaccination_historic7$Series_Complete_18to65/vaccination_historic7$age18to65)*100,2)
vaccination_historic7$pctFullyvac65plus <- vaccination_historic7$Series_Complete_65PlusPop_Pct

vaccination_historic7$Series_Complete_5to12[vaccination_historic7$Series_Complete_5to12 <0] <- 0
vaccination_historic7$Series_Complete_12to18[vaccination_historic7$Series_Complete_12to18 <0] <- 0
vaccination_historic7$Series_Complete_18to65[vaccination_historic7$Series_Complete_18to65 <0] <- 0
vaccination_historic7$pctFullyvac5to12[vaccination_historic7$pctFullyvac5to12 <0] <- 0
vaccination_historic7$pctFullyvac12to18[vaccination_historic7$pctFullyvac12to18 <0] <- 0
vaccination_historic7$pctFullyvac18to65[vaccination_historic7$pctFullyvac18to65 <0] <- 0

static <- subset(vaccination_historic7, Date == max(vaccination_historic7$Date))# check the time
vaccination_historic8 <- subset(vaccination_historic7, Date >= "2021-01-15")

setwd(onedrive)
 write.csv(vaccination_historic8,"./Data/Upload/VaccineTrackertimeseries1.csv",row.names=FALSE,na="")
 write.csv(static,"./Data/Upload/VaccineTrackerstatic1.csv",row.names=FALSE,na="")

setwd(local)
write.csv(vaccination_historic8,"./VaccineTrackertimeseries1.csv",row.names=FALSE,na="")
write.csv(static,"./VaccineTrackerstatic1.csv",row.names=FALSE,na="")

###############Vaccination County Data and Merge it to nationalraw

vaccination_county_link <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"

vaccination_county <- jsonlite::fromJSON(vaccination_county_link)[2][[1]]


date <- unique(vaccination_county$Date)
write.csv(vaccination_county,paste0("O:/Research/Dashboard/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",date,".csv"),row.names = FALSE)

path_c19dashboard_shared_folder=paste0(onedrive)
write.csv(vaccination_county,paste0(path_c19dashboard_shared_folder, "/Data/Raw/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",date,".csv"),row.names = FALSE)

# apporto
write.csv(vaccination_county,paste0(path_c19dashboard_shared_folder, "/Data/Raw/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",date,".csv"),row.names = FALSE)

f = list.files(paste0(onedrive,"/Data/Raw/Vaccinations"))
f = f[regexpr("CDC_Covid Data Tracker_County Vaccination_",f)>0]

vaccination_ts <- purrr::map_dfr(f,.f=function(x){
  read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/",x), show_col_types = FALSE)%>% 
    dplyr::mutate_at(dplyr::vars(contains("Series")),~as.numeric(.))
})

countynames <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/nationalraw.csv")) %>% 
  distinct(state,county,countyname)

names(vaccination_county)[1]<-"Date"
names(vaccination_county)[which(names(vaccination_county) == "StateName")]<-"state_name"
names(vaccination_county)[which(names(vaccination_county) == "County")]<-"county_name"

vaccination_county$countyname<- paste(vaccination_county$county_name, vaccination_county$StateAbbr, sep = ", ")

a=usmap::countypop
vaccination_county$countyname[vaccination_county$state_name=="Alaska"]<-paste(a$county[a$abbr=="AK"],vaccination_county$StateAbbr[vaccination_county$state_name=="Alaska"], sep = ", ")

vaccination_county$countyname<-trimws(vaccination_county$countyname, "r")

indices_data$countyname
merge<-left_join(indices_data,vaccination_county)    

merge1<-merge[-c(97:102, 105:106)]
names(merge1)[97]<-"seriesComplete5Plus"
names(merge1)[98]<-"seriesComplete5PlusPopPct"
names(merge1)[99]<-"seriesComplete18Plus"
names(merge1)[100]<-"seriesComplete18PlusPopPct"
names(merge1)[101]<-"seriesComplete65Plus"
names(merge1)[102]<-"seriesComplete65PlusPopPct"
names(merge1)[103]<-"seriesCompleteYes"
names(merge1)[104]<-"seriesCompletePopPct"
names(merge1)[105]<-"CompletenessPct"
merge1<-merge1[-c(106)]
names(merge1)[106]<-"seriesComplete12Plus"
names(merge1)[107]<-"seriesComplete12PlusPopPct"
merge1<-merge1[-c(108:147)]

variable<-c("seriesComplete18Plus","seriesComplete18PlusPopPct","seriesComplete65Plus","seriesComplete65PlusPopPct","seriesCompleteYes","seriesCompletePopPct","CompletenessPct","seriesComplete12Plus","seriesComplete12PlusPopPct","seriesComplete5Plus","seriesComplete5PlusPopPct")
merge1[, variable][is.na(merge1[, variable])] <- -1


merge_county<-subset(merge1,!is.na(county))
merge_state<-subset(merge1,is.na(county)&is.na(nation))
merge_nation<-subset(merge1,nation==1)
merge_county1=merge_county
merge_county1$seriesComplete18PlusPopPct<-ifelse(merge_county$seriesComplete18PlusPopPct==-1,0,merge_county$seriesComplete18PlusPopPct)
merge_county1$seriesComplete65PlusPopPct<-ifelse(merge_county$seriesComplete65PlusPopPct==-1,0,merge_county$seriesComplete65PlusPopPct)
merge_county1$seriesComplete12PlusPopPct<-ifelse(merge_county$seriesComplete12PlusPopPct==-1,0,merge_county$seriesComplete12PlusPopPct)
merge_county1$seriesComplete5PlusPopPct<-ifelse(merge_county$seriesComplete5PlusPopPct==-1,0,merge_county$seriesComplete5PlusPopPct)

x<-aggregate(merge_county1$seriesComplete18PlusPopPct,by=list(state=merge_county1$state),FUN = "mean")
y<-aggregate(merge_county1$seriesComplete65PlusPopPct,by=list(state=merge_county1$state),FUN ="mean")
m<-aggregate(merge_county1$seriesComplete12PlusPopPct,by=list(state=merge_county1$state),FUN = "mean")
q<-aggregate(merge_county1$seriesComplete5PlusPopPct,by=list(state=merge_county1$state),FUN = "mean")

merge_state <- merge_state[-52,]

merge_state$seriesComplete18PlusPopPct<-x$x
merge_state$seriesComplete65PlusPopPct<-y$x
merge_state$seriesComplete12PlusPopPct<-m$x
merge_state$seriesComplete5PlusPopPct<-q$x

merge_nation$seriesComplete18PlusPopPct<-mean(merge_state$seriesComplete18PlusPopPct)
merge_nation$seriesComplete65PlusPopPct<-mean(merge_state$seriesComplete65PlusPopPct)
merge_nation$seriesComplete12PlusPopPct<-mean(merge_state$seriesComplete12PlusPopPct)
merge_nation$seriesComplete5PlusPopPct<-mean(merge_state$seriesComplete5PlusPopPct)

merge_state$seriesComplete18PlusPopPct<-ifelse(merge_state$seriesComplete18PlusPopPct==0,-1,merge_state$seriesComplete18PlusPopPct)
merge_state$seriesComplete65PlusPopPct<-ifelse(merge_state$seriesComplete65PlusPopPct==0,-1,merge_state$seriesComplete65PlusPopPct)
merge_state$seriesComplete12PlusPopPct<-ifelse(merge_state$seriesComplete12PlusPopPct==0,-1,merge_state$seriesComplete12PlusPopPct)
merge_state$seriesComplete5PlusPopPct<-ifelse(merge_state$seriesComplete5PlusPopPct==0,-1,merge_state$seriesComplete5PlusPopPct)

merge3<-rbind(merge_state,merge_county,merge_nation)


z<-merge3[match(rownames(merge1), rownames(merge3)),]
names(z)[29]<-"urbanrural_text"
names(z)[30]<-"urbanrural"
z <- z[,-9]
z$urbanrural_text[z$urbanrural_text=="Small Metro"]<-"Remote rural areas"
z$urbanrural_text[z$urbanrural_text=="NonCore (Nonmetro)"]<-"Rural areas near cities"
z$urbanrural_text[z$urbanrural_text=="Micropolitan (Nonmetro)"]<-"Small cities"
z$urbanrural_text[z$urbanrural_text=="Medium Metro"]<-"Small suburbs"
z$urbanrural_text[z$urbanrural_text=="Large Fringe Metro"]<-"Large suburbs"
z$urbanrural_text[z$urbanrural_text=="Large Central Metro"]<-"Inner city"

setwd(onedrive)

write.csv(z,"./Data/Upload/nationalraw.csv",row.names=FALSE,na="")

setwd(local)
write.csv(z,"./nationalraw.csv",row.names=FALSE,na="")


#Check zero cases/deaths, if so then replace zero with previous non-zero cases/deaths
modify <- trial
modify_state <- subset(modify,!statename=="")
modify_county <- subset(modify,!county=="")
modify_nation <-subset(modify,nation==1)



modify_state1 <- modify_state %>% 
  dplyr::group_by(statename) %>% 
  dplyr::mutate(dailycases = replace(dailycases,cumsum(dailycases !=0) >0 & dailycases == 0, NA)) %>% 
  fill(dailycases)

modify_state1 <- modify_state1 %>% 
  dplyr::group_by(statename) %>% 
  dplyr::mutate(dailydeaths = replace(dailydeaths,cumsum(dailydeaths !=0) >0 & dailydeaths == 0, NA)) %>% 
  fill(dailydeaths)


modify_state1<-modify_state1[
  with(modify_state1, order(state,county)),
]


modify_county1 <- modify_county %>% 
  dplyr::group_by(countyname) %>% 
  dplyr::mutate(dailycases = replace(dailycases,cumsum(dailycases !=0) >0 & dailycases == 0, NA)) %>% 
  fill(dailycases)

modify_county1 <- modify_county1 %>% 
  dplyr::group_by(countyname) %>% 
  dplyr::mutate(dailydeaths = replace(dailydeaths,cumsum(dailydeaths !=0) >0 & dailydeaths == 0, NA)) %>% 
  fill(dailydeaths)

modify_county1<-modify_county1[
  with(modify_county1, order(state,county)),
]



final <- rbind(modify_state1,modify_county1,modify_nation)

x <- final[order(match(trial$county,final$county)), ]

x<-x[
  with(x, order(state,county,date)),
]

x_nation <- subset(x,nation==1)
x_nonnation <-subset(x,!nation==1)

x_nonnation<-x_nonnation[
  with(x_nonnation, order(state,county,date)),
]
x1 <- rbind(x_nonnation,x_nation)

setwd(local)
write.csv(x1,"./covidtimeseries.csv", na="", row.names=F)
setwd(onedrive)
write.csv(x1,"./Data/Upload/covidtimeseries.csv", na="", row.names=F)


# vaccination by cat bar plot----------------------
setwd(local)
write.csv(z,"./nationalraw.csv",row.names=FALSE,na="")

nationalraw <- read_csv("./nationalraw.csv")
nationcate <- nationalraw |> 
  dplyr::select(countyname, ccvi, poverty, urbanrural, region, black, resSeg, anycondition) |> 
  filter(!is.na(countyname)) |> 
  mutate(county = substr(countyname, 1, (nchar(countyname)-4)),
         state = substr(countyname, nchar(countyname)-1, nchar(countyname))) |> 
  select(-countyname) |> 
  arrange(state, county)

vaccination_county <-  vaccination_county  |> 
  dplyr::select(StateAbbr, county_name, Series_Complete_Pop_Pct) |> 
  mutate(state = StateAbbr,
         county = county_name) |> 
  select(state, county, Series_Complete_Pop_Pct) |> 
  arrange(state, county)

vac_cate <- merge(nationcate, vaccination_county, by = c("state", "county")) |> 
  dplyr::select(county, ccvi, poverty, urbanrural, region, black, resSeg, anycondition, Series_Complete_Pop_Pct) |> 
  dplyr::mutate(ccvi_cut = quantcut(ccvi,5),
                poverty_cut = quantcut(poverty,5),
                black_cut = quantcut(black,5),
                resSeg_cut = quantcut(resSeg,5),
                anycondition_cut = quantcut(anycondition,5))

ccvi_bar <- aggregate(Series_Complete_Pop_Pct~ccvi_cut,vac_cate,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="ccvi") %>% dplyr::rename(quintgroup=ccvi_cut)
poverty_bar <- aggregate(Series_Complete_Pop_Pct~poverty_cut,vac_cate,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=poverty_cut)
black_bar <- aggregate(Series_Complete_Pop_Pct~black_cut,vac_cate,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=black_cut)

urbanrural_bar <- aggregate(Series_Complete_Pop_Pct~urbanrural,vac_cate,FUN="mean") %>% filter(!urbanrural=="") %>% mutate(quintileVar="urbanrural",lbl = c("Inner city", "Large suburbs", "Remote rural areas", "Rural areas near cities", "Small cities", "Small suburbs")) %>% dplyr::rename(quintgroup=urbanrural) |> 
  mutate(quintgroup = factor(quintgroup)) |> 
  dplyr::select("quintgroup", "Series_Complete_Pop_Pct", "lbl", "quintileVar")

region_bar <- aggregate(Series_Complete_Pop_Pct~region,vac_cate,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = c("South", "West", "Northeast", "Midwest")) %>% dplyr::rename(quintgroup=region) |> 
  mutate(quintgroup = factor(quintgroup)) |> 
  dplyr::select("quintgroup", "Series_Complete_Pop_Pct", "lbl", "quintileVar",  )

residseg_bar <- aggregate(Series_Complete_Pop_Pct~resSeg_cut,vac_cate,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=resSeg_cut)

anycondition_bar <- aggregate(Series_Complete_Pop_Pct~anycondition_cut,vac_cate,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anycondition_cut)    


vac_bar <- full_join(ccvi_bar, black_bar) %>% full_join(poverty_bar)  %>% 
  full_join(urbanrural_bar)  %>% full_join(region_bar)  %>%
  full_join(residseg_bar) %>% full_join(anycondition_bar)  %>%
  dplyr::select(Series_Complete_Pop_Pct,everything())


vac_bar_output <- gather(vac_bar,
                        key="variable",
                        value="measure",
                        Series_Complete_Pop_Pct) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)

setwd(onedrive)
write.csv(vac_bar_output,"./Data/Upload/vac_by_bar.csv", na="", row.names=F)

# write.csv(vaccination_ts,"./Data/Upload/vaccination_ts.csv", na="", row.names=F)

vac_bar <- full_join(ccvi_bar, black_bar) %>% full_join(poverty_bar)  %>% 
  full_join(urbanrural_bar)  %>% full_join(region_bar)  %>%
  full_join(residseg_bar) %>% full_join(anycondition_bar)  %>%
  dplyr::select(Series_Complete_Pop_Pct,everything())


vac_bar_output <- gather(vac_bar,
                        key="variable",
                        value="measure",
                        Series_Complete_Pop_Pct) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)

setwd(local)
write.csv(vac_bar_output,"./Data/Upload/vac_by_bar.csv", na="", row.names=F)

#2.Variant Static ##########################################
# download the dataset from website
variant<-read_excel("/Users/zhangziwei/Downloads/0-Datatable\ export-20.xlsx")
variantstatic <- variant
variantstatic <- variantstatic %>%
  fill(`Day of Week Ending`) 
variantstatic$`Day of Week Ending`<- format(mdy(variantstatic$`Day of Week Ending`), "%m/%d/%Y")
variantstatic$`Day of Week Ending`<- anydate (variantstatic$`Day of Week Ending`)

variantstatic <- subset (variantstatic, `Day of Week Ending` == max(variantstatic$`Day of Week Ending`))
variantstatic <- variantstatic %>%
  fill(`Usa Or Hhsregion`) 
variantstatic <- variantstatic [,-c(2,4,7)]
names(variantstatic)[1]<-"weekEnding"
names(variantstatic)[2]<-"regionCode"
names(variantstatic)[3]<-"variant"
names(variantstatic)[4]<-"proportion"
data("hhs_regions")
hhs_regions <- hhs_regions [,c(2:4)]
names(hhs_regions)[1]<-"regionCode"
names(hhs_regions)[2]<-"region"
names(hhs_regions)[3]<-"statename"
variantstatic<-as.data.frame(variantstatic)
hhs_regions$regionCode<-as.character(hhs_regions$regionCode)
variantstatic1 <- left_join(variantstatic, hhs_regions)

variantstatic1$proportion<-as.numeric(variantstatic1$proportion)
variantstatic1$proportion<-variantstatic1$proportion*100
variantstatic1$proportion<-round(variantstatic1$proportion,2)
variantstatic1$FIPS<-fips(variantstatic1$statename)
variantstatic1$FIPS[is.na(variantstatic1$region)] <- "_nation"
variantstatic1<-variantstatic1[!is.na(variantstatic1$FIPS), ] 
variantstatic1$region[is.na(variantstatic1$region)] <- "USA"
variantstatic1$statename[is.na(variantstatic1$statename)] <- "USA"
variantstatic1 <- variantstatic1 %>%
  dplyr::mutate(whoLabel = case_when(variant=="B.1.617.2" ~ "Delta",
                                     variant=="B.1.1.7" ~ "Alpha",
                                     variant=="P.1"  ~ "Gamma",
                                     variant=="B.1.621" ~ "N/A",
                                     variant=="AY.2" ~ "Delta",
                                     variant=="B.1.628" ~ "N/A",
                                     variant=="B.1.621.1" ~ "N/A",
                                     variant=="AY.1" ~ "Delta",
                                     variant=="B.1.526" ~ "Iota",
                                     variant=="B.1.351" ~ "Beta",
                                     variant=="B.1.525" ~ "Eta",
                                     variant=="BA.1.1" ~ "Omicron",
                                     variant=="B.1.1.529" ~ "Omicron",
                                     variant=="BA.2" ~ "Omicron",
                                     
                                     TRUE ~ "Other"
  ))


variantstatic1<- variantstatic1[,c(1,6,7,2,5,3,8,4)]


variantstatic2 <- variantstatic1
variantstatic2$variant <- ifelse(
  variantstatic2$whoLabel=="N/A",
  variantstatic2$variant <- variantstatic2$variant,
  variantstatic2$variant <- paste(variantstatic2$whoLabel,variantstatic2$variant,sep="_")
)
variantstatic2 <- variantstatic2[,-7]
variantstatic3 <- variantstatic2%>%
  pivot_wider(names_from = variant, values_from = proportion)

variantstatic3$Omicron_B.1.1.529[variantstatic3$statename=="USA"] = variantstatic3$Omicron_B.1.1.529[variantstatic3$statename=="USA"] + variantstatic3$Omicron_BA.1.1[variantstatic3$statename=="USA"]
write.csv(variantstatic3,"./Data/Upload/variantStatic.csv")
#####################Variant Timeseries (biweekly Data##########################################

variant1 <- variant
variant1 <- variant1 %>%
  fill(`Day of Week Ending`)
variant1$`Day of Week Ending`<- format(mdy(variant1$`Day of Week Ending`), "%m/%d/%Y")
variant1$`Day of Week Ending`<- anydate (variant1$`Day of Week Ending`)
variant1 <- variant1 %>%
  fill(`Usa Or Hhsregion`) 

variant1 <- variant1 [,-c(2,4,7)]
names(variant1)[1]<-"weekEnding"
names(variant1)[2]<-"regionCode"
names(variant1)[3]<-"variant"
names(variant1)[4]<-"proportion"
variant1 <- left_join(variant1, hhs_regions)
variant1$proportion<-as.numeric(variant1$proportion)
variant1$proportion<-variant1$proportion*100
variant1$proportion<-round(variant1$proportion,2)
variant1$FIPS<-fips(variant1$statename)
variant1$FIPS[is.na(variant1$region)] <- "_nation"
variant1<-variant1[!is.na(variant1$FIPS), ] 
variant1$region[is.na(variant1$region)] <- "USA"
variant1$statename[is.na(variant1$statename)] <- "USA"
variant1 <- variant1 %>%
  dplyr::mutate(whoLabel = case_when(variant=="B.1.617.2" ~ "Delta",
                                     variant=="B.1.1.7" ~ "Alpha",
                                     variant=="P.1"  ~ "Gamma",
                                     variant=="B.1.621" ~ "N/A",
                                     variant=="AY.2" ~ "Delta",
                                     variant=="B.1.628" ~ "N/A",
                                     variant=="B.1.621.1" ~ "N/A",
                                     variant=="AY.1" ~ "Delta",
                                     variant=="B.1.526" ~ "Iota",
                                     variant=="B.1.351" ~ "Beta",
                                     variant=="B.1.525" ~ "Eta",
                                     variant=="BA.1.1" ~ "Omicron",
                                     variant=="B.1.1.529" ~ "Omicron",
                                     variant=="BA.2" ~ "Omicron",
                                     
                                     TRUE ~ "Other"
  ))
variant1 <- variant1[,c(1,6,7,2,5,3,8,4)]
variant2 <- variant1
variant2$variant <- ifelse(
  variant2$whoLabel=="N/A",
  variant2$variant <- variant2$variant,
  variant2$variant <- paste(variant2$whoLabel,variant2$variant,sep="_")
)
variant2 <- variant2[,-7]
variant3 <- variant2%>%
  pivot_wider(names_from = variant, values_from = proportion)
names(variant3)
variant3$Omicron_B.1.1.529[variant3$statename=="USA"] = variant3$Omicron_B.1.1.529[variant3$statename=="USA"] + variant3$Omicron_BA.1.1[variant3$statename=="USA"]


write.csv(variant3,"./Data/Upload/variantTimeseries.csv")

# 3. Statedeath---------
stateDeath1<-read.csv("https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD") %>% 
  dplyr::select(-c("Start.Date", "End.Date", "Year", "Month", "Footnote")) %>% 
  rename("date" = "Data.as.of",
         "state" = "State") %>% 
  filter(Group=="By Total") %>% 
  dplyr::select(-"Group")

transpose<-stateDeath1 %>% 
  rename(White = "Non.Hispanic.White",
         "African American" = "Non.Hispanic.Black.or.African.American",
         "American Natives" = "Non.Hispanic.American.Indian.or.Alaska.Native",
         "Asian" = "Non.Hispanic.Asian",
         "NHPI" = "Non.Hispanic.Native.Hawaiian.or.Other.Pacific.Islander",
         "Non Hispanic Multiple Races" = "Non.Hispanic.more.than.one.race")

transpose1<-transpose%>%pivot_longer(cols=c(4:10))
names(transpose1)[5]<-"totalDeaths"
names(transpose1)[4]<-"race"
transpose1$FIPS<-fips(transpose1$state)
transpose1$FIPS[is.na(transpose1$FIPS)]<-"_nation"
transpose1$totalDeaths[is.na(transpose1$totalDeaths)] <- -9999
transpose2<-subset(transpose1,Indicator=="Count of COVID-19 deaths")
transpose2<-transpose2[,-c(3)]
names(transpose2)[4]<-"covidDeaths"
transpose2$coviddeathDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Distribution of COVID-19 deaths (%)"]
transpose2$popDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Unweighted distribution of population (%)"]
transpose2$weighteddeathDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Weighted distribution of population (%)"]
transpose2<-transpose2[,c(1:2,5,3:4,6:8)]
transpose2<-transpose2[-c(239:245),]
setwd(onedrive)
statePop<-read_excel("./Data/Raw/US Census 2019 Population/Census_Single-RacePopulationEstimatesSTATES_2019.xlsx",sheet=2)
nationPop<-read_excel("./Data/Raw/US Census 2019 Population/Census_Single-RacePopulationEstimatesNATION_2019.xlsx",sheet=2)
names(statePop)[1]<-"state"
names(statePop)[2]<-"FIPS"
nationPop$FIPS<-"_nation"
nationPop$state<-"United States"
nationPop<-nationPop[,c(7,6,1:5)]
pop<-rbind(nationPop,statePop)
detach("package:plyr", unload = TRUE)
pop1<-pop%>%group_by(state,Ethnicity)%>%
  summarise(Population=sum(Population))
pop1<-subset(pop1,Ethnicity=="Hispanic or Latino")
his<-subset(pop,Ethnicity=="Hispanic or Latino")
nonhis<-subset(pop,Ethnicity=="Not Hispanic or Latino")
names(pop1)[2]<-"race"
pop1$race<-"Hispanic"
nonhis<-nonhis[,c(1,5,7)]
names(nonhis)[2]<-"race"
pop2<-rbind(nonhis,pop1)
pop2<-pop2[
  with(pop2, order(state,race)),
]
pop2$race[pop2$race=="American Indian or Alaska Native"]<-"American Natives"
pop2$race[pop2$race=="Black or African American"]<-"African American"
pop2$race[pop2$race=="More than one race"]<-"Non Hispanic Multiple Races"
pop2$race[pop2$race=="Native Hawaiian or Other Pacific Islander"]<-"NHPI"
transpose3<-left_join(transpose2,pop2)
transpose3$covidDeaths<-ifelse(transpose3$covidDeaths<30,-9999.0,transpose3$covidDeaths)
transpose3$coviddeathDistribution<-ifelse(transpose3$coviddeathDistribution<1,-9999.0,transpose3$coviddeathDistribution)
transpose3$coviddeathRate<-transpose3$covidDeaths/transpose3$Population*100000
transpose3$coviddeathRateR<-round(transpose3$coviddeathRate,1)
transpose3$coviddeathRate<-ifelse(transpose3$coviddeathRate<0,-9999.0,transpose3$coviddeathRate)
transpose3<-transpose3[,-8]
transpose3$coviddeathRateR<-ifelse(transpose3$coviddeathRateR<0,-9999.0,transpose3$coviddeathRateR)
(sum(transpose3[!transpose3$covidDeaths==-9999,]$covidDeaths)-sum(transpose3$covidDeaths[transpose3$FIPS=="_nation"]))/sum(transpose3$covidDeaths[transpose3$FIPS=="_nation"])


write.csv(transpose3,"./Data/Upload/stateDeaths.csv")
saveRDS(transpose3, "./Data/Processed/stateDeath/stateDeath.rds") 

# 4. Vaccination trends-----
vaccination_ts <- read.csv(paste0(local, "/data/vaccination_ts.csv"))
date <- unique(vaccination_county$Date)

vaccination_ts_cleaned <- vaccination_ts %>% 
  dplyr::rename(date = Date,
                fips = FIPS,
                statename = StateName,
                state_abbreviation = StateAbbr,
                # countyname = County,
                nComplete18Plus = Series_Complete_18Plus,
                pctComplete18PlusInPop = Series_Complete_18PlusPop_Pct,
                nComplete65Plus = Series_Complete_65Plus,
                pctComplete65PlusInPop = Series_Complete_65PlusPop_Pct,
                nComplete = Series_Complete_Yes,
                pctCompleteInPop = Series_Complete_Pop_Pct,
                pctCompleteness = Completeness_pct) %>% 
  dplyr::select(-County)  %>% 
  mutate(date = lubridate::ymd(date),
         state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()) %>% 
  mutate(fips = as.numeric(fips)) %>% 
  dplyr::rename(countycode = fips) %>% 
  left_join(countynames,
            by = c("state","county"))


covidtimeseries <-x1
#covidtimeseries <- read.csv("/Users/zhangziwei/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/covidtimeseries.csv")
setwd(onedrive)
vaxseries <- read.csv("./Data/Upload/VaccineTrackertimeseries1.csv")
nationalraw <- read.csv("./Data/Upload/nationalraw.csv")
nationalraw <- nationalraw[,-25]
vaxseries_cleaned <- vaccination_ts_cleaned

county_toDate <- covidtimeseries[!is.na(covidtimeseries$county),]
county_pos_dailyCases <- county_toDate[county_toDate$percent14dayDailyCases > 0,]$County_Code
county_neg0_dailyCases <- county_toDate[county_toDate$percent14dayDailyCases <= 0,]$County_Code
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$pctCompleteInPop),]
countyAbvMvax <- vaxseries_cleaned[vaxseries_cleaned$pctCompleteInPop > mean(vaxseries_cleaned$pctCompleteInPop),]$countycode
countyBelMvax <- vaxseries_cleaned[vaxseries_cleaned$pctCompleteInPop <= mean(vaxseries_cleaned$pctCompleteInPop),]$countycode

vaccNation <- vaxseries_cleaned%>%
  group_by(date)%>%
  dplyr::summarise(Average = mean(pctCompleteInPop, na.rm = T))


#vaccination by college
nationalraw <- nationalraw[!is.na(nationalraw$County_Code),]
abvM_College <- nationalraw[!is.na(nationalraw$county) & nationalraw$college > mean(nationalraw$college, na.rm = T),]$County_Code
belM_College <- nationalraw[!is.na(nationalraw$county) & nationalraw$college <= mean(nationalraw$college, na.rm = T),]$County_Code
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

vaxAbvMCollege <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_College,]
vaxBelMCollege <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_College,]

sum_vaxAbvMCollege <- vaxAbvMCollege%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMCollege = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMCollege <- vaxBelMCollege%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMCollege = mean(pctCompleteInPop, na.rm = T))

vaccByAvgCollege <- left_join(sum_vaxAbvMCollege, sum_vaxBelMCollege, by = "date")

rm(abvM_College, belM_College, vaxAbvMCollege, sum_vaxBelMCollege)

#vaccination by American Natives
abvM_Natives <- nationalraw[!is.na(nationalraw$county) & nationalraw$natives > mean(nationalraw$natives, na.rm = T),]$County_Code
belM_Natives <- nationalraw[!is.na(nationalraw$county) & nationalraw$natives <= mean(nationalraw$natives, na.rm = T),]$County_Code
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

vaxAbvMNatives <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_Natives,]
vaxBelMNatives <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_Natives,]

sum_vaxAbvMNatives <- vaxAbvMNatives%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMNatives = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMNatives <- vaxBelMNatives%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMNatives = mean(pctCompleteInPop, na.rm = T))

vaccByAvgNatives <- left_join(sum_vaxAbvMNatives, sum_vaxBelMNatives, by = "date")

rm(abvM_Natives, belM_Natives, vaxAbvMNatives, sum_vaxBelMNatives)


#vaccination by minority
abvM_Minority <- nationalraw[!is.na(nationalraw$county) & nationalraw$minority > mean(nationalraw$minority, na.rm = T),]$County_Code
belM_Minority <- nationalraw[!is.na(nationalraw$county) & nationalraw$minority <= mean(nationalraw$minority, na.rm = T),]$County_Code
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

vaxAbvMMinority <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_Minority,]
vaxBelMMinority <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_Minority,]

sum_vaxAbvMMinority <- vaxAbvMMinority%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMMinority = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMMinority <- vaxBelMMinority%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMMinority = mean(pctCompleteInPop, na.rm = T))

vaccByAvgMinority <- left_join(sum_vaxAbvMMinority, sum_vaxBelMMinority, by = "date")

rm(abvM_Minority, belM_Minority, vaxAbvMMinority, sum_vaxBelMMinority)


#vaccination by African Americans
abvM_AAP <- nationalraw[!is.na(nationalraw$county) & nationalraw$black > mean(nationalraw$black, na.rm = T),]$County_Code
belM_AAP <- nationalraw[!is.na(nationalraw$county) & nationalraw$black <= mean(nationalraw$black, na.rm = T),]$County_Code
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

vaxAbvMAAP <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_AAP,]
vaxBelMAAP <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_AAP,]

sum_vaxAbvMAAP <- vaxAbvMAAP%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMAAP = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMAAP <- vaxBelMAAP%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMAAP = mean(pctCompleteInPop, na.rm = T))

vaccByAvgAAP <- left_join(sum_vaxAbvMAAP, sum_vaxBelMAAP, by = "date")

rm(abvM_AAP, belM_AAP, vaxAbvMAAP, sum_vaxBelMAAP)


#vaccination by uninsured
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]
abvM_Uninsured <- nationalraw[!is.na(nationalraw$county) & nationalraw$PCTUI > mean(nationalraw$PCTUI, na.rm = T),]$County_Code
belM_Uninsured <- nationalraw[!is.na(nationalraw$county) & nationalraw$PCTUI <= mean(nationalraw$PCTUI, na.rm = T),]$County_Code
vaxAbvMUninsured <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_Uninsured,]
vaxBelMUninsured <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_Uninsured,]

sum_vaxAbvMUninsured <- vaxAbvMUninsured%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMUninsured = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMUninsured <- vaxBelMUninsured%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMUninsured = mean(pctCompleteInPop, na.rm = T))

vaccByAvgUninsured <- left_join(sum_vaxAbvMUninsured, sum_vaxBelMUninsured, by = "date")

rm(abvM_Uninsured, belM_Uninsured, vaxAbvMUninsured, sum_vaxBelMUninsured)

#vaccination by hispanics
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]
abvM_HIS <- nationalraw[!is.na(nationalraw$county) & nationalraw$hispanic > mean(nationalraw$hispanic, na.rm = T),]$County_Code
belM_HIS <- nationalraw[!is.na(nationalraw$county) & nationalraw$hispanic <= mean(nationalraw$hispanic, na.rm = T),]$County_Code
vaxAbvMHIS <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_HIS,]
vaxBelMHIS <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_HIS,]

sum_vaxAbvMHIS <- vaxAbvMHIS%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMHIS = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMHIS <- vaxBelMHIS%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMHIS = mean(pctCompleteInPop, na.rm = T))

vaccByAvgHIS <- left_join(sum_vaxAbvMHIS, sum_vaxBelMHIS, by = "date")

rm(abvM_HIS, belM_HIS, vaxAbvMHIS, sum_vaxBelMHIS)


#vaccinationby poverty
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]
abvM_POV <- nationalraw[!is.na(nationalraw$county) & nationalraw$poverty > mean(nationalraw$poverty, na.rm = T),]$County_Code
belM_POV <- nationalraw[!is.na(nationalraw$county) & nationalraw$poverty <= mean(nationalraw$poverty, na.rm = T),]$County_Code

vaxAbvMPOV <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_POV,]
vaxBelMPOV <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_POV,]

sum_vaxAbvMPOV <- vaxAbvMPOV%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMPOV = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMPOV <- vaxBelMPOV%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMPOV = mean(pctCompleteInPop, na.rm = T))

vaccByAvgPOV <- left_join(sum_vaxAbvMPOV, sum_vaxBelMPOV, by = "date")

rm(abvM_POV, belM_POV, vaxAbvMPOV, sum_vaxBelMPOV)

#vaccinationby underlying conditions
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]
abvM_COND <- nationalraw[!is.na(nationalraw$county) & nationalraw$anycondition > mean(nationalraw$anycondition, na.rm = T),]$County_Code
belM_COND <- nationalraw[!is.na(nationalraw$county) & nationalraw$anycondition <= mean(nationalraw$anycondition, na.rm = T),]$County_Code
vaxAbvMCOND <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_COND,]
vaxBelMCOND <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_COND,]

sum_vaxAbvMCOND <- vaxAbvMCOND%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMCOND = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMCOND <- vaxBelMCOND%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMCOND = mean(pctCompleteInPop, na.rm = T))

vaccByAvgCOND <- left_join(sum_vaxAbvMCOND, sum_vaxBelMCOND, by = "date")

rm(abvM_COND, belM_COND, vaxAbvMCOND, sum_vaxBelMCOND)

vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

#vaccination by age over 65
abvM_AGE65 <- nationalraw[!is.na(nationalraw$county) & nationalraw$age65over > mean(nationalraw$age65over, na.rm = T),]$County_Code
belM_AGE65 <- nationalraw[!is.na(nationalraw$county) & nationalraw$age65over <= mean(nationalraw$age65over, na.rm = T),]$County_Code
vaxAbvMAGE65 <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_AGE65,]
vaxBelMAGE65 <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_AGE65,]

sum_vaxAbvMAGE65 <- vaxAbvMAGE65%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMAGE65 = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMAGE65 <- vaxBelMAGE65%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMAGE65 = mean(pctCompleteInPop, na.rm = T))

vaccByAvgAGE65 <- left_join(sum_vaxAbvMAGE65, sum_vaxBelMAGE65, by = "date")

rm(abvM_AGE65, belM_AGE65, vaxAbvMAGE65, sum_vaxBelMAGE65)

#vaccination by urbanicity
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]


lcm_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Inner city",]$County_Code
lfm_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Large suburbs",]$County_Code
mm_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Small suburbs",]$County_Code
mp_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Small cities",]$County_Code
nc_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Rural areas near cities",]$County_Code
sm_urbanrural <- nationalraw[!is.na(nationalraw$county) & nationalraw$urbanrural_text == "Remote rural areas",]$County_Code

vax_lcm_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% lcm_urbanrural,]
vax_lfm_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% lfm_urbanrural,]
vax_mm_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% mm_urbanrural,]
vax_mp_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% mp_urbanrural,]
vax_nc_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% nc_urbanrural,]
vax_sm_urbanrural <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% sm_urbanrural,]

sum_vax_lcm_urbanrural <- vax_lcm_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_lcm_urbanrural = mean(pctCompleteInPop, na.rm = T))
sum_vax_lfm_urbanrural <- vax_lfm_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_lfm_urbanrural = mean(pctCompleteInPop, na.rm = T))
sum_vax_mm_urbanrural <- vax_mm_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_mm_urbanrural = mean(pctCompleteInPop, na.rm = T))
sum_vax_mp_urbanrural <- vax_mp_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_mp_urbanrural = mean(pctCompleteInPop, na.rm = T))
sum_vax_nc_urbanrural <- vax_nc_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_nc_urbanrural = mean(pctCompleteInPop, na.rm = T))
sum_vax_sm_urbanrural <- vax_sm_urbanrural%>%
  group_by(date)%>%
  dplyr::summarise(vax_sm_urbanrural = mean(pctCompleteInPop, na.rm = T))

vax_by_urbanrural <- left_join(sum_vax_lcm_urbanrural, sum_vax_lfm_urbanrural, by = "date")
vax_by_urbanrural <- left_join(vax_by_urbanrural, sum_vax_mm_urbanrural, by = "date")
vax_by_urbanrural <- left_join(vax_by_urbanrural, sum_vax_mp_urbanrural, by = "date")
vax_by_urbanrural <- left_join(vax_by_urbanrural, sum_vax_nc_urbanrural, by = "date")
vax_by_urbanrural <- left_join(vax_by_urbanrural, sum_vax_sm_urbanrural, by = "date")


rm(abvM_urbanrural, belM_urbanrural, vaxAbvMurbanrural, sum_vaxBelMurbanrural)

#vaccination by residential segregation
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]
abvM_resSeg <- nationalraw[!is.na(nationalraw$county) & nationalraw$resSeg > mean(nationalraw$resSeg, na.rm = T),]$County_Code
belM_resSeg <- nationalraw[!is.na(nationalraw$county) & nationalraw$resSeg <= mean(nationalraw$resSeg, na.rm = T),]$County_Code
vaxAbvMresSeg <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% abvM_resSeg,]
vaxBelMresSeg <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% belM_resSeg,]

sum_vaxAbvMresSeg <- vaxAbvMresSeg%>%
  group_by(date)%>%
  dplyr::summarise(vaxAbvMresSeg = mean(pctCompleteInPop, na.rm = T))

sum_vaxBelMresSeg <- vaxBelMresSeg%>%
  group_by(date)%>%
  dplyr::summarise(vaxBelMresSeg = mean(pctCompleteInPop, na.rm = T))

vaccByAvgresSeg <- left_join(sum_vaxAbvMresSeg, sum_vaxBelMresSeg, by = "date")

# fig_vaccByAvgresSeg <- plot_ly(vaccByAvgresSeg, x = ~date, y = ~vaxAbvMresSeg, name = 'Vaccination in counties with above average resSeg', type = 'scatter', mode = 'lines') 
# fig_vaccByAvgresSeg <- fig_vaccByAvgresSeg %>% add_trace(y = ~vaxBelMresSeg, name = "Vaccination in counties with below average resSeg", mode = 'lines') 
# fig_vaccByAvgresSeg
rm(abvM_resSeg, belM_resSeg, vaxAbvMresSeg, sum_vaxBelMresSeg)


#vaccination by region
vaxseries_cleaned <- vaxseries_cleaned[!is.na(vaxseries_cleaned$countycode),]

regionS <- nationalraw[!is.na(nationalraw$county) & nationalraw$region_text == "South",]$County_Code
regionW <- nationalraw[!is.na(nationalraw$county) & nationalraw$region_text == "West",]$County_Code
regionNE <- nationalraw[!is.na(nationalraw$county) & nationalraw$region_text == "Northeast",]$County_Code
regionMW <- nationalraw[!is.na(nationalraw$county) & nationalraw$region_text == "Midwest",]$County_Code


vax_S_region <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% regionS,]
vax_W_region <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% regionW,]
vax_NE_region <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% regionNE,]
vax_MW_region <- vaxseries_cleaned[vaxseries_cleaned$countycode %in% regionMW,]


sum_vax_S_region <- vax_S_region%>%
  group_by(date)%>%
  dplyr::summarise(vax_S_region = mean(pctCompleteInPop, na.rm = T))
sum_vax_W_region <- vax_W_region%>%
  group_by(date)%>%
  dplyr::summarise(vax_W_region = mean(pctCompleteInPop, na.rm = T))
sum_vax_NE_region <- vax_NE_region%>%
  group_by(date)%>%
  dplyr::summarise(vax_NE_region = mean(pctCompleteInPop, na.rm = T))
sum_vax_MW_region <- vax_MW_region%>%
  group_by(date)%>%
  dplyr::summarise(vax_MW_region = mean(pctCompleteInPop, na.rm = T))


vax_by_region <- left_join(sum_vax_S_region, sum_vax_W_region, by = "date")
vax_by_region <- left_join(vax_by_region, sum_vax_NE_region, by = "date")
vax_by_region <- left_join(vax_by_region, sum_vax_MW_region, by = "date")


rm(abvM_region, belM_region, vaxAbvMregion, sum_vaxBelMregion)

colnames(vaccNation)[2] <- c("National Average")
vaccNation <- vaccNation %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgAAP)[2:3] <- c("Counties with high proportion of African Americans", 
                                 "Counties with low proportion of African Americans")
vaccByAvgAAP <- vaccByAvgAAP %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgAGE65)[2:3] <- c("Counties with high proportion of population age 65+",
                                   "Counties with low proportion of population age 65+")
vaccByAvgAGE65 <- vaccByAvgAGE65 %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgCOND)[2:3] <- c("Counties with high proportion with underlying condition",
                                  "Counties with low proportion with underlying condition")
vaccByAvgCOND <- vaccByAvgCOND %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgHIS)[2:3] <- c("Counties with high proportion of Hispanic Americans",
                                 "Counties with low proportion of Hispanic Americans")
vaccByAvgHIS <- vaccByAvgHIS %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgPOV)[2:3] <- c("Counties with high proportion of population living in poverty",
                                 "Counties with low proportion of population living in poverty")
vaccByAvgPOV <- vaccByAvgPOV %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgresSeg)[2:3] <- c("Counties with high residential segregation",
                                    "Counties with low residential segregation")
vaccByAvgresSeg <- vaccByAvgresSeg %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgMinority)[2:3] <- c("Counties with high proportion of minorities",
                                      "Counties with low proportion of minorities")
vaccByAvgMinority <- vaccByAvgMinority %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgNatives)[2:3] <- c("Counties with high proportion of Native Americans",
                                     "Counties with low proportion of Native Americans")
vaccByAvgNatives <- vaccByAvgNatives %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgUninsured)[2:3] <- c("Counties with high proportion of uninsured population",
                                       "Counties with low proportion of uninsured population")
vaccByAvgUninsured <- vaccByAvgUninsured %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vax_by_region)[2:5] <- c("Counties in the South", "Counties in the West",
                                  "Counties in the Northeast", "Counties in the Midwest")
vax_by_region <- vax_by_region %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vax_by_urbanrural)[2:7] <- c("Inner City", "Large suburbs", "Small suburbs", "Small cities",
                                      "Rural areas near cities", "Remote rural areas")
vax_by_urbanrural <- vax_by_urbanrural %>% gather("mean", "percentFullyVaccinated", -date)

colnames(vaccByAvgCollege)[2:3] <- c("Counties with high proportion of population with college education",
                                     "Counties with low proportion of population with college education")
vaccByAvgCollege <- vaccByAvgCollege %>% gather("mean", "percentFullyVaccinated", -date)

exploratoryVaccineTrends <- rbind(vaccNation, vaccByAvgAAP, vaccByAvgAGE65, vaccByAvgCOND, 
                                  vaccByAvgHIS, vaccByAvgPOV, vaccByAvgresSeg,
                                  vaccByAvgMinority, vaccByAvgNatives, vaccByAvgUninsured, 
                                  vax_by_region, vaccByAvgCollege, vax_by_urbanrural)
setwd(onedrive)
write.csv(exploratoryVaccineTrends, "./Data/Upload/exploratoryVaccineTrends.csv")
setwd(local)
write.csv(exploratoryVaccineTrends,"./data/exploratoryVaccineTrends.csv")

# 5. Demographic---------
setwd(onedrive)

##Fixing Date error
updateTimePassed = as.numeric(format(strptime(Sys.time(),format = "%Y-%m-%d %H:%M:%S"),"%H"))>=19 

#############  Race demog popdata ###########

rac <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
racdemog <- as.data.frame(jsonlite::fromJSON(rac)[1]) 
colnames(racdemog) <- sub(".*_percent.", "", colnames(racdemog)) 
racdemog2 <- racdemog %>% 
  dplyr::select(race_eth_new,col_per_Grand_Total) %>% 
  dplyr::rename(percentPop=col_per_Grand_Total)

##### DOWNLOAD DATA : CASES BY RACE #########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

library(plyr)
casesdata <- jsonlite::fromJSON(raw_data)[[6]] %>% 
  plyr::join(racdemog2)  #for race by age data
casesdata$race_eth_new <-  plyr::revalue(casesdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                                   "Black, Non-Hispanic"="Non-Hispanic African American",
                                                                   "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Native",
                                                                   "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                                   "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                                   "Multiple/Other, Non-Hispanic"="Non-Hispanic Multiple/Other",
                                                                   "Unknown" = "Unknown",
                                                                   "Hispanic/Latino" = "Hispanic"))


casesdata_race <- casesdata %>% 
  dplyr::select(Demographic,race_eth_new,Grand_Total,col_per_Grand_Total,percentPop) %>%
  dplyr::rename(demographicVar=Demographic,demographic=race_eth_new,cases=Grand_Total,percentCases=col_per_Grand_Total) %>% 
  mutate(totalcases = sum(cases),demographicVar="race",
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA))

rm(casesdata,raw_data)

###################  DEATHS BY RACE DATA ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathdata <- jsonlite::fromJSON(raw_data)[[7]] %>% 
  plyr::join(racdemog2)  #for race by age data
deathdata$race_eth_new <- plyr::revalue(deathdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                                  "Black, Non-Hispanic"="Non-Hispanic African American",
                                                                  "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Native",
                                                                  "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                                  "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                                  "Multiple/Other, Non-Hispanic"="Non-Hispanic Multiple/Other",
                                                                  "Hispanic/Latino" = "Hispanic"))

deathdata_race <- deathdata %>% 
  dplyr::select(Demographic,race_eth_new,Grand_Total,col_per_Grand_Total,percentPop) %>%
  dplyr::rename(demographicVar=Demographic,demographic=race_eth_new,deaths=Grand_Total,percentDeaths=col_per_Grand_Total) %>% 
  mutate(totaldeaths = sum(deaths),demographicVar="race",
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA))


rm(deathdata,raw_data)

############## JOIN CASES AND DEATHS DATA #############

racecdc <- full_join(casesdata_race,deathdata_race) %>% 
  mutate(demogLabel = gsub("Non-Hispanic ","",demographic),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01")) %>%dplyr::select(date,everything())

rm(casesdata_race,deathdata_race)
############  CASERATE AND DEATHRATE  #############

raceNation <- racecdc %>% 
  mutate(caserate=round(100000*(cases/demogPop),2),
         deathrate=round(100000*(deaths/demogPop),2)) 

whiteCR <- raceNation %>% 
  filter(demographic=="Non-Hispanic White") %>% 
  dplyr::select(date,demographic,caserate) %>% 
  dplyr::rename(WhiteCR=caserate) %>% 
  dplyr::select(-demographic)

crr = plyr::join(raceNation,whiteCR) %>% mutate(CaserateRatio = round(caserate/WhiteCR,2))

HwhiteDR <- raceNation %>% filter(demographic=="Non-Hispanic White") %>%dplyr::select(date,demographic,deathrate) %>% dplyr::rename(WhiteDR=deathrate) %>%dplyr::select(-demographic)

drr = plyr::join(raceNation,HwhiteDR) %>% mutate(DeathrateRatio = round(deathrate/WhiteDR,2))

raceUS_final <- plyr::join(crr,drr) %>%
  dplyr::select(-WhiteCR,-WhiteDR) %>% mutate(demographicVar="race")

rm(crr,drr,HwhiteDR,raceNation,whiteCR)

#############  age demog data ############
age <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
agedemog <- as.data.frame(jsonlite::fromJSON(age)[3])
colnames(agedemog) <- sub(".*_percent.", "", colnames(agedemog))
agedemog2 <- agedemog %>%dplyr::select(age_group,Percent) %>% 
  dplyr::rename(percentPop=Percent)

#########  DOWNLOAD DATA : CASES BY AGE  ###########
raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

casesdata <- jsonlite::fromJSON(raw_data)[[2]] %>% plyr::join(agedemog2) %>% 
  dplyr::select(-b) %>% 
  dplyr::rename(cases=count,demographic=age_group,percentCases=Percent) %>%
  mutate(totalcases = sum(cases),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"),
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>%dplyr::select(date,everything()) 




#########  DOWNLOAD DATA : DEATHS BY AGE  ###########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata <- jsonlite::fromJSON(raw_data)[[3]] %>% plyr::join(agedemog2) %>% 
  dplyr::select(-b) %>% 
  dplyr::rename(deaths=count,demographic=age_group,percentDeaths=Percent) %>%
  mutate(totaldeaths = sum(deaths),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"),
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA)) %>%dplyr::select(date,everything()) 



########## CASES AND DEATHS BY AGE: merging based on age groups ###############

covid_age <- plyr::join(casesdata,deathsdata) %>% mutate(demogLabel=demographic,as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"))  %>%
  mutate(demographicVar="age")

covid_age$demographic <- recode(covid_age$demographic,"Unknown"="Unknown Age")

########  remove unnecessary dataframes ########

rm(casesdata,deathsdata)


#############  sex demog data ############

sex <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
sexdemog <- as.data.frame(jsonlite::fromJSON(sex)[2])
colnames(sexdemog) <- sub(".*_percent.", "", colnames(sexdemog))
sexdemog2 <- sexdemog %>%dplyr::select(sex_new,col_per_Grand_Total) %>%
  mutate(percentPop=as.numeric(col_per_Grand_Total)) %>%dplyr::select(-col_per_Grand_Total)


########## CASES  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

##check here-------
casesdata_sex <- jsonlite::fromJSON(raw_data)[[4]] %>% plyr::join(sexdemog2) %>%
  filter(!sex_new=="Grand_Total") %>%
  dplyr::select(sex_new,Grand_Total,col_per_Grand_Total,percentPop) %>% 
  dplyr::rename(cases=Grand_Total,demographic=sex_new,percentCases=col_per_Grand_Total) %>%
  mutate(totalcases = sum(cases),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"),
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>%dplyr::select(date,everything()) 


########## DEATHS  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata_sex <- jsonlite::fromJSON(raw_data)[[5]] %>% plyr::join(sexdemog2) %>%
  filter(!sex_new=="Grand_Total") %>%
  dplyr::select(sex_new,Grand_Total,col_per_Grand_Total,percentPop) %>% 
  dplyr::rename(deaths=Grand_Total,demographic=sex_new,percentDeaths=col_per_Grand_Total) %>%
  mutate(totaldeaths = sum(deaths),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")),origin="1988-01-01")) %>%dplyr::select(date,everything()) 


# date= as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")))

#deaths count missing, adding using following code

deathsTotSex <- as.numeric(jsonlite::fromJSON(raw_data)[[5]] %>% filter(sex_new=="Grand_Total") %>% 
                             dplyr::select(Grand_Total))
library(janitor)
deathssex <- data.frame(adorn_totals(deathsdata_sex, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totaldeaths=ifelse(demographic=="total",totaldeaths/4,totaldeaths))
deathssex$deaths[deathssex$demographic=="total"] <- deathsTotSex
deathsdata_sex$totaldeaths <- deathsTotSex


deathsdata_sex <- deathsdata_sex %>% 
  mutate(missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA))


########## CASES AND DEATHS BY SEX: merging based on sex groups ###############

covid_sex <- plyr::join(casesdata_sex,deathsdata_sex) %>% 
  mutate(demogLabel=demographic,
         date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"))  %>%
  mutate(demographicVar="sex")

covid_sex$demographic <- recode(covid_sex$demographic,"Unknown"="Unknown Sex")


########  remove unnecessary dataframes ########

rm(casesdata_sex,deathsdata_sex)


final_data <- full_join(raceUS_final,covid_age) %>% full_join(covid_sex) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  mutate(demogLabel=ifelse(demographicVar=="age",gsub(" Years","",demogLabel),demogLabel)) %>%
  dplyr::select(date,demographicVar,demographic,demogLabel,popUS,demogPop,percentPop,cases,totalcases,percentCases,availableCases,missingCases,caserate,deaths,totaldeaths,percentDeaths,availableDeaths,missingDeaths,deathrate) 


final_data$percentCases[final_data$demogLabel=="Unknown"] <- -9999
final_data$percentDeaths[final_data$demogLabel=="Unknown"] <- -9999
final_data$popUS[final_data$demogLabel=="Unknown"] <- -9999



##############  VACCINATION BY RACE DATA ########################


usdemog_link <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/vaccination_demographics.json")
usdemog <- as.data.frame(jsonlite::fromJSON(usdemog_link)[1]) 
colnames(usdemog) <- sub(".*_percents.", "", colnames(usdemog)) 
usdemog2 <- usdemog %>% 
  dplyr::select(-id,-Date) %>% 
  dplyr::rename(percentPop=US_Total) 



##############  VACCINATION BY RACE DATA ########################

vacclink <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_demographics_data")
vaccdata <- jsonlite::fromJSON(vacclink)[[2]]

df <- vaccdata %>% plyr::join(usdemog2) %>%dplyr::select(-census) %>%
  dplyr::rename(date=Date,demogLabel=Demographic_category,admDose1=Administered_Dose1,pctAdmDose1=Administered_Dose1_pct_known,
                pctKnownAdmDose1=Administered_Dose1_pct_US,admDose2=Administered_Dose2,pctAdmDose2=Administered_Dose2_pct_known,
                pctKnownAdmDose2=Administered_Dose2_pct_US) %>%
  mutate(date=as.Date(ifelse(updateTimePassed==TRUE,as.Date(Sys.Date(),origin="1988-01-01"),as.Date(Sys.Date()-1,origin="1988-01-01")), origin="1988-01-01"),
         demographicVar = ifelse(str_detect(demogLabel, "Age"),"vaccineAge",ifelse(str_detect(demogLabel, "Race"),"vaccineRace",ifelse(str_detect(demogLabel, "Sex"),"vaccineSex","total"))),
         demogLabel = ifelse(demographicVar=="vaccineAge",gsub("Ages_","",demogLabel),
                             ifelse(demographicVar=="vaccineRace",gsub("Race_eth_","",demogLabel),
                                    ifelse(demographicVar=="vaccineSex",gsub("Sex_","",demogLabel),demogLabel))))

library(snakecase)
colnames(df) <- to_lower_camel_case(colnames(df))

df$admDose1[df$demogLabel=="US"] -> USTotDose1
df$admDose2[df$demogLabel=="US"] -> USTotDose2


df2 <- df %>% mutate(demogLabel = ifelse(demographicVar=="vaccineAge",gsub("_yrs","",demogLabel),
                                         ifelse(demographicVar=="vaccineRace",gsub("NH","",demogLabel),demogLabel)))


df2$demogLabel <- plyr::revalue(df2$demogLabel,c("<2yrs"="<2",
                                                 "Age_known" = "Unknown",
                                                 "Age_unknown" = "unknown",
                                                 "known"="Unknown",
                                                 "OPI" = "NHPI",
                                                 "AIAN"="American Native",
                                                 "Black"="African American",
                                                 "Multiracial"="Multiple/Other"))

finalvacc <- df2 %>% filter(!demogLabel=="US",!demogLabel=="unknown") %>%
  mutate(pctKnownAdmDose1 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose1),
         pctKnownAdmDose2 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose2),
         pctAdmDose1 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose1),
         pctAdmDose2 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose2),
         seriesCompletePopPctKnown = ifelse(demogLabel=="Unknown",-9999,seriesCompletePopPctKnown),
         seriesCompletePopPctUs = ifelse(!demogLabel=="Unknown",-9999,seriesCompletePopPctUs),
         # administeredDose1PctAgegroup = ifelse(demogLabel=="Unknown"|!demographicVar=="vaccineAge",-9999,administeredDose1PctAgegroup),
         # seriesCompletePopPctAgegroup = ifelse(demogLabel=="Unknown"|!demographicVar=="vaccineAge",-9999,seriesCompletePopPctAgegroup)
  ) %>%
  mutate(demographic = ifelse(demographicVar=="vaccineAge" & !demogLabel=="Unknown",paste(demogLabel,"years"),
                              ifelse(demographicVar=="vaccineRace" & !demogLabel=="Unknown" & !demogLabel=="Hispanic",paste("Non-Hispanic",demogLabel),demogLabel)),
         percentPop = ifelse(is.na(percentPop),-9999,percentPop),
         demographic = ifelse(demographicVar=="age",paste(demogLabel,"years"),demographic)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  arrange(demographicVar) 


finalvacc$popUS <- 327167439

library(zoo)
mergedVacc23 <- na.locf(finalvacc, fromFirst = TRUE)

mergedVacc23 <- mergedVacc23 %>% 
  mutate(demogLabel=ifelse(demographicVar=="age",gsub("-"," - ",demogLabel),demogLabel)) %>%
  arrange(demographicVar)   

mergedVacc23$demographic[mergedVacc23$demographicVar=="age" & mergedVacc23$demographic=="Unknown years"] <- "Unknown"


vaccupload <- full_join(mergedVacc23,final_data) %>% 
  mutate(percentPop=round(percentPop,1)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  arrange(demographicVar,demographic)

setwd(onedrive)
write.csv(vaccupload,"./Data/Upload/USDemogData.csv",na="",row.names=F)

vac14 <- df2 |> 
  filter(demographicVar %in% c("vaccineRace", "total") &  demogLabel %in% c("Hispanic", "American Native", "Asian", "African American", "NHPI", "White", "Multiple/Other", "US")) |> 
  dplyr::rename(Race = demogLabel,
                PctatleastOneDose = pctAdmDose1,
                NumatleastOneDose = admDose1,
                PctUSPopulation = percentPop,
                PctVacLast14 = administeredDose1PctKnownLast14Days,
                NumVacLast14 = administeredDose1Last14Days) |> 
  dplyr::select("date", "Race", "PctatleastOneDose", "NumatleastOneDose", "PctUSPopulation", "PctVacLast14", "NumVacLast14")

write.csv(vac14,"./Data/Upload/pctAmongFullyVacLast14.csv")








)








