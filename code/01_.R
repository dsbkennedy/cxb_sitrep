
# 01 - cleaningscript2 ----------------------------------------------------

#CLEANING FILE - GOOGLE DOCS DAT

#cleaning the column names - changing to all lower cases and spaces separated by _
cleaned_colnames <- epitrix::clean_labels(colnames(FDMN_Only))
colnames(FDMN_Only) <- cleaned_colnames
rm(cleaned_colnames)

#Remove blank rows - removing rows where all the entries are "NA"
FDMN_Only2<- FDMN_Only %>%
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  mutate(year=year(date_of_case_detection)) %>% 
  filter(Week <= ThisWeek | year==2020) 

# Set number of cases [FDMN]
n_cases_FDMN<-nrow(FDMN_Only2)

# Replacing all "NA"s in the Severity of disease section with "Unknown"
FDMN_Only2$severity_of_disease[is.na(FDMN_Only2$severity_of_disease)] <- 'Unknown'
FDMN_Only2 <- FDMN_Only2 %>% 
  mutate (camp_of_residence=ifelse(camp_of_residence=="NULL", "NA",camp_of_residence))

## CLEANING TESTING DATA

#cleaning the column names
cleaned_colnames <- epitrix::clean_labels(colnames(Testing))
colnames(Testing) <- cleaned_colnames
rm(cleaned_colnames)

#Remove blank rows - removing rows where all the entries are "NA"
Testing<- Testing %>%
  filter_all(any_vars(!is.na(.)))

#cleaning the column names
cleaned_colnames <- epitrix::clean_labels(colnames(ari_ili))
colnames(ari_ili) <- cleaned_colnames
rm(cleaned_colnames)

# Remove data which have "NA" in the date column
TestingClean<- subset(Testing, date!="Total") 

#Transforming to dates
TestingClean$date<-excel_numeric_to_date(as.numeric(as.character(TestingClean$date)), date_system = "modern")    

#Remove blank rows - removing rows where all the entries are "NA"
TestingClean<- TestingClean %>%
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(year=isoyear(date)) %>% 
  mutate(week=isoweek(date))

#For graphs
test_nationality <- TestingClean %>%
  filter(!date %in% c('NULL', 'Total')) %>% 
  mutate(date_format=ymd(date)) %>% 
  select(-date) %>% 
  pivot_longer(-date_format)

#cut into age groups
FDMN_Only2$agegp<-cut(FDMN_Only2$age_in_years,
                      breaks = c(-Inf, 11, 22, 31, 41, 51, 61, +Inf), 
                      labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "60+"),
                      right=F)

# The camp name is imported as a numeric rather than as a factor and as a list
FDMN_Only2$camp_of_residence<-as.character(unlist(FDMN_Only2$camp_of_residence))

# Fixing Typos
FDMN_Only2 <- FDMN_Only2 %>% 
  mutate (camp_of_residence=ifelse(camp_of_residence=="Nyapara RC", "Nayapara RC",camp_of_residence))

FDMN_Only2 <- FDMN_Only2 %>% 
  mutate (`30_day_outcome`=ifelse(`30_day_outcome`=="Recoverd", "Recovered",`30_day_outcome`))



# 02 - figuresetting_summary ----------------------------------------------

#SETTING SUMMARY FIGURES

#Set CFR
Outcome_Proportions<- FDMN_Only2 %>%
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  mutate(year=year(date_of_case_detection)) %>% 
  dplyr::filter(year==2020 | (year==2021 & Week<=ThisWeek)) %>% 
  #filter(Week <= ThisWeek) %>% 
  count(`30_day_outcome`) %>% 
  mutate(Percentage=n/sum(n)) %>% 
  mutate(Percentage=scales::percent(Percentage, accuracy=.1)) 

# Proportions_2<-Outcome_Proportions$n/ n_cases_FDMN *100
# Outcome_Proportions['Percentage'] = Proportions_2

# outcomes at 30 days
#sum_tab_30<- as.data.frame(table(FDMN_Only$`30_day_outcome`))
sum_tab_30 <- FDMN_Only2 %>%
  #filter(Week<=ThisWeek) %>%
  clean_names() %>%
  count(x30_day_outcome)

#names(sum_tab_30) <- c('Outcome at 30 days','Count')
# FlexTab_outcomesat30<-flextable(sum_tab_30)
# FlexTab_outcomesat30<-autofit(FlexTab_outcomesat30)

#Deaths at 30 days follow up
deathscount<-sum_tab_30 %>%  filter(x30_day_outcome=='Death') %>% pull(n)

#Set CRF = death/all results (column 3, row 1)
#CFR<-round(Outcome_Proportions[1,3], 1)

#Set number of camps with at least one confirmed case
CasesCamp<- FDMN_Only2 %>%
  group_by(camp_of_residence) %>%
  summarise(count=n())

#CasesCampVal<-unique(CasesCamp$camp_of_residence)
#length(CasesCampVal)
CasesCampVal <-  CasesCamp %>%  filter(!camp_of_residence=='Outside Camp') %>% pull(camp_of_residence)

CampsWithCases<-length(CasesCampVal)

# #Individuals missing camp of residence information
# MissingInfoCampResidence<-
#   FDMN_Only$camp_of_residence

# Median age of cases (min and max)

medianage<-median(FDMN_Only2$age_in_years)
RangeUpper<-max(FDMN_Only2$age_in_years)
RangeLow<-min(FDMN_Only2$age_in_years)

#FDMN_Only$Week <-week(FDMN_Only$date_of_case_detection)

FDMN_Only <- FDMN_Only2 %>%  
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  filter(Week <= ThisWeek)

# Sex
Sex<- FDMN_Only2 %>%
  dplyr::filter(year==2020 | (year==2021 & Week<=ThisWeek)) %>% 
  group_by(sex) %>%
  summarise(count=n())

# Cases split by sex
Male<-Sex[2,2]
Female<-Sex[1,2]

#Male and Female - %s
Male_Prop<-round(sum(Male/(Male+Female)*100), 1)
Female_Prop<-round(sum(Female/(Male+Female)*100), 1)

#Setting time frames used in this report
first_date <- min(TestingClean$date) 
last_date <- today() 
# 
# ThisWeek<- week(today()) 
# LastWeek <- week(today())-1


LastWeekCases<- FDMN_Only2 %>% 
  filter(Week <=(LastWeek)) %>% 
  summarise(count=n())

DifferenceLastWeek<-n_cases_FDMN - LastWeekCases 

# Setting an attack rate 
FDMN_Pop<-as.integer(sum(population$Total_Pop))

AR<-round((n_cases_FDMN/FDMN_Pop)*100,3)
AR100k<-round((n_cases_FDMN/FDMN_Pop)*100000,1)


# 03 - testingandpositivity -----------------------------------------------

#TESTING AND POSITIVITY
library(magrittr)
library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(epitrix)
library(dplyr)
library(kableExtra)
library(EpiCurve) 
library(epitools) 
library(flextable)
library(officer)
library(coarseDataTools)
library(janitor)
library(here)
library(googlesheets4)
library(purrr)
library(scales)

ARI_ILI_Pos <-ari_ili %>%
  select(case_id, sample_type, date_of_case_detection, health_facility_name=health_facility_name_sample_collection_site, camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
  filter(laboratory_result %in% c('Positive', 'Negative')) %>% 
  filter(nationality=='FDMN') %>% 
  filter(!sample_type %in% c('Follow-up', 'Humanitarian Worker')) %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  mutate(camp=ifelse(camp=="Camp 20 EXT" |camp=="Camp 20 ext", "Camp 20 Ext", camp))


# #Keep only positive and negatives for positivity by week 
# ARI_ILI_Pos<-filter(ARI_ILI_Pos, laboratory_result == "Positive" | laboratory_result == "Negative") %>%
#   filter(nationality=="FDMN") %>%
#   filter(sample_type!= "Follow-up" | sample_type!= "Humanitarian Worker")
# 
# #Add in Week of case detection
# ARI_ILI_Pos$Week<-week(ARI_ILI_Pos$date_of_case_detection)
# 
# ARI_ILI_Pos <- ARI_ILI_Pos %>% 
#   mutate (camp=ifelse(camp=="Camp 20 EXT" |camp=="Camp 20 ext", "Camp 20 Ext", camp))

TestCamps<-ARI_ILI_Pos %>%
  group_by(camp) %>%
  summarise(n=n())

# number positives and negatives by camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos %>% 
  group_by(camp, laboratory_result) %>%
  summarise(n=n())%>%
  spread(laboratory_result, n) 

# number positives and negatives by camp & week
ARI_ILI_Pos_Week_Split <- ARI_ILI_Pos %>% 
  group_by(camp, laboratory_result, Week) %>%
  summarise(n=n())%>%
  spread(laboratory_result, n)


#Overall positivity by camp
ARI_ILI_Pos_Split$Positivity<-round(ARI_ILI_Pos_Split$Positive/(ARI_ILI_Pos_Split$Negative+ARI_ILI_Pos_Split$Positive)*100, 1)

#Left joining with the number of tests
ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCamps, by ="camp")

# %>% 
#   arrange(desc(Positivity))
#  
# Report_ARI_ILI_CampTable<-flextable(ARI_ILI_CampTable)
# 
# Report_ARI_ILI_CampTable<-autofit(Report_ARI_ILI_CampTable) %>% 
#   set_header_labels(camp = "Camp (Residence)",
#                     n = "Total tests",
#                     Positivity = "Positivity") %>%
#   bold(bold = TRUE, part = "header")
# 
# Report_ARI_ILI_CampTable<- fontsize(Report_ARI_ILI_CampTable, size = 10, part="all") 
# Report_ARI_ILI_CampTable<-autofit(Report_ARI_ILI_CampTable)

#Testing in total
FDMN_Test<-as.integer(sum(TestingClean$fdmn))


# Testing in the last 7 days
# Making a % positive

FDMNPos<- TestingClean[, c(1,4,5,7,8)]
FDMNPos$Positivity<-round(FDMNPos$fdmn_positive/FDMNPos$fdmn*100, 1)

FDMNOverallPos<- FDMNPos %>%
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek)) %>% 
  summarise(total_tests = sum(fdmn, na.rm=TRUE),
            total_cases = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(Overall_Pos = round(as.numeric(total_cases/total_tests*100), 1))

FDMNOverallPosNum<-FDMNOverallPos$Overall_Pos

FDMNPos$date<-as.Date(FDMNPos$date)

FDMNPositvity7day<- FDMNPos %>%
  mutate(Week=isoweek(date)) %>% 
  filter(Week == LastWeek) %>%
  #filter(date >  today()- 7) %>%
  summarise(total_tests_7days = sum(fdmn, na.rm=TRUE),
            total_cases_7days = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_7days/total_tests_7days*100), 1))

# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_Week_FDMN<-ARI_ILI_Pos %>% 
  filter(Week == LastWeek) %>%
  filter(nationality == "FDMN")

# Test in the last week and positivity in the last week
n_Tests_Week_FDMN<-nrow(ARI_ILI_Pos_Week_FDMN)
WeekPositivity<-FDMNPositvity7day$WeekPositivty

# Number of tests done by camp last week
TestCampsWeek<-ARI_ILI_Pos_Week_FDMN %>%
  group_by(camp) %>%
  summarise(n=n())
TestCampsWeek<- TestCampsWeek

# number positives and negatives by week and camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos_Week_FDMN %>% 
  group_by(camp, laboratory_result) %>%
  summarise(n=n())%>%
  spread(laboratory_result, n) %>% 
  mutate(Positive=0)

#Overall positivity by camp
ARI_ILI_Pos_Split$positivity<-round(
  ARI_ILI_Pos_Split$Positive/(ARI_ILI_Pos_Split$Negative + ARI_ILI_Pos_Split$Positive)*100, 1)

#Left joining with the number of tests
Week_ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCampsWeek, by ="camp" )

#creating a flextable ordering positivity from high to low
Week_Camp<-Week_ARI_ILI_CampTable[order(Week_ARI_ILI_CampTable$positivity, na.last = T, decreasing = T),] 

#Filtering for cases by camp in the last week 
WeekCamp2<-filter(Week_Camp, Positive >=1)

Week_CampFlex<-flextable(WeekCamp2)

Week_CampFlex<-autofit(Week_CampFlex) %>% 
  set_header_labels(camp = "Camp",
                    n = "Total tests - last 7 days",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header")

Week_CampFlex<- fontsize(Week_CampFlex, size = 10, part="all") 
Week_CampFlex_Report<-autofit(Week_CampFlex)


##First testing date
first_test_fdmn <- ari_ili %>%  filter(nationality=='FDMN') %>%  
  filter(date_of_case_detection==min(date_of_case_detection)) %>%  pull(date_of_case_detection)
first_test_fdmn_week <- isoweek(first_test_fdmn)

first_test_fdmn_day <- append_date_suffix(first_test_fdmn)
first_test_fdmn_month <- month(first_test_fdmn, label=TRUE, abbr=FALSE)

first_test_fdmn_day_month <- sprintf("the %s of %s", first_test_fdmn_day, first_test_fdmn_month)

##First case detection
first_case_fdmn <- ari_ili %>%  filter(nationality=='FDMN') %>%  filter(laboratory_result=='Positive') %>% 
  filter(date_of_case_detection==min(date_of_case_detection)) %>%  pull(date_of_case_detection)
first_case_fdmn_week <- isoweek(first_case_fdmn)

first_case_fdmn_day <- append_date_suffix(first_case_fdmn)
first_case_fdmn_month <- month(first_case_fdmn, label=TRUE, abbr=FALSE)

first_case_fdmn_day_month <- sprintf("the %s of %s", first_case_fdmn_day, first_case_fdmn_month)


###Median age of test

week_test_df <- ari_ili %>%  
  clean_data() %>% 
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  filter(!sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  # filter(week <= ThisWeek | year==2020) %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  #filter(date_of_case_detection>=ymd('2020-05-01')) %>% 
  #mutate(week=date2week(date_of_case_detection,week_start = "sun", floor_day = TRUE)) %>% 
  select(yearweek,week,date_of_case_detection, laboratory_result, age) %>% 
  #filter(week>19) %>% 
  #mutate(week=factor(week, levels=unique(week))) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(age=as.numeric(as.character(unlist(age))))

tests_age_gph <- ggplot(week_test_df, aes(x=yearweek, y=age, group=factor(yearweek))) + 
  #geom_jitter(colour="lightblue", alpha=0.5, width=0.1) +
  geom_point(stat="summary", fun="mean") + 
  #geom_boxplot(alpha = 0.80) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Week", y="Age (mean with 95% CI)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(breaks=seq(0,100,10))

tests_age_overall <- week_test_df %>% summarize(median = median(age,na.rm=TRUE), 
                                                minimum = min(age,na.rm=TRUE), 
                                                maximum = max(age,na.rm=TRUE))

tests_age_thisweek <- week_test_df %>% filter(week==ThisWeek) %>%  summarize(median = median(age,na.rm=TRUE), 
                                                                             minimum = min(age,na.rm=TRUE), 
                                                                             maximum = max(age,na.rm=TRUE))

tests_age_lastweek <- week_test_df %>% filter(week==LastWeek) %>% summarize(median = median(age,na.rm=TRUE), 
                                                                            minimum = min(age,na.rm=TRUE), 
                                                                            maximum = max(age,na.rm=TRUE))


# 04 -lastweek ------------------------------------------------------------

# 04 - Cases in the last week 

# This week
#Generate a line list of cases in the current week table to add 
ThisWeekCasesCAMP<-FDMN_Only %>% 
  filter(Week==(ThisWeek)) %>% 
  group_by(camp_of_residence) 

MedianAgeThisWeek<-median(ThisWeekCasesCAMP$age_in_years)

# Male and Female breakdown 
SexThisWeek<- ThisWeekCasesCAMP %>%
  group_by(sex, .drop=FALSE) %>%
  summarise(count=n())

# Cases split by sex
ThisWeekMale<-SexThisWeek[2,2]
ThisWeekFemale<-SexThisWeek[1,2]

#Male and Female - %s
Male_Prop_ThisWeek<-round(sum(ThisWeekMale/(ThisWeekMale+ThisWeekFemale)*100), 1)
Female_Prop_ThisWeek<-round(sum(ThisWeekFemale/(ThisWeekMale+ThisWeekFemale)*100), 1)

#Generate a line list of cases in the last week table to add 
LastWeekCasesCAMP<- FDMN_Only %>% 
  filter(Week==(LastWeek)) 

# Male and Female breakdown 
SexLastWeek<- 
  FDMN_Only %>% 
  count(Week, sex) %>% 
  complete(Week,sex, fill = list(n = 0)) %>% 
  filter(Week==(LastWeek)) 

# LastWeekCasesCAMP %>%
# count(sex, .drop=FALSE)
# group_by(sex, .drop=FALSE) %>%
# summarise(count=n())

# Cases split by sex
LastWeekMale<-SexLastWeek[2,3]
LastWeekFemale<- SexLastWeek[1,3]

#Male and Female - %s
Male_Prop_Week<-round(sum(LastWeekMale/(LastWeekMale+LastWeekFemale)*100), 1)
Female_Prop_Week<-round(sum(LastWeekFemale/(LastWeekMale+LastWeekFemale)*100), 1)

#Median age of cases last week
MedianAgeLastWeek<-median(LastWeekCasesCAMP$age_in_years)

# # Age sex pyramid for cases lasy week
# AgeSex_Week<-
#   ggplot(LastWeekCasesCAMP,aes(x=agegp, fill=sex)) +
#             geom_bar(data=subset(LastWeekCasesCAMP,sex=="F"), color="white", width=1) +
#             geom_bar(data=subset(LastWeekCasesCAMP,sex=="M"), aes(y=..count..*(-1)), color="white", width=1) +
#             theme_minimal() +
#             xlab("Age group") +
#             scale_y_continuous(breaks=seq(-10,10,1),labels=abs(seq(-10,10,1))) +
#             scale_x_discrete(limits=levels(LastWeekCasesCAMP$agegp)) +
#             geom_hline(yintercept=0) +
#             coord_flip() +
#            theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
#  ggtitle("Figure 3: Confirmed cases in the last week - age and sex pyramid") +
#   theme(plot.title = element_text(size=10))+
#   scale_fill_discrete(name="Sex")

### Testing in the last week ###

# Testing in the last 7 days
# Making a % positive
# 
# FDMNPos<- TestingClean[, c(1,4,5)]
# FDMNPos$Positivity<-round(FDMNPos$fdmn_positive/FDMNPos$fdmn*100, 1)

FDMNPos <- TestingClean %>%  select(date, fdmn, fdmn_positive) %>% 
  mutate(Positivity=round(fdmn_positive/fdmn*100),1)

FDMNOverallPos<- FDMNPos %>%
  summarise(total_tests = sum(fdmn, na.rm=TRUE),
            total_cases = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(Overall_Pos = round(as.numeric(total_cases/total_tests*100), 1))
FDMNPos$date<-as.Date(FDMNPos$date)

#Overall positivity
FDMNOverallPosNum<-FDMNOverallPos$Overall_Pos

# positivity this week - 
FDMNPositvityThisWeek<- FDMNPos %>%
  mutate(Week=lubridate::isoweek(date)) %>% 
  filter(Week==ThisWeek) %>% 
  #filter(date >  today()- 7) %>%
  summarise(total_tests_ThisWeek = sum(fdmn, na.rm=TRUE),
            total_cases_ThisWeek = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_ThisWeek/total_tests_ThisWeek*100), 1))

# positivity last week
FDMNPositvityLastWeek<- FDMNPos %>%
  mutate(Week=lubridate::isoweek(date)) %>% 
  filter(Week==LastWeek) %>% 
  #filter(date >  today()- 7) %>%
  summarise(total_tests_LastWeek = sum(fdmn, na.rm=TRUE),
            total_cases_LastWeek = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_LastWeek/total_tests_LastWeek*100), 1))

# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_Week_FDMN<-ARI_ILI_Pos %>% 
  filter(Week == LastWeek) %>%
  filter(nationality == "FDMN")

# Test in the last week and positivity in the last week
n_Tests_Week_FDMN<-nrow(ARI_ILI_Pos_Week_FDMN)
WeekPositivity<-FDMNPositvity7day$WeekPositivty

# Number of tests done by camp
TestCampsWeek<-ARI_ILI_Pos_Week_FDMN %>%
  group_by(camp) %>%
  summarise(n=n())
TestCampsWeek<- TestCampsWeek

# number positives and negatives by week and camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos_Week_FDMN %>% 
  group_by(camp, laboratory_result) %>%
  summarise(n=n())%>%
  spread(laboratory_result, n) %>% 
  mutate(Positive=0)

#Overall positivity by camp
ARI_ILI_Pos_Split$positivity<-round(ARI_ILI_Pos_Split$Positive/(ARI_ILI_Pos_Split$Negative + ARI_ILI_Pos_Split$Positive)*100, 1)

#Left joining with the number of tests
Week_ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCampsWeek, by ="camp" )

#creating a flextable ordering positivity from high to low
Week_Camp<-Week_ARI_ILI_CampTable[order(Week_ARI_ILI_CampTable$positivity, na.last = T, decreasing = T),] 

#Filtering for cases by camp in the last week 
WeekCamp2<-filter(Week_Camp, Positive >=1)

Week_CampFlex<-flextable(WeekCamp2)

Week_CampFlex<-autofit(Week_CampFlex) %>% 
  set_header_labels(camp = "Camp of Residence",
                    n = "Total tests - last 7 days",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header")

Week_CampFlex<- fontsize(Week_CampFlex, size = 10, part="all") 
Week_CampFlex_Report<-autofit(Week_CampFlex)





# This week's table -------------------------------------------------------

# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_ThisWeek_FDMN<- ARI_ILI_Pos %>% 
  filter(Week == ThisWeek) %>% 
  filter(nationality == "FDMN")

# Test in the last week and positivity in the last week

FDMNPositvity_ThisWeek<- FDMNPos %>%
  mutate(Week=isoweek(date)) %>% 
  filter(Week == ThisWeek) %>%
  #filter(date >  today()- 7) %>%
  summarise(total_tests_7days = sum(fdmn, na.rm=TRUE),
            total_cases_7days = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_7days/total_tests_7days*100), 1))

n_Tests_ThisWeek_FDMN<-nrow(ARI_ILI_Pos_ThisWeek_FDMN)
ThisWeekPositivity<-FDMNPositvity_ThisWeek$WeekPositivty

# Number of tests done by camp
TestCamps_ThisWeek<-ARI_ILI_Pos_ThisWeek_FDMN %>%
  count(camp) %>% 
  mutate(key=sub('Camp ', '', camp))
#group_by(camp) %>%
#summarise(n=n())
#TestCampsWeek<- TestCampsWeek

# number positives and negatives by week and camp
ARI_ILI_Pos_Split_ThisWeek<-ARI_ILI_Pos_ThisWeek_FDMN %>% 
  count(camp, laboratory_result) %>%
  #summarise(n=n())%>%
  spread(laboratory_result, n) %>% 
  mutate(key=sub('Camp ', '', camp)) %>% 
  mutate(Positive=0)

#Overall positivity by camp
ARI_ILI_Pos_Split_ThisWeek$positivity<-round(ARI_ILI_Pos_Split_ThisWeek$Positive/
                                               (ARI_ILI_Pos_Split_ThisWeek$Negative + ARI_ILI_Pos_Split_ThisWeek$Positive)*100, 1)

#Left joining with the number of tests
ThisWeek_ARI_ILI_CampTable_ThisWeek<-left_join(ARI_ILI_Pos_Split_ThisWeek,TestCamps_ThisWeek, by ="key" )

ThisWeek_ARI_ILI_CampTable_ThisWeek<-full_join(ARI_ILI_Pos_Split_ThisWeek,TestCamps_ThisWeek, by ="key" )

#creating a flextable ordering positivity from high to low
ThisWeek_Camp<-ThisWeek_ARI_ILI_CampTable_ThisWeek[order(ThisWeek_ARI_ILI_CampTable_ThisWeek$positivity, na.last = T, decreasing = T),] 

#Filtering for cases by camp in the last week 
ThisWeekCamp2<-filter(ThisWeek_Camp, Positive >=1)

ThisWeek_CampFlex<-flextable(ThisWeekCamp2)

ThisWeek_CampFlex<-autofit(ThisWeek_CampFlex) %>% 
  set_header_labels(camp = "Camp of Residence",
                    n = "Total tests - last 7 days",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header")

ThisWeek_CampFlex<- fontsize(ThisWeek_CampFlex, size = 10, part="all") 
ThisWeek_CampFlex_Report<-autofit(ThisWeek_CampFlex)



# 05 -maps -----------------------------------------------------------------


#05 - CxB Map Script


#install.packages("extrafont")
extrafont::loadfonts(device="win")

my_colors <- brewer.pal(9, "Reds") 


shp_file_host <- read_sf(here('data', 'shapefiles', '190321_Outline_RRC_Camp-A1.shp')) %>%  select(New_Camp_N, geometry)



# Number of cases by camp
CasesCamp<- FDMN_Only2 %>%
  group_by(camp_of_residence) %>%
  summarise(cases=n()) %>% 
  mutate(camp_of_residence=as.character(camp_of_residence))

# Remove the word "camp" from the population data to allow merges
population<-population %>%
  mutate(camp_of_residence = gsub("Camp", "", New_Camp_Name)) %>% 
  mutate(camp_of_residence=case_when(New_Camp_Name=='Camp 4 Extension' ~ '4 Ext', 
                                     New_Camp_Name=='Camp 20 Extension' ~ '20 Ext', 
                                     TRUE ~ camp_of_residence))

#triming white space in the dataframe
population$camp_of_residence<-trimws(population$camp_of_residence, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#Merge the CasesCamp and Population data together
TablePop<- left_join(population, CasesCamp, by="camp_of_residence") %>% select(cases, Total_Pop, New_Camp_Name,Upazila)

#Generate the rates per 100,000 and add to the Table Pop dataframe
Ratesper100k<-round(TablePop$cases/TablePop$Total_Pop*100000, 1)
TablePop$Ratesper100k<-Ratesper100k

#Sort from high to low based on rates per 100,000
TablePopOrdered<-TablePop[order(TablePop$Ratesper100k, na.last= TRUE, decreasing = TRUE),]
#TablePopOrdered<-TablePopOrdered[-13]
TablePopOrdered$New_Camp_N<-TablePopOrdered$New_Camp_Name
#TablePopOrdered<-TablePopOrdered[-2]

Map_Rates100kData<-
  full_join(shp_file_host, TablePopOrdered, by="New_Camp_N")

# my_spfd<-
#   readOGR(
#   dsn = ("U:\\CxB - Virtual Deployment\\shape_files\\190310_Outline_Rohingya_Refugee_Camp_A1.shp"),
#   verbose = FALSE)

#my_spfd <- readOGR(here('data', 'shapefiles', '190310_Outline_Rohingya_Refugee_Camp_A1.shp'))

map_rates100k<-
  ggplot(data = Map_Rates100kData$geometry) +
  geom_sf(aes(fill=Map_Rates100kData$Ratesper100k)) +
  scale_fill_distiller(palette="YlGn", trans = "reverse") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(fill="Rate per 100,000")


TablePopOrdered_df <- TablePopOrdered %>% 
  mutate(camp_key=stringr::str_remove(New_Camp_N, "^Camp 0+")) %>% 
  mutate(camp_key=gsub('Camp ', '',camp_key)) %>% 
  mutate(camp_key=case_when(camp_key=='20 Extension' ~ '20 Ext',
                            camp_key=='4 Extension' ~ '4 ext',
                            TRUE ~ camp_key))

Map_Tests100kData <- ARI_ILI_CampTable %>% 
  mutate(camp_key=stringr::str_remove(camp, "^Camp 0+")) %>% 
  mutate(camp_key=gsub('Camp ', '',camp_key)) %>% 
  # mutate(New_Camp_N=sub("0+", "", camp)) %>% 
  ungroup() %>% 
  select(camp_key, tests=n) %>% 
  left_join(TablePopOrdered_df,.,by="camp_key") %>% 
  mutate(tests100k=tests/Total_Pop*100000) %>% 
  left_join(shp_file_host,.,by="New_Camp_N")




# moved-code --------------------------------------------------------------

FDMN_Only$count <- 1

summary_week  <-  FDMN_Only2 %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek)
#min_week <- min(summary_week$yearweek)

Epicurve<-
  ggplot(summary_week, aes(x=yearweek, y=n)) +
  geom_col(position="stack", fill = "#ED7D31") +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) + 
  labs(x = "Week", y = "Number of cases") +
  scale_fill_manual(values=c("#4472C4")) +
  #coord_cartesian(xlim = c(min_week, ThisWeek))  +
  scale_x_yearweek(date_breaks = "4 week",) +
  #scale_x_continuous(limits = c(min_week, ThisWeek)) %>% 
  #scale_x_continuous(limits=c(min_week, ThisWeek)) %>% 
  # ggtitle("Figure 1: Confirmed cases by specimen collection week (FDMN/ Rohingya Refugees)") + 
  theme(plot.title = element_text(size=10))


cases_tests_table <- camp_test_fortnight_flex %>% 
  add_header_lines(., values = "Table 1: Number of cases, tests and % COVID-19 positive tests for camps reporting at least 1 case in the last 2 weeks")

summary_Sex <- FDMN_Only2 %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek, sex) %>% 
  mutate(sex=case_when(sex=='M' ~ 'Male',
                       sex=='F' ~ 'Female'))

EpicurveSex<-
  ggplot(summary_Sex, aes(x=yearweek, y=n, fill=sex)) +
  geom_col() +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) + 
  scale_fill_brewer(palette="Dark2", direction=-1) +
  #scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  scale_x_yearweek(date_breaks = "4 week",) +
  labs(x = "Week", y = "Number of cases", fill='') +
  #ggtitle("Figure 2: Confirmed cases by specimen collection week (FDMN/ Rohingya Refugees) by sex") + 
  theme(plot.title = element_text(size=10))
#scale_fill_discrete(name="")


FDMN_df <- FDMN_Only2 %>%  mutate(sex=case_when(sex=='M' ~ 'Male',
                                                sex=='F' ~ 'Female')) %>% 
  mutate(age_grp=cut(age_in_years, breaks=c(-Inf,18,59, +Inf), 
                     labels=c('Under 18', '18 to 59' ,'60 and over')))

AgeSex <-    ggplot(FDMN_df,aes(x=age_grp, fill=sex)) +
  geom_bar(data=subset(FDMN_df,sex=="Female"), color="white", width=1) +
  geom_bar(data=subset(FDMN_df,sex=="Male"), aes(y=..count..*(-1)), color="white", width=1) +
  theme_minimal() +
  labs(x='Age group', y='Number of cases') +
  scale_y_continuous(breaks=seq(-100,100,10),labels=abs(seq(-100,100,10))) +
  #scale_x_discrete(limits=levels(.$agerp)) +
  geom_hline(yintercept=0) +
  scale_fill_brewer(palette="Dark2", direction=-1) +
  coord_flip() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.title=element_blank())

population_agegrp <- readxl::read_xlsx(here('data','block_population.xlsx'), sheet='Final', skip=1) %>% 
  clean_names %>%  
  clean_data() %>% 
  filter(grepl('total', camp)) %>% 
  filter(!camp=='grand_total') %>% 
  select(-block) %>% 
  mutate(across(c(infant_below_1:x16), as.numeric)) %>% 
  mutate(age_0_18 = rowSums(.[4:11])) %>% 
  mutate(age_18_59 = rowSums(.[12:13])) %>% 
  mutate(age_over60 = rowSums(.[14:15])) %>% 
  select(camp, contains('total'), contains('age')) %>% 
  mutate(camp=gsub('_total', '', camp)) %>% 
  mutate(camp=gsub('camp_', '', camp)) %>% 
  mutate(camp=trimws(camp))  %>% 
  select(camp, contains('age')) %>% 
  summarise(age_0_18=sum(age_0_18,na.rm=TRUE),
            age_18_59=sum(age_18_59,na.rm=TRUE),
            age_over60=sum(age_over60,na.rm=TRUE)) %>% 
  mutate(dummy=1) %>% 
  pivot_longer(-dummy) %>% 
  mutate(age_grp=case_when(name=='age_0_18' ~ 'Under 18',
                           name=='age_18_59' ~ '18 to 59',
                           name=='age_over60' ~ '60 and over')) %>% 
  select(-c(dummy,name))

cases_agegrp <- FDMN_Only2 %>% 
  mutate(age_grp=cut(age_in_years, breaks=c(-Inf,18,59, +Inf), labels=c('Under 18', '18 to 59' ,'60 and over'))) %>% 
  #  mutate(week=isoweek(date_of_case_detection)) %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek,age_grp, .drop=FALSE) %>% 
  left_join(population_agegrp, by='age_grp') %>% 
  mutate(per_capita=(n/value)*100000) %>% 
  mutate(age_grp=factor(age_grp, levels=c('Under 18', '18 to 59', '60 and over'))) %>% 
  ggplot(., aes(x=yearweek,y=per_capita, fill=age_grp)) +
  geom_col(width = 3) +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2", direction=-1) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) +
  #scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  scale_x_yearweek(date_breaks = "4 week") +
  labs(x="Week", y="Cases 100,000 people", fill='Age group')

Summary_Severity<- FDMN_Only2 %>%
  mutate(severity_of_disease=factor(severity_of_disease, levels=c('Critical', 'Severe', 'Mild/Moderate', 'Asymptomatic', 'Unknown'))) %>% 
  group_by(severity_of_disease) %>%
  summarise(count=n()) %>% 
  adorn_totals(where=c('row')) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() 



Summary_Severity_Flex<-flextable(Summary_Severity)

Table1<-Summary_Severity_Flex %>%
  set_header_labels(severity_of_disease = "Severity of Disease",
                    count = "Frequency",
                    Percentage = "Percentage of total (%)") %>%
  bold(bold = TRUE, part = "header")
Table1<-autofit(Table1)
Table1<- fontsize(Table1, size = 10, part="all") 

case_severity <- Table1 %>% 
  add_header_lines(., values = "Table 2: Cases by severity of disease at presentation")


summary_severity <- FDMN_Only2 %>%   mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek, severity_of_disease) %>% 
  mutate(severity_of_disease=ifelse(is.na(severity_of_disease), 'Unknown',severity_of_disease))


SeverityEpicurve<-
  ggplot(summary_severity, aes(x=yearweek, y=n, fill=factor(severity_of_disease, levels=c('Critical', 'Severe', 'Mild/Moderate', 'Asymptomatic', 'Unknown')))) +
  geom_col(position="stack") +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2") +
  #scale_fill_manual(values=c("yellow", "red", "forest green", "dark orange", "dark grey")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) +
  #scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  scale_x_yearweek(date_breaks = "4 week",) +
  labs(x = "Week", y = "Number of cases", fill='Severity of disease at presentation') 
#ggtitle("Figure 3: Confirmed cases last week by severity (FDMN/ Rohingya Refugees)") + theme(plot.title = element_text(size=10))


EpicurveSex<-
  ggplot(summary_Sex, aes(x=yearweek, y=n, fill=sex)) +
  geom_col() +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) + 
  scale_fill_brewer(palette="Dark2", direction=-1) +
  #scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  scale_x_yearweek(date_breaks = "4 week",) +
  labs(x = "Week", y = "Number of cases", fill='') +
  #ggtitle("Figure 2: Confirmed cases by specimen collection week (FDMN/ Rohingya Refugees) by sex") + 
  theme(plot.title = element_text(size=10))
#scale_fill_discrete(name="")


#Sum_tab_Severity<-as.data.frame(table(FDMN_Only$`30_day_outcome`, FDMN_Only$severity_of_disease))


Sum_tab_Severity <- FDMN_Only2 %>% 
  mutate(severity_of_disease=factor(severity_of_disease, levels=c('Critical', 'Severe', 'Mild/Moderate', 'Asymptomatic', 'Unknown'))) %>% 
  clean_names() %>% 
  tabyl(severity_of_disease,x30_day_outcome) %>% 
  adorn_totals(where=c('row', 'col')) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>% 
  rename('Disease severity'=severity_of_disease)

#names(Sum_tab_Severity) <- c('Outcome at 30 days', 'Severity of disease','count')


# Sum_tab_Severity <- Sum_tab_Severity %>% 
#   pivot_wider(names_from='Outcome at 30 days', values_from=count, values_fill = 0)


Sum_tab_Severity2<-flextable(Sum_tab_Severity)
Sum_tab_Severity2<-autofit(Sum_tab_Severity2)
Sum_tab_Severity2<- fontsize(Sum_tab_Severity2, size = 10, part="all") %>% 
  bold(bold = TRUE, part = "header")



# DeathsAsymp<-Sum_tab_Severity[1,2]
# DeatsCritical <-Sum_tab_Severity[2,2]
# DeathsSevere<-Sum_tab_Severity[4,2]
# DeathsMildModerate	<-Sum_tab_Severity[3,2]
# DeathsUnknown<-Sum_tab_Severity[5,2]

severity_outcome <- Sum_tab_Severity2  %>% 
  add_header_lines(., values = "Table 3: Cases by severity of disease at presentation & 30-day outcome")


#CasesCampTable<-CasesCamp[order(-CasesCamp$cases), ]

# CasesCampTable<-flextable(CasesCampTable)
# CasesCampTable<-autofit(CasesCampTable)

# Number of cases by camp
CasesCamp<- FDMN_Only2 %>%
  group_by(camp_of_residence) %>%
  summarise(count=n()) %>% 
  mutate(camp_of_residence=as.character(camp_of_residence))

# Remove the word "camp" from the population data to allow merges
population<-population %>%
  mutate(camp_of_residence = gsub("Camp", "", New_Camp_Name))

#triming white space in the dataframe
population$camp_of_residence<-trimws(population$camp_of_residence, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#Merge the CasesCamp and Population data together
TablePop<- left_join(population, CasesCamp, by="camp_of_residence")

#Generate the rates per 100,000 and add to the Table Pop dataframe
Ratesper100k<-round(TablePop$count/TablePop$Total_Pop*100000, 1)
TablePop$Ratesper100k<-Ratesper100k

#Sort from high to low based on rates per 100,000
TablePopOrdered<-TablePop[order(TablePop$Ratesper100k, na.last= TRUE, decreasing = TRUE),]

cases_map <-    Map_Rates100kData %>% 
  mutate(camp_label=gsub('Camp ', '', New_Camp_Name)) %>% 
  mutate(camp_label=gsub('Extension', 'E', camp_label)) %>% 
  mutate(upazila_upd=case_when(camp_label %in% c('21','22','23') ~ 'Teknaf North',
                               camp_label %in% c('24', '25', '26', '27', 'Nayapara RC') ~ 'Teknaf South',
                               TRUE ~ Upazila)) %>% 
  mutate(upazila_upd=factor(upazila_upd, levels=c('Ukhia', 'Teknaf South', 'Teknaf North'))) %>% 
  filter(!is.na(Upazila)) %>% 
  tm_shape() +
  tm_polygons("Ratesper100k", title="Cases per 100,000", palette="YlGn",textNA = "No reported cases") +
  tm_facets(by="upazila_upd", nrow=2) +
  #tm_text("camp_label", fontface = "bold") +
  tm_borders()

tests_map <- Map_Tests100kData %>% 
  mutate(camp_label=gsub('Camp ', '', New_Camp_Name)) %>% 
  mutate(camp_label=gsub('Extension', 'E', camp_label)) %>% 
  mutate(upazila_upd=case_when(camp_label %in% c('21','22','23') ~ 'Teknaf North',
                               camp_label %in% c('24', '25', '26', '27', 'Nayapara RC') ~ 'Teknaf South',
                               TRUE ~ Upazila)) %>% 
  mutate(upazila_upd=factor(upazila_upd, levels=c('Ukhia', 'Teknaf South', 'Teknaf North'))) %>% 
  filter(!is.na(Upazila)) %>% 
  tm_shape() +
  tm_polygons("tests100k", title="Tests per 100,000", palette="BuPu",textNA = "No reported tests") +
  tm_facets(by="upazila_upd", nrow=2) +
  #tm_text("camp_label",  fontface = "bold") +
  tm_borders()



FDMN_Test_graph<-test_nationality %>% 
  filter(name %in% c('fdmn', 'fdmn_positive')) %>% 
  mutate(week=isoweek(date_format)) %>% 
  mutate(year=year(date_format)) %>% 
  filter(week <= ThisWeek | year==2020) %>% 
  mutate(yearweek=yearweek(date_format)) %>% 
  select(yearweek,name,value) %>% 
  group_by(yearweek,name) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(name_fac=factor(name, levels = c('fdmn', 'fdmn_positive'),
                         labels=c('Tests', 'Cases'))) %>% 
  ggplot(., aes(x=yearweek, y=value, fill=name_fac)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  scale_x_yearweek(date_breaks = "8 week",) +
  #      scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  # scale_x_date(date_breaks= '30 day', date_minor_breaks = '7 day',
  #              date_labels = '%d-%m') +
  labs(caption = "Data source: Cox’s Bazar - IEDCR Field Lab",
       x="Week",
       y= "Number",
       fill="") +
  theme_minimal()


#Making a % positive
FDMNPos<- TestingClean[, c(1,4,5)]
FDMNPos$Positivity<-round(FDMNPos$fdmn_positive/FDMNPos$fdmn*100, 1)

FDMNOverallPos<- FDMNPos %>%
  summarise(total_tests = sum(fdmn, na.rm=TRUE),
            total_cases = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(Overall_Pos = round(as.numeric(total_cases/total_tests*100), 1))

FDMNOverallPosNum<-FDMNOverallPos$Overall_Pos

FDMNPos$date<-as.Date(FDMNPos$date)

Host_Test_graph <- test_nationality %>% 
  filter(name %in% c('host', 'host_positive')) %>% 
  mutate(week=isoweek(date_format)) %>% 
  mutate(year=year(date_format)) %>% 
  select(year,week,name,date_format,value) %>% 
  filter(week <= ThisWeek | year==2020) %>% 
  mutate(yearweek=yearweek(date_format)) %>% 
  group_by(yearweek,name) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(name_fac=factor(name, levels = c('host', 'host_positive'),
                         labels=c('Tests', 'Cases'))) %>% 
  ggplot(., aes(x=yearweek, y=value, fill=name_fac)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  scale_x_yearweek(date_breaks = "8 week",) +
  #scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  # scale_x_continuous(limits=c(10,ThisWeek)) +
  # scale_x_date(date_breaks= '30 day', date_minor_breaks = '7 day',
  #              date_labels = '%d-%m') +
  labs(caption = "Data source: Cox’s Bazar - IEDCR Field Lab",
       x="Week",
       y= "Number",
       fill="") +
  theme_minimal()

#Bangladesh data from Our World In Data
bgd_data <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>% 
  filter(iso_code=='BGD') %>%        
  mutate(date_format=ymd(date)) %>% 
  select(date=date_format, new_tests,new_cases,new_deaths,population) %>% 
  mutate(population_group='Bangladesh') %>% 
  mutate(week=isoweek(date)) %>% 
  mutate(year=year(date)) %>% 
  #select(year,week,name,date_format,value) %>% 
  filter(week <= ThisWeek | year==2020) %>% 
  mutate(yearweek=yearweek(date)) 
#mutate(week=isoweek(date)) %>% 
#filter(week<=ThisWeek)

bgd_graph <- bgd_data %>% 
  select(yearweek, new_cases, new_tests) %>% 
  group_by(yearweek) %>% 
  summarise(cases=sum(new_cases,na.rm=TRUE),
            tests=sum(new_tests,na.rm=TRUE)) %>% 
  pivot_longer(-yearweek) %>% 
  mutate(name_fac=factor(name, levels = c('tests', 'cases'),
                         labels=c('Tests', 'Cases'))) %>% 
  ggplot(., aes(x=yearweek, y=as.numeric(value), fill=name_fac)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  scale_x_yearweek(date_breaks = "8 week",) +
  # scale_x_continuous(breaks=breaks_pretty(n = 8)) +
  # scale_x_date(date_breaks= '30 day', date_minor_breaks = '7 day',
  #              date_labels = '%d-%m', limits=c(ymd('2020-03-17'), today())) +
  labs(caption = "Data source: IEDCR (https://covid19bd.idare.io/)",
       x="Week",
       y= "Number",
       fill="") +
  theme_minimal()

