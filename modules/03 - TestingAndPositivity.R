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

FDMNPos<- TestingClean[, c(1,4,5)]
FDMNPos$Positivity<-round(FDMNPos$fdmn_positive/FDMNPos$fdmn*100, 1)

FDMNOverallPos<- FDMNPos %>%
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
  spread(laboratory_result, n)

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
  #filter(date_of_case_detection>=ymd('2020-05-01')) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  #mutate(week=date2week(date_of_case_detection,week_start = "sun", floor_day = TRUE)) %>% 
  select(week,date_of_case_detection, laboratory_result, age) %>% 
  filter(week>19) %>% 
  #mutate(week=factor(week, levels=unique(week))) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(age=as.numeric(as.character(unlist(age))))

tests_age_gph <- ggplot(week_test_df, aes(x=week, y=age, group=factor(week))) + 
  #geom_jitter(colour="lightblue", alpha=0.5, width=0.1) +
  #geom_point(stat="summary", fun.y="mean") + 
  geom_boxplot(alpha = 0.80) +
  #geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Week", y="Age (median with interquartile range)") +
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
