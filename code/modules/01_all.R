#Custom function for median/minimum/maximum
fn_medminmax <- function(x,df,z){
  if(missing(z)) {
    df %>% 
      select(my_x=!!enquo(x)) %>% 
      summarize(median=median(my_x,na.rm=TRUE),
                minimum=min(my_x,na.rm=TRUE), 
                maximum=max(my_x,na.rm=TRUE))} else {
                  df %>% 
                    filter(week==z) %>% 
                    select(my_x=!!enquo(x)) %>% 
                    summarize(median=median(my_x,na.rm=TRUE),
                              minimum=min(my_x,na.rm=TRUE), 
                              maximum=max(my_x,na.rm=TRUE))
                }
}
# 01 - cleaningscript2 ----------------------------------------------------

#cleaning the column names - changing to all lower cases and spaces separated by _
cleaned_colnames <- epitrix::clean_labels(colnames(FDMN_Only))
colnames(FDMN_Only) <- cleaned_colnames
rm(cleaned_colnames)

#Remove blank rows - removing rows where all the entries are "NA"
FDMN_Only2<- FDMN_Only %>%
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  mutate(year=year(date_of_case_detection)) %>% 
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek))
  
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

#Remove blank rows - removing rows where all the entries are "NA"
TestingClean<- TestingClean %>%
  mutate(date=excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"),
         year=isoyear(date),
         week=isoweek(date)) %>% 
  filter_all(any_vars(!is.na(.))) 


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


# Fixing Typos
FDMN_Only2 <- FDMN_Only2 %>% 
  mutate(camp_of_residence=as.character(unlist(camp_of_residence)),
         camp_of_residence=ifelse(camp_of_residence=="Nyapara RC", "Nayapara RC",camp_of_residence),
         `30_day_outcome`=ifelse(`30_day_outcome`=="Recoverd", "Recovered",`30_day_outcome`))


# 02 - figuresetting_summary ----------------------------------------------

#SETTING SUMMARY FIGURES

#Set CFR
Outcome_Proportions<- FDMN_Only2 %>%
  mutate(Week=isoweek(date_of_case_detection),
         year=year(date_of_case_detection)) %>% 
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek)) %>% 
  count(`30_day_outcome`) %>% 
  mutate(Percentage=n/sum(n),
         Percentage=scales::percent(Percentage, accuracy=.1)) 

# outcomes at 30 days
sum_tab_30 <- FDMN_Only2 %>%
  clean_names() %>%
  count(x30_day_outcome)

#Deaths at 30 days follow up
deathscount<-sum_tab_30 %>%  filter(x30_day_outcome=='Death') %>% pull(n)


#Set number of camps with at least one confirmed case
CasesCamp<- FDMN_Only2 %>%
  group_by(camp_of_residence) %>%
  summarise(count=n())

CasesCampVal <-  CasesCamp %>%  filter(!camp_of_residence=='Outside Camp') %>% pull(camp_of_residence)

CampsWithCases<-length(CasesCampVal)

# Median age of cases (min and max)
case_age_range <- fn_medminmax(age_in_years,FDMN_Only2)

medianage<- case_age_range %>% pull(median)
RangeUpper<- case_age_range %>% pull(maximum)
RangeLow<- case_age_range %>% pull(minimum)


FDMN_Only <- FDMN_Only2 %>%  
  filter(week <= ThisWeek)

# Sex
Sex<- FDMN_Only2 %>%
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek)) %>% 
  count(sex)

# Cases split by sex
Male<- Sex %>% filter(sex=='M') %>% pull(n)
Female<- Sex %>% filter(sex=='F') %>% pull(n)

#Male and Female - %s
Male_Prop<-round(sum(Male/(Male+Female)*100), 1)
Female_Prop<-round(sum(Female/(Male+Female)*100), 1)

#Setting time frames used in this report
first_date <- min(TestingClean$date) 
last_date <- today() 

LastWeekCases<- FDMN_Only2 %>% 
  filter(week <=(LastWeek)) %>% 
  summarise(count=n())

DifferenceLastWeek<-n_cases_FDMN - LastWeekCases 

# Setting an attack rate 
FDMN_Pop<-as.integer(sum(population$Total_Pop))

AR<-round((n_cases_FDMN/FDMN_Pop)*100,3)
AR100k<-round((n_cases_FDMN/FDMN_Pop)*100000,1)


# 03 - testingandpositivity -----------------------------------------------

#TESTING AND POSITIVITY

ARI_ILI_Pos <-ari_ili %>%
  filter(laboratory_result %in% c('Positive', 'Negative') & nationality=='FDMN' & !sample_type %in% c('Follow-up', 'Humanitarian Worker')) %>% 
  select(case_id, sample_type, date_of_case_detection, health_facility_name=health_facility_name_sample_collection_site, 
         camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
  mutate(Week=isoweek(date_of_case_detection), 
         camp=ifelse(camp=="Camp 20 EXT" |camp=="Camp 20 ext", "Camp 20 Ext", camp))

TestCamps<-ARI_ILI_Pos %>%
  count(camp)

# number positives and negatives by camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos %>% 
  count(camp, laboratory_result) %>%
  spread(laboratory_result, n) 

# number positives and negatives by camp & week
ARI_ILI_Pos_Week_Split <- ARI_ILI_Pos %>% 
  count(camp, laboratory_result, Week) %>%
  spread(laboratory_result, n) %>% 
  mutate(Positivity=round((Positive/(Negative+Positive))*100,1))

#Left joining with the number of tests
ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCamps, by ="camp")

#Testing in total
FDMN_Test<-as.integer(sum(TestingClean$fdmn))

# Testing in the last 7 days
# Making a % positive
FDMNPos <- TestingClean %>% 
  select(date,fdmn,fdmn_positive,year,week) %>% 
  mutate(Positivity=round(fdmn_positive/fdmn*100,1))

FDMNOverallPos<- FDMNPos %>%
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek)) %>% 
  summarise(total_tests = sum(fdmn, na.rm=TRUE),
            total_cases = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(Overall_Pos = round(as.numeric(total_cases/total_tests*100), 1))

FDMNOverallPosNum<-FDMNOverallPos$Overall_Pos

FDMNPositvity7day<- FDMNPos %>%
  mutate(Week=isoweek(date)) %>% 
  filter(Week == LastWeek) %>%
  summarise(total_tests_7days = sum(fdmn, na.rm=TRUE),
            total_cases_7days = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_7days/total_tests_7days*100), 1))

# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_Week_FDMN<-ARI_ILI_Pos %>% 
  filter(Week == LastWeek & nationality == "FDMN")

# Test in the last week and positivity in the last week
n_Tests_Week_FDMN<-nrow(ARI_ILI_Pos_Week_FDMN)
WeekPositivity<-FDMNPositvity7day$WeekPositivty

# Number of tests done by camp last week
TestCampsWeek<-ARI_ILI_Pos_Week_FDMN %>%
  count(camp)

# number positives and negatives by week and camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos_Week_FDMN %>% 
  count(camp, laboratory_result) %>%
  spread(laboratory_result, n) %>% 
  mutate(Positivity=round((Positive/(Negative+Positive))*100,1))


#Left joining with the number of tests
Week_ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCampsWeek, by ="camp" )

#creating a flextable ordering positivity from high to low

Week_CampFlex<- Week_ARI_ILI_CampTable %>% arrange(Positivity) %>%
  filter(Positive >=1) %>% 
  select(camp, n,Positivity) %>% 
  flextable() %>% 
  autofit(.) %>% 
set_header_labels(camp = "Camp",
                  n = "Total tests - last 7 days",
                  Positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header") %>% 
  fontsize(., size = 10, part="all") 

Week_CampFlex_Report<-autofit(Week_CampFlex)

##First testing date
first_test_fdmn <- ari_ili %>%  filter(nationality=='FDMN' & 
                                         date_of_case_detection==min(date_of_case_detection)) %>%  
  pull(date_of_case_detection)

first_test_fdmn_week <- isoweek(first_test_fdmn)
first_test_fdmn_day <- append_date_suffix(first_test_fdmn)
first_test_fdmn_month <- month(first_test_fdmn, label=TRUE, abbr=FALSE)
first_test_fdmn_day_month <- sprintf("the %s of %s", first_test_fdmn_day, first_test_fdmn_month)

##First case detection
first_case_fdmn <- ari_ili %>%  filter(nationality=='FDMN' & 
                                         laboratory_result=='Positive' & 
                                         date_of_case_detection==min(date_of_case_detection)) %>%  
  pull(date_of_case_detection)

first_case_fdmn_week <- isoweek(first_case_fdmn)
first_case_fdmn_day <- append_date_suffix(first_case_fdmn)
first_case_fdmn_month <- month(first_case_fdmn, label=TRUE, abbr=FALSE)
first_case_fdmn_day_month <- sprintf("the %s of %s", first_case_fdmn_day, first_case_fdmn_month)

###Median age of test

week_test_df <- ari_ili %>%  
  clean_data() %>% 
  filter(!laboratory_result %in% c('n_a','not_done') & 
           nationality=='fdmn' & 
           !sample_type %in% c('follow_up', 'humanitarian_worker'),
         laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(week=isoweek(date_of_case_detection), 
         yearweek=yearweek(date_of_case_detection), 
         age=as.numeric(as.character(unlist(age)))) %>% 
  select(yearweek,week,date_of_case_detection, laboratory_result, age) 

tests_age_gph <- ggplot(week_test_df, aes(x=yearweek, y=age, group=factor(yearweek))) + 
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Week", y="Age (mean with 95% CI)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(breaks=seq(0,100,10))

tests_age_overall <- fn_medminmax(age,week_test_df)
tests_age_thisweek <- fn_medminmax(age,week_test_df, ThisWeek)
tests_age_lastweek <- fn_medminmax(age,week_test_df, LastWeek)

# 04 -lastweek ------------------------------------------------------------

# This week
#Generate a line list of cases in the current week table to add 
ThisWeekCasesCAMP<-FDMN_Only %>% 
  filter(week==(ThisWeek)) 

MedianAgeThisWeek <- fn_medminmax(age_in_years,ThisWeekCasesCAMP, ThisWeek) %>% pull(median)

# Male and Female breakdown 
SexThisWeek<- ThisWeekCasesCAMP %>%
  count(sex, .drop=FALSE) %>% 
  rename(count=n)

# Cases split by sex
ThisWeekMale<-SexThisWeek %>% filter(sex=='M') %>% pull(count)
ThisWeekFemale<-SexThisWeek %>% filter(sex=='F') %>% pull(count)

#Male and Female - %s
Male_Prop_ThisWeek<-round(sum(ThisWeekMale/(ThisWeekMale+ThisWeekFemale)*100), 1)
Female_Prop_ThisWeek<-round(sum(ThisWeekFemale/(ThisWeekMale+ThisWeekFemale)*100), 1)

#Generate a line list of cases in the last week table to add 
LastWeekCasesCAMP<- FDMN_Only %>% 
  filter(week==(LastWeek))  

# Male and Female breakdown 
SexLastWeek<- 
  FDMN_Only %>% 
  count(week, sex) %>% 
  complete(week,sex, fill = list(n = 0)) %>% 
  filter(week==(LastWeek)) 

# Cases split by sex
LastWeekMale<-SexLastWeek %>% filter(sex=='M') %>%  pull(n)
LastWeekFemale<- SexLastWeek %>% filter(sex=='F') %>%  pull(n)

#Male and Female - %s
Male_Prop_Week<-round(sum(LastWeekMale/(LastWeekMale+LastWeekFemale)*100), 1)
Female_Prop_Week<-round(sum(LastWeekFemale/(LastWeekMale+LastWeekFemale)*100), 1)

#Median age of cases last week
MedianAgeLastWeek <- fn_medminmax(age_in_years,LastWeekCasesCAMP, LastWeek) %>% pull(median)

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
  mutate(week=lubridate::isoweek(date)) %>% 
  filter(week==ThisWeek) %>% 
  summarise(total_tests_ThisWeek = sum(fdmn, na.rm=TRUE),
            total_cases_ThisWeek = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_ThisWeek/total_tests_ThisWeek*100), 1))

# positivity last week
FDMNPositvityLastWeek<- FDMNPos %>%
  mutate(week=lubridate::isoweek(date)) %>% 
  filter(week==LastWeek) %>% 
  summarise(total_tests_LastWeek = sum(fdmn, na.rm=TRUE),
            total_cases_LastWeek = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_LastWeek/total_tests_LastWeek*100), 1))

# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_Week_FDMN<-ARI_ILI_Pos %>% 
  rename(week=Week) %>% 
  filter(week == LastWeek & nationality == "FDMN")

# Test in the last week and positivity in the last week
n_Tests_Week_FDMN<-nrow(ARI_ILI_Pos_Week_FDMN)
WeekPositivity<-FDMNPositvity7day$WeekPositivty

# Number of tests done by camp
TestCampsWeek<-ARI_ILI_Pos_Week_FDMN %>%
  count(camp) 

TestCampsWeek<- TestCampsWeek

# number positives and negatives by week and camp
ARI_ILI_Pos_Split<-ARI_ILI_Pos_Week_FDMN %>% 
  count(camp, laboratory_result) %>%
  spread(laboratory_result, n) %>% 
  mutate(positivity=round((Positive/(Negative+Positive))*100,1))


#Left joining with the number of tests
Week_ARI_ILI_CampTable<-left_join(ARI_ILI_Pos_Split,TestCampsWeek, by ="camp" )

#creating a flextable ordering positivity from high to low
Week_CampFlex <- Week_ARI_ILI_CampTable %>% 
  filter(Positive>=1) %>% 
  arrange(positivity) %>% 
  flextable() %>% 
  set_header_labels(camp = "Camp of Residence",
                    n = "Total tests - last 7 days",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header") %>% 
  fontsize(., size = 10, part="all") 

Week_CampFlex_Report<-autofit(Week_CampFlex)


# Tests in the last week - FDMN
#Filter for tests done last week 
ARI_ILI_Pos_ThisWeek_FDMN<- ARI_ILI_Pos %>% 
  rename(week=Week) %>% 
  filter(week == ThisWeek, 
         nationality == "FDMN")

# Test in the last week and positivity in the last week
FDMNPositvity_ThisWeek<- FDMNPos %>%
  mutate(week=isoweek(date)) %>% 
  filter(week == ThisWeek) %>%
  summarise(total_tests_7days = sum(fdmn, na.rm=TRUE),
            total_cases_7days = sum(fdmn_positive, na.rm=TRUE)) %>%
  mutate(WeekPositivty = round(as.numeric(total_cases_7days/total_tests_7days*100), 1))

n_Tests_ThisWeek_FDMN<-nrow(ARI_ILI_Pos_ThisWeek_FDMN)
ThisWeekPositivity<-FDMNPositvity_ThisWeek$WeekPositivty

# Number of tests done by camp
TestCamps_ThisWeek<-ARI_ILI_Pos_ThisWeek_FDMN %>%
  count(camp) %>% 
  mutate(key=sub('Camp ', '', camp))


# number positives and negatives by week and camp
ARI_ILI_Pos_Split_ThisWeek<-ARI_ILI_Pos_ThisWeek_FDMN %>% 
  count(camp, laboratory_result) %>%
  spread(laboratory_result, n) %>% 
  mutate(key=sub('Camp ', '', camp),
         positivity=round((Positive/(Negative+Positive))*100,1)) %>% 
  select(-camp)

#Left joining with the number of tests

ThisWeek_ARI_ILI_CampTable_ThisWeek<-full_join(ARI_ILI_Pos_Split_ThisWeek,TestCamps_ThisWeek, by ="key" )

#creating a flextable ordering positivity from high to low
ThisWeek_CampFlex <- ThisWeek_ARI_ILI_CampTable_ThisWeek %>% 
  arrange(positivity) %>% 
  filter(Positive >=1) %>% 
  select(camp, n, positivity) %>% 
  flextable() %>% 
  set_header_labels(camp = "Camp of Residence",
                    n = "Total tests - last 7 days",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header") %>% 
  fontsize( size = 10, part="all")

ThisWeek_CampFlex_Report<-autofit(ThisWeek_CampFlex)

# week-counts -------------------------------------------------------------

#Total number of cases

total_cases <- FDMN_Only2 %>% 
  # mutate(week=isoweek(date_of_case_detection), 
  #        year=year(date_of_case_detection)) %>% 
  dplyr::filter(year==2020 | (year==2021 & week<=ThisWeek)) %>% 
  summarise(total_cases=sum(n())) %>% 
  pull(total_cases)


#Number of cases by week
cases_week <- FDMN_Only2 %>% 
  #mutate(week=isoweek(date_of_case_detection)) %>% 
  count(week) %>% 
  filter(week %in% c(ThisWeek, LastWeek)) %>% 
  pull(n)


#Number of cases by week by camp
cases_week_camp <- FDMN_Only2 %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  count(week, camp_of_residence) %>% 
  filter(week %in% c(ThisWeek, LastWeek)) %>% 
  select(week, camp_of_residence, cases=n) %>% clean_data()

camps_to_include <- cases_week_camp %>% select(camp_of_residence) %>%  distinct() %>% pull(camp_of_residence)


## Tests by location
ARI_ILI_df <-ari_ili %>%
  select(case_id, sample_type, date_of_case_detection,date_of_lab_result_received, 
         health_facility_name=health_facility_name_sample_collection_site, camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
  clean_data() %>% 
  filter(laboratory_result %in% c('positive', 'negative') & 
           nationality=='fdmn' & 
           !sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  mutate(date_of_case_detection=case_when(date_of_case_detection==ymd('2020-11-22') ~ ymd('2020-11-23'),
                                          TRUE ~ date_of_case_detection),
         week=isoweek(date_of_case_detection)) 

tests_week_camp <- ARI_ILI_df %>% 
  mutate(camp_of_residence=gsub('camp_', '', camp)) %>% 
  count(week, camp_of_residence) %>% 
  filter(week %in% c(ThisWeek, LastWeek)) %>% 
  select(week, camp_of_residence, tests=n) %>% 
  mutate(camp_of_residence=sub("^0+", "", camp_of_residence)) %>% clean_data()


###UPDATED TABLE WITH TWO WEEKS INCLUDED

this_week_col <- paste0('x', ThisWeek, '_positivity')
last_week_col <- paste0('x', LastWeek, '_positivity')

activity_2week_df <- cases_week_camp %>% mutate(camp_of_residence=as.character(camp_of_residence)) %>% 
  full_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
  group_by(week,camp_of_residence) %>% 
  summarise(cases=sum(cases,na.rm=TRUE),
            tests=sum(tests,na.rm=TRUE)) %>% 
  filter(camp_of_residence %in% camps_to_include) %>% 
  mutate(positivity=round((cases/tests)*100,1)) %>% 
  arrange(week,camp_of_residence) %>% 
  pivot_longer(-c(week,camp_of_residence)) %>% 
  replace_na(list(value=0)) %>% 
  pivot_wider(id_cols=camp_of_residence, names_from=c(week,name), values_from=value) %>% 
  mutate(camp_number=as.numeric(str_extract(camp_of_residence, "[[:digit:]]+"))) %>% 
  arrange(camp_number) %>% 
  select(-camp_number) %>% 
  clean_names() 

camp_test_fortnight_flex <- flextable(activity_2week_df) %>% 
  set_header_labels(camp_of_residence = "Camp",
                    x16_cases = glue("Cases ", "(Week ", {LastWeek}, ")"),
                    x16_tests = glue("Tests ", "(Week ", {LastWeek}, ")"),
                    x16_positivity = glue("% positive ", "(Week ", {LastWeek}, ")"),
                    x17_cases = glue("Cases ", "(Week ", {ThisWeek}, ")"),
                    x17_tests = glue("Tests ", "(Week ", {ThisWeek}, ")"),
                    x17_positivity = glue("% positive ", "(Week ", {ThisWeek}, ")")) %>% 
  bold(bold = TRUE, part = "header")

# Test summary from last week ---------------------------------------------

camp_test_fortnight <- ARI_ILI_df %>% 
  mutate(week=isoweek(date_of_lab_result_received)) %>% 
  count(week, laboratory_result) %>% 
  complete(week, laboratory_result, fill=list(n=0)) %>% 
  filter(week %in% c(LastWeek, ThisWeek)) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total_tests=positive+negative,
         positivity=positive/total_tests,
         positivity=scales::percent(positivity, accuracy=.1)) %>% 
  select(week, total_tests, positivity)


# 05 -maps -----------------------------------------------------------------

shp_file_host <- read_sf(here('data', 'shapefiles', '190321_Outline_RRC_Camp-A1.shp')) %>%  
  select(New_Camp_N, geometry)

# Number of cases by camp
CasesCamp<- FDMN_Only2 %>%
  count(camp_of_residence, name="cases") %>%
  mutate(camp_of_residence=as.character(camp_of_residence))

# Remove the word "camp" from the population data to allow merges
population<-population %>%
  mutate(camp_of_residence = gsub("Camp", "", New_Camp_Name),
         camp_of_residence=case_when(New_Camp_Name=='Camp 4 Extension' ~ '4 Ext', 
                                     New_Camp_Name=='Camp 20 Extension' ~ '20 Ext', 
                                     TRUE ~ camp_of_residence),
         #triming white space in the dataframe
         camp_of_residence=trimws(camp_of_residence, which = c("both", "left", "right"), whitespace = "[ \t\r\n]"))

#Merge the CasesCamp and Population data together
TablePop<- left_join(population, CasesCamp, by="camp_of_residence") %>% select(cases, Total_Pop, New_Camp_Name,Upazila)

#Generate the rates per 100,000 and add to the Table Pop dataframe
Ratesper100k<-round(TablePop$cases/TablePop$Total_Pop*100000, 1)
TablePop$Ratesper100k<-Ratesper100k

Map_Rates100kData<- TablePop %>% 
  full_join(shp_file_host, ., by=c("New_Camp_N"="New_Camp_Name"))

cases_map <-  Map_Rates100kData %>% 
  mutate(camp_label=gsub('Camp ', '', New_Camp_N),
         camp_label=gsub('Extension', 'E', camp_label),
         upazila_upd=case_when(camp_label %in% c('21','22','23') ~ 'Teknaf North',
                               camp_label %in% c('24', '25', '26', '27', 'Nayapara RC') ~ 'Teknaf South',
                               TRUE ~ Upazila),
         upazila_upd=factor(upazila_upd, levels=c('Ukhia', 'Teknaf South', 'Teknaf North'))) %>% 
  filter(!is.na(Upazila)) %>% 
  tm_shape() +
  tm_polygons("Ratesper100k", title="Cases per 100,000", palette="YlGn",textNA = "No reported cases") +
  tm_facets(by="upazila_upd", nrow=2) +
  tm_borders()


TablePopOrdered_df <- TablePop %>% 
  mutate(camp_key=stringr::str_remove(New_Camp_Name, "^Camp 0+"),
         camp_key=gsub('Camp ', '',camp_key),
         camp_key=case_when(camp_key=='20 Extension' ~ '20 Ext',
                            camp_key=='4 Extension' ~ '4 ext',
                            TRUE ~ camp_key))

Map_Tests100kData <- ARI_ILI_CampTable %>% 
  mutate(camp_key=stringr::str_remove(camp, "^Camp 0+"),
         camp_key=gsub('Camp ', '',camp_key)) %>% 
  ungroup() %>% 
  select(camp_key, tests=n) %>% 
  left_join(TablePopOrdered_df,.,by="camp_key") %>% 
  mutate(tests100k=tests/Total_Pop*100000) %>% 
  left_join(shp_file_host,.,by=c("New_Camp_N"="New_Camp_Name"))

tests_map <- Map_Tests100kData %>% 
  mutate(camp_label=gsub('Camp ', '', New_Camp_N),
         camp_label=gsub('Extension', 'E', camp_label),
         upazila_upd=case_when(camp_label %in% c('21','22','23') ~ 'Teknaf North',
                               camp_label %in% c('24', '25', '26', '27', 'Nayapara RC') ~ 'Teknaf South',
                               TRUE ~ Upazila),
         upazila_upd=factor(upazila_upd, levels=c('Ukhia', 'Teknaf South', 'Teknaf North'))) %>% 
  filter(!is.na(Upazila)) %>% 
  tm_shape() +
  tm_polygons("tests100k", title="Tests per 100,000", palette="BuPu",textNA = "No reported tests") +
  tm_facets(by="upazila_upd", nrow=2) +
  tm_borders()

Epicurve<- FDMN_Only2 %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek) %>% 
  ggplot(., aes(x=yearweek, y=n)) +
  geom_col(position="stack", fill = "#ED7D31") +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) + 
  labs(x = "Week", y = "Number of cases") +
  scale_fill_manual(values=c("#4472C4")) +
  scale_x_yearweek(date_breaks = "4 week",) +
  theme(plot.title = element_text(size=10))

cases_tests_table <- camp_test_fortnight_flex %>% 
  add_header_lines(., values = "Table 1: Number of cases, tests and % COVID-19 positive tests for camps reporting at least 1 case in the last 2 weeks")

EpicurveSex<- FDMN_Only2 %>% 
  mutate(yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek, sex) %>% 
  mutate(sex=case_when(sex=='M' ~ 'Male',
                       sex=='F' ~ 'Female')) %>% 
  ggplot(., aes(x=yearweek, y=n, fill=sex)) +
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
  theme(plot.title = element_text(size=10))


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

# moved-code --------------------------------------------------------------


population_agegrp <- readxl::read_xlsx(here('data','block_population.xlsx'), sheet='Final', skip=1) %>% 
  clean_names %>%  
  clean_data() %>% 
  filter(grepl('total', camp)) %>% 
  filter(!camp=='grand_total') %>% 
  select(-block) %>% 
  mutate(across(c(infant_below_1:x16), as.numeric)) %>% 
    mutate(age_0_18 = rowSums(.[4:11]),
         age_18_59 = rowSums(.[12:13]),
         age_over60 = rowSums(.[14:15])) %>% 
  select(camp, contains('total'), contains('age')) %>% 
  mutate(camp=gsub('_total', '', camp),
         camp=gsub('camp_', '', camp)) %>% 
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
  mutate(age_grp=cut(age_in_years, breaks=c(-Inf,18,59, +Inf), labels=c('Under 18', '18 to 59' ,'60 and over')),
         yearweek=yearweek(date_of_case_detection)) %>% 
  count(yearweek,age_grp, .drop=FALSE) %>% 
  left_join(population_agegrp, by='age_grp') %>% 
  mutate(per_capita=(n/value)*100000,
         age_grp=factor(age_grp, levels=c('Under 18', '18 to 59', '60 and over'))) %>% 
  ggplot(., aes(x=yearweek,y=per_capita, fill=age_grp)) +
  geom_col(width = 3) +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2", direction=-1) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) +
  scale_x_yearweek(date_breaks = "4 week") +
  labs(x="Week", y="Cases 100,000 people", fill='Age group')

Summary_Severity<- FDMN_Only2 %>%
  mutate(severity_of_disease=factor(severity_of_disease, 
                                    levels=c('Critical', 'Severe', 'Mild/Moderate', 'Asymptomatic', 'Unknown'))) %>% 
  group_by(severity_of_disease) %>%
  summarise(count=n()) %>% 
  adorn_totals(where=c('row')) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() 

case_severity<-flextable(Summary_Severity) %>% 
  set_header_labels(severity_of_disease = "Severity of Disease",
                    count = "Frequency",
                    Percentage = "Percentage of total (%)") %>%
  bold(bold = TRUE, part = "header") %>% 
  autofit(.) %>% 
  fontsize(., size = 10, part="all") %>% 
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


Sum_tab_Severity <- FDMN_Only2 %>% 
  mutate(severity_of_disease=factor(severity_of_disease, levels=c('Critical', 'Severe', 'Mild/Moderate', 'Asymptomatic', 'Unknown'))) %>% 
  clean_names() %>% 
  tabyl(severity_of_disease,x30_day_outcome) %>% 
  adorn_totals(where=c('row', 'col')) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>% 
  rename('Disease severity'=severity_of_disease)


severity_outcome <- flextable(Sum_tab_Severity)  %>% 
  autofit() %>%
  fontsize(., size = 10, part="all") %>% 
  bold(bold = TRUE, part = "header") %>% 
  add_header_lines(., values = "Table 3: Cases by severity of disease at presentation & 30-day outcome")


# Number of cases by camp
CasesCamp<- FDMN_Only2 %>%
  count(camp_of_residence, name="count") %>%
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



# Appendix ----------------------------------------------------------------

FDMN_Test_graph<-test_nationality %>% 
  filter(name %in% c('fdmn', 'fdmn_positive')) %>% 
  mutate(week=isoweek(date_format),
         year=year(date_format)) %>% 
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

Host_Test_graph <- test_nationality %>% 
  filter(name %in% c('host', 'host_positive')) %>% 
  mutate(week=isoweek(date_format), year=year(date_format)) %>% 
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
  labs(caption = "Data source: Cox’s Bazar - IEDCR Field Lab",
       x="Week",
       y= "Number",
       fill="") +
  theme_minimal()

#Bangladesh data from Our World In Data
bgd_graph <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>% 
  filter(iso_code=='BGD') %>%        
  mutate(date_format=ymd(date)) %>% 
  select(date=date_format, new_tests,new_cases,new_deaths,population) %>% 
  mutate(population_group='Bangladesh',
         week=isoweek(date),
         year=year(date)) %>% 
  filter(week <= ThisWeek | year==2020) %>% 
  mutate(yearweek=yearweek(date)) %>% 
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
  labs(caption = "Data source: IEDCR (https://covid19bd.idare.io/)",
       x="Week",
       y= "Number",
       fill="") +
  theme_minimal()

