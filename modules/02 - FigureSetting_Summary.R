#SETTING SUMMARY FIGURES

#Set CFR
Outcome_Proportions<- FDMN_Only %>%
  filter(Week <= ThisWeek) %>% 
  count(`30_day_outcome`) %>% 
  mutate(Percentage=n/sum(n)) %>% 
  mutate(Percentage=scales::percent(Percentage, accuracy=.1)) 

# Proportions_2<-Outcome_Proportions$n/ n_cases_FDMN *100
# Outcome_Proportions['Percentage'] = Proportions_2

# outcomes at 30 days
#sum_tab_30<- as.data.frame(table(FDMN_Only$`30_day_outcome`))
sum_tab_30 <- FDMN_Only %>%
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
CasesCamp<- FDMN_Only %>%
  group_by(camp_of_residence) %>%
  summarise(count=n())

CasesCampVal<-unique(CasesCamp$camp_of_residence)
length(CasesCampVal)

CampsWithCases<-length(CasesCampVal)

# #Individuals missing camp of residence information
# MissingInfoCampResidence<-
#   FDMN_Only$camp_of_residence

# Median age of cases (min and max)

medianage<-median(FDMN_Only$age_in_years)
RangeUpper<-max(FDMN_Only$age_in_years)
RangeLow<-min(FDMN_Only$age_in_years)

#FDMN_Only$Week <-week(FDMN_Only$date_of_case_detection)

FDMN_Only <- FDMN_Only %>%  
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  filter(Week <= ThisWeek)

# Sex
Sex<- FDMN_Only %>%
  filter(Week<=ThisWeek) %>% 
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


LastWeekCases<- FDMN_Only %>% 
  filter(Week <=(LastWeek)) %>% 
  summarise(count=n())

DifferenceLastWeek<-n_cases_FDMN - LastWeekCases 

# Setting an attack rate 
FDMN_Pop<-as.integer(sum(population$Total_Pop))

AR<-round((n_cases_FDMN/FDMN_Pop)*100,3)
AR100k<-round((n_cases_FDMN/FDMN_Pop)*100000,1)

