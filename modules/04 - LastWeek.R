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
  mutate(key=sub('Camp ', '', camp))

#Overall positivity by camp
ARI_ILI_Pos_Split_ThisWeek$positivity<-round(ARI_ILI_Pos_Split_ThisWeek$Positive/(ARI_ILI_Pos_Split_ThisWeek$Negative + ARI_ILI_Pos_Split_ThisWeek$Positive)*100, 1)

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


