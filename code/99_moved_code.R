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
