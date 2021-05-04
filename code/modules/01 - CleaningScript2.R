
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



