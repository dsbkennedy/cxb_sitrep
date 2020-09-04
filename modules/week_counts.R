
#Tottal number of cases

total_cases <- FDMN_Only %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  filter(Week <=ThisWeek) %>% 
  summarise(total_cases=sum(n())) %>% 
  pull(total_cases)
  

#Number of cases by week
cases_week <- FDMN_Only %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  count(Week) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  pull(n)


#Number of cases by week by camp
cases_week_camp <- FDMN_Only %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  count(Week, camp_of_residence) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  select(Week, camp_of_residence, cases=n) %>% clean_data()


## Tests by location
library(linelist)
ARI_ILI_df <-ari_ili %>%
  select(case_id, sample_type, date_of_case_detection, health_facility_name, camp, block, upazila, laboratory_result, nationality) %>% 
  clean_data() %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  filter(nationality=='fdmn') %>% 
  filter(!sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  mutate(Week=isoweek(date_of_case_detection)) 
  
tests_week_camp <- ARI_ILI_df %>% 
  mutate(camp_of_residence=gsub('camp_', '', camp)) %>% 
  count(Week, camp_of_residence) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  select(Week, camp_of_residence, tests=n) %>% 
  mutate(camp_of_residence=sub("^0+", "", camp_of_residence)) %>% clean_data()

camp_test_fortnight <- cases_week_camp %>% 
  left_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
  mutate(positivity=scales::percent(cases/tests,accuracy = .1)) %>% 
  arrange(week,camp_of_residence)
  
# camp_test_fortnight <- ARI_ILI_df %>% 
#   count(camp, laboratory_result, Week) %>% 
#   filter(Week>=LastWeek) %>% 
#   pivot_wider(names_from=laboratory_result, values_from=n) %>% 
#   filter(positive>=1) %>%  
#   arrange(camp, Week) %>% 
#   mutate(total_tests=positive+negative) %>% 
#   mutate(positivity=round((positive/total_tests)*100,1)) %>% 
#   mutate(camp=sub('camp_', '', camp)) %>% 
#   select(-negative)


camp_test_fortnight_flex <-flextable(camp_test_fortnight) %>% 
  autofit() %>% 
  set_header_labels(week = "Week",
                    camp_of_residence = "Camp",
                    cases = "Cases", 
                    tests = "Tests",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header")

ThisWeek_CampFlex<-autofit(ThisWeek_CampFlex) %>% 
  set_header_labels(camp = "Camp",
                    total_tests = "Tests",
                    positivity = "Positivity") %>%
  bold(bold = TRUE, part = "header")

ThisWeek_CampFlex<- fontsize(ThisWeek_CampFlex, size = 10, part="all") 
ThisWeek_CampFlex_Report<-autofit(ThisWeek_CampFlex)




# Test summary from last week ---------------------------------------------

camp_test_fortnight <- ARI_ILI_df %>% 
  count(Week, laboratory_result) %>% 
  filter(Week>=LastWeek) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total_tests=positive+negative) %>% 
  mutate(positivity=positive/total_tests) %>% 
  mutate(positivity=scales::percent(positivity, accuracy=.1)) %>% 
  select(Week, total_tests, positivity)

