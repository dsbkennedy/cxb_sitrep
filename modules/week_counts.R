
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

camps_to_include <- cases_week_camp %>% select(camp_of_residence) %>%  distinct() %>% pull(camp_of_residence)


## Tests by location
library(linelist)
ARI_ILI_df <-ari_ili %>%
  select(case_id, sample_type, date_of_case_detection, health_facility_name=health_facility_name_sample_collection_site, camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
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
  full_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
  filter(camp_of_residence %in% camps_to_include) %>% 
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


###UPDATED TABLE WITH TWO WEEKS INCLUDED

this_week_col <- paste0('x_', ThisWeek, '_positivity')
last_week_col <- paste0('x_', LastWeek, '_positivity')

activity_2week_df <- cases_week_camp %>% 
  full_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
  filter(camp_of_residence %in% camps_to_include) %>% 
  mutate(positivity=cases/tests) %>% 
  #mutate(positivity=scales::percent(cases/tests,accuracy = .1)) %>% 
  arrange(week,camp_of_residence) %>% 
  pivot_longer(-c(week,camp_of_residence)) %>% 
  replace_na(list(value=0)) %>% 
  pivot_wider(id_cols=camp_of_residence, names_from=c(week,name), values_fill = 0) %>% 
  mutate(camp_number=str_extract(camp_of_residence, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  select(-camp_number) %>% 
  clean_names() %>% 
    #mutate(across(contains('positivity'), percent(.,accuracy=.1)))
  mutate(x46_positivity=scales::percent(x46_positivity, accuracy=.1),
         x47_positivity=scales::percent(x47_positivity, accuracy=.1))
  
  
  # activity_2week_df %>% 
  # clean_names() %>% 
  #   mutate(across(contains('positivity'), percent(.,accuracy=.1)))
  # mutate(3=scales::percent(3, accuracy=.1),
  #        6=scales::percent(6, accuracy=.1))

camp_test_fortnight_flex <- flextable(activity_2week_df) %>% 
  autofit() %>% 
  set_header_labels(camp_of_residence = "Camp",
                    x46_cases = "Cases (Week 46)",
                    x46_tests = "Tests (Week 46)",
                    x46_positivity = "% positive (Week 46)",
                    x47_cases = "Cases (Week 47)",
                    x47_tests = "Tests (Week 47)",
                    x47_positivity = "% positive (Week 47)") %>% 
  bold(bold = TRUE, part = "header")

# Test summary from last week ---------------------------------------------

camp_test_fortnight <- ARI_ILI_df %>% 
  count(Week, laboratory_result) %>% 
  filter(Week>=LastWeek) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total_tests=positive+negative) %>% 
  mutate(positivity=positive/total_tests) %>% 
  mutate(positivity=scales::percent(positivity, accuracy=.1)) %>% 
  select(Week, total_tests, positivity)

