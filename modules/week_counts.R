
#Tottal number of cases

total_cases <- FDMN_Only2 %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  mutate(year=year(date_of_case_detection)) %>% 
  dplyr::filter(year==2020 | (year==2021 & Week<=ThisWeek)) %>% 
  summarise(total_cases=sum(n())) %>% 
  pull(total_cases)
  

#Number of cases by week
cases_week <- FDMN_Only2 %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  count(Week) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  pull(n)


#Number of cases by week by camp
cases_week_camp <- FDMN_Only2 %>% 
  mutate(Week=isoweek(date_of_case_detection)) %>% 
  count(Week, camp_of_residence) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  select(Week, camp_of_residence, cases=n) %>% clean_data()

camps_to_include <- cases_week_camp %>% select(camp_of_residence) %>%  distinct() %>% pull(camp_of_residence)


## Tests by location
library(linelist)
ARI_ILI_df <-ari_ili %>%
<<<<<<< HEAD
  select(case_id, sample_type, date_of_case_detection,date_of_lab_result_received, health_facility_name=health_facility_name_sample_collection_site, camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
=======
  select(case_id, sample_type, date_of_case_detection, health_facility_name=health_facility_name_sample_collection_site, camp=camp_patient_s_residence, upazila, laboratory_result, nationality) %>% 
>>>>>>> e577dec3173b0f632ab79191466c5d6c183c850e
  clean_data() %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  filter(nationality=='fdmn') %>% 
  filter(!sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  mutate(date_of_case_detection=case_when(date_of_case_detection==ymd('2020-11-22') ~ ymd('2020-11-23'),
                                          TRUE ~ date_of_case_detection)) %>% 
  mutate(Week=isoweek(date_of_case_detection)) 
  
tests_week_camp <- ARI_ILI_df %>% 
  mutate(camp_of_residence=gsub('camp_', '', camp)) %>% 
  count(Week, camp_of_residence) %>% 
  filter(Week %in% c(ThisWeek, LastWeek)) %>% 
  select(Week, camp_of_residence, tests=n) %>% 
  mutate(camp_of_residence=sub("^0+", "", camp_of_residence)) %>% clean_data()

# camp_test_fortnight <- cases_week_camp %>% 
#   full_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
#   filter(camp_of_residence %in% camps_to_include) %>% 
#   mutate(positivity=scales::percent(cases/tests,accuracy = .1)) %>% 
#   arrange(week,camp_of_residence)
  
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


# camp_test_fortnight_flex <-flextable(camp_test_fortnight) %>% 
#   autofit() %>% 
#   set_header_labels(week = "Week",
#                     camp_of_residence = "Camp",
#                     cases = "Cases", 
#                     tests = "Tests",
#                     positivity = "Positivity") %>%
#   bold(bold = TRUE, part = "header")

# ThisWeek_CampFlex<-autofit(ThisWeek_CampFlex) %>% 
#   set_header_labels(camp = "Camp",
#                     total_tests = "Tests",
#                     positivity = "Positivity") %>%
#   bold(bold = TRUE, part = "header")

# ThisWeek_CampFlex<- fontsize(ThisWeek_CampFlex, size = 10, part="all") 
# ThisWeek_CampFlex_Report<-autofit(ThisWeek_CampFlex)


###UPDATED TABLE WITH TWO WEEKS INCLUDED

this_week_col <- paste0('x', ThisWeek, '_positivity')
last_week_col <- paste0('x', LastWeek, '_positivity')

activity_2week_df <- cases_week_camp %>% mutate(camp_of_residence=as.character(camp_of_residence)) %>% 
  full_join(tests_week_camp, by=c('week','camp_of_residence')) %>% 
  group_by(week,camp_of_residence) %>% 
  summarise(cases=sum(cases,na.rm=TRUE),
            tests=sum(tests,na.rm=TRUE)) %>% 
  filter(camp_of_residence %in% camps_to_include) %>% 
  mutate(positivity=cases/tests) %>% 
  #mutate(positivity=scales::percent(cases/tests,accuracy = .1)) %>% 
  arrange(week,camp_of_residence) %>% 
  pivot_longer(-c(week,camp_of_residence)) %>% 
  replace_na(list(value=0)) %>% 
  pivot_wider(id_cols=camp_of_residence, names_from=c(week,name), values_from=value) %>% 
  mutate(camp_number=str_extract(camp_of_residence, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  select(-camp_number) %>% 
  clean_names() %>% 
<<<<<<< HEAD
  mutate(x15_positivity=scales::percent(x15_positivity, accuracy=.1),
         x16_positivity=scales::percent(x16_positivity, accuracy=.1))
=======
    #mutate(across(contains('positivity'), percent(.,accuracy=.1)))
  mutate(x46_positivity=scales::percent(x46_positivity, accuracy=.1),
         x47_positivity=scales::percent(x47_positivity, accuracy=.1))
>>>>>>> e577dec3173b0f632ab79191466c5d6c183c850e
  
  

camp_test_fortnight_flex <- flextable(activity_2week_df) %>% 
  #autofit() %>% 
  set_header_labels(camp_of_residence = "Camp",
<<<<<<< HEAD
                    x15_cases = "Cases (Week 15)",
                    x15_tests = "Tests (Week 15)",
                    x15_positivity = "% positive (Week 15)",
                    x16_cases = "Cases (Week 16)",
                    x16_tests = "Tests (Week 16)",
                    x16_positivity = "% positive (Week 16)") %>% 
=======
                    x46_cases = "Cases (Week 46)",
                    x46_tests = "Tests (Week 46)",
                    x46_positivity = "% positive (Week 46)",
                    x47_cases = "Cases (Week 47)",
                    x47_tests = "Tests (Week 47)",
                    x47_positivity = "% positive (Week 47)") %>% 
>>>>>>> e577dec3173b0f632ab79191466c5d6c183c850e
  bold(bold = TRUE, part = "header")

# Test summary from last week ---------------------------------------------

camp_test_fortnight <- ARI_ILI_df %>% 
  mutate(Week=isoweek(date_of_lab_result_received)) %>% 
           count(Week, laboratory_result) %>% 
  complete(Week, laboratory_result, fill=list(n=0)) %>% 
  filter(Week %in% c(LastWeek, ThisWeek)) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total_tests=positive+negative) %>% 
  mutate(positivity=positive/total_tests) %>% 
  mutate(positivity=scales::percent(positivity, accuracy=.1)) %>% 
  select(Week, total_tests, positivity)

