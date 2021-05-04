
#Define weeks
ThisWeek<- week(today()) -1
LastWeek <- week(today())-2

getOption("lubridate.week.start", 1)

##Combine cleaning steps for FDMN file
FDMN_Only <- FDMN_Only %>% 
  #clean variable names and data
  clean_data() %>% 
  #Make week variable
  mutate(week=week(date_of_case_detection)) %>% 
  #Drop empty rows and columns
  remove_empty() %>% 
  #Only want data before current week
  filter(week<=ThisWeek)
