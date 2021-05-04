
#05 - CxB Map Script


#install.packages("extrafont")
extrafont::loadfonts(device="win")

my_colors <- brewer.pal(9, "Reds") 


shp_file_host <- read_sf(here('data', 'shapefiles', '190321_Outline_RRC_Camp-A1.shp')) %>%  select(New_Camp_N, geometry)



# Number of cases by camp
CasesCamp<- FDMN_Only2 %>%
  group_by(camp_of_residence) %>%
  summarise(cases=n()) %>% 
  mutate(camp_of_residence=as.character(camp_of_residence))

# Remove the word "camp" from the population data to allow merges
population<-population %>%
  mutate(camp_of_residence = gsub("Camp", "", New_Camp_Name)) %>% 
  mutate(camp_of_residence=case_when(New_Camp_Name=='Camp 4 Extension' ~ '4 Ext', 
                                     New_Camp_Name=='Camp 20 Extension' ~ '20 Ext', 
                                     TRUE ~ camp_of_residence))

#triming white space in the dataframe
population$camp_of_residence<-trimws(population$camp_of_residence, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#Merge the CasesCamp and Population data together
TablePop<- left_join(population, CasesCamp, by="camp_of_residence") %>% select(cases, Total_Pop, New_Camp_Name,Upazila)

#Generate the rates per 100,000 and add to the Table Pop dataframe
Ratesper100k<-round(TablePop$cases/TablePop$Total_Pop*100000, 1)
TablePop$Ratesper100k<-Ratesper100k

#Sort from high to low based on rates per 100,000
TablePopOrdered<-TablePop[order(TablePop$Ratesper100k, na.last= TRUE, decreasing = TRUE),]
#TablePopOrdered<-TablePopOrdered[-13]
TablePopOrdered$New_Camp_N<-TablePopOrdered$New_Camp_Name
  #TablePopOrdered<-TablePopOrdered[-2]

Map_Rates100kData<-
  full_join(shp_file_host, TablePopOrdered, by="New_Camp_N")

# my_spfd<-
#   readOGR(
#   dsn = ("U:\\CxB - Virtual Deployment\\shape_files\\190310_Outline_Rohingya_Refugee_Camp_A1.shp"),
#   verbose = FALSE)

#my_spfd <- readOGR(here('data', 'shapefiles', '190310_Outline_Rohingya_Refugee_Camp_A1.shp'))

map_rates100k<-
ggplot(data = Map_Rates100kData$geometry) +
  geom_sf(aes(fill=Map_Rates100kData$Ratesper100k)) +
  scale_fill_distiller(palette="YlGn", trans = "reverse") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
                           axis.title = element_blank(),
                           axis.text = element_blank()) +
  labs(fill="Rate per 100,000")


TablePopOrdered_df <- TablePopOrdered %>% 
  mutate(camp_key=stringr::str_remove(New_Camp_N, "^Camp 0+")) %>% 
  mutate(camp_key=gsub('Camp ', '',camp_key)) %>% 
  mutate(camp_key=case_when(camp_key=='20 Extension' ~ '20 Ext',
                            camp_key=='4 Extension' ~ '4 ext',
                            TRUE ~ camp_key))

Map_Tests100kData <- ARI_ILI_CampTable %>% 
  mutate(camp_key=stringr::str_remove(camp, "^Camp 0+")) %>% 
  mutate(camp_key=gsub('Camp ', '',camp_key)) %>% 
  # mutate(New_Camp_N=sub("0+", "", camp)) %>% 
  ungroup() %>% 
  select(camp_key, tests=n) %>% 
  left_join(TablePopOrdered_df,.,by="camp_key") %>% 
  mutate(tests100k=tests/Total_Pop*100000) %>% 
  left_join(shp_file_host,.,by="New_Camp_N")



