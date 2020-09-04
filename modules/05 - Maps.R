
#05 - CxB Map Script

# install.packages("library")
# install.packages("here")
# install.packages("purr")
# install.packages("linelist")
# install.packages("ggthemes")
# install.packages("gt")
# install.packages("RcppRoll")
# install.packages("scales")
# install.packages("patchwork")
# install.packages("googlesheets4")
# install.packages("plotly")
# install.packages("ggupset") 
# install.packages("furrr")
# install.packages("table1")
# install.packages("rdrop2")
# install.packages("ggpubr") 
# install.packages("tmap")
# install.packages("mapproj")

#install.packages("extrafont")
extrafont::loadfonts(device="win")

library(here) 
library(purrr) 
library(linelist) 
library(ggthemes) 
library(gt) 
library(RcppRoll) 
library(scales) 
library(patchwork) 
library(flexdashboard) 
library(googlesheets4) 
library(plotly) 
library(httr) 
library(jsonlite) 
library(ggupset) 
library(furrr) 
library(table1) 
library(rdrop2)  
library(ggpubr) 
library(tmap) 
library(htmltools) 
library(magrittr)
library(mapproj)
library(sf)
library(ggplot2)
library(RColorBrewer)

my_colors <- brewer.pal(9, "Reds") 

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
                 
#shp_file_host <- read_sf("U:\\CxB - Virtual Deployment\\shape_files\\190310_Outline_Rohingya_Refugee_Camp_A1.shp")
shp_file_host <- read_sf(here('data', 'shapefiles', '190321_Outline_RRC_Camp-A1.shp')) %>%  select(New_Camp_N, geometry)
#plot(shp_file_host$geometry)

#Joining camp and population data and generating attack rates
CasesCampTable<-CasesCamp[order(-CasesCamp$count), ]
# CasesCampTable<-flextable(CasesCampTable)
# CasesCampTable<-autofit(CasesCampTable)

# Number of cases by camp
CasesCamp<- FDMN_Only %>%
  group_by(camp_of_residence) %>%
  summarise(cases=n())

# Remove the word "camp" from the population data to allow merges
population<-population %>%
  mutate(camp_of_residence = gsub("Camp", "", New_Camp_Name))

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
  left_join(shp_file_host, TablePopOrdered, by="New_Camp_N")

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

# map_rates100k <-   Map_Rates100kData %>% 
#   split(.$Upazila) %>% 
#   imap(function(Map_Rates100kData, Upazila){
#     ggplot(data = Map_Rates100kData) + 
#       geom_sf(aes(fill=Ratesper100k)) +
#       scale_fill_distiller(palette="YlGn", trans = "reverse", limits=c(0,45)) +
#       theme_minimal() + 
#       theme(panel.grid = element_blank(),
#             axis.title = element_blank(),
#             axis.text = element_blank()) +
#       labs(fill="Rate per 100,000") +
#       ggtitle(paste(Upazila)) 
#   })

# ARI_ILI_CampTable$New_Camp_N<-ARI_ILI_CampTable$camp
# 
# ARI_ILI_CampData<-ARI_ILI_CampTable %>% 
#   mutate(New_Camp_N=sub("0+", "", New_Camp_N)) 

# Map_Tests100kData<-
#   left_join(TablePopOrdered, ARI_ILI_CampData,  by="New_Camp_N")

Map_Tests100kData <- ARI_ILI_CampTable %>% 
  mutate(New_Camp_N=sub("0+", "", camp)) %>% 
  ungroup() %>% 
  select(New_Camp_N, tests=n) %>% 
  left_join(TablePopOrdered,.,by="New_Camp_N") %>% 
  mutate(tests100k=tests/Total_Pop*100000) %>% 
  left_join(shp_file_host,.,by="New_Camp_N")

# Map_Tests100kData<-
#   left_join(Map_Tests100kData,shp_file_host, by="New_Camp_N")

#Map_Tests100kData$tests100k<-round((Map_Tests100kData$n/Map_Tests100kData$Total_Pop*100000),1)

# map_tests100k<-
#   ggplot(data = Map_Tests100kData$geometry) +
#   geom_sf(aes(fill=Map_Tests100kData$tests100k)) +
#   scale_fill_distiller(palette="BuPu", trans = "reverse") +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank()) +
#   labs(fill="Tests per 100,000") 
#   #facet_wrap(~Upazila.y)


# tmap(Map_Tests100kData)
# 
# tm_shape(Map_Tests100kData) +
#   tm_polygons(col = "code",
#               legend.hist = TRUE) 
# 
# Map_Tests100kData %>% 
#   ggplot(.) +
#   geom_sf(aes(fill=tests100k)) +
#   scale_fill_distiller(palette="BuPu", trans = "reverse") +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank()) +
#   labs(fill="Tests per 100,000")
# 
# 
# ggplot(data = Map_Tests100kData) +
#   geom_sf(aes(geometry)) 
# 
# Map_Tests100kData %>% 
#   ggplot(aes(fill = tests100k, color = tests100k)) +
#   geom_sf() +
#   coord_sf(crs = 5070, datum = NA) 
# 
# 
# ggplot() + geom_sf(data = Map_Tests100kData)
# 
# Map_Tests100kData %>% 
#   ggplot() +
#   geom_sf(~geometry)
# 
#   geom_sf(data = Map_Tests100kData, aes(fill = tests100k)) 
# 
#   geom_sf(data = counties, aes(fill = area)) +

#display.brewer.all()

