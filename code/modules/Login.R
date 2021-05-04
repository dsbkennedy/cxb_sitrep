#Login info


#Automatic load in of the Google Docs line list 
sheet_names <- c('fdmn', 'ari_ili', 'testing')
gdrive_link <- "1Gnutu0OxxFJJq73KUzLQzHZHteFwpQP-B_tXct7OGwE"
#gs4_deauth()
#gsheet_data <- map(sheet_names, ~read_sheet(gdrive_link, sheet=.)) %>%  set_names(sheet_names)

#saveRDS(gsheet_data, here('data', 'gsheet_data.Rds'))

#gsheet_data <- readRDS(here('data', 'gsheet_data.Rds'))

gsheet_data_2020 <- readRDS(here('data', 'gsheet_data_2020.Rds'))
gsheet_data_2021 <- readRDS(here('data', 'gsheet_data_2021.Rds'))

### FDMN
FDMN_Only <- gsheet_data_2020$fdmn %>% bind_rows(gsheet_data_2021$fdmn)

#FDMN_Only <- gsheet_data$fdmn
### ARI/ILI
ari_ili <- gsheet_data_2020$ari_ili %>% bind_rows(gsheet_data_2021$ari_ili)

#ari_ili<- gsheet_data$ari_ili
### Testing data
Testing <- gsheet_data_2020$testing %>% bind_rows(gsheet_data_2021$testing)
#Testing<- gsheet_data$testing