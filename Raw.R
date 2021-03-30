library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)

setwd('/Users/saujanya/Project_1')

chicago_crime <- read.csv('Dataset/chicago_crime.csv', na.strings = "")

chicago_school <- read.csv('Dataset/chicago_school.csv', na.strings = "")
  

df <- chicago_crime %>%
      group_by(Community.Area) %>%
      summarise(Number_of_incidents = n()) %>%
      drop_na()

write.csv(df, 'Number_of_crime_CA.csv')
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  write.csv(df, 'Wardwise_crimeno.csv')

school <- chicago_school %>%
          select(NAME_OF_SCHOOL, Longitude,Latitude,COMMUNITY_AREA_NUMBER) %>%
          group_by(COMMUNITY_AREA_NUMBER) %>%
          drop_na()

write.csv(school,'school_location.csv')

chicago_crime$Date <- mdy_hms(chicago_crime$Date)

chicago_crime$Month <- month(chicago_crime$Date)

chicago_crime$Year <- year(chicago_crime$Date)

chicago_crime$Month_name <- month.name[chicago_crime$Month]

month.name[c(1,2,3,4,5)]

crime_by_month <- chicago_crime %>%
                  group_by(Month) %>%
                  summarise(Number_of_incidents = n())
crime_by_month$Month_Name <- month.abb[crime_by_month$Month]
write.csv(crime_by_month,'Number_by_month.csv')

chicago_crime$Crime_hour <- hour(chicago_crime$Date)

crime_by_hour <- chicago_crime %>%
                 group_by(Crime_hour) %>%
                 summarise(Number_of_incidents = n())


CA_crimes <- chicago_crime %>%
  group_by(Community.Area) %>%
  summarise(Number_of_incidents = n()) %>%
  arrange((desc(Number_of_incidents))) %>%
  drop_na()

CA_crimes <- tail(CA_crimes, 16)

CA_crimes <- CA_crimes[-16,]

write.csv('')

crime_type <- chicago_crime %>%
              group_by(Primary.Type) %>%
              summarise(Number = n())
schools_number <- chicago_school %>%
                  group_by(COMMUNITY_AREA_NUMBER) %>%
                  summarise(Number_school =n()) %>%
                  drop_na()
unique(CA_crimes$Community.Area)
name <- c(50, 59, 62, 57, 64, 72, 37, 13, 36, 74, 18, 55, 12, 47,  9)

Number_school_filter <- schools_number %>%
                        filter(COMMUNITY_AREA_NUMBER == 50 |
                                COMMUNITY_AREA_NUMBER == 59 | COMMUNITY_AREA_NUMBER == 62 | COMMUNITY_AREA_NUMBER == 57 |
                                 COMMUNITY_AREA_NUMBER == 64 | COMMUNITY_AREA_NUMBER == 72 | COMMUNITY_AREA_NUMBER == 37 | COMMUNITY_AREA_NUMBER == 13 | COMMUNITY_AREA_NUMBER == 36 |COMMUNITY_AREA_NUMBER == 74 |COMMUNITY_AREA_NUMBER == 18 |
                                 COMMUNITY_AREA_NUMBER == 18 |COMMUNITY_AREA_NUMBER == 55 |COMMUNITY_AREA_NUMBER == 12 |COMMUNITY_AREA_NUMBER == 47 |COMMUNITY_AREA_NUMBER == 9) #62, 57, 64, 72, 37, 13, 36, 74, 18, 55, 12, 47,  9)
schools_per_ca <- chicago_school %>%
                  group_by(COMMUNITY_AREA_NUMBER) %>%
                  summarise(No_of_schools = n(),
                            Long = mean(Longitude),
                            Lat = mean(Latitude)) %>%
                  drop_na()
write.csv(schools_per_ca, 'schools_per_ca.csv')


crime_pd <- chicago_crime %>%
            group_by(District) %>%
            summarise(Number_of_crime = n()) %>%
            drop_na()

write.csv(crime_pd, 'crime_acc_pd.csv')

schools_pd <- chicago_school %>%
              group_by(Police.District) %>%
              summarise(Number_of_schools = n()) %>%
              drop_na()

crime_per <- read.csv('Dataset/Number_of_crime_CA.csv', na.strings = "")

#crime_per$multi <- crime_per$Total.Population/10000

crime_per$Crime10k <- (crime_per$Number_of_incidents/crime_per$Total.Population)*10000

crime_per$Crime10k <- round(crime_per$Crime10k, digits = 0)

write.csv(crime_per, 'CrimeRate_Per_10k.csv')


com_name <- chicago_school %>%
            group_by(COMMUNITY_AREA_NAME,COMMUNITY_AREA_NUMBER) %>%
            summarise(Number_of_schools = n()) %>%
            drop_na()
write.csv(com_name,'com_names.csv')

crime_per_name <- read.csv('Dataset/CrimeRate_Per_10k.csv')
crime_per_asec <- crime_per_name %>%
                  select(Community.Area,COMMUNITY_AREA_NAME, Crime10k) %>%
                  arrange(Crime10k)
least_15_CA <- head(crime_per_asec, 15)  

write.csv(least_15_CA, 'Least_Crime_Rate.csv')

crime_by_year <- chicago_crime %>%
                 filter(Primary.Type == 'KIDNAPPING' | 
                          Primary.Type == 'STALKING' | 
                          Primary.Type == 'CRIMINAL DAMAGE' |
                          Primary.Type == 'SEX OFFENSE' | 
                          Primary.Type == 'WEAPONS VIOLATION' |
                          Primary.Type == 'NARCOTICS') %>%
                 group_by(Year, Month) %>%
                 summarise(Number_of_Crime = n()) %>%
                 drop_na()
crime_by_year <- head(crime_by_year, 60)

crime_by_year$Month_name <- month.name[crime_by_year$Month]
write.csv(crime_by_year,'Crime_By_Year.csv' ) 

#Crime Per Hour related to children

crime_h_ac <- chicago_crime %>%
              filter(Primary.Type == 'KIDNAPPING' | 
                       Primary.Type == 'STALKING' | 
                       Primary.Type == 'CRIMINAL DAMAGE' |
                       Primary.Type == 'SEX OFFENSE' | 
                       Primary.Type == 'WEAPONS VIOLATION' |
                       Primary.Type == 'NARCOTICS') %>%
             group_by(Crime_hour) %>%
             summarise(Crime_count = n()) %>%
             drop_na()
write.csv(crime_h_ac, 'Number_by_hour.csv')        


crime_in_year2016 <- chicago_crime %>%
                     filter(Year == 2016) %>%
                     filter(Primary.Type == 'KIDNAPPING' | 
                              Primary.Type == 'STALKING' | 
                              Primary.Type == 'CRIMINAL DAMAGE' |
                              Primary.Type == 'SEX OFFENSE' | 
                              Primary.Type == 'WEAPONS VIOLATION' |
                              Primary.Type == 'NARCOTICS') %>%
                     group_by(Crime_hour) %>%
                     summarise(Number_of_crime = n()) %>%
                     drop_na()
write.csv(crime_in_year2016, 'Crime_in_2016.csv')

crime_in_year2015 <- chicago_crime %>%
  filter(Year == 2015) %>%
  filter(Primary.Type == 'KIDNAPPING' | 
           Primary.Type == 'STALKING' | 
           Primary.Type == 'CRIMINAL DAMAGE' |
           Primary.Type == 'SEX OFFENSE' | 
           Primary.Type == 'WEAPONS VIOLATION' |
           Primary.Type == 'NARCOTICS') %>%
  group_by(Crime_hour) %>%
  summarise(Number_of_crime = n()) %>%
  drop_na()                      
write.csv(crime_in_year2015, 'Crime_in_2015.csv')


pie_data <- chicago_crime %>%
            filter(Community.Area == 9 | Community.Area == 12 | Community.Area == 74 | Community.Area == 10 | Community.Area == 17 |
                   Community.Area == 64 | Community.Area == 77 | Community.Area == 11 | Community.Area == 2 | Community.Area == 18 |
                   Community.Area == 72 | Community.Area == 5 | Community.Area == 4 | Community.Area == 13 | Community.Area == 14 ) %>%
             filter(Primary.Type == 'KIDNAPPING' | 
           Primary.Type == 'STALKING' | 
           Primary.Type == 'CRIMINAL DAMAGE' |
           Primary.Type == 'SEX OFFENSE' | 
           Primary.Type == 'WEAPONS VIOLATION' |
           Primary.Type == 'NARCOTICS') %>%
            group_by(Community.Area,Month) %>%
            summarise(Number_of_incidents = n()) %>%
            drop_na()
pie_data$Month_name <- month.name[pie_data$Month]

write.csv(pie_data,'crime_per_month_ca.csv')
  
  
  
crime_per_hour_ca <- chicago_crime %>%
           filter(Community.Area == 9 | Community.Area == 12 | Community.Area == 74 | Community.Area == 10 | Community.Area == 17 |
           Community.Area == 64 | Community.Area == 77 | Community.Area == 11 | Community.Area == 2 | Community.Area == 18 |
           Community.Area == 72 | Community.Area == 5 | Community.Area == 4 | Community.Area == 13 | Community.Area == 14 ) %>%
           filter(Primary.Type == 'KIDNAPPING' | 
           Primary.Type == 'STALKING' | 
           Primary.Type == 'CRIMINAL DAMAGE' |
           Primary.Type == 'SEX OFFENSE' | 
           Primary.Type == 'WEAPONS VIOLATION' |
           Primary.Type == 'NARCOTICS') %>%
           group_by(Community.Area, Crime_hour, Primary.Type) %>%
           summarise(Number_each_hour = n()) %>%
           drop_na()
write.csv(crime_per_hour_ca, 'crime_per_hour_ca_rc_primaryT.csv')  



rate_misconducts <- chicago_school %>%
                    filter(Community.Area == 9 | Community.Area == 12 | Community.Area == 74 | Community.Area == 10 | Community.Area == 17 |
                    Community.Area == 64 | Community.Area == 77 | Community.Area == 11 | Community.Area == 2 | Community.Area == 18 |
                    Community.Area == 72 | Community.Area == 5 | Community.Area == 4 | Community.Area == 13 | Community.Area == 14 ) %>%
                    group_by(COMMUNITY_AREA_NUMBER)
                    









