# ECON511A Project

# Installing the packages
# install.packages("tidyverse")
# install.packages("dummy")
# install.packages("smotefamily")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("dummy")

# Load the packages
library(tidyverse)
library(dummy)
library(smotefamily)
library(corrplot)
library(olsrr)
library(dummy)

# Setting the working directory 
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/Fall 2022/ECON 511A/00_Flight")

# Read flight_data_2018_to_2022.csv file into a tibble flightDelay
flightDelay = read_csv(file = "flight_data_2018_to_2022_modified.csv",
                       col_names = TRUE)
print(flightDelay)
str(flightDelay)
summary(flightDelay)

flightDelay <- flightDelay %>% 
  mutate(Cancelled = as.logical(Cancelled),
         Diverted = as.logical(Diverted),
         IATA_Code_Operating_Airline = as.factor(IATA_Code_Operating_Airline),
         CRSDepTimeMinutes = as.numeric(CRSDepTime),
         CRSArrTimeMinutes = as.numeric(CRSArrTime),
         DayofMonth = as.factor(DayofMonth),
         DayOfWeek = as.factor(DayOfWeek),
         OriginStateName = as.factor(OriginStateName),
         DestStateName = as.factor(DestStateName)
         )

flightDelay <- flightDelay %>%
  mutate(DayName = ifelse(DayOfWeek == 1,'Monday',
                          ifelse(DayOfWeek == 2,'Tuesday',
                                 ifelse(DayOfWeek == 3,'Wednesday',
                                        ifelse(DayOfWeek == 4,'Thursday',
                                               ifelse(DayOfWeek == 5,'Friday',
                                                      ifelse(DayOfWeek == 6,'Sataurday',
                                                             'Sunday'
                                                             )))))))

flightDelay <- flightDelay %>% 
  mutate(DayName = as.factor(DayName))

flightDelay <- flightDelay %>%
  filter(Cancelled == 0 & Diverted == 0)

flightDelay <- flightDelay %>%
  mutate(CarrierDelay = ifelse(
    is.na(CarrierDelay),
    0,
    CarrierDelay),
    WeatherDelay = ifelse(
      is.na(WeatherDelay),
      0,
      WeatherDelay),
    SecurityDelay = ifelse(
      is.na(SecurityDelay),
      0,
      SecurityDelay),
    NASDelay = ifelse(
      is.na(NASDelay),
      0,
      NASDelay),
    LateAircraftDelay = ifelse(
      is.na(LateAircraftDelay),
      0,
      LateAircraftDelay)
    )

summary(flightDelay)

flightDelay1 <- flightDelay %>%
  select(DayofMonth,
         #DayOfWeek,
         #DayName,
         IATA_Code_Operating_Airline,
         CRSDepTimeMinutes,
         CRSArrTimeMinutes,
         ArrDelayMinutes,
         AirTime,
         Distance,
         OriginStateName,
         DestStateName
         # CarrierDelay,
         # WeatherDelay,
         # NASDelay,
         # SecurityDelay,
         # LateAircraftDelay
         ) 

# flightDelay1 <- flightDelay1 %>%
#   mutate(Delay = ifelse(ArrDelayMinutes > 0, 1, 0))
# 
# flightDelay1 <- flightDelay1 %>% 
#   mutate(Delay = as.logical(Delay))

flightDelay1 <- as_tibble(cbind(flightDelay1, dummy(flightDelay1, int = TRUE))) 

flightDelay1 <- flightDelay1 %>%
  select(-DayofMonth,
         #-DayOfWeek,
         #-DayName,
         -IATA_Code_Operating_Airline,
         -OriginStateName,
         -DestStateName,
         -DayofMonth_31, 
         -IATA_Code_Operating_Airline_ZW,
         -OriginStateName_Wyoming,  
         -DestStateName_Wyoming, 
         ) 
         
# # Randomly split the dataset into flightDelay1Training (75% of records) and 
# # flightDelay1Testing (25% of records) using 511 as the random seed
# set.seed(511)
# sampleSet <- sample(nrow(flightDelay1),
#                     round(nrow(flightDelay1) * 0.75),
#                     replace = FALSE)
# 
# # Store the training dataset in a new tibble flightDelay1Training
# flightDelay1Training <- flightDelay1[sampleSet, ]
# 
# # Store the testing dataset in a new tibble flightDelay1Testing 
# flightDelay1Testing <- flightDelay1[-sampleSet, ]
# 
# summary(flightDelay1Training$Delay)

summary(flightDelay1)

# Generate the linear regression model
flightDelay1Modellm <- lm(data= flightDelay1, 
                       formula = ArrDelayMinutes ~ .)

summary(flightDelay1Modellm)

# Check for correlations between variables
cor(flightDelay1)


summary(flightDelay)
summary(flightDelay1)

sd(flightDelay1$AirTime)
sd(flightDelay1$Distance)
sd(flightDelay1$ArrDelayMinutes)
sd(flightDelay$CarrierDelay)

sd(flightDelay$AirTime)
sd(flightDelay$Distance)
sd(flightDelay$ArrDelayMinutes)

summarize(.data = flightDelay1, mean(AirTime))
summarize(.data = flightDelay1, mean(Distance))
summarize(.data = flightDelay1, mean(ArrDelayMinutes))
summarize(.data = flightDelay, mean(CarrierDelay))

summarize(.data = flightDelay1, sd(AirTime))
summarize(.data = flightDelay1, sd(Distance))
summarize(.data = flightDelay1, sd(ArrDelayMinutes))
summarize(.data = flightDelay, sd(CarrierDelay))