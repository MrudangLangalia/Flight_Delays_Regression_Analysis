# Regression 2:

flightDelay2 <- flightDelay %>%
  select(# DayofMonth,
    # DayOfWeek,
    # DayName,
    IATA_Code_Operating_Airline,
    # CRSDepTimeMinutes,
    # CRSArrTimeMinutes,
    # ArrDelayMinutes,
    # AirTime,
    # Distance,
    OriginStateName,
    DestStateName,
    CarrierDelay,
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

flightDelay2 <- as_tibble(cbind(flightDelay2, dummy(flightDelay2, int = TRUE))) 

flightDelay2 <- flightDelay2 %>%
  select(# -DayofMonth,
    # -DayOfWeek,
    # -DayName,
    -IATA_Code_Operating_Airline,
    -OriginStateName,
    -DestStateName,
    # -DayofMonth_31,
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

# # Generate the logistic regression model
# flightDelay1Model <- glm(data = flightDelay1,
#                         family = binomial,
#                         formula = Delay ~ .)
# 
# summary(flightDelay1Model)

# Generate the linear regression model
flightDelay2Modellm <- lm(data= flightDelay2, 
                          formula = CarrierDelay ~ .)

summary(flightDelay2Modellm)

# Check for correlations between variables
cor(flightDelay2)








flightDelay3 <- flightDelay %>%
  select(DayofMonth,
         #DayOfWeek,
         #DayName,
         IATA_Code_Operating_Airline,
         CRSDepTimeMinutes,
         CRSArrTimeMinutes,
         # ArrDelayMinutes,
         AirTime,
         Distance,
         OriginStateName,
         DestStateName,
         # CarrierDelay
         # WeatherDelay,
         # NASDelay,
         # SecurityDelay,
         LateAircraftDelay
  ) 

# flightDelay1 <- flightDelay1 %>%
#   mutate(Delay = ifelse(ArrDelayMinutes > 0, 1, 0))
# 
# flightDelay1 <- flightDelay1 %>% 
#   mutate(Delay = as.logical(Delay))

flightDelay3 <- as_tibble(cbind(flightDelay3, dummy(flightDelay3, int = TRUE))) 

flightDelay3 <- flightDelay3 %>%
  select(-DayofMonth,
         #-DayOfWeek,
         #-DayName,
         -IATA_Code_Operating_Airline,
         -OriginStateName,
         -DestStateName,
         -DayofMonth_31, 
         -IATA_Code_Operating_Airline_ZW,
         -OriginStateName_Wyoming,  
         -DestStateName_Wyoming 
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


# Generate the linear regression model
flightDelay3Modellm <- lm(data= flightDelay3, 
                          formula = LateAircraftDelay ~ .)

summary(flightDelay3Modellm)

# Check for correlations between variables
cor(flightDelay3)
