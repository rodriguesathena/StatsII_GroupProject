library(dplyr)
library(readr)
library(readxl)
library(tidyr)


# LOADING DATA FOR PROCESSING
sqf_data <- read_csv("C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/combined_SQF.csv", show_col_types = FALSE)
population_data <- read_csv("C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/precinct_population.csv", show_col_types = FALSE)
crime_data <- read_excel("C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/seven_major_felonies.xlsx", sheet = 1)


# SQF DATA PROCESSING
#race standardisation and categorising
sqf_data$SUSPECT_RACE_DESCRIPTION <- toupper(sqf_data$SUSPECT_RACE_DESCRIPTION)
sqf_data$SUSPECT_RACE_DESCRIPTION <- case_when(
  sqf_data$SUSPECT_RACE_DESCRIPTION == "BLACK" ~ "Black",
  sqf_data$SUSPECT_RACE_DESCRIPTION %in% c("BLACK HISPANIC", "WHITE HISPANIC") ~ "Hispanic",
  sqf_data$SUSPECT_RACE_DESCRIPTION == "WHITE" ~ "White")

# BINARY OR NUMERIC 
convert_to_binary <- function(column) {
  ifelse(column == "Y", 1, ifelse(column == "N", 0, NA))}

#Stop Outcome to Binary
sqf_data$Summons_Issued <- convert_to_binary(sqf_data$SUMMONS_ISSUED_FLAG)
sqf_data$Arrest_Made <- convert_to_binary(sqf_data$SUSPECT_ARRESTED_FLAG)
#Year and Precinct to Numeric
sqf_data$YEAR2 <- as.numeric(sqf_data$YEAR2)
sqf_data$STOP_LOCATION_PRECINCT <- as.numeric(sqf_data$STOP_LOCATION_PRECINCT)
#Frisked and Searched to Binary
sqf_data$Frisked <- convert_to_binary(sqf_data$FRISKED_FLAG)
sqf_data$Searched <- convert_to_binary(sqf_data$SEARCHED_FLAG)
#Age and Weight to numeric and filtered
sqf_data$Suspect_Age <- as.numeric(sqf_data$SUSPECT_REPORTED_AGE)
sqf_data <- sqf_data %>% filter(Suspect_Age >= 10 & Suspect_Age <= 100)
sqf_data <- sqf_data %>% mutate(Suspect_Weight = as.numeric(SUSPECT_WEIGHT)) %>% 
  filter(Suspect_Weight >= 50 & Suspect_Weight <= 400)
#Sex, Violent Crime, Other Persons, Proximity to Scene to Binary
sqf_data$Suspect_Sex <- ifelse(sqf_data$SUSPECT_SEX == "MALE", 1, 
                               ifelse(sqf_data$SUSPECT_SEX == "FEMALE", 0, NA))
sqf_data$Violent_Crime_Flag <- ifelse(sqf_data$BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG == "Y", 1, 0)
sqf_data$Other_Person_Stopped <- convert_to_binary(sqf_data$OTHER_PERSON_STOPPED_FLAG)
sqf_data$Proximity_To_Scene <- ifelse(sqf_data$SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG == "Y", 1, 0)
#Age Squared, COVID, Trend Change
sqf_data$Post_2021 <- ifelse(sqf_data$YEAR2 >= 2022, 1, 0)
sqf_data <- sqf_data %>%
  mutate(Age_Squared = Suspect_Age^2)

sqf_data$Covid_Period <- ifelse(sqf_data$YEAR2 %in% c(2020, 2021), 1, 0)

sqf_data <- sqf_data %>% 
  mutate(
    Centered_Year = YEAR2 - 2022,  # 2022 as the reference year
    Post_2022_Trend = Post_2021 * Centered_Year,  # Interaction term for trend change
    Age_Squared = Suspect_Age^2  # Quadratic term for age
  )
#FORCE OUTCOME
force_cols <- c("PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", 
                "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
                "PHYSICAL_FORCE_OTHER_FLAG",  
                "PHYSICAL_FORCE_CEW_FLAG", 
                "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG", 
                "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG", 
                "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG")

sqf_data[force_cols] <- lapply(sqf_data[force_cols], convert_to_binary)

#creating non-weapon and weapon force
sqf_data$Non_Weapon_Force <- ifelse(rowSums(sqf_data[, c("PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
                                                         "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
                                                         "PHYSICAL_FORCE_OTHER_FLAG")], na.rm = TRUE) > 0, 1, 0)

sqf_data$Weapon_Force <- ifelse(rowSums(sqf_data[, c("PHYSICAL_FORCE_CEW_FLAG",
                                                     "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
                                                     "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
                                                     "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG")], na.rm = TRUE) > 0, 1, 0)
#highest level of force recorded
sqf_data$Non_Weapon_Force <- ifelse(sqf_data$Weapon_Force == 1, 0, sqf_data$Non_Weapon_Force)
#creating force category
sqf_data$Force_Level <- factor(ifelse(sqf_data$Weapon_Force == 1, "Weapon_Force",
                                      ifelse(sqf_data$Non_Weapon_Force == 1, "Non_Weapon_Force", "No_Force")),
                               levels = c("No_Force", "Non_Weapon_Force", "Weapon_Force"))  # Set No_Force as reference

# BOROUGH INFORMATION
boroughs <- c("MANHATTAN", "BROOKLYN", "QUEENS", "BRONX", "STATEN ISLAND")
sqf_data <- sqf_data %>%
  mutate(Borough = toupper(STOP_LOCATION_BORO_NAME)) %>%
  filter(Borough %in% boroughs)
sqf_data$Borough <- factor(sqf_data$Borough)


# POPULATION DATA PROCESSING
population_data <- population_data %>% rename(STOP_LOCATION_PRECINCT = PRECINCT, Total_Population = P1_001N)

# CRIME DATA PROCESSING
crime_data <- crime_data %>% rename(STOP_LOCATION_PRECINCT = PRECINCT)
crime_data <- crime_data %>% pivot_longer(cols = -STOP_LOCATION_PRECINCT, names_to = "Year", values_to = "Total_Crimes") %>%
  mutate(Year = as.numeric(Year), STOP_LOCATION_PRECINCT = as.numeric(STOP_LOCATION_PRECINCT), Total_Crimes = as.numeric(Total_Crimes))

# MERGING DATA
crime_and_population <- crime_data <- crime_data %>%
  left_join(population_data, by = "STOP_LOCATION_PRECINCT") %>%
  mutate(Crime_Rate = ifelse(Total_Population > 0, (Total_Crimes / Total_Population) * 1000, NA)) #calculate crime rate

final_dataset <- sqf_data <- sqf_data %>%
  left_join(crime_and_population, by = c("STOP_LOCATION_PRECINCT", "YEAR2" = "Year"))
# CLEANING FINAL DATA
#keeping necessary columns
keep <- c("YEAR2", "STOP_LOCATION_PRECINCT", "SUSPECT_RACE_DESCRIPTION", 
          "Summons_Issued", "Arrest_Made", "Post_2021", "Covid_Period", "Frisked", 
          "Searched", "Suspect_Age", "Suspect_Sex", "Suspect_Weight", 
          "Violent_Crime_Flag", "Other_Person_Stopped", 
          "Proximity_To_Scene", "Crime_Rate", 
          "Non_Weapon_Force", "Weapon_Force", "Centered_Year", "Age_Squared", "Borough")
final_dataset <- final_dataset[, keep, drop = FALSE]
#removing NAs
total_rows_before <- nrow(final_dataset)
rows_with_NA <- sum(!complete.cases(final_dataset))
print(total_rows_before)
print(rows_with_NA)
final_dataset <- na.omit(final_dataset)

write_csv(final_dataset, "C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/SQFwithcrimerates.csv")
