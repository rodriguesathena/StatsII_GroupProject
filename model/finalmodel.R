library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(nnet)
library(ggplot2)
library(tidyr)

final_dataset <- read_csv("C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/SQFwithcrimerates.csv", show_col_types = FALSE)

# REFERENCE CATEGORIES
final_dataset$Force_Level <- factor(ifelse(final_dataset$Weapon_Force == 1, "Weapon_Force",
                                      ifelse(final_dataset$Non_Weapon_Force == 1, "Non_Weapon_Force", "No_Force")),
                               levels = c("No_Force", "Non_Weapon_Force", "Weapon_Force"))  # Reference = No_Force
final_dataset$SUSPECT_RACE_DESCRIPTION <- relevel(factor(final_dataset$SUSPECT_RACE_DESCRIPTION), ref = "White")
final_dataset$Stop_Outcome <- factor(ifelse(final_dataset$Summons_Issued == 1, "Summons",
                                       ifelse(final_dataset$Arrest_Made == 1, "Arrest", "No_Charge")),
                                levels = c("No_Charge", "Summons", "Arrest"))  # Reference = No Charge
#removing precincts 14 and 22 (include why!)
final_dataset <- final_dataset %>%
  filter(!STOP_LOCATION_PRECINCT %in% c(14, 22))


#ITS Model
force_model <- multinom(Force_Level ~ Centered_Year * Post_2021 + 
                          SUSPECT_RACE_DESCRIPTION * Post_2021 + 
                          Covid_Period + Stop_Outcome + Violent_Crime_Flag + 
                          Proximity_To_Scene + Other_Person_Stopped + Suspect_Sex + 
                          Age_Squared + Suspect_Weight + Crime_Rate + factor(Borough), 
                        data = final_dataset)
summary(force_model)
#z-score, p-values
z_scores <- summary(force_model)$coefficients / summary(force_model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))
print(p_values)

#odds ratios and CIs
exp_coefficients <- exp(summary(force_model)$coefficients)
conf_int_lower <- summary(force_model)$coefficients - 1.96 * summary(force_model)$standard.errors
conf_int_upper <- summary(force_model)$coefficients + 1.96 * summary(force_model)$standard.errors
exp_conf_int_lower <- exp(conf_int_lower)
exp_conf_int_upper <- exp(conf_int_upper)

conf_intervals <- data.frame(
  Estimate = exp_coefficients,
  Lower_95_CI = exp_conf_int_lower,
  Upper_95_CI = exp_conf_int_upper
)

print("Exponentiated Coefficients (Odds Ratios) with 95% Confidence Intervals:")
print(conf_intervals)

# RESIDUALS and ACF
residuals <- residuals(force_model, type = "deviance")
acf(residuals)
#partial ACF
# for (i in 1:ncol(residuals)) {
#   cat("PACF for category:", colnames(residuals)[i], "\n")
#   pacf(residuals[, i])
# }

# PLOTS PROCESSING
#total stops per race group per year
race_year_counts <- final_dataset %>%
  group_by(Centered_Year, SUSPECT_RACE_DESCRIPTION) %>%
  summarise(Total_Stops = n(), .groups = "drop")
#force level counts per race per year
force_counts <- final_dataset %>%
  group_by(Centered_Year, SUSPECT_RACE_DESCRIPTION, Force_Level) %>%
  summarise(Force_Count = n(), .groups = "drop")  # Count each force level per race per year
#proportion of total stops with force
force_proportions <- force_counts %>%
  left_join(race_year_counts, by = c("Centered_Year", "SUSPECT_RACE_DESCRIPTION")) %>%
  mutate(Force_Proportion = ifelse(Total_Stops > 0, Force_Count / Total_Stops, NA))
#centered to actual year
force_counts <- force_counts %>%
  mutate(YEAR2 = Centered_Year + 2022)
force_proportions <- force_proportions %>%
  mutate(YEAR2 = Centered_Year + 2022) 

#PLOT 1: Force Level COUNTS Over Time by Race
ggplot(force_counts, aes(x = YEAR2, y = Force_Count, color = Force_Level)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ SUSPECT_RACE_DESCRIPTION) +  # Separate graphs for each race
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red") +  # Policy change marker at 2022
  scale_y_continuous(limits = c(0, 10000)) +  # Fixed Y-axis from 0 to 10,000
  scale_x_continuous(breaks = 2017:2024) +  # Ensure X-axis has all years
  labs(title = "Total Force Level Counts Over Time by Race",
       x = "Year",
       y = "Number of Stops per Force Level",
       color = "Force Level") +
  theme_minimal() +
  theme(legend.position = "bottom")

#PLOT 2: Force Level PROPORTIONS Over Time by Race
ggplot(force_proportions, aes(x = YEAR2, y = Force_Proportion, color = Force_Level)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~ SUSPECT_RACE_DESCRIPTION) +  # Separate graphs for each race
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red") +  # Policy change marker at 2022
  scale_y_continuous(limits = c(0, 1)) +  # Fixed Y-axis from 0 to 1
  scale_x_continuous(breaks = 2017:2024) +  # Ensure X-axis has all years
  labs(title = "Proportion of Stops by Force Level and Race Over Time",
       x = "Year",
       y = "Proportion of Stops",
       color = "Force Level") +
  theme_minimal() +
  theme(legend.position = "bottom")
