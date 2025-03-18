library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(nnet)  
library(car)   
library(MASS)

final_dataset <- read_csv("C:/Users/athen/Documents/GitHub/StatsII_GroupProject/data/SQFwithcrimerates.csv", show_col_types = FALSE)

#MULTINOMIAL LOGISTIC MODEL
final_dataset$Force_Level <- factor(ifelse(final_dataset$Weapon_Force == 1, "Weapon_Force",
                                      ifelse(final_dataset$Non_Weapon_Force == 1, "Non_Weapon_Force", "No_Force")),
                               levels = c("No_Force", "Non_Weapon_Force", "Weapon_Force"))

force_model <- multinom(Force_Level ~ Elected + SUSPECT_RACE_DESCRIPTION + 
                          Frisked + Searched + Violent_Crime_Flag + 
                          Other_Person_Stopped + Proximity_To_Scene + Crime_Rate,
                        data = final_dataset)
summary(force_model)

# VIF
vif_values <- vif(force_model)
print("Variance Inflation Factor (VIF) Values:")
print(vif_values)
high_vif <- vif_values[vif_values > 5]
cat("⚠️ Variables with VIF > 5 (Potential Multicollinearity):\n")
print(high_vif)


#ORDERED MULTINOMIAL LOGISTIC REGRESSION
final_dataset$Force_Level <- factor(final_dataset$Force_Level, 
                               levels = c("No_Force", "Non_Weapon_Force", "Weapon_Force"), 
                               ordered = TRUE)
ordered_force_model <- polr(Force_Level ~ YEAR2 + Elected + SUSPECT_RACE_DESCRIPTION + 
                              Frisked + Searched + Violent_Crime_Flag + 
                              Other_Person_Stopped + Proximity_To_Scene + Crime_Rate,
                            data = final_dataset, method = "logistic")
summary(ordered_force_model)
vif_values <- vif(force_model)
print("Variance Inflation Factor (VIF) Values:")
print(vif_values)
high_vif <- vif_values[vif_values > 5]
cat("⚠️ Variables with VIF > 5 (Potential Multicollinearity):\n")
print(high_vif)
