# Empty the Environment
rm(list = ls())

# Load in necessary libraries, and install them beforehand if needed
library(tidyverse)
library(pROC)
library(glmnet)
library(lubridate)
library(sf)
library(tigris)
library(ggplot2)
library(rmapshaper)

# Load in our other code files necessary in order to receive the proper variables/data frames.
source("code/predict_FSBAL.R") #Lasso/ridge
source("code/ACSCleaning.R")#clean ACS

# Prepare ACS data for predictions
acs_fsbal <- as.matrix(subset(acs_data, select = -c(serialno, PUMA, weight,FAMINC_numeric)))

# Generating predictions using Ridge regression (received a better score than Lasso in our CPS predictions)
acs_ridge_pred = predict(final_ridge, acs_fsbal, type = "response")[,1] # Using ridge, it's a hair better

# Create a new column with the predictions into the ACS dataset
acs_data <- cbind(acs_data, acs_ridge_pred)

# Split ACS data by PUMA
puma_groups <- split(acs_data, acs_data$PUMA)

# Used ChatGPT here to help us figure out the grouping.
# Calculating weighted means for each PUMA
results <- data.frame(PUMA = character(), weighted_mean = numeric(), stringsAsFactors = FALSE)
for (PUMA in names(puma_groups)) {
  group <- puma_groups[[PUMA]]
  
  weighted_mean <- weighted.mean(group$acs_ridge_pred, group$weight)
  
  results <- rbind(results, data.frame(PUMA = PUMA, weighted_mean = weighted_mean))
}

# We used ChatGPT to help us setup the PUMA maps below
# Load PUMA shapefile
puma_shapefile <- tigris::pumas(state = "IA", year = 2022) # Make sure our year matches the year of our ACS dataset

# Ensure columns are the same type
puma_shapefile$GEOID20 <- as.character(puma_shapefile$GEOID20)
results$PUMA <- as.character(results$PUMA)

# Reduce the complexity of the shapefile (without this simplifying line, our computers would crash before outputting the map)
puma_shapefile_simplified <- ms_simplify(puma_shapefile, keep = 0.05)

# Merging Puma shapefile (simplified) with the calculated results (weighted means and predictions)
map_data <- puma_shapefile_simplified %>%
  left_join(results, by = c("GEOID20" = "PUMA"))

# Plot the predicted proportion of people not getting balanced meals
ggplot(data = map_data) +
  geom_sf(aes(fill = weighted_mean), color = "black") +
  scale_fill_gradient(low = "white", high = "black", name = "Proportion") +  
  theme_minimal() +
  labs(
    title = "Predicted Proportion of People not Getting Balanced Meals",
    caption = "Data source: ACS dataset and Ridge Regression Model"
  ) +
  theme(legend.position = "bottom")

# Load additional data on senior population by PUMA
PUMA_seniors <- read.csv("data/total_iowa_seniors_by_puma.csv")
PUMA_seniors$GEOID <- as.character(PUMA_seniors$GEOID)

# Merge senior population data with results
results <- left_join(results, PUMA_seniors, by = c("PUMA"="GEOID"))

# Calculate the predicted number of seniors not getting balanced meals
results$FSBAL_senior <- results$weighted_mean * results$senior_population

# Merge data to visualize the senior predictions
map_data <- puma_shapefile_simplified %>%
  left_join(results, by = c("GEOID20" = "PUMA"))

# Plot the predicted number of seniors not getting balanced meals
ggplot(data = map_data) +
  geom_sf(aes(fill = FSBAL_senior), color = "black") +
  scale_fill_gradient(low = "white", high = "black", name = "Seniors") + 
  theme_minimal() +
  labs(
    title = "Predicted Seniors not Getting Balanced Meals",
    caption = "Data source: ACS dataset and Ridge Regression Model"
  ) +
  theme(legend.position = "right")