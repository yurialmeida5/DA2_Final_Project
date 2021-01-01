# Load necessary packages
library(tidyverse)

# Clear variable environmental memory
rm(list=ls())

# Define columns names
col_name <- c("Obs", "Species", "Weight", "Length1", "Length2"	, "Length3",	"Height", "Width", "Sex")

# Import fish dataset
fish_data <- read.table('data/raw/fish_data_raw.txt', header = FALSE , col.names = col_name)

# Note that there are two values that can't be used on our dependent variable weight (rows 14 and 47). 
# One the value corresponds to 0 (It doesn't make any sense to have any fish size equals to 0). 
# And the other shows as a Missing Value.Both of them are errors in the collection process and are going to be excluded.

fish_data <- fish_data %>% filter(!is.na(Weight),
                                  !Weight == "0")


# Remove the observation column
fish_data <- select(fish_data, -Obs)


# Check for extreme values under the dataset
# All HISTOGRAMS

fish_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Analyzing the distributions of the following variables, 
# It`s possible to perceive a presence of a few outliers and some skewed patterns.
# Example: Weight (Strong Right-Tailed Distribution)
# However, those observations aren't a product of measurement error and no outlier will be discarded.

# Create the dummy variables for the variable species

fish_data$Species1 <- ifelse(fish_data$Species == '1', 1, 0)
fish_data$Species2 <- ifelse(fish_data$Species == '2', 1, 0)
fish_data$Species3 <- ifelse(fish_data$Species == '3', 1, 0)
fish_data$Species4 <- ifelse(fish_data$Species == '4', 1, 0)
fish_data$Species5 <- ifelse(fish_data$Species == '5', 1, 0)
fish_data$Species6 <- ifelse(fish_data$Species == '6', 1, 0)
fish_data$Species7 <- ifelse(fish_data$Species == '7', 1, 0)

# Generate the cleaned file for analysis

mypath <- 'D:/CEU/Data_Analysis_2/DA2_Final_Project/data/clean/'
write.csv(fish_data, paste0(mypath,'fish_data_clean.csv'), row.names = FALSE)

