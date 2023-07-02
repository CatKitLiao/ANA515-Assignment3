#Step 1
getwd()
library(tidyverse)
setwd("/Users/xinyuliao/Desktop")
data2000 <- read.csv("StormEvents_details-ftp_v1.0_d2000_c20220425.csv") 
#Step 2
myvars <- c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")
new_data2000 <- data2000[myvars]
#Step 3
library(dplyr)
new_arranged_data2000 <- arrange(new_data2000, STATE)
#Step 4
library(stringr)
new_arranged_data2000$STATE <- str_to_title(new_arranged_data2000$STATE)
new_arranged_data2000$CZ_NAME <- str_to_title(new_arranged_data2000$CZ_NAME)
#Step 5
limited_data <- new_arranged_data2000 %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)
#Step 6
limited_data$STATE_FIPS <- str_pad(limited_data$STATE_FIPS, width = 2, pad = "0")
limited_data$CZ_FIPS <- str_pad(limited_data$CZ_FIPS, width = 3, pad = "0")
limited_data <- unite(limited_data, FIPS, STATE_FIPS, CZ_FIPS, sep = "-")
#Step 7
limited_data <- limited_data %>%
  rename_all(tolower)
colnames(limited_data)
#Step 8
data("state")
us_state_info <- data.frame(state = state.name, region = state.region, area = state.area)
#Step 9
colnames(events_per_state)
events_per_state <- data.frame(table(limited_data$state))
events_per_state_new <- rename(events_per_state, c("state" = "Var1"))
merged <- merge(x = events_per_state_new, y = us_state_info, by.x = "state", by.y = "state", all.x = TRUE)
head(merged)
merged <- merged[complete.cases(merged), ]
#Step 10
library(ggplot2)
storm_plot <- ggplot(merged, aes(x = area, y = Freq)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area (square miles)",
       y = "# of storm events in 2020")
storm_plot