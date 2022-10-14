#----------------------------------#
#         Data Wrangling:          #
#           Homework 3             #
#----------------------------------#

rm(list=ls())

# install.packages("lubridate")

# Load packages
library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(magrittr)

#-----------#
#  PART 1   #
#-----------#

# Read the data
uof <- read_csv("uof_louisville.csv")

#-----------#
#  PART 1   #
#-----------#

# Part 2.a

# Create one column with hour of occurrence
uof <- uof %>%
  mutate(hour = hour(time_of_occurrence)) 
  
# Save the most common hour
frequent_hour <- uof %>%
  count(hour, sort = T) %>%
  slice(1) %>%
  pull(hour)

frequent_hour

# Part 2.b
# Create one column with month of occurrence
uof <- uof %>%
  mutate(month = month(date_of_occurrence) )

# Save the least common month
least_frequent_month <- uof %>%
  count(month) %>%
  arrange(n) %>%
  slice(1) %>%
  pull(month)

least_frequent_month

# Part 2.c
# Create a new column with day of occurrence
uof <- uof %>%
  mutate(day = wday(date_of_occurrence, label = T))

# Save the most frequent day
most_frequent_day <- uof %>%
  count(day, sort = T) %>%
  slice(1) %>%
  pull(day)

most_frequent_day

# Part 2.d
# Distribution of crimes within months

day_distribution <- uof %>%
  mutate(day = day(date_of_occurrence)) %>%
  count(day, sort = T) %>%
  mutate(fraction = n/nrow(uof)) %>%
  adorn_totals()


#-----------#
#  PART 3   #
#-----------#

# Part 3.a
# Save all unique categories of force used
force_used_1 <- uof %>%
  select(force_used_1) %>%
  unique()

# Part 3.b 
# Again but for force_used_2
force_used_2 <- uof %>%
  distinct(force_used_2)

# Part 3.c
# Extract all types of force used
all_force <- uof %>%
  select(force_used_1, 
         force_used_2, 
         force_used_3,
         force_used_4,
         force_used_5,
         force_used_6, 
         force_used_7, 
         force_used_8) %>%
  t() %>%
  c() %>%
  unique()

# Part 3.d
## copy and run this line of code
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used")

# Part 3.e

uof <- uof %>%
  mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force, 1, 0) )

# Part 3.f

violent_force_service_table <- uof %>%
  filter(violent_uof_1 == 1) %>%
  count(service_rendered, sort = T) %>%
  mutate(fraction = n/sum(n) ) %>%
  adorn_totals()
  
  
# Part 4.a
uof_filtered <- uof %>%
  filter(citizen_gender == "female" | citizen_gender == "male") %>%
  filter(!is.na(citizen_race)) %>%
  mutate(force_used_1_effective_binary = ifelse(force_used_1_effective == "yes", 1, 0)  )

tabyl(uof_filtered$force_used_1_effective_binary)

# Part 4.b 

uof_filtered_table <- uof_filtered %>%
  group_by(citizen_gender, citizen_race) %>%
  summarize(effective_1 = sum(force_used_1_effective_binary, na.rm = T), counts = n() ) %>%
  adorn_totals() %>%
  mutate(fraction_effective = effective_1/counts)
  

#












  
  
  
  
  
  
  
  

















  


  
















  








