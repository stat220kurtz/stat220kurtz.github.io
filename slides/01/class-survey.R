library(tidyverse)
library(googlesheets4)
library(here)
survey <- read_sheet("https://docs.google.com/spreadsheets/d/1EnH23IO3X7Y2r6e6-NqGn5VxV21TySknBFM5-w8hgX0/edit?usp=sharing")

survey <- survey |>
  select(class_year = `What is your class year?`,
         sleep = `How many hours of sleep per night do you typically get during the term?`,
         northfield_food = `Where can you find the best food in Northfield?`)

write_csv(survey, here(here(), "data/class_survey_sm.csv"))
