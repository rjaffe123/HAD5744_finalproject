library(usethis)

gitcreds::gitcreds_set()



library(tidyverse)
library(readxl)
library(janitor)
library(lubridate) 
library(purrr)

## Add physician data

workforce <- read_excel("physician_total.xlsx", sheet = "Sheet4")
final_data <- final_data %>% full_join(workforce)

#Add SEP data
injection <- read_excel("injection.xlsx",sheet = "Sheet2")
final_data <- final_data %>% full_join(injection)

