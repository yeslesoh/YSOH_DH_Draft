
# About this script -------------------------------------------------------

# Purpose: Clean datasets (1.COVID cases by age group-district, 2.Total COVID cases-district)
# Author: Yesle Soh
# Last updated: 19 Jan, 2024
# Contact: yeslesoh@gmail.com

install.packages("pacman")

pacman::p_load(
  rio,
  here,
  dplyr,
  skimr,
  epikit,
  forcats,
  readr,
  styler, 
  tidyverse,
  janitor,
  stringr
)

data_age <- read.csv(here("data", "VDH-COVID-19-PublicUseDataset-Cases_By-Age-Group_20240119.csv"))
data_district <- read.csv(here("data", "VDH-COVID-19-PublicUseDataset-Cases_By-District-Death-Hospitalization_20240119.csv"))

# Filter by district_2023 ----------------------------------------------
# shortcut for pipe: cmd, shift, m
alexandria_2023 <- data_district %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>% 
  filter(health_district == "Alexandria" &
           (report_date == mdy("01/01/2023") |
              report_date == mdy("02/01/2023") |
              report_date == mdy("03/01/2023") |
              report_date == mdy("04/01/2023") |
              report_date == mdy("05/01/2023") |
              report_date == mdy("06/01/2023") |
              report_date == mdy("07/01/2023") |
              report_date == mdy("08/01/2023") |
              report_date == mdy("09/01/2023") |
              report_date == mdy("10/01/2023") |
              report_date == mdy("11/01/2023") |
              report_date == mdy("12/01/2023") |
              report_date == mdy("01/01/2024")))

arlington_2023 <- data_district %>% 
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>% 
  filter(health_district == "Arlington" &
           (report_date == mdy("01/01/2023") |
              report_date == mdy("02/01/2023") |
              report_date == mdy("03/01/2023") |
              report_date == mdy("04/01/2023") |
              report_date == mdy("05/01/2023") |
              report_date == mdy("06/01/2023") |
              report_date == mdy("07/01/2023") |
              report_date == mdy("08/01/2023") |
              report_date == mdy("09/01/2023") |
              report_date == mdy("10/01/2023") |
              report_date == mdy("11/01/2023") |
              report_date == mdy("12/01/2023") |
              report_date == mdy("01/01/2024")))

fairfax_2023 <- data_district %>% 
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>% 
  filter(health_district == "Fairfax" &
           (report_date == mdy("01/01/2023") |
              report_date == mdy("02/01/2023") |
              report_date == mdy("03/01/2023") |
              report_date == mdy("04/01/2023") |
              report_date == mdy("05/01/2023") |
              report_date == mdy("06/01/2023") |
              report_date == mdy("07/01/2023") |
              report_date == mdy("08/01/2023") |
              report_date == mdy("09/01/2023") |
              report_date == mdy("10/01/2023") |
              report_date == mdy("11/01/2023") |
              report_date == mdy("12/01/2023") |
              report_date == mdy("01/01/2024")))

loudoun_2023 <- data_district %>% 
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>% 
  filter(health_district == "Loudoun" &
           (report_date == mdy("01/01/2023") |
              report_date == mdy("02/01/2023") |
              report_date == mdy("03/01/2023") |
              report_date == mdy("04/01/2023") |
              report_date == mdy("05/01/2023") |
              report_date == mdy("06/01/2023") |
              report_date == mdy("07/01/2023") |
              report_date == mdy("08/01/2023") |
              report_date == mdy("09/01/2023") |
              report_date == mdy("10/01/2023") |
              report_date == mdy("11/01/2023") |
              report_date == mdy("12/01/2023") |
              report_date == mdy("01/01/2024")))

prince_william_2023 <- data_district %>% 
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>% 
  filter(`health_district` == "Prince William" &
           (report_date == mdy("01/01/2023") |
              report_date == mdy("02/01/2023") |
              report_date == mdy("03/01/2023") |
              report_date == mdy("04/01/2023") |
              report_date == mdy("05/01/2023") |
              report_date == mdy("06/01/2023") |
              report_date == mdy("07/01/2023") |
              report_date == mdy("08/01/2023") |
              report_date == mdy("09/01/2023") |
              report_date == mdy("10/01/2023") |
              report_date == mdy("11/01/2023") |
              report_date == mdy("12/01/2023") |
              report_date == mdy("01/01/2024")))

# Combined cases 2023 -----------------------------------------------------
cases_alex <- alexandria_2023 %>% 
  select(report_date, health_district, number_of_cases)

cases_arl <- arlington_2023 %>% 
  select(report_date, health_district, number_of_cases)

cases_fair <- fairfax_2023 %>% 
  select(report_date, health_district, number_of_cases)

cases_lou <- loudoun_2023 %>% 
  select(report_date, health_district, number_of_cases)

cases_pw <- prince_william_2023 %>% 
  select(report_date, health_district, number_of_cases)

cases_2023 <- bind_rows(
  cases_alex %>% rename(District = health_district),
  cases_arl %>% rename(District = health_district),
  cases_fair %>% rename(District = health_district),
  cases_lou %>% rename(District = health_district),
  cases_pw %>% rename(District = health_district)
)

# Combined hospitalizations 2023 ------------------------------------------
hosp_alex <- alexandria_2023 %>% 
  select(report_date, health_district, number_of_hospitalizations)

hosp_arl <- arlington_2023 %>% 
  select(report_date, health_district, number_of_hospitalizations)

hosp_fair <- fairfax_2023 %>% 
  select(report_date, health_district, number_of_hospitalizations)

hosp_lou <- loudoun_2023 %>% 
  select(report_date, health_district, number_of_hospitalizations)

hosp_pw <- prince_william_2023 %>% 
  select(report_date, health_district, number_of_hospitalizations)

hosp_2023 <- bind_rows(
  hosp_alex %>% rename(District = health_district),
  hosp_arl %>% rename(District = health_district),
  hosp_fair %>% rename(District = health_district),
  hosp_lou %>% rename(District = health_district),
  hosp_pw %>% rename(District = health_district)
)

# Combined deaths 2023 ----------------------------------------------------
death_alex <- alexandria_2023 %>% 
  select(report_date, health_district, number_of_deaths)

death_arl <- arlington_2023 %>% 
  select(report_date, health_district, number_of_deaths)

death_fair <- fairfax_2023 %>% 
  select(report_date, health_district, number_of_deaths)

death_lou <- loudoun_2023 %>% 
  select(report_date, health_district, number_of_deaths)

death_pw <- prince_william_2023 %>% 
  select(report_date, health_district, number_of_deaths)

death_2023 <- bind_rows(
  death_alex %>% rename(District = health_district),
  death_arl %>% rename(District = health_district),
  death_fair %>% rename(District = health_district),
  death_lou %>% rename(District = health_district),
  death_pw %>% rename(District = health_district)
)

# Filter by district_age group Jan.16 -------------------------------------
alexandria_Jan2024 <- data_age %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>%
  filter(health_district == "Alexandria" &
           (report_date == mdy("01/16/2024") &
              (age_group == "0-9 Years" |
                 age_group == "10-19 Years" |
                 age_group == "20-29 Years" |
                 age_group == "30-39 Years" |
                 age_group == "40-49 Years" |
                 age_group == "50-59 Years" |
                 age_group == "60-69 Years" |
                 age_group == "70-79 Years" |
                 age_group == "80+ Years"))) %>% 
  mutate(age_group = str_replace(age_group, " Years", "")) %>%
  select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)

arlington_Jan2024 <- data_age %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>%
  filter(health_district == "Arlington" &
           (report_date == mdy("01/16/2024") &
              (age_group == "0-9 Years" |
                 age_group == "10-19 Years" |
                 age_group == "20-29 Years" |
                 age_group == "30-39 Years" |
                 age_group == "40-49 Years" |
                 age_group == "50-59 Years" |
                 age_group == "60-69 Years" |
                 age_group == "70-79 Years" |
                 age_group == "80+ Years"))) %>% 
  mutate(age_group = str_replace(age_group, " Years", "")) %>%
  select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)

fairfax_Jan2024 <- data_age %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>%
  filter(health_district == "Fairfax" &
           (report_date == mdy("01/16/2024") &
              (age_group == "0-9 Years" |
                 age_group == "10-19 Years" |
                 age_group == "20-29 Years" |
                 age_group == "30-39 Years" |
                 age_group == "40-49 Years" |
                 age_group == "50-59 Years" |
                 age_group == "60-69 Years" |
                 age_group == "70-79 Years" |
                 age_group == "80+ Years"))) %>% 
  mutate(age_group = str_replace(age_group, " Years", "")) %>%
  select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)

loudoun_Jan2024 <- data_age %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>%
  filter(health_district == "Loudoun" &
           (report_date == mdy("01/16/2024") &
              (age_group == "0-9 Years" |
                 age_group == "10-19 Years" |
                 age_group == "20-29 Years" |
                 age_group == "30-39 Years" |
                 age_group == "40-49 Years" |
                 age_group == "50-59 Years" |
                 age_group == "60-69 Years" |
                 age_group == "70-79 Years" |
                 age_group == "80+ Years"))) %>% 
  mutate(age_group = str_replace(age_group, " Years", "")) %>%
  select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)

prince_william_Jan2024 <- data_age %>%
  clean_names() %>% 
  mutate(report_date = mdy(report_date)) %>%
  filter(`health_district` == "Prince William" &
           (report_date == mdy("01/16/2024") &
              (age_group == "0-9 Years" |
                 age_group == "10-19 Years" |
                 age_group == "20-29 Years" |
                 age_group == "30-39 Years" |
                 age_group == "40-49 Years" |
                 age_group == "50-59 Years" |
                 age_group == "60-69 Years" |
                 age_group == "70-79 Years" |
                 age_group == "80+ Years"))) %>% 
  mutate(age_group = str_replace(age_group, " Years", "")) %>%
  select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)
