

library(tidyverse)
library(lubridate)
library(readr)
library(plotly)
library(ggridges)
library(stargazer)


### Treatment Data
treatments = read_csv("treatments.csv")


### Assemble 2016 - 2020 data
arr16 = readRDS("~/michigan_arrests/data/ucr_arrests_monthly_all_crimes_race_sex_2016.rds")
arr16 = arr16 %>%
  filter(state_abb == "MI") %>%
  select(fips_county_code, state, agency_name, year, month, starts_with("dui"))


arr17 = readRDS("~/michigan_arrests/data/ucr_arrests_monthly_all_crimes_race_sex_2017.rds")
arr17 = arr17 %>%
  filter(state_abb == "MI") %>%
  select(fips_county_code, state, agency_name, year, month, starts_with("dui"))


arrest = rbind(arr16, arr17)


arr18 = readRDS("~/michigan_arrests/data/ucr_arrests_monthly_all_crimes_race_sex_2018.rds")
arr18 = arr18 %>%
  filter(state_abb == "MI") %>%
  select(fips_county_code, state, agency_name, year, month, starts_with("dui"))


arrest = rbind(arrest, arr18)


arr19 = readRDS("~/michigan_arrests/data/ucr_arrests_monthly_all_crimes_race_sex_2019.rds")
arr19 = arr19 %>%
  filter(state_abb == "MI") %>%
  select(fips_county_code, state, agency_name, year, month, starts_with("dui"))


arrest = rbind(arrest, arr19)


arr20 = readRDS("~/michigan_arrests/data/ucr_arrests_monthly_all_crimes_race_sex_2020.rds")
arr20 = arr20 %>%
  filter(state_abb == "MI") %>%
  select(fips_county_code, state, agency_name, year, month, starts_with("dui"))



arrest = rbind(arrest, arr20)


### Remove to save RAM
remove(arr16, arr17, arr18, arr19, arr20)


### Create Dates
arrest = arrest %>%
  mutate(monthyear = paste(month, year)) %>%
  mutate(d = my(monthyear))





### Attach swab count to respective county and phase
phase1_treated = treatments %>%
  filter(phase == "one") %>%
  select(fips_county_code, number_of_tests)

phase1_ids = phase1_treated$fips_county_code


phase2_treated = treatments %>%
  filter(phase == "two") %>%
  select(fips_county_code, number_of_tests)

phase2_ids = phase2_treated$fips_county_code

treated_ids = unique(treatments$fips_county_code)


phase1_start = my("november 2017")
phase1_end = my("october 2018") # Using October because phase 1 ended on 11/8
phase2_start = my("october 2019")
phase2_end = my("september 2020")


phase1_interval = interval(start = phase1_start, end = phase1_end)
phase2_interval = interval(start = phase2_start, end = phase2_end)


arrest = arrest %>%
  left_join(phase1_treated) %>% 
  mutate(number_of_tests = replace_na(number_of_tests, 0)) %>%
  rename(phase1_swabs = number_of_tests) %>%
  mutate(phase1_swabs = ifelse(d %within% phase1_interval, phase1_swabs, 0)) %>%
  left_join(phase2_treated) %>% 
  mutate(number_of_tests = replace_na(number_of_tests, 0)) %>%
  rename(phase2_swabs = number_of_tests) %>%
  mutate(phase2_swabs = ifelse(d %within% phase2_interval, phase2_swabs, 0))


check4 = arrest %>%
  group_by(fips_county_code, d) %>%
  summarise(DUI_Total = sum(dui_tot_arrests),
            DUI_p.Black = sum(dui_tot_black) / sum(dui_tot_arrests)) %>%
  left_join(phase1_treated) %>% 
  mutate(number_of_tests = replace_na(number_of_tests, 0)) %>%
  rename(phase1_swabs = number_of_tests) %>%
  mutate(phase1_swabs = ifelse(d %within% phase1_interval, phase1_swabs, 0)) %>%
  left_join(phase2_treated) %>% 
  mutate(number_of_tests = replace_na(number_of_tests, 0)) %>%
  rename(phase2_swabs = number_of_tests) %>%
  mutate(phase2_swabs = ifelse(d %within% phase2_interval, phase2_swabs, 0)) %>%
  mutate(fd = as.factor(as.character(d))) 

check4 = check4 %>%
  mutate(DUI_p.Black = ifelse(is.nan(DUI_p.Black), 0, DUI_p.Black))

check4 = check4 %>%
  mutate(treat1 = ifelse(fips_county_code %in% phase1_ids, 1, 0)) %>%
  mutate(treat2 = ifelse(fips_county_code %in% phase2_ids, 1, 0)) %>%
  mutate(treat1_date = my("november 2017")) %>%
  mutate(treat2_date = my("october 2019"))

check5 = check4 %>%
  mutate(time_to_t1 = interval(treat1_date, d) %/% months(1)) %>%
  mutate(time_to_t2 = interval(treat2_date, d) %/% months(1)) %>%
  mutate(tm5 = ifelse(treat1 == 1 & time_to_t1 == -5, 1, 0)) %>%
  mutate(tm4 = ifelse(treat1 == 1 & time_to_t1 == -4, 1, 0)) %>%
  mutate(tm3 = ifelse(treat1 == 1 & time_to_t1 == -3, 1, 0)) %>%
  mutate(tm2 = ifelse(treat1 == 1 & time_to_t1 == -2, 1, 0)) %>%
  mutate(tm1 = ifelse(treat1 == 1 & time_to_t1 == -1, 1, 0)) %>%
  mutate(tp1 = ifelse(treat1 == 1 & time_to_t1 == 1, 1, 0)) %>%
  mutate(tp2 = ifelse(treat1 == 1 & time_to_t1 == 2, 1, 0)) %>%
  mutate(tp3 = ifelse(treat1 == 1 & time_to_t1 == 3, 1, 0)) %>%
  mutate(tp4 = ifelse(treat1 == 1 & time_to_t1 == 4, 1, 0)) %>%
  mutate(tp5 = ifelse(treat1 == 1 & time_to_t1 == 5, 1, 0)) %>%
  mutate(tm5 = ifelse(treat2 == 1 & time_to_t2 == -5, 1, tm5)) %>%
  mutate(tm4 = ifelse(treat2 == 1 & time_to_t2 == -4, 1, tm4)) %>%
  mutate(tm3 = ifelse(treat2 == 1 & time_to_t2 == -3, 1, tm3)) %>%
  mutate(tm2 = ifelse(treat2 == 1 & time_to_t2 == -2, 1, tm2)) %>%
  mutate(tm1 = ifelse(treat2 == 1 & time_to_t2 == -1, 1, tm1)) %>% 
  mutate(tp1 = ifelse(treat2 == 1 $ time_to_t2 == 1, 1, tp1)) %>%
  mutate(tp2 = ifelse(treat2 == 1 $ time_to_t2 == 2, 1, tp2)) %>%
  mutate(tp3 = ifelse(treat2 == 1 $ time_to_t2 == 3, 1, tp3)) %>%
  mutate(tp4 = ifelse(treat2 == 1 $ time_to_t2 == 4, 1, tp4)) %>%
  mutate(tp5 = ifelse(treat2 == 1 $ time_to_t2 == 5, 1, tp5)) 


           






library(data.table) ## For some minor data wrangling
library(fixest)     ## NB: Requires version >=0.9.0

# Load and prepare data
dat = fread("https://raw.githubusercontent.com/LOST-STATS/LOST-STATS.github.io/master/Model_Estimation/Data/Event_Study_DiD/bacon_example.csv") 

# Let's create a more user-friendly indicator of which states received treatment
dat[, treat := ifelse(is.na(`_nfd`), 0, 1)]

# Create a "time_to_treatment" variable for each state, so that treatment is
# relative for all treated units. For the never-treated (i.e. control) units,
# we'll arbitrarily set the "time_to_treatment" value at 0. This value 
# doesn't really matter, since it will be canceled by the treat==0 interaction
# anyway. But we do want to make sure they aren't NA, otherwise feols would drop 
# these never-treated observations at estimation time and our results will be 
# off.
dat[, time_to_treat := ifelse(treat==1, year - `_nfd`, 0)]


