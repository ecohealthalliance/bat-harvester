library(here)
library(tidyverse)
library(scales)

bh <- read_csv(here("data", "bat_harvester_questionnaire.csv")) 

# PPE Exposures

ppe <- bh %>%
  select(site_name, contains("ppe")) %>%
  select(-ppe_other_type, -ppe_other, -ppe_notes) %>%
  separate(ppe_mask, c("ppe_mask_low", "ppe_mask_high"), remove = T, fill = "left") %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(ppe_mask_low = as.numeric(ppe_mask_low),
         ppe_mask_high = as.numeric(ppe_mask_high),
         ppe_mask = (ppe_mask_low + ppe_mask_high)/2) %>%
  select(-ppe_mask_low, -ppe_mask_high) %>% 
  mutate_if(is.numeric, funs(case_when(. == 0 ~ 2,
                                       . == 100 ~ 0,
                                       TRUE ~ 1))) %>%
  mutate(exp_ppe = rowSums(.[2:(ncol(ppe)-1)]),
         exp_ppe = rescale(exp_ppe, to = c(0,1), from = c(0, ((ncol(ppe)-2)*2))))

# WASH Exposures

wash <- bh %>%
  select(site_name, contains("wash"), soaping = harvesters_soaping_range) %>%
  separate(harvesters_washing_range, c("washing_low", "washing_high"), remove = T, fill = "left") %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(washing_low = as.numeric(washing_low),
         washing_high = as.numeric(washing_high),
         washing = (washing_high + washing_low)/2) %>%
  mutate_if(is.numeric, funs(case_when(. == 0 ~ 2,
                                       . == 100 ~ 0,
                                       TRUE ~ 1))) %>%
  mutate(soaping = case_when(
    soaping == "100" ~ 0,
    soaping == "unknown" ~ 2,
    soaping == "0" ~ 2,
    TRUE ~ 1),
    soaping = as.numeric(soaping)) %>%
  select(-c(harvesters_washing, washing_low, washing_high)) %>%
  mutate(exp_wash = rowSums(.[2:(ncol(wash)-1)]),
         exp_wash = rescale(exp_wash, to = c(0,1), from = c(0, ((ncol(wash)-2)*2))))


# Collection Exposures

collect <- bh %>%
  select(site_name, contains("tools")) %>%
  select(-tools_other, -tools_other_type) %>%
  mutate_at(c("tools_shovels", "tools_spades", "tools_brooms"),
            funs(case_when(. == 0 ~ 2, 
                           . == 100 ~ 0,
                           TRUE ~1))) %>%
  mutate(tools_hands_only = case_when(
    tools_hands_only == 100 ~ 2,
    tools_hands_only == 0 ~ 0,
    TRUE ~ 1)) %>%
  mutate(exp_collect = rowSums(.[2:(ncol(collect)-1)]),
         exp_collect = rescale(exp_collect, to = c(0, 1), from = c(0, ((ncol(collect)-2)*2))))

  
# Guano and Bat Exposure
  
guano <- bh %>%
  select(site_name, area_guano, depth_guano,  bat_roost_size)  %>%
  mutate(area_guano = case_when(area_guano == ">500" ~ 2,
                                area_guano == "100-500" ~ 1,
                                area_guano == "<50" ~ 0,
                                is.na(area_guano) ~ 2,
                                TRUE ~ NA_real_),
         depth_guano = case_when(depth_guano == ">30" ~ 2,
                                 depth_guano == "<5" ~ 0,
                                 TRUE ~ 1),
         bat_roost_size = case_when(str_detect(bat_roost_size, "[:digit:]{5,}") ~ 2,
                                    bat_roost_size == "5000" ~ 1,
                                    bat_roost_size == ">1000" ~0)) %>%
  mutate(exp_guano = rowSums(.[2:(ncol(guano)-1)]),
         exp_guano = rescale(exp_guano, to = c(0,1), from = c(0, ((ncol(guano)-2)*2))))


# Exposure Groups

exposure_df <- list(ppe, wash, collect, guano)
exposures <- lapply(exposure_df,function(x) x[,ncol(x)]) %>%
  do.call("cbind", .) %>%
  cbind(., bh$site_name)


# Frequency and Duration of Exposures

freq <- bh %>%
  select(site_name, duration_active_harvest, frequency_of_harvesting, months_harvested) %>%
  mutate(duration = str_extract_all(duration_active_harvest, "[:digit:]{2,3}(?= minutes typical)|[:digit:]{2,3}(?= minutes$)")) %>%
  mutate(frequency = case_when(frequency_of_harvesting == "1 time per year" ~ 1/365,
                               frequency_of_harvesting == "3 times per month" ~ 3*12/365,
                               frequency_of_harvesting == "2 times per year" ~ 2/365,
                               frequency_of_harvesting == "3 times per week" ~ 3*52/365,
                               frequency_of_harvesting == "2 times per week" ~ 2*52/365,
                               frequency_of_harvesting == "daily" ~ 365/365)) %>%
  mutate(number_of_months = str_count(months_harvested, "[:alpha:](?=;)") + 1,
         number_of_months = replace_na(number_of_months, 1, number_of_months))


# Frequency and Duration of Exposures

freq <- bh %>%
  select(site_name, duration_active_harvest, frequency_of_harvesting, months_harvested) %>%
  mutate(duration = str_extract_all(duration_active_harvest, "[:digit:]{2,3}(?= minutes typical)|[:digit:]{2,3}(?= minutes$)")) %>%
  mutate(frequency = case_when(frequency_of_harvesting == "1 time per year" ~ 1/365,
                               frequency_of_harvesting == "3 times per month" ~ 3*12/365,
                               frequency_of_harvesting == "2 times per year" ~ 2/365,
                               frequency_of_harvesting == "3 times per week" ~ 3*52/365,
                               frequency_of_harvesting == "2 times per week" ~ 2*52/365,
                               frequency_of_harvesting == "daily" ~ 365/365)) %>%
  mutate(number_of_months = str_count(months_harvested, "[:alpha:](?=;)") + 1,
         number_of_months = replace_na(number_of_months, 1, number_of_months))
  

# Per Person Risk

pp <- bh %>% 
  select(site_name, number_of_harvesters, frequency_per_person, guano_weight_per_person) %>%
  left_join(freq, by = "site_name") %>%
  mutate(frequency_per_person = replace_na(frequency_per_person, "3 times per week", frequency_per_person)) %>%
  mutate(frequency_per_person_annual = case_when(frequency_per_person == "1 time per year" ~ 1/365,
                                          frequency_per_person == "1-3 times per month" ~ 2*12/365,
                                          frequency_per_person == "1 time per 3-6 months, 1 time per 4 months typical" ~ 3/365,
                                          frequency_per_person == "2 times per week" ~ 2*52/365,
                                          frequency_per_person == "3 times per week" ~ 3*52/365,
                                          frequency_per_person == "daily" ~ 365/365),
         frequency_per_person_month = frequency_per_person_annual / 12,
         frequency_month_harvested = round(frequency_per_person_month * number_of_months, 4)) %>%
         #frequency_month_harvested = paste(frequency_month_harvested, " times per month")) %>%
  mutate(guano_person_month = case_when(
    is.na(months_harvested) ~ as.numeric(str_extract(guano_weight_per_person, "[:digit:]+")), 
    str_detect(guano_weight_per_person, "kg/day") ~ 
      as.numeric(str_extract(guano_weight_per_person, "[:digit:][:punct:]?[:digit:]{1,3}")) * 365/12,
    str_detect(guano_weight_per_person, "kg/week") ~ 
      as.numeric(str_extract(guano_weight_per_person, "[:digit:]{1,3}")) * 52/12,
    str_detect(guano_weight_per_person, "kg/month") ~ 
      as.numeric(str_extract(guano_weight_per_person, "[:digit:]{1,3}")),
    str_detect(guano_weight_per_person, "kg/3-4 months") ~ 
      as.numeric(str_extract(guano_weight_per_person, "(?<=-)[:digit:]{4}")) * 4/12)) %>%
  mutate(guano_person_year = if_else(guano_person_month == 400, guano_person_month, guano_person_month * number_of_months)) %>%
  select(site_name, number_of_harvesters, number_of_months, months_harvested, days_per_year = frequency_per_person_annual, guano_person_month, guano_person_year)

                                             