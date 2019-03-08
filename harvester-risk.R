library(here)
library(tidyverse)

bh <- read_csv(here("data", "bat_harvester_questionnaire.csv")) 

# Risk as a function of exposure x hazard

exposures <- bh %>%
  select(site_name, frequency_per_person, 
         collection_method, duration_active_harvest, 
         frequency_per_person, months_harvested)

hazard <- bh %>%
  select(site_name, guano_weight_per_person, guano_fresh_dry, bat_species_contribution, bat_roost_size)


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
  mutate(exp_collect = rowSums(.[2:(ncol(collect)-1)]) %>%
           rescale(to = c(0, 1), from = c(0, ((ncol(collect)-2)*2))))

# Frequency and Duration of Exposures

freq <- bh %>%
  select(duration_active_harvest, frequency_of_harvesting, frequency_per_person, months_harvested) %>%
  mutate(duration = str_extract_all(duration_active_harvest, "[:digit:]{2,3}(?= minutes typical)|[:digit:]{2,3}(?= minutes$)")) %>%
  mutate(frequency = case_when(frequency_of_harvesting == "1 time per year" ~ 1/365,
      frequency_of_harvesting == "3 times per month" ~ 3*12/365,
      frequency_of_harvesting == "2 times per year" ~ 2/365,
      frequency_of_harvesting == "3 times per week" ~ 3*52/365,
      frequency_of_harvesting == "2 times per week" ~ 2*52/365,
      frequency_of_harvesting == "daily" ~ 365/365)) %>%
  mutate(number_of_months = str_count(months_harvested, "[:alpha:](?=;)") + 1)

  
# Per Person Risk

  
# -----------------------------------------------------------------------------------------  
# weight exposures by category (environment, activity, behavior)
# weight hazard by category (bat type, viral HP3 values, guano)

# derive two values per site: one for individual, and one for population at site
# popup to show values for risk per site
# popup also to show the values per category

# table to show the values per category 
# map with colors indicating levels of risk at each site
