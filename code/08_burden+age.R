rm(list=ls())

require(tidyverse)
require(openxlsx)
source("code/summary_stats.R")

ESP <- data.frame(age = c("<1 year",
                          "1 to 4",
                          "5 to 9",
                          "10 to 14",
                          "15 to 19",
                          "20 to 24",
                          "25 to 29",
                          "30 to 34",
                          "35 to 39",
                          "40 to 44",
                          "45 to 49",
                          "50 to 54",
                          "55 to 59",
                          "60 to 64",
                          "65 to 69",
                          "70 to 74",
                          "75 to 79",
                          "80 to 84",
                          "85 to 89",
                          "90 plus"), ESP = c(1000, 
                                             4000,
                                             5500,
                                             5500,
                                             5500,
                                             6000,
                                             6000,
                                             6500,
                                             7000,
                                             7000,
                                             7000,
                                             7000,
                                             6500,
                                             6000,
                                             5500,
                                             5000,
                                             4000,
                                             2500,
                                             1500,
                                             1000))

GBD2014 <- read.csv2(file = "data/IHME-GBD_2017_DATA-affdf515-1.csv", sep = ",")

GBD2014 <- GBD2014 %>%
  select(measure = measure_name, geo = location_name, sex = sex_name, age = age_name, 
         cause = cause_name, metric = metric_name, year, val) %>%
  mutate(val = as.numeric(val)) %>%
  mutate(age = ifelse(age %in% c("90 to 94", "95 plus"), "90 plus", age)) %>%
  group_by(measure, geo, sex, age, cause, metric, year) %>%
  summarise(val = sum(val))

treatable_burden_sexlimits <- left_join(GBD2014, ESP, by = "age") %>%
  filter(!age %in% c("85 to 89", "90 plus")) %>%
  filter(!(age == "80 to 84" & sex == "Male")) %>%
  mutate(ESP_rate = val * ESP/100000)

treatable_burden_total_sexlimits <- treatable_burden_sexlimits %>%
  group_by(geo, sex, measure) %>%
  summarise(ESP_burden = sum(ESP_rate))

treatable_burden_byicd10_sexlimits <- treatable_burden_sexlimits %>%
  mutate(disease_group = case_when(cause %in% c("Ischemic heart disease", "Stroke", "Rheumatic heart disease", "Aortic aneurysm") ~ "Circulatory disease",
                                   str_detect(cause, "cancer") | cause %in% c("Malignant skin melanoma", "Hodgkin lymphoma") ~ "Cancers",
                                   TRUE ~ "Other")) %>%
  group_by(geo, sex, disease_group, measure) %>%
  summarise(ESP_burden = sum(ESP_rate))

country_codes <- data.frame(code = c("AT",
                                     "BE",
                                     "BG",
                                     "CY",
                                     "CZ",
                                     "DE",
                                     "DK",
                                     "EE",
                                     "EL",
                                     "ES",
                                     "FI",
                                     "FR",
                                     "HR",
                                     "HU",
                                     "IE",
                                     "IS",
                                     "IT",
                                     "LT",
                                     "LU",
                                     "LV",
                                     "MT",
                                     "NL",
                                     "NO",
                                     "PL",
                                     "PT",
                                     "RO",
                                     "SE",
                                     "SI",
                                     "SK",
                                     "UK"), name = c("Austria",
                                                     "Belgium",
                                                     "Bulgaria",
                                                     "Cyprus",
                                                     "Czech Republic",
                                                     "Germany",
                                                     "Denmark",
                                                     "Estonia",
                                                     "Greece",
                                                     "Spain",
                                                     "Finland",
                                                     "France",
                                                     "Croatia",
                                                     "Hungary",
                                                     "Ireland",
                                                     "Iceland",
                                                     "Italy",
                                                     "Lithuania",
                                                     "Luxembourg",
                                                     "Latvia",
                                                     "Malta",
                                                     "Netherlands",
                                                     "Norway",
                                                     "Poland",
                                                     "Portugal",
                                                     "Romania",
                                                     "Sweden",
                                                     "Slovenia",
                                                     "Slovakia",
                                                     "United Kingdom"))



treatable_YLL_sexlimits_total <- treatable_burden_total_sexlimits %>%
  filter(measure == "YLLs (Years of Life Lost)") %>%
  mutate(DALYs = round(ESP_burden, 2)) %>%
  group_by(sex) %>%
  mutate(rank = rank(DALYs)) %>%
  select(geo, DALYs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, DALYs, rank) %>%
  pivot_wider(names_from = sex, values_from = c(DALYs, rank))

treatable_YLL_sexlimits_total <- bind_rows(treatable_YLL_sexlimits_total, summary_stats(treatable_YLL_sexlimits_total$DALYs_Female, 
                                                                                        treatable_YLL_sexlimits_total$DALYs_Male, 
                                                                    "DALYs_Female", "DALYs_Male"))

write.xlsx(treatable_YLL_sexlimits_total, 'results/treatable_YLL_sexlimits_total.xlsx')

treatable_YLL_sexlimits_byicd10 <- treatable_burden_byicd10_sexlimits %>%
  filter(measure == "YLLs (Years of Life Lost)") %>%
  mutate(DALYs = round(ESP_burden, 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank = rank(DALYs)) %>%
  select(geo, sex, disease_group, DALYs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, DALYs, rank) %>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(DALYs, rank))

write.xlsx(treatable_YLL_sexlimits_byicd10, 'results/treatable_YLL_sexlimits_bydisease.xlsx')

## NO age limits

treatable_burden_nolimits <- left_join(GBD2014, ESP, by = "age") %>%
  mutate(ESP_rate = val * ESP/100000)

treatable_burden_total_nolimits <- treatable_burden_nolimits %>%
  group_by(geo, sex, measure) %>%
  summarise(ESP_burden = sum(ESP_rate, na.rm=T))

treatable_burden_byicd10_nolimits <- treatable_burden_nolimits %>%
  mutate(disease_group = case_when(cause %in% c("Ischemic heart disease", "Stroke", "Rheumatic heart disease", "Aortic aneurysm") ~ "Circulatory disease",
                                   str_detect(cause, "cancer") | cause %in% c("Malignant skin melanoma", "Hodgkin lymphoma") ~ "Cancers",
                                   TRUE ~ "Other")) %>%
  group_by(geo, sex, disease_group, measure) %>%
  summarise(ESP_burden = sum(ESP_rate))


treatable_YLL_nolimits_total <- treatable_burden_total_nolimits %>%
  filter(measure == "YLLs (Years of Life Lost)") %>%
  mutate(DALYs = round(ESP_burden, 2)) %>%
  group_by(sex) %>%
  mutate(rank = rank(DALYs)) %>%
  select(geo, DALYs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, DALYs, rank) %>%
  pivot_wider(names_from = sex, values_from = c(DALYs, rank))

treatable_YLL_nolimits_total <- bind_rows(treatable_YLL_nolimits_total, summary_stats(treatable_YLL_nolimits_total$DALYs_Female, 
                                                                                      treatable_YLL_nolimits_total$DALYs_Male, 
                                                                                        "DALYs_Female", "DALYs_Male"))

write.xlsx(treatable_YLL_nolimits_total, 'results/treatable_YLL_nolimits_total.xlsx')

treatable_YLL_nolimits_byicd10 <- treatable_burden_byicd10_nolimits %>%
  filter(measure == "YLLs (Years of Life Lost)") %>%
  mutate(DALYs = round(ESP_burden, 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank = rank(DALYs)) %>%
  select(geo, sex, disease_group, DALYs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, DALYs, rank) %>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(DALYs, rank))

write.xlsx(treatable_YLL_nolimits_byicd10, 'results/treatable_YLL_nolimits_byicd10.xlsx')





