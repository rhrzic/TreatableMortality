require(tidyverse)
require(stringr)
require(ggplot2)

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

GBD2014 <- read.csv2(file = "Data/IHME-GBD_2017_DATA-affdf515-1.csv", sep = ",")

GBD2014 <- GBD2014 %>%
  select(measure = measure_name, geo = location_name, sex = sex_name, age = age_name, 
         cause = cause_name, metric = metric_name, year, val, upper, lower) %>%
  mutate(val = as.numeric(val),
         upper = as.numeric(upper),
         lower = as.numeric(lower))

GBD2014 <- GBD2014 %>%
  mutate(age = ifelse(age %in% c("90 to 94", "95 plus"), "90 plus", age)) %>%
  group_by(age, sex, geo, cause, measure) %>%
  summarise(val = mean(val, na.rm = T), 
            upper = mean(upper, na.rm = T), 
            lower = mean(lower, na.rm =T))

treatable_burden <- left_join(GBD2014, ESP, by = c("age"))

treatable_burden <- treatable_burden %>%
  mutate(ESP_rate = val * ESP/100000,
         ESP_rate_upper = upper * ESP/100000,
         ESP_rate_lower = lower * ESP/100000) %>%
  group_by(geo, sex, measure) %>%
  summarise(ESP_burden = sum(ESP_rate), 
            ESP_burden_upper = sum(ESP_rate_upper),
            ESP_burden_lower = sum(ESP_rate_lower))

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

treatable_DALY <- treatable_burden %>%
  filter(measure == "DALYs (Disability-Adjusted Life Years)") %>%
  mutate(DALYs = round(ESP_burden, 1)) %>%
  group_by(sex) %>%
  mutate(rank = rank(DALYs)) %>%
  select(geo, DALYs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, DALYs, rank) %>%
  pivot_wider(names_from = sex, values_from = c(DALYs, rank))


treatable_YLD <- treatable_burden %>%
  filter(measure == "YLDs (Years Lived with Disability)") %>%
  mutate(YLDs = round(ESP_burden, 1)) %>%
  group_by(sex) %>%
  mutate(rank = rank(YLDs)) %>%
  select(geo, YLDs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, YLDs, rank) %>%
  pivot_wider(names_from = sex, values_from = c(YLDs, rank))

treatable_YLL <- treatable_burden %>%
  filter(measure == "YLLs (Years of Life Lost)") %>%
  mutate(YLLs = round(ESP_burden, 1)) %>%
  group_by(sex) %>%
  mutate(rank = rank(YLLs)) %>%
  select(geo, YLLs, rank) %>%
  left_join(., country_codes, by = c("geo" = "name")) %>%
  select(code, sex, YLLs, rank) %>%
  pivot_wider(names_from = sex, values_from = c(YLLs, rank))








