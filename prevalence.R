require(tidyverse)
require(stringr)
require(ggplot2)

ESP <- data.frame(age_group = c("Y15-24",
                          "Y25-34",
                          "Y35-44",
                          "Y45-54",
                          "Y55-64",
                          "Y65-74",
                          "Y_GE75"), ESP = c(11500,
                                             12500,
                                             14000,
                                             14000,
                                             11500,
                                             10500,
                                             9000))

population_in <- read.csv2(file = "population.csv", sep = ",")

ehis_prevalence_in <- read.csv2(file = "ehis_prevalence.csv", sep = ",")

population_in <- population_in %>%
  filter(age != "Y00",
         age != "Y01-04",
         age != "Y05-09",
         age != "Y10-14") %>%
  mutate(age_group = case_when(age %in% c("Y15-19", "Y20-24") ~ "Y15-24",
                               age %in% c("Y25-29", "Y30-34") ~ "Y25-34",
                               age %in% c("Y35-39", "Y40-44") ~ "Y35-44",
                               age %in% c("Y45-49", "Y50-54") ~ "Y45-54",
                               age %in% c("Y55-59", "Y60-64") ~ "Y55-64",
                               age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                               TRUE ~ "Y_GE75")) %>%
  group_by(age_group, sex, geo) %>%
  summarise(population = sum(population))

ehis_prevalence_in <- ehis_prevalence_in %>%
  select(hlth_pb, sex, age_group = age, geo, proportion = values)

prevalent_cases <- left_join(ehis_prevalence_in, population_in, by = c("age_group", "sex", "geo"))

prevalent_cases <- prevalent_cases %>%
  mutate(proportion = as.numeric(proportion),
         prevalent_cases = (proportion / 100) * population,
         icd10 = case_when(hlth_pb == "PB2311" ~ "J45_J46",
                           hlth_pb == "PB2312" ~ "J40-J44_J47",
                           hlth_pb == "PB2412" ~ "I20-I25",
                           hlth_pb == "PB2413" ~ "I60-I69",
                           hlth_pb == "PB2421" ~ "K70_K73_K74",
                           hlth_pb == "PB2423" ~ "N00-N29",
                           hlth_pb == "PB2430" ~ "E10-E14"))

deaths_in <- read.csv2(file = "deaths.csv", sep = ",")

deaths_of_interest <- c("J45_J46", "J40-J44_J47", "I20-I25", "I60-I69", "K70_K73_K74", "N00-N29", "E10-E14")

deaths_in <- deaths_in %>%
  filter(age != "Y00",
         age != "Y01-04",
         age != "Y05-09",
         age != "Y10-14") %>%
  mutate(age_group = case_when(age %in% c("Y15-19", "Y20-24") ~ "Y15-24",
                               age %in% c("Y25-29", "Y30-34") ~ "Y25-34",
                               age %in% c("Y35-39", "Y40-44") ~ "Y35-44",
                               age %in% c("Y45-49", "Y50-54") ~ "Y45-54",
                               age %in% c("Y55-59", "Y60-64") ~ "Y55-64",
                               age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                               TRUE ~ "Y_GE75")) %>%
  filter(icd10 %in% deaths_of_interest) %>%
  group_by(age_group, sex, geo, icd10) %>%
  summarise(deaths = sum(deaths))

adjusted_mortality_rate <- left_join(deaths_in, prevalent_cases, by = c("age_group", "sex", "geo", "icd10"))

adjusted_mortality_rate <- adjusted_mortality_rate %>%
  mutate(case_fatality = deaths / prevalent_cases) %>%
  mutate(case_fatality = replace(case_fatality, case_fatality > 1, NA))

adjusted_mortality_rate <- left_join(adjusted_mortality_rate, ESP, by = "age_group")

result <- adjusted_mortality_rate %>%
  mutate(ESP_adjusted_rate = case_fatality * ESP / 0.83) %>%
  group_by(geo, sex) %>%
  summarise(mean_case_fatality = mean(case_fatality, na.rm = T),
            adjusted_avoidable_mortality = sum(ESP_adjusted_rate, na.rm = T)) %>%
  mutate(adjusted_avoidable_mortality = round(adjusted_avoidable_mortality,1),
         mean_case_fatality = round(mean_case_fatality, 3))

result <- result %>%
  group_by(sex) %>%
  mutate(rank = rank(adjusted_avoidable_mortality)) %>%
  select(geo, sex, mean_case_fatality, adjusted_avoidable_mortality, rank) %>%
  pivot_wider(names_from = sex, values_from = c(adjusted_avoidable_mortality, mean_case_fatality, rank))



