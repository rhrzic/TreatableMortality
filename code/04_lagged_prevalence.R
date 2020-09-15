rm(list=ls())

require(tidyverse)
require(ineq)
require(openxlsx)

population_2014 <- readRDS(file = "data/population.rds") %>%
  filter(time == 2014)

population_2016 <- readRDS(file = "data/population.rds") %>%
  filter(time == 2016)

deaths_2016 <- readRDS(file = "data/deaths.rds") %>%
  filter(time == 2016)

ehis_prevalence_2014 <- readRDS("data/ehis_prevalence.rds")

mortality_2016 <- left_join(deaths_2016, population_2016, by = c("time", "geo", "age", "sex"))

deaths_of_interest <- c("J45_J46", "J40-J44_J47", "I20-I25", "I60-I69", "K70_K73_K74", "N00-N29", "E10-E14")

mortality_2016 <- mortality_2016 %>%
  mutate(age = case_when(age %in% c("Y15-19", "Y20-24") ~ "Y15-24",
                               age %in% c("Y25-29", "Y30-34") ~ "Y25-34",
                               age %in% c("Y35-39", "Y40-44") ~ "Y35-44",
                               age %in% c("Y45-49", "Y50-54") ~ "Y45-54",
                               age %in% c("Y55-59", "Y60-64") ~ "Y55-64",
                               age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                         TRUE ~ "Other")) %>%
  filter(age %in% c("Y15-24", "Y25-34", "Y35-44", "Y45-54", "Y55-64", "Y65-74"),
         icd10 %in% deaths_of_interest) %>%
  group_by(geo, sex, age, icd10) %>%
  summarise(deaths = sum(deaths),
            population = sum(population))

ehis_prevalence_2014 <- ehis_prevalence_2014 %>%
  mutate(icd10 = case_when(hlth_pb == "PB2311" ~ "J45_J46",
                           hlth_pb == "PB2312" ~ "J40-J44_J47",
                           hlth_pb == "PB2412" ~ "I20-I25",
                           hlth_pb == "PB2413" ~ "I60-I69",
                           hlth_pb == "PB2421" ~ "K70_K73_K74",
                           hlth_pb == "PB2423" ~ "N00-N29",
                           hlth_pb == "PB2430" ~ "E10-E14"),
         prevalence = prevalence/100)

population_2014 <- population_2014 %>%
  mutate(age = case_when(age %in% c("Y15-19", "Y20-24") ~ "Y15-24",
                         age %in% c("Y25-29", "Y30-34") ~ "Y25-34",
                         age %in% c("Y35-39", "Y40-44") ~ "Y35-44",
                         age %in% c("Y45-49", "Y50-54") ~ "Y45-54",
                         age %in% c("Y55-59", "Y60-64") ~ "Y55-64",
                         age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                         TRUE ~ "Other")) %>%
  filter(age %in% c("Y15-24", "Y25-34", "Y35-44", "Y45-54", "Y55-64", "Y65-74")) %>%
  group_by(geo, sex, age) %>%
  summarise(population_2014 = sum(population))

prevalence_2014 <- left_join(population_2014, ehis_prevalence_2014, by = c("age", "sex", "geo"))

prevalence_2014  <- prevalence_2014 %>%
  mutate(prevalent_cases = prevalence * population_2014)
  
df <- left_join(mortality_2016, prevalence_2014, by = c("age", "sex", "geo", "icd10"))

df <- df %>%
  group_by(geo, sex, icd10) %>%
  summarise(deaths = sum(deaths, na.rm=T),
            prevalent_cases = sum(prevalent_cases, na.rm=T))

result_byicd10 <- df %>%
  mutate(disease_group = case_when(icd10 == "E10-E14" ~ "Diabetes",
                                   str_starts(icd10, "I") ~ "Circulatory disease",
                                   str_starts(icd10, "J") ~ "Respiratory disease",
                                   TRUE ~ "Other causes")) %>%
  group_by(geo, sex, disease_group) %>%
  summarise(deaths = sum(deaths, na.rm=T),
            prevalent_cases = sum(prevalent_cases, na.rm=T),
            case_fatality_100k = round(100000*deaths/prevalent_cases, 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank = rank(case_fatality_100k)) %>%
  select(geo, sex, disease_group, case_fatality_100k, rank) %>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(case_fatality_100k, rank))

write.xlsx(result_byicd10, 'results/Lagged_prevalence_by_disease.xlsx')


result_total <- df %>%
  group_by(geo, sex) %>%
  summarise(deaths = sum(deaths, na.rm=T),
            prevalent_cases = sum(prevalent_cases, na.rm=T),
            case_fatality_100k = round(100000*deaths/prevalent_cases, 2)) %>%
  group_by(sex) %>%
  mutate(rank = rank(case_fatality_100k)) %>%
  select(geo, sex, case_fatality_100k, rank) %>%
  pivot_wider(names_from = sex, values_from = c(case_fatality_100k, rank))

write.xlsx(result_total, 'results/Lagged_prevalence_total.xlsx')

round(mean(result_total$case_fatality_100k_F),2)
round(median(result_total$case_fatality_100k_F), 2)
round(range(result_total$case_fatality_100k_F), 2)
round(IQR(result_total$case_fatality_100k_F),2)
round(var.coeff(result_total$case_fatality_100k_F),2)
round(Gini(result_total$case_fatality_100k_F),2)
round(Theil(result_total$case_fatality_100k_F),2)

round(mean(result_total$case_fatality_100k_M),2)
round(median(result_total$case_fatality_100k_M), 2)
round(range(result_total$case_fatality_100k_M), 2)
round(IQR(result_total$case_fatality_100k_M),2)
round(var.coeff(result_total$case_fatality_100k_M),2)
round(Gini(result_total$case_fatality_100k_M),2)
round(Theil(result_total$case_fatality_100k_M),2)



