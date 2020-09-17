rm(list=ls())

require(eurostat)
require(tidyverse)
require(openxlsx)
source("code/summary_stats.R")

ESP <- data.frame(age = c("Y00",
                          "Y01-04",
                          "Y05-09",
                          "Y10-14",
                          "Y15-19",
                          "Y20-24",
                          "Y25-29",
                          "Y30-34",
                          "Y35-39",
                          "Y40-44",
                          "Y45-49",
                          "Y50-54",
                          "Y55-59",
                          "Y60-64",
                          "Y65-69",
                          "Y70-74",
                          "Y75-79",
                          "Y80-84",
                          "Y_GE85"), ESP = c(1000, 
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
                                             2500))


sentinels <- c("C33_C34", "C18-C21", "C50", "I21_I22", "E10-E14", "X60-X84_Y870")


## Deaths ##

geo_include = c("AT", "BE", "BG", "CZ", "DK", "DE", "DE_TOT", "FR", "EE", "IE", "EL", "ES", "HR", "IT", "CY", "LV", 
                "LT", "LU", "HU", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK", "IS", "NO")

## Deaths ##

deaths <- get_eurostat("hlth_cd_aro", time_format = "num")

deaths <- deaths %>%
  filter(sex %in% c("M", "F"),
         geo %in% geo_include,
         resid == "TOT_IN",
         !age %in% c("TOTAL", "Y_LT15", "Y_LT25", "Y_LT65", "Y_GE65", "Y85-89", "Y90-94", "Y_GE95",
                     "Y15-24")) %>%
  mutate(age = replace(age, age == "Y1-4", "Y01-04"),
         age = replace(age, age == "Y5-9", "Y05-09"),
         age = replace(age, age == "Y_LT1", "Y00")) 

deaths <- deaths %>% 
  filter(icd10 %in% sentinels) %>%
  mutate(deaths = values) %>%
  select(sex, age, icd10, geo, time, deaths)

population <- get_eurostat("demo_pjangroup", time_format = "num", filters = list(sex = c("M", "F"),
                                                                                 geo = geo_include))
population <- population %>%
  filter(!age %in% c("TOTAL", "Y_GE75", "Y_GE80", "UNK", "Y_LT5")) %>%
  mutate(age = replace(age, age == "Y5-9", "Y05-09"))

population_LT1 <- get_eurostat("demo_pjan", time_format = "num", filters = list(age = "Y_LT1",
                                                                                sex = c("M", "F"),
                                                                                geo = geo_include))

population_LT1 <- population_LT1 %>%
  mutate(age = replace(age, age == "Y_LT1", "Y00"))


population_1_4 <- get_eurostat("demo_pjan", time_format = "num", filters = list(age = c("Y1", "Y2", "Y3", "Y4"),
                                                                                sex = c("M", "F"),
                                                                                geo = geo_include))

population_1_4 <- population_1_4 %>%
  pivot_wider(names_from = age, values_from = values) %>%
  mutate(age = "Y01-04",
         values = Y1+Y2+Y3+Y4) %>%
  select(unit, age, sex, geo, time, values)

population <- rbind(population_LT1, population_1_4, population)

age_groups <- unique(deaths$age)

population <- population %>%
  mutate(population = values,
         geo = replace(geo, geo == "DE_TOT", "DE")) %>%
  filter(age %in% age_groups) %>%
  select(sex, age, geo, time, population)

mortality_2014 <- left_join(deaths, population, by = c("time", "geo", "age", "sex")) %>%
  filter(time == 2014)


sentinel <- left_join(mortality_2014, ESP, by = c("age")) %>%
  filter(!(sex == "M" & icd10 == "C50")) %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP,
         sentinel = case_when(icd10 %in% c("C18-C21", "C50") ~ "Breast and Colorectal cancer",
                              icd10 == "I21_I22" ~ "Acute myocardial infarction",
                              icd10 %in% c("E10-E14", "X60-X84_Y870") ~ "Diabetes and suicide",
                              TRUE ~ "Lung cancer")) %>%
  group_by(sex, geo, sentinel) %>%
  summarise(avoidable_mort = round(sum(ESP_death_rate, na.rm = T),2)) %>%
  group_by(sex, sentinel) %>%
  mutate(rank = rank(avoidable_mort)) %>%
  select(geo, avoidable_mort, rank) %>%
  pivot_wider(names_from = c(sex, sentinel), values_from = c(avoidable_mort, rank))

sentinel_women <- select(sentinel, geo, starts_with("avoidable_mort_F"), starts_with("rank_F"))

sentinel_men <- select(sentinel, geo, starts_with("avoidable_mort_M"), starts_with("rank_M"))

write.xlsx(sentinel_women, 'results/sentinel_women.xlsx')
write.xlsx(sentinel_men, 'results/sentinel_men.xlsx')


