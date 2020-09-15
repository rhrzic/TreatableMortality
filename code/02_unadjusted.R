rm(list=ls())

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

population2014 <- readRDS(file = "data/population.rds") %>%
  filter(time == 2014)

deaths2014 <- readRDS(file = "data/deaths.rds") %>%
  filter(time == 2014)

df <- left_join(deaths2014, population2014, by = c("time", "geo", "age", "sex"))

saveRDS(df, "data/mortality_rates.rds")


df <- left_join(df, ESP, by = c("age"))

#Age cutoff < 75
df <- df %>%
  filter(!age %in% c("Y75-79", "Y80-84", "Y_GE85")) %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)
  
df_byicd10 <- df %>%
  mutate(disease_group = case_when(str_starts(icd10, "C") ~ "Cancer",
                                   str_starts(icd10, "I") ~ "Circulatory disease",
                                   TRUE ~ "Other causes")) %>%
  group_by(sex, geo, disease_group) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T),
            avoidable_mort = round(avoidable_mort, 2))

df_total <- df %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T),
            avoidable_mort= round(avoidable_mort,2))

df_total <- df_total %>%
  group_by(sex) %>%
  mutate(rank = rank(avoidable_mort))

df_byicd10 <- df_byicd10 %>%
  group_by(sex, disease_group) %>%
  mutate(rank = round(rank(avoidable_mort),0))

result_total <- df_total %>%
  select(geo, avoidable_mort, rank) %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort, rank))

result_total <- bind_rows(result_total, summary_stats(result_total$avoidable_mort_F, 
                                                      result_total$avoidable_mort_M, 
                                                                    "avoidable_mort_F", "avoidable_mort_M"))

write.xlsx(result_total, 'results/Unadjusted_total.xlsx')


result_byicd10 <- df_byicd10 %>%
  select(geo, avoidable_mort, rank) %>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(avoidable_mort, rank))

write.xlsx(result_byicd10, 'results/Unadjusted_by_disease.xlsx')
