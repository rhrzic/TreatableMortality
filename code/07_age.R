rm(list=ls())

require(tidyverse)
require(ineq)
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

mortality_2014 <- readRDS("data/mortality_rates.rds")

df <- left_join(mortality_2014, ESP, by = c("age"))

df_nolimit_total <- df %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP) %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort_nolimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex) %>%
  mutate(rank_nolimit = rank(avoidable_mort_nolimit)) %>%
  select(geo, sex, avoidable_mort_nolimit, rank_nolimit) %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort_nolimit, rank_nolimit))

df_nolimit_total <- bind_rows(df_nolimit_total, summary_stats(df_nolimit_total$avoidable_mort_nolimit_F, 
                                                              df_nolimit_total$avoidable_mort_nolimit_M, 
                                                                    "avoidable_mort_nolimit_F", "avoidable_mort_nolimit_M"))

write.xlsx(df_nolimit_total, 'results/age_nolimit_total.xlsx')


df_higherlimit_total <- df %>%
  filter(age != "Y80-84",
         age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP) %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort_higherlimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex) %>%
  mutate(rank_higherlimit = rank(avoidable_mort_higherlimit)) %>%
  select(geo, sex, avoidable_mort_higherlimit, rank_higherlimit) %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort_higherlimit, rank_higherlimit))

df_higherlimit_total <- bind_rows(df_higherlimit_total, summary_stats(df_higherlimit_total$avoidable_mort_higherlimit_F, 
                                                                      df_higherlimit_total$avoidable_mort_higherlimit_M, 
                                                              "avoidable_mort_higherlimit_F", "avoidable_mort_higherlimit_M"))


write.xlsx(df_higherlimit_total, 'results/age_higherlimit_total.xlsx')


df_sexlimit_total <- df %>%
  filter(!(age == "Y80-84" & sex == "M")) %>%
  filter(age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)%>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort_sexlimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex) %>%
  mutate(rank_sexlimit = rank(avoidable_mort_sexlimit)) %>%
  select(geo, sex, avoidable_mort_sexlimit, rank_sexlimit) %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort_sexlimit, rank_sexlimit))

df_sexlimit_total <- bind_rows(df_sexlimit_total, summary_stats(df_sexlimit_total$avoidable_mort_sexlimit_F, 
                                                                df_sexlimit_total$avoidable_mort_sexlimit_M, 
                                                                      "avoidable_mort_sexlimit_F", "avoidable_mort_sexlimit_M"))


write.xlsx(df_sexlimit_total, 'results/age_sexlimit_total.xlsx')



df_byicd10_nolimit <- df %>%
  mutate(disease_group = case_when(str_starts(icd10, "C") ~ "Cancer",
                                   str_starts(icd10, "I") ~ "Circulatory disease",
                                   TRUE ~ "Other causes")) %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP) %>%
  group_by(sex, geo, disease_group) %>%
  summarise(avoidable_mort_nolimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank_nolimit = rank(avoidable_mort_nolimit)) %>%
  select(geo, sex, disease_group, avoidable_mort_nolimit, rank_nolimit)%>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(avoidable_mort_nolimit, rank_nolimit))

write.xlsx(df_byicd10_nolimit, 'results/age_nolimit_bydisease.xlsx')

df_byicd10_higherlimit <- df %>%
  mutate(disease_group = case_when(str_starts(icd10, "C") ~ "Cancer",
                                   str_starts(icd10, "I") ~ "Circulatory disease",
                                   TRUE ~ "Other causes")) %>%
  filter(!age %in% c("Y80-84","Y_GE85")) %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP) %>%
  group_by(sex, geo, disease_group) %>%
  summarise(avoidable_mort_higherlimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank_higherlimit = rank(avoidable_mort_higherlimit)) %>%
  select(geo, sex, disease_group, avoidable_mort_higherlimit, rank_higherlimit)%>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(avoidable_mort_higherlimit, rank_higherlimit))

write.xlsx(df_byicd10_higherlimit, 'results/age_higherlimit_bydisease.xlsx')


df_byicd10_sexlimit <- df %>%
  mutate(disease_group = case_when(str_starts(icd10, "C") ~ "Cancer",
                                   str_starts(icd10, "I") ~ "Circulatory disease",
                                   TRUE ~ "Other causes")) %>%
  filter(!(age == "Y80-84" & sex == "M")) %>%
  filter(age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)%>%
  group_by(sex, geo, disease_group) %>%
  summarise(avoidable_mort_sexlimit = round(sum(ESP_death_rate, na.rm = T), 2)) %>%
  group_by(sex, disease_group) %>%
  mutate(rank_sexlimit = rank(avoidable_mort_sexlimit)) %>%
  select(geo, sex, avoidable_mort_sexlimit, rank_sexlimit)%>%
  pivot_wider(names_from = c(sex, disease_group), values_from = c(avoidable_mort_sexlimit, rank_sexlimit))

write.xlsx(df_byicd10_sexlimit, 'results/age_sexlimit_bydisease.xlsx')

