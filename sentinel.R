require(tidyverse)
require(eurostat)

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

geo_include = c("AT", "BE", "BG", "CZ", "DK", "DE", "DE_TOT", "FR", "EE", "IE", "EL", "ES", "HR", "IT", "CY", "LV", 
                "LT", "LU", "HU", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK", "IS", "NO")

sentinels <- c("C33_C34", "C18-C21", "C50", "I21_I22", "E10-E14", "X60-X84_Y870")


## Deaths ##

deaths <- get_eurostat("hlth_cd_aro", time_format = "num")

deaths <- deaths %>%
  filter(sex %in% c("M", "F"),
         geo %in% geo_include,
         icd10 %in% sentinels,
         resid == "TOT_IN",
         !age %in% c("TOTAL", "Y_LT15", "Y_LT25", "Y_LT65", "Y_GE65", "Y85-89", "Y90-94", "Y_GE95",
                     "Y15-24")) %>%
  mutate(age = replace(age, age == "Y1-4", "Y01-04"),
         age = replace(age, age == "Y5-9", "Y05-09"),
         age = replace(age, age == "Y_LT1", "Y00")) %>%
  select(time, geo, icd10, sex, age, deaths = values)

df <- left_join(population2014, deaths, by = c("time", "geo", "age", "sex"))

df <- left_join(df, ESP, by = c("age"))

df <- df %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)

df_sentinel <- df %>%
  filter(!(icd10 == "C50" & sex == "M")) %>%
  mutate(sentinel = case_when(icd10 %in% c("C18-C21", "C50") ~ "Breast and Colorectal cancer",
                              icd10 == "I21_I22" ~ "Acute myocardial infarction",
                              icd10 %in% c("E10-E14", "X60-X84_Y870") ~ "Diabetes and suicide",
                              TRUE ~ "Lung cancer")) %>%
  group_by(sex, geo, sentinel) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T),
            avoidable_mort = round(avoidable_mort, 4))

df_sentinel <- df_sentinel %>%
  group_by(sex, sentinel) %>%
  mutate(rank = rank(avoidable_mort))

result <- df_sentinel %>%
  select(geo, avoidable_mort, rank) %>%
  pivot_wider(names_from = c(sex, sentinel), values_from = c(avoidable_mort, rank))
