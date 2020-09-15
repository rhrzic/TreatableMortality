rm(list=ls())

require(eurostat)
require(tidyverse)
require(ineq)
require(openxlsx)
require(apaTables)

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

population <- readRDS(file = "data/population.rds") %>%
  filter(time %in% 2011:2017)

deaths <- readRDS(file = "data/deaths.rds") %>%
  filter(time %in% 2011:2017)

##Top mortality causes and countries over time

mortality <- left_join(deaths, population, by = c("time", "geo", "age", "sex")) %>%
  left_join(., ESP, by = "age") %>%
  filter(!age %in% c("Y75-79", "Y80-84", "Y_GE85")) %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP) %>%
  group_by(time, geo, sex, icd10) %>%
  summarise(mort_rate = sum(ESP_death_rate, na.rm = T))

top_mortality <- mortality %>%
  group_by(geo, sex, icd10) %>%
  summarise(mort_rate = mean(mort_rate)) %>%
  pivot_wider(names_from = sex, values_from = mort_rate) %>%
  arrange(desc(M)) %>%
  head(n = 50) %>%
  summarise(geo = unique(geo),
            icd10 = unique(icd10))

#Top morbidity causes in 2014

ehis_prevalence_2014 <- readRDS(file = "data/ehis_prevalence.rds")
population_2014 <- readRDS(file = "data/population.rds") %>%
  filter(time == 2014)

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

prevalence_2014 <- left_join(population_2014, ehis_prevalence_2014, by = c("age", "sex", "geo")) %>%
  mutate(prevalent_cases = prevalence * population_2014) %>%
  group_by(sex, geo, icd10) %>%
  summarise(population = sum(population_2014), 
            prevalent_cases = sum(prevalent_cases),
            overall_prevalence = prevalent_cases/population)

top_prevalence <- prevalence_2014 %>%
  pivot_wider(names_from = sex, values_from = overall_prevalence) %>%
  arrange(desc(M)) %>%
  head(n = 50) %>%
  summarise(geo = unique(geo),
            icd10 = unique(icd10))


##Models datasets

countries_of_interest <- c("HU", "BG", "LT", "DE", "FI")

GDPpc <- get_eurostat("nama_10_pc", time_format = "num", filters = list(na_item = "B1GQ",
                                                                        unit = "CP_PPS_HAB")) %>%
  filter(time %in% 2011:2017,
         geo %in% countries_of_interest) %>%
  select(time, geo, gdppc = values)

spending <- get_eurostat("hlth_sha11_hf", time_format = "num", filters = list(icha11_hf = "TOT_HF",
                                                                              unit = "PPS_HAB")) %>%
  filter(time %in% 2011:2017,
         geo %in% countries_of_interest) %>%
  select(time, geo, spending = values)


df_cvs <- mortality %>%
  filter(icd10 == "I60-I69",
         geo %in% countries_of_interest)

df_cvs <-left_join(df_cvs, prevalence_2014, by = c("geo", "sex", "icd10"))

df_cvs <- left_join(df_cvs, spending, by = c("geo", "time"))

df_cvs <- left_join(df_cvs, GDPpc, by = c("geo", "time"))

df_cvs <- df_cvs %>%
  arrange(time, sex, geo) %>%
  group_by(sex, geo) %>%
  mutate(start_mort = first(mort_rate))



df_cancers <- mortality %>%
  filter(str_starts(icd10, "C") == T,
         geo %in% countries_of_interest) %>%
  group_by(time, sex, geo) %>%
  summarise(mort_rate = sum(mort_rate))

df_cancers <- left_join(df_cancers, spending, by = c("geo", "time"))

df_cancers <- left_join(df_cancers, GDPpc, by = c("geo", "time"))

df_cancers <- df_cancers %>%
  arrange(time, sex, geo) %>%
  group_by(sex, geo) %>%
  mutate(start_mort = first(mort_rate))

#Unadjusted plot
ggplot(df_cvs, aes(x = time, y = mort_rate, group = geo, colour = geo))+
  geom_line()+
  facet_grid(sex ~ .)

ggplot(df_cancers, aes(x = time, y = mort_rate, group = geo, colour = geo))+
  geom_line()+
  facet_grid(sex ~ .)

#Baseline model
model1_cancer <- lm(mort_rate ~ time + geo + sex + start_mort, df_cancers)
model1_cvs <- lm(mort_rate ~ time + geo + sex + start_mort, df_cvs)

#Adjusted for prevalence
model2_cvs <- lm(mort_rate ~ time + geo + sex + start_mort + overall_prevalence, df_cvs)

#Model adjusted for economic indicators
model3_cancer <- lm(mort_rate ~ time + geo + sex + start_mort + gdppc + spending, df_cancers)
model3_cvs <- lm(mort_rate ~ time + geo + sex + start_mort + overall_prevalence + gdppc + spending, df_cvs)


apa.reg.table(model1_cvs, model2_cvs, model3_cvs, filename = "results/Table_cvs.doc", table.number = 3)
apa.reg.table(model1_cancer, model3_cancer, filename = "results/Table_cancers.doc", table.number = 3)

