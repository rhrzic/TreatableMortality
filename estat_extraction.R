require(eurostat)
require(tidyverse)
require(stringr)
require(ggplot2)

geo_include = c("AT", "BE", "BG", "CZ", "DK", "DE", "DE_TOT", "FR", "EE", "IE", "EL", "ES", "HR", "IT", "CY", "LV", 
                "LT", "LU", "HU", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK", "IS", "NO")

## Deaths ##

deaths <- get_eurostat("hlth_cd_aro", time_format = "num")

deaths <- deaths %>%
  filter(sex %in% c("M", "F"),
         geo %in% geo_include,
         resid == "TOT_IN",
         !(age %in% c("TOTAL", "Y_LT15", "Y_LT25", "Y_LT65", "Y_GE65", "Y85-89", "Y90-94", "Y_GE95",
                      "Y15-24"))) %>%
  mutate(age = replace(age, age == "Y1-4", "Y01-04"),
         age = replace(age, age == "Y5-9", "Y05-09"),
         age = replace(age, age == "Y_LT1", "Y00")) 

causes_of_interest <- c("A15-A19_B90", "B15-B19_B942", "B180-B182", "B20-B24",
                        "C00-C14", "C15", "C16", "C18-C21", "C22", "C33_C34", 
                        "C43", "C50", "C53", "C54_C55", "C67", "C73", "C81-C86",
                        "C91-C95", "E10-E14", "I20-I25", "I60-I69", "J09-J11",
                        "J12-J18", "J40-J44_J47", "J45_J46", "K25-K28", "K70_K73_K74",
                        "N00-N29", "O", "P", "V01-Y89", "V01-Y89_OTH", "X60-X84_Y870", 
                        "X85-Y09_Y871", "Y10-Y34_Y872")

deaths2014 <- deaths %>% 
  filter(icd10 %in% causes_of_interest & time == 2014) %>%
  mutate(deaths = values) %>%
  select(sex, age, icd10, geo, time, deaths)

deaths2017 <- deaths %>% 
  filter(icd10 %in% causes_of_interest & time == 2017) %>%
  mutate(deaths = values) %>%
  select(sex, age, icd10, geo, time, deaths)

write.csv(deaths2014, "deaths2014.csv", row.names=F)

write.csv(deaths2014, "deaths.csv", row.names=F)

write.csv(deaths2017, "deaths2017.csv", row.names=F)

deaths %>%
  group_by(icd10, age, sex, geo) %>%
  summarise(missing = sum(is.na(values)) / n()) %>%
  ggplot(aes(x = age, y = icd10, fill = missing))+
  geom_tile()+
  facet_grid(sex ~ geo) +
  theme_bw()

## Prevalence ##

ehis_prevalence <- get_eurostat("hlth_ehis_cd1c", time_format = "num")

conditions_of_interest <- c("PB2311", "PB2312", "PB2412", "PB2413", "PB2421", "PB2423", "PB2430")

ehis_prevalence <- ehis_prevalence %>%
  filter(sex %in% c("M", "F"),
         citizen == "NAT",
         geo %in% geo_include,
         !(age %in% c("TOTAL", "Y15-29", "Y15-44", "Y15-64", "Y25-64", "Y45-64", "Y_GE65")),
         hlth_pb %in% conditions_of_interest)

write.csv(ehis_prevalence, "ehis_prevalence.csv", row.names=F)


## Population ##

population <- get_eurostat("demo_pjangroup", time_format = "num", filters = list(time = 2014,
                                                                                 sex = c("M", "F"),
                                                                                 geo = geo_include))
population <- population %>%
  filter(!(age %in% c("TOTAL", "Y_GE75", "Y_GE80", "UNK", "Y_LT5"))) %>%
  mutate(age = replace(age, age == "Y5-9", "Y05-09"))

population_LT1 <- get_eurostat("demo_pjan", time_format = "num", filters = list(age = "Y_LT1",
                                                                                sex = c("M", "F"),
                                                                                geo = geo_include,
                                                                                time = 2014))

population_LT1 <- population_LT1 %>%
  mutate(age = replace(age, age == "Y_LT1", "Y00"))


population_1_4 <- get_eurostat("demo_pjan", time_format = "num", filters = list(age = c("Y1", "Y2", "Y3", "Y4"),
                                                                                sex = c("M", "F"),
                                                                                geo = geo_include,
                                                                                time = 2014))

population_1_4 <- population_1_4 %>%
  pivot_wider(names_from = age, values_from = values) %>%
  mutate(age = "Y01-04",
         values = Y1+Y2+Y3+Y4) %>%
  select(unit, age, sex, geo, time, values)

population <- rbind(population_LT1, population_1_4, population)

age_groups <- unique(deaths_in$age)

population <- population %>%
  mutate(population = values,
         geo = replace(geo, geo == "DE_TOT", "DE")) %>%
  filter(age %in% age_groups) %>%
  select(sex, age, geo, time, population)

write.csv(population, "population.csv", row.names=F)


## 
