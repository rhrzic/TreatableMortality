require(tidyverse)
require(stringr)
require(ggplot2)

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

population_in <- read.csv2(file = "population.csv", sep = ",")
deaths_in <- read.csv2(file = "deaths.csv", sep = ",")

df <- left_join(deaths_in, population_in, by = c("time", "geo", "age", "sex"))

df <- left_join(df, ESP, by = c("age"))

df <- df %>%
  filter(age != "Y75-79",
         age != "Y80-84",
         age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)
  
df <- df %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T))

df$avoidable_mort_round <- round(df$avoidable_mort,1)

df <- df %>%
  group_by(sex) %>%
  mutate(rank = rank(avoidable_mort_round))

result <- df %>%
  select(geo, avoidable_mort_round, rank) %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort_round, rank))
