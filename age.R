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

population_in <- read.csv2(file = "population.csv", sep = ",") %>%
  filter(time == 2014)
deaths_in <- read.csv2(file = "deaths.csv", sep = ",") %>%
  filter(time == 2014)

df <- left_join(deaths_in, population_in, by = c("time", "geo", "age", "sex"))

df <- left_join(df, ESP, by = c("age"))

df_1 <- df %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)

df_1 <- df_1 %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T))

df_1$avoidable_mort_round_s1 <- round(df_1$avoidable_mort,1)

df_1 <- df_1 %>%
  group_by(sex) %>%
  mutate(rank_s1 = rank(avoidable_mort_round_s1)) %>%
  select(geo, sex, avoidable_mort_round_s1, rank_s1)

df_2 <- df %>%
  filter(age != "Y80-84",
         age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)

df_2 <- df_2 %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T))

df_2$avoidable_mort_round_s2 <- round(df_2$avoidable_mort,1)

df_2 <- df_2 %>%
  group_by(sex) %>%
  mutate(rank_s2 = rank(avoidable_mort_round_s2)) %>%
  select(geo, sex, avoidable_mort_round_s2, rank_s2)

df_3 <- df %>%
  filter(!(age == "Y80-84" & sex == "M")) %>%
  filter(age != "Y_GE85") %>%
  mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)

#Check
# unique(df_3[c("sex", "age")])

df_3 <- df_3 %>%
  group_by(sex, geo) %>%
  summarise(avoidable_mort = sum(ESP_death_rate, na.rm = T))

df_3$avoidable_mort_round_s3 <- round(df_3$avoidable_mort,1)

df_3 <- df_3 %>%
  group_by(sex) %>%
  mutate(rank_s3 = rank(avoidable_mort_round_s3)) %>%
  select(geo, sex, avoidable_mort_round_s3, rank_s3)

result <- left_join(df_1, df_2, by = c("sex", "geo"))

result <- left_join(result, df_3, by = c("sex", "geo"))

result <- result %>%
  pivot_wider(names_from = sex, values_from = c(avoidable_mort_round_s1, rank_s1,
                                                avoidable_mort_round_s2, rank_s2,
                                                avoidable_mort_round_s3, rank_s3))
