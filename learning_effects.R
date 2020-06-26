require(tidyverse)
require(stringr)
require(ggplot2)

ehis_prevalence_in <- read.csv2(file = "ehis_prevalence.csv", sep = ",")

top_decile <- ehis_prevalence_in %>%
  mutate(values = as.numeric(values)) %>%
  group_by(sex) %>%
  mutate(cutoff = quantile(values, prob = 0.99)) %>%
  mutate(top= ifelse(values > cutoff, 1, 0)) %>%
  select(hlth_pb, sex, age, geo, values, cutoff, top) %>%
  filter(top == 1)

countries_of_interest <- c("HU", "BG", "LT", "DE", "FI")
diseases_of_interest <- c("PB2413", "I60-I69")

deaths_in <- read.csv2(file = "deaths.csv", sep = ",") %>%
  filter(time %in% 2011:2017,
         age %in% c("Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y_GE85"),
         icd10 %in% diseases_of_interest,
         geo %in% countries_of_interest) %>%
  mutate(age_group = case_when(age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                               TRUE ~ "Y_GE75")) %>%
  group_by(age_group, sex, time, geo, icd10) %>%
  summarise(deaths = sum(deaths))

population_in <- read.csv2(file = "population.csv", sep = ",") %>%
  filter(time %in% 2011:2017,
         age %in% c("Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y_GE85"),
         geo %in% countries_of_interest)  %>%
  mutate(age_group = case_when(age %in% c("Y65-69", "Y70-74") ~ "Y65-74",
                               TRUE ~ "Y_GE75")) %>%
  group_by(age_group, sex, time, geo) %>%
  summarise(population = sum(population))

ehis_prevalence_in <- read.csv2(file = "ehis_prevalence.csv", sep = ",") %>%
  filter(age %in% c("Y65-74", "Y_GE75"),
         hlth_pb %in% diseases_of_interest,
         geo %in% countries_of_interest) %>%
  mutate(prevalence = as.numeric(values))

ESP <- data.frame(age_group = c("Y65-74",
                                "Y_GE75"), ESP = c(10500,
                                                   9000))

df <- left_join(deaths_in, population_in, by = c("time", "geo", "age_group", "sex"))

df <- left_join(df, ESP, by = c("age_group"))

df <- df %>% mutate(death_rate = deaths / population,
         ESP_death_rate = death_rate * ESP)

df <- left_join(df, ehis_prevalence_in, by = c("geo", "age_group" = "age", "sex"))


GDPpc <- get_eurostat("nama_10_pc", time_format = "num", filters = list(na_item = "B1GQ",
                                                                        unit = "CP_PPS_HAB")) %>%
  filter(time %in% 2011:2017,
         geo %in% countries_of_interest) %>%
  mutate(gdppc = values)

spending <- get_eurostat("hlth_sha11_hf", time_format = "num", filters = list(icha11_hf = "TOT_HF",
                                                                        unit = "PPS_HAB")) %>%
  filter(time %in% 2011:2017,
         geo %in% countries_of_interest) %>%
  mutate(spending = values)

df <- left_join(df, GDPpc, by = c("time.x" = "time", "geo"))
df <- left_join(df, spending, by = c("time.x" = "time", "geo"))


#Unadjusted plot
ggplot(df, aes(x = time.x, y = ESP_death_rate, group = geo))+
  geom_line()+
  facet_grid(sex ~ age_group)

#Unadjusted model
model1 <- lm(ESP_death_rate ~ time.x + geo + sex + age_group, df)

#Adjusted for prevalence in 2014
model2 <- lm(ESP_death_rate ~ time.x + geo + sex + age_group + prevalence, df)

#Model adjusted for economic indicators
model3 <- lm(ESP_death_rate ~ time.x + geo + sex + age_group + prevalence + gdppc + spending, df)
