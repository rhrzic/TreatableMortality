rm(list=ls())

require(tidyverse)
require(eurostat)
library(readxl)
require(openxlsx)
library(corrr)

geo_include = c("AT", "BE", "BG", "CZ", "DK", "DE", "DE_TOT", "FR", "EE", "IE", "EL", "ES", "HR", "IT", "CY", "LV", 
                "LT", "LU", "HU", "MT", "NL", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK", "IS", "NO")

total_spending <- get_eurostat("hlth_sha11_hf", time_format = "num", filters = list(icha11_hf = "TOT_HF",
                                                                              unit = "PPS_HAB")) %>%
  filter(time == 2014,
         geo %in% geo_include) %>%
  select(Country = geo, total_spending = values) %>%
  mutate(total_rank = rank(total_spending))

govt_spending <- get_eurostat("hlth_sha11_hf", time_format = "num", filters = list(icha11_hf = "HF1",
                                                                                    unit = "PPS_HAB")) %>%
  filter(time == 2014,
         geo %in% geo_include) %>%
  select(Country = geo, govt_spending = values)%>%
  mutate(govt_rank = rank(govt_spending))

oop_spending <- get_eurostat("hlth_sha11_hf", time_format = "num", filters = list(icha11_hf = "HF31",
                                                                                         unit = "PPS_HAB")) %>%
  filter(time == 2014,
         geo %in% geo_include) %>%
  select(Country = geo, oop_spending = values)%>%
  mutate(oop_rank = rank(oop_spending)) %>%
  filter(!is.na(oop_rank)) %>%
  select(Country, ends_with("_rank"))

spending <- left_join(total_spending, govt_spending, by = "Country") %>%
  select(Country, ends_with("_rank"))

female_ranks <- read_xlsx(path = "data/women.xlsx", col_types = c("text", rep("numeric", 10)))
oop_female_ranks <- right_join(female_ranks, oop_spending, by = "Country")
female_ranks <- left_join(female_ranks, spending, by = "Country")
  

male_ranks <- read_xlsx(path = "data/men.xlsx", col_types = c("text", rep("numeric", 10)))
oop_male_ranks <- right_join(male_ranks, oop_spending, by = "Country")
male_ranks <- left_join(male_ranks, spending, by = "Country")


female_num <- select(female_ranks, Unadjusted:govt_rank)
female_num_oop <- select(oop_female_ranks, Unadjusted:oop_rank)

male_num <- select(male_ranks, Unadjusted:govt_rank)
male_num_oop <- select(oop_male_ranks, Unadjusted:oop_rank)


female_cor <- correlate(x = female_num, method = "spearman") %>%
  mutate(total_rank = round(total_rank, 2),
         govt_rank = round(govt_rank, 2)) %>%
  select(rowname, total_rank, govt_rank)

female_cor_oop <- correlate(x = female_num_oop, method = "spearman") %>%
  mutate(oop_rank = round(oop_rank, 2)) %>%
  select(rowname, oop_rank)

female_cor <- left_join(female_cor, female_cor_oop)

write.xlsx(female_cor, 'results/female_spending_corr.xlsx')


male_cor <- correlate(x = male_num, method = "spearman") %>%
  mutate(total_rank = round(total_rank, 2),
         govt_rank = round(govt_rank, 2)) %>%
  select(rowname, total_rank, govt_rank)

male_cor_oop <- correlate(x = male_num_oop, method = "spearman") %>%
  mutate(oop_rank = round(oop_rank, 2)) %>%
  select(rowname, oop_rank)

male_cor <- left_join(male_cor, male_cor_oop)

write.xlsx(male_cor, 'results/male_spending_corr.xlsx')

