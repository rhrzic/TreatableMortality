require(tidyverse)
require(stringr)
require(ggplot2)

female_ranks <- read.csv2(file = "Data/women.csv", sep = ";")

female_ranks <- female_ranks %>%
  mutate(diff_prevalence = Prevalence.adjusted - Unadjusted,
         diff_lagged = Lagged.prevalence - Unadjusted,
         diff_DALY = Treatable.DALYs - Unadjusted,
         diff_YLD = Treatable.YLDs - Unadjusted,
         diff_YLL = Treatable.YLLs - Unadjusted,
         diff_no_age = No.age.limits - Unadjusted,
         diff_high_age = as.numeric(Higher.age) - Unadjusted,
         diff_sex_age = Sex.specific.age - Unadjusted) %>%
  select(Country, Unadjusted, diff_prevalence:diff_sex_age)

female_per_country <- female_ranks %>%
  select(starts_with("diff")) %>%
  mutate(MAD = round(rowSums(abs(.))/8,1))

female_ranks_summary <- female_ranks %>%
  summarise(across(starts_with("diff"), ~sum(abs(.x))/30))

male_ranks <- read.csv2(file = "Data/men.csv", sep = ";")

male_ranks <- male_ranks %>%
  mutate(diff_prevalence = Prevalence.adjusted - Unadjusted,
         diff_lagged = Lagged.prevalence - Unadjusted,
         diff_DALY = Treatable.DALYs - Unadjusted,
         diff_YLD = Treatable.YLDs - Unadjusted,
         diff_YLL = Treatable.YLLs - Unadjusted,
         diff_no_age = No.age.limits - Unadjusted,
         diff_high_age = as.numeric(Higher.age) - Unadjusted,
         diff_sex_age = Sex.specific.age - Unadjusted) %>%
  select(Country, Unadjusted, diff_prevalence:diff_sex_age)

male_per_country <- male_ranks %>%
  select(starts_with("diff")) %>%
  mutate(MAD = round(rowSums(abs(.))/8,1))

male_ranks_summary <- male_ranks %>%
  summarise(across(starts_with("diff"), ~sum(abs(.x))/30))

            