rm(list=ls())

require(tidyverse)
library(readxl)
require(openxlsx)
library(corrr)

female_ranks <- read_xlsx(path = "data/women.xlsx", col_types = c("text", rep("numeric", 11)))

male_ranks <- read_xlsx(path = "data/men.xlsx", col_types = c("text", rep("numeric", 11)))

female_num <- select(female_ranks, Unadjusted:Treatable_YLLs_nolimit)

male_num <- select(male_ranks, Unadjusted:Treatable_YLLs_nolimit)


female_cor <- correlate(x = female_num, method = "spearman") %>%
  select(rowname:Unadjusted) %>%
  mutate(Unadjusted_female = round(Unadjusted, 2))

male_cor <- correlate(x = male_num, method = "spearman") %>%
  select(rowname:Unadjusted) %>%
  mutate(Unadjusted_male = round(Unadjusted, 2))

corr_table <- bind_cols(female_cor, male_cor) %>%
  select(rowname...1, Unadjusted_female, Unadjusted_male)

write.xlsx(corr_table, 'results/corr_table.xlsx')

female_ranks_diff <- female_ranks %>%
  mutate(diff_prevalence = Prevalence_adjusted - Unadjusted,
         diff_lagged = Lagged_prevalence - Unadjusted,
         diff_DALY = Treatable_DALYs - Unadjusted,
         diff_YLD = Treatable_YLDs - Unadjusted,
         diff_YLL = Treatable_YLLs - Unadjusted,
         diff_no_age = No_age_limits - Unadjusted,
         diff_high_age = as.numeric(Higher_age) - Unadjusted,
         diff_sex_age = Sex_specific_age - Unadjusted,
         diff_YLL_85 = Treatable_YLLs_under_85 - Unadjusted,
         diff_YLL_nolimit = Treatable_YLLs_nolimit - Unadjusted) %>%
  select(Country, Unadjusted, diff_prevalence:diff_YLL_nolimit)

female_per_country <- female_ranks_diff %>%
  select(starts_with("diff")) %>%
  mutate(MAD = round(rowSums(abs(.))/(ncol(.)),1))

female_ranks_summary <- female_ranks_diff %>%
  summarise(across(starts_with("diff"), ~round(sum(abs(.x))/30, 1)))

female_ranks_overall <- cbind(rbind(female_ranks[,1:2], c("MAD", "-")), bind_rows(female_per_country, female_ranks_summary))

write.xlsx(female_ranks_overall, 'results/female_ranks_summary.xlsx')


##MALE



male_ranks_diff <- male_ranks %>%
  mutate(diff_prevalence = Prevalence_adjusted - Unadjusted,
         diff_lagged = Lagged_prevalence - Unadjusted,
         diff_DALY = Treatable_DALYs - Unadjusted,
         diff_YLD = Treatable_YLDs - Unadjusted,
         diff_YLL = Treatable_YLLs - Unadjusted,
         diff_no_age = No_age_limits - Unadjusted,
         diff_high_age = as.numeric(Higher_age) - Unadjusted,
         diff_sex_age = Sex_specific_age - Unadjusted,
         diff_YLL_80 = Treatable_YLLs_under_80 - Unadjusted,
         diff_YLL_nolimit = Treatable_YLLs_nolimit - Unadjusted) %>%
  select(Country, Unadjusted, diff_prevalence:diff_YLL_nolimit)

male_per_country <- male_ranks_diff %>%
  select(starts_with("diff")) %>%
  mutate(MAD = round(rowSums(abs(.))/(ncol(.)),1))

male_ranks_summary <- male_ranks_diff %>%
  summarise(across(starts_with("diff"), ~round(sum(abs(.x))/30, 1)))

male_ranks_overall <- cbind(rbind(male_ranks[,1:2], c("MAD", "-")), bind_rows(male_per_country, male_ranks_summary))

write.xlsx(male_ranks_overall, 'results/male_ranks_summary.xlsx')