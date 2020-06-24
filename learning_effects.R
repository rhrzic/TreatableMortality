require(tidyverse)
require(stringr)
require(ggplot2)

ehis_prevalence_in <- read.csv2(file = "ehis_prevalence.csv", sep = ",")

top_centiles <- ehis_prevalence_in %>%
  mutate(values = as.numeric(values)) %>%
  group_by(sex) %>%
  mutate(cutoff = quantile(values, prob = 0.99)) %>%
  mutate(top= ifelse(values > cutoff, 1, 0)) %>%
  select(hlth_pb, sex, age, geo, values, cutoff, top) %>%
  filter(top == 1)


