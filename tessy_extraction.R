require(tidyverse)
require(stringr)
require(ggplot2)

tuberculosis <- read.csv2(file = "Data/ECDC_surveillance_data_Tuberculosis.csv", sep = ",")

tuberculosis <- tuberculosis %>%
  filter(Time == 2014)

