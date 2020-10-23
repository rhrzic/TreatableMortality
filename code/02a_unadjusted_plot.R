plot <- result_total %>%
  filter(str_length(geo) == 2) %>%
  mutate(median_F = median(avoidable_mort_F),
         rel_dev_F = avoidable_mort_F/median_F,
         median_M = median(avoidable_mort_M),
         rel_dev_M = avoidable_mort_M/median_M) %>%
  select(geo,rel_dev_F, rel_dev_M) %>%
  pivot_longer(rel_dev_F:rel_dev_M) %>%
  mutate(Sex = ifelse(name == "rel_dev_F", "Female", "Male"))

saveRDS(plot, file = "results/unadjusted_plot.rds")

ggsave('plots/p1.png', p1, scale = 2, width = 4, height = 3, units = "in")