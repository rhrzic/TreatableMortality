p1 <- readRDS("results/unadjusted_plot.rds")

plot <- result_total %>%
  filter(str_length(geo) == 2) %>%
  mutate(median_F = median(case_fatality_100k_F),
         rel_dev_F = case_fatality_100k_F/median_F,
         median_M = median(case_fatality_100k_M),
         rel_dev_M = case_fatality_100k_M/median_M) %>%
  select(geo, rel_dev_F, rel_dev_M) %>%
  pivot_longer(rel_dev_F:rel_dev_M) %>%
  mutate(Sex = ifelse(name == "rel_dev_F", "Female", "Male"))

p2 <- ggplot() +
  geom_point(data = p1, aes(x = factor(geo, levels = rev(levels(factor(geo)))), y = value), stat = "identity", fill = "black", size = 3)+
  geom_segment(data = p1, aes(y = 1, x = geo, yend = value, xend = geo), colour = "black")+
  geom_point(data = plot, aes(x = factor(geo, levels = rev(levels(factor(geo)))), y = value), stat = "identity", fill = "blue", size = 6)+
  geom_segment(data = plot, aes(y = 1, x = geo, yend = value, xend = geo), colour = "black")+
  geom_text(data = plot, aes(x = factor(geo, levels = rev(levels(factor(geo)))), y = value, label = round(value,2)), color="white", size=2) +
  scale_y_continuous(trans='log2') +
  coord_flip()+
  facet_grid(. ~ Sex)+
  ylab("Relative deviation from median")+
  xlab("")+
  theme_bw()

ggsave('plots/p2.png', p2, scale = 2, width = 4, height = 3, units = "in")
