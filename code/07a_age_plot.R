p1 <- readRDS("results/unadjusted_plot.rds")

plot <- df_higherlimit_total %>%
  filter(str_length(geo) == 2) %>%
  mutate(median_F = median(avoidable_mort_higherlimit_F),
         rel_dev_F = avoidable_mort_higherlimit_F/median_F,
         median_M = median(avoidable_mort_higherlimit_M),
         rel_dev_M = avoidable_mort_higherlimit_M/median_M) %>%
  select(geo, rel_dev_F, rel_dev_M) %>%
  pivot_longer(rel_dev_F:rel_dev_M) %>%
  mutate(Sex = ifelse(name == "rel_dev_F", "Female", "Male"))

p6 <- ggplot() +
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

ggsave('plots/p6.png', p6, scale = 2, width = 4, height = 3, units = "in")




plot <- df_nolimit_total %>%
  filter(str_length(geo) == 2) %>%
  mutate(median_F = median(avoidable_mort_nolimit_F),
         rel_dev_F = avoidable_mort_nolimit_F/median_F,
         median_M = median(avoidable_mort_nolimit_M),
         rel_dev_M = avoidable_mort_nolimit_M/median_M) %>%
  select(geo, rel_dev_F, rel_dev_M) %>%
  pivot_longer(rel_dev_F:rel_dev_M) %>%
  mutate(Sex = ifelse(name == "rel_dev_F", "Female", "Male"))

p7 <- ggplot() +
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

ggsave('plots/p7.png', p7, scale = 2, width = 4, height = 3, units = "in")






plot <- df_sexlimit_total %>%
  filter(str_length(geo) == 2) %>%
  mutate(median_F = median(avoidable_mort_sexlimit_F),
         rel_dev_F = avoidable_mort_sexlimit_F/median_F,
         median_M = median(avoidable_mort_sexlimit_M),
         rel_dev_M = avoidable_mort_sexlimit_M/median_M) %>%
  select(geo, rel_dev_F, rel_dev_M) %>%
  pivot_longer(rel_dev_F:rel_dev_M) %>%
  mutate(Sex = ifelse(name == "rel_dev_F", "Female", "Male"))

p8 <- ggplot() +
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

ggsave('plots/p8.png', p8, scale = 2, width = 4, height = 3, units = "in")