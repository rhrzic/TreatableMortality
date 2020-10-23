require(eurostat)
require(tidyverse)
require(sf)

male_ranks_overall$Sex = "Male"
female_ranks_overall$Sex = "Female"

gisco0 <- get_eurostat_geospatial(output_class = "sf", resolution = "03", nuts_level = "0", year = "2016", cache = TRUE)

per_country <- rbind(select(male_ranks_overall, Country, Sex, MAD),
                     select(female_ranks_overall, Country, Sex, MAD)) %>%
  filter(str_length(Country) == 2)

map <- left_join(per_country, gisco0, by = c("Country" = "id"))

map1 <- ggplot(map) + 
  geom_sf(aes(fill = MAD, geometry = geometry), colour = "transparent") +
  scale_fill_gradient2(low = "white", mid = "gainsboro", high = "black")+
  coord_sf(xlim = c(-10, +30), ylim = c(35, 70)) +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(fill = "Mean absolute\ndifference\nin rank")+
  facet_grid(. ~ Sex)

ggsave('plots/m1.png', map1, scale = 2, width = 5, height = 2, units = "in")
