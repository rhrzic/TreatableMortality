male_ranks$Sex <- "Male"
female_ranks$Sex <- "Female"

Prevalence_adjusted <- rbind(select(male_ranks, Country:Prevalence_adjusted, Sex), 
                                select(female_ranks, Country:Prevalence_adjusted, Sex))

r2 <- ggplot(Prevalence_adjusted) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Prevalence_adjusted), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Prevalence_adjusted, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank())

ggsave('plots/r2.png', r2, scale = 2, width = 4, height = 3, units = "in")


Lagged_prevalence <- rbind(select(male_ranks, Country:Lagged_prevalence, Sex), 
                             select(female_ranks, Country:Lagged_prevalence, Sex))

r3 <- ggplot(Lagged_prevalence) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Lagged_prevalence), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Lagged_prevalence, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r3.png', r3, scale = 2, width = 4, height = 3, units = "in")


Treatable_DALYs <- rbind(select(male_ranks, Country:Treatable_DALYs, Sex), 
                           select(female_ranks, Country:Treatable_DALYs, Sex))

r4 <- ggplot(Treatable_DALYs) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Treatable_DALYs), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Treatable_DALYs, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r4.png', r4, scale = 2, width = 4, height = 3, units = "in")



Treatable_YLDs <- rbind(select(male_ranks, Country:Treatable_YLDs, Sex), 
                         select(female_ranks, Country:Treatable_YLDs, Sex))

r5 <- ggplot(Treatable_YLDs) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Treatable_YLDs), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Treatable_YLDs, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r5.png', r5, scale = 2, width = 4, height = 3, units = "in")


Treatable_YLLs <- rbind(select(male_ranks, Country:Treatable_YLLs, Sex), 
                        select(female_ranks, Country:Treatable_YLLs, Sex))

r6 <- ggplot(Treatable_YLLs) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Treatable_YLLs), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Treatable_YLLs, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r6.png', r6, scale = 2, width = 4, height = 3, units = "in")



No_age_limits <- rbind(select(male_ranks, Country:No_age_limits, Sex), 
                        select(female_ranks, Country:No_age_limits, Sex))

r7 <- ggplot(No_age_limits) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=No_age_limits), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = No_age_limits, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r7.png', r7, scale = 2, width = 4, height = 3, units = "in")



Higher_age <- rbind(select(male_ranks, Country:Higher_age, Sex), 
                       select(female_ranks, Country:Higher_age, Sex))

r8 <- ggplot(Higher_age) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Higher_age), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Higher_age, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r8.png', r8, scale = 2, width = 4, height = 3, units = "in")



Sex_specific_age <- rbind(select(male_ranks, Country:Sex_specific_age, Sex), 
                    select(female_ranks, Country:Sex_specific_age, Sex))

r9 <- ggplot(Sex_specific_age) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Sex_specific_age), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Sex_specific_age, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r9.png', r9, scale = 2, width = 4, height = 3, units = "in")



Treatable_YLLs_nolimit <- rbind(select(male_ranks, Country, Unadjusted, Treatable_YLLs_nolimit, Sex), 
                          select(female_ranks, Country, Unadjusted, Treatable_YLLs_nolimit, Sex))

r10 <- ggplot(Treatable_YLLs_nolimit) + 
  geom_text(aes(x = 1, y = Unadjusted, label = Country))+
  geom_segment(aes(x=1.1, xend=1.9, y=Unadjusted, yend=Treatable_YLLs_nolimit), size=.5, show.legend=F)+
  geom_text(aes(x = 2, y = Treatable_YLLs_nolimit, label = Country))+
  labs(x="Effect of adjustment on rank", y="Countries ordered highest to lowest")+
  facet_grid(.~Sex)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank())

ggsave('plots/r10.png', r9, scale = 2, width = 4, height = 3, units = "in")