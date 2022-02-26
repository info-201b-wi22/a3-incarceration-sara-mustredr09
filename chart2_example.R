library(ggplot2)

prison_adm_total<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na()

# Chart 2.

ggplot(data = prison_adm_total) +
  geom_point(
    mapping = aes(x = black_prison_adm, y = white_prison_adm, color = region),
    alpha = .6
  ) + 
  facet_wrap(~year)+
  scale_color_brewer(palette = "PiYG")+
  labs(title = 'Average Black vs white incarceration during the Obama administration',
       subtitle = 'by U.S. geographic region',
       x = 'Average admissions of Black prisoners',
       y = 'Average admissions of white prisoners',
       fill = 'region')

