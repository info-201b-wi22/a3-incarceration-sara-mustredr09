library(ggplot2)
library(dplyr)
library(tidyr)

incarceration_df<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

prison_adm_total<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na()

# Chart 2.

ggplot(data = prison_adm_total) +
  geom_point(
    mapping = aes(x = white_prison_adm, y = black_prison_adm, color = region),
    alpha = .6
  ) + 
  facet_wrap(~year)+
  scale_color_brewer(palette = "PiYG")+
  labs(title = 'Total Black vs white incarceration during the Obama administration',
       subtitle = 'by U.S. geographic region',
       x = 'Total admissions of white prisoners',
       y = 'Total admissions of Black prisoners',
       fill = 'region')


