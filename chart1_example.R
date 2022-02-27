library(dplyr)
library(ggplot2)
incarceration_df<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

avg_black_adm<-incarceration_df %>% 
  group_by(year, state, region) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, county_name, black_prison_adm, black_male_prison_adm, black_female_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),.groups = 'drop')

custom_colors<- c("#8DCE67", "#6DAB6C", "#73B882", "#64AC88", "#71C9C3")

ggplot(avg_black_adm) +
  geom_col(mapping = aes(x = year, y = avg_black_adm, fill = region), position = "dodge")+
  labs(title = 'Average Black incarceration during the Obama Administration',
       subtitle = 'by U.S. geographic region',
       x = 'Year',
       y = 'Average Black prisoner admissions',
       fill = 'Region')+
  scale_fill_manual(values = custom_colors)
