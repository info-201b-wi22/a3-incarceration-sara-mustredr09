x<-year
y<-avg_black_adm

avg_total<-incarceration_df %>% 
  group_by(year, region) %>% 
  filter(year > 2008) %>%
  select(year, location, state, region, county_name, black_prison_adm, black_male_prison_adm,black_female_prison_adm, white_prison_adm,
         white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),
  )

ggplot(avg_total) +
  geom_col(mapping = aes(x = year, y = avg_black_adm, fill = region), position = "dodge")+
  labs(title = 'Average Black incarceration during the Obama Administration',
       subtitle = 'by U.S. geographic region',
       x = 'Year',
       y = 'Average Black prisoner admissions',
       fill = 'Region')+
  scale_fill_manual(values = custom_colors)

RColorBrewer::display.brewer.all()

custom_colors<- c("#8DCE67", "#6DAB6C", "#73B882", "#64AC88", "#71C9C3")



