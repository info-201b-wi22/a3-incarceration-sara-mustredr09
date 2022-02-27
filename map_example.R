incarceration_df<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# I‘m gonna write my steps down because it helps me understand what I’m doing. 

# Load state shape data.
state_shape <- map_data('state')

# Load state abbrevs.
state_abbrevs <- data.frame(state.abb, state.name)

#Load dataframe i’ll be working with.

total_black_adm<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year,region, state, black_prison_adm) %>% 
  drop_na()

#THIS IS THE GOOD ONE
sum_total_black_adm <- total_black_adm %>% 
  group_by(state) %>% 
  summarize(state_total=sum(black_prison_adm))

# -------------------------------------------------------------------------------------------

# Join `np_state_data` and `state_abbrevs`
# Hint: by=c('left_column' = 'right_column')
sum_total_black_adm <- left_join(sum_total_black_adm, state_abbrevs, by=c('state' = 'state.abb'))

# Add a new column to the `sum_total_black_adm` dataframe called "state2" that includes a lowercase version of the state name
# Hint: don't forget tolower()!
sum_total_black_adm <- sum_total_black_adm %>% 
  mutate(state.name=tolower(state.name))

# Join the two dataframes

state_shape_2<- left_join(sum_total_black_adm, state_shape, by=c('state.name'='region')) 

ggplot(state_shape_2)+
  geom_polygon(mapping=aes(x=long, y=lat, group=group, fill=state_total))+
  coord_map()+
  labs(title = 'Average admissions of Black prisoners during the Obama administration', fill = 'state_total')+
  scale_fill_continuous(low='grey', high='black',labels=scales::label_number_si())
