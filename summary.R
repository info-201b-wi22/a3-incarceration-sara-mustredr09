incarceration_df<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library('tidyverse')

# 1a. Total admissions for Black and white populations.
prison_adm_total<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na()

# 1b. An average of admissions for Black and white populations.
avg_total<-incarceration_df %>% 
  group_by(year, region) %>% 
  filter(year > 2008) %>%
  select(year, state, region, county_name, black_prison_adm, black_male_prison_adm,black_female_prison_adm, white_prison_adm,
         white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>%
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),.groups = 'drop')
            
# 2a. Total Black admissions per year, with gender. This is during the Obama administration.
total_black_adm<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year, region, state, county_name, black_prison_adm, black_male_prison_adm, black_female_prison_adm) %>% 
  drop_na()

# 2b. Total white admissions per year, with gender. This is during the Obama administration.
total_white_adm<-incarceration_df %>% 
  group_by(year) %>% 
  filter(year > 2008) %>%
  select(year, region, state, county_name, white_prison_adm, white_male_prison_adm, white_female_prison_adm) %>% 
  drop_na()

# 3a. Average Black admissions per year, with gender. 
#This is during the Obama administration.
avg_black_adm<-incarceration_df %>% 
  group_by(year, state, region) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, county_name, black_prison_adm, black_male_prison_adm, black_female_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_black_female_adm = mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm = mean(black_male_prison_adm, na.rm=TRUE),.groups = 'drop')

# 3b. Average white admissions per year, with gender. 
#This is during the Obama administration.
avg_white_adm<-incarceration_df %>% 
  group_by(year, state, region) %>% 
  filter(year > 2008) %>% 
  select(year, state, region, county_name, white_prison_adm, white_male_prison_adm, white_female_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_white_adm = mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm = mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm = mean(white_male_prison_adm, na.rm=TRUE),.groups = 'drop')

View(avg_black_adm)

# 4a. General incarceration data from  each region during 
# the Obama administration.

northeast_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"Northeast")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  group_by(state) 

south_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"South")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  group_by(state)

west_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"West")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  group_by(state)

midwest_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"Midwest")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  group_by(state)

#4b. Average incarceration in each region during the Obama administration. 

avg_northeast_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"Northeast")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),.groups = 'drop')

avg_south_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"South")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),.groups = 'drop')

avg_west_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"West")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),.groups = 'drop')

avg_midwest_adm<-incarceration_df %>% 
  filter(str_detect(incarceration_df$region,"Midwest")) %>% 
  filter(year > 2008) %>% 
  select(year, region, state, black_prison_adm, black_male_prison_adm,black_female_prison_adm, 
         white_prison_adm,white_female_prison_adm,white_male_prison_adm) %>% 
  drop_na() %>% 
  summarize(avg_black_adm = mean(black_prison_adm, na.rm=TRUE),
            avg_black_female_adm=mean(black_female_prison_adm, na.rm=TRUE),
            avg_black_male_adm=mean(black_male_prison_adm, na.rm=TRUE),
            avg_white_adm=mean(white_prison_adm, na.rm=TRUE),
            avg_white_female_adm=mean(white_female_prison_adm, na.rm=TRUE),
            avg_white_male_adm=mean(white_male_prison_adm, na.rm=TRUE),.groups = 'drop')

# In the first section, I analyze total prison admissions for both Black and white populations, and I also highlight differences in gender (1a). 
#I also analyze the average prison admissions for the abovementioned populations, but I focus on the different geographical regions, 
#such as the Midwest, West, Northeast, and  the South (1b).

#In the second section, I focus on total admissions per year, dividing them between Black (2a) and white (2b) populations. California 
#had some of the highest admission counts for both Black and white populations, and I assume that is because of the population density. 
#Other counties that had high counts of incarceration for both populations had big cities in them and were located in the South and West areas. 
#This might have to do with more diverse populations and unequal wealth distributions within large urban settings.

#In the third section, I calculate the abovementioned values‘ averages. While California leads in both, states with higher 
#Black populations—such as  Maryland—remain towards the top (3a). States with higher white populations, such as Rhode Island, 
#which is almost 90% white, have the highest prison admissions (3b).

#In the fourth section, I analyze Black and white admission counts by region. In total counts, States that have high Black 
#populations generally top the charts. However, I was surprised with southern statistics, as states with higher Black populations, 
#such as Mississippi, Louisiana and Georgia, do not have such high counts (4a). Averages, however, reflect many of the observations I made in the graphs (4b).