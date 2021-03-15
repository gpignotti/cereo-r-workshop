# explore the bike data characteristics
# 
# explore the bike data to look at relationship b/n temperature
# and number of riders
#
# step 1: load bike data and look at metadata

library(tidyverse)

### read in data ----
bike = read_csv("data/daily_bike_data.csv")

head(bike)
str(bike)
sort(names(bike))

# time trend of ridership
ggplot(data=bike) +
  geom_line(aes(x=dteday, y=cnt))

ggplot(data=bike) +
  geom_point(aes(x=temp, y=cnt))

### Data cleaning ----
# dplyr verbs for data transformation
# select: select columns that we want to keep
# filter: select rows that we want to keep
# mutate: transforms data while keeping other columns
# transmuste: create new columns does not keep old columns
# %>%: "pipe" - sends output from one command as input for next command

# e.g.
bike %>% 
  select(dteday, season, weathersit, temp, cnt)

# one way to select spring records
spring_bike_temp_cnt = bike %>%
  filter(season=="spring") %>%
  select(temp,cnt)

spring_bike_temp_cnt2 = bike %>%
  filter(season=="winter") %>%
  select(weathersit, cnt)

### mutate and transmust w/ factors and dates
summary(bike$weathersit)
unique(bike$weathersit)

bike2 = bike %>% 
  mutate(weather_fac = factor(weathersit, 
                         levels=c(1,2,3,4),
                         labels=c("Clear","Cloudy","Rainy","Heavy Rain")))

bike2 %>%
  select(dteday, weathersit, weather_fac)

### converting from and to dates
bike_dates = bike %>%
  transmute(instant=instant, date=dteday, date_num=as.numeric(dteday), date_char=as.character(dteday))

bike_dates %>%
  transmute(instant=instant, date=date, 
            date_num=as.Date(date_num, origin="1970-01-01"), 
            date_char=as.Date(date_char))

### additional filtering and selecting
bike %>% 
  select(dteday, cnt)

bike %>%
  select(dteday, cnt, temp) %>%
  select(-temp)

keep_vars = c("dteday", "cnt", "temp")
keep_vars = paste0("temp", 1:12)
bike %>%
  select(all_of(keep_vars))

#filter
bike %>%
  filter(season=="spring")

bike %>%
  filter(season!="spring") %>%
  select(season) %>%
  distinct()

# | = OR
bike %>%
  filter(season=="summer" | season=="winter")

seasons=c("summer", "winter")
bike %>%
  filter(season %in% seasons)

### more dplyr verbs
#summarize: summary of multiple rows for a column/variable 
#group_by: perform operation separately for each group
bike2 %>%
  filter(season=="summer") %>%
  summarize(temp_mean = mean(temp),
            cnt_mean = mean(cnt),
            cnt_sum = sum(cnt))

bike2 %>%
  group_by(season) %>%
  summarize(temp_mean=mean(temp),
            ride_sum=sum(cnt))

# what are the season definitions?
sort(names(bike))
bike %>%
  select(season, mnth) %>%
  distinct()

#seasons messed up, create new season w/ correct labels
bike3 = bike2 %>%
  mutate(season2= 1*(mnth %in% c("December","January","February")) +
           2 * (mnth %in% c("March","April","May")) + 
           3 * (mnth %in% c("June","July","August")) +
           4 * (mnth %in% c("September","October","November"))) %>%
  mutate(season2 = factor(season2, levels=0:4,
         labels=c("Unknown","Winter","Spring","Summer","Fall")))

bike3 %>%
  group_by(season2) %>%
  summarize(temp_mean=mean(temp),
            ride_sum=sum(cnt))

### facetting
ggplot(data=bike3) +
  geom_point(aes(x=temp, y=cnt)) +
  geom_smooth(aes(x=temp, y=cnt), method="lm", formula=y~poly(x,2)) +
  facet_wrap(~season2)

### pivoting wide to long, and long to wide
# long to wide: data in mult columns, classified
# wide to long: data in one column, classifier in other
# tidyr allows transformations
months=c("January","February","March","April","May","June","July","August","September","October","November","December")
tidybike = bike3 %>%
  select(yr, mnth, temp, cnt) %>%
  mutate(month=factor(mnth, levels=months, labels=months)) %>%
  group_by(yr, month) %>% 
  summarize(temp_mean=mean(temp),
            rides=sum(cnt))
  
#tidyr functions for long to wide
#spread
#pivot_wider
tidybike %>% 
  select(-rides) %>%
  pivot_wider(values_from=temp_mean,
                         names_from=month,
                         names_prefix="temp_")
tidybike %>%
  select(-rides) %>%
  spread(key = month, value = temp_mean)


rides = tidybike %>%
  select(-temp_mean) %>%
  pivot_wider(values_from=rides,
              names_from=month,
              names_prefix="rides_") %>%
  rename_with(tolower) %>%
  rename(year=yr)

# wide to long
#pivot_longer
rides %>%
  gather(key="month", value="rides", -year)

rides %>%
  select(year, rides_january, rides_february) %>%
  pivot_longer(names_to="month", values_to="rides", cols=c("rides_january","rides_february")) %>%
  mutate(month=substr(month, 7, nchar(month))) %>%
  mutate(month=toupper(month))



  
  


