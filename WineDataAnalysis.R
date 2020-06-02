# This is an exercise I did to learn more about dplyr. All the data are download from kaggle.com

# I set my work directory
setwd("C:/My Drive/SummerInternSorensonImpact2020/Github/Exercise")

# Loads some necessary packages and reads the data set.

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

wine = read.csv('wine.csv',stringsAsFactors = FALSE, encoding = "UTF-8")
View(wine)

# Tidy up the unnecessary columns by deleting no.1 and 3 columns

wine = wine[,-c(1,3)]                
View(wine)    

# I want to see the top countries that produce wines in quantity by using arrange function
wine %>% group_by(country) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

# I'm going to only focus on the top 10 selected countries that produces wine, again in quantity

selected_countries = wine %>% 
  group_by(country) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  top_n(10) %>% 
  select(country)
selected_countries    

# I want to check out the structure

class(selected_countries)

# and I want to turn this into a vector of characters

selected_countries <-   as.character(selected_countries$country)
selected_countries
class(selected_countries)

# I want to check out the quality of those wines by referring "points", which is a rating outcome

select_point <- wine %>% 
  filter(country %in% selected_countries) %>% 
  select(country,points) %>% 
  arrange(country)
select_point

# I want to explore the price and point relationship, so I use graph

ggplot(wine,aes(points, price)) +
  geom_point()+
  geom_smooth()

# I want to see the relationship between countries and points, so I know which country has better wine in quality
# I want to use average country points, so I reorder my country on x axis

ggplot(select_point,aes(x = reorder(country,points,median),y = points))+
  geom_boxplot(aes(fill = country))+
  xlab("Country") + ylab("Points")+
  ggtitle("Distribution of Top Ten Wine Producing Countries") +
  theme(plot.title = element_text (hjust = .5))


# I want to find if there's any country produce best quality wine, but not a mass production country
# my thinking process: I did not seperate mass produce countries and others

wine %>% group_by(country) %>%
  summarize(median = median(points)) %>% 
  arrange(desc(median)) %>% 
  top_n(10)
# Data Science Dojo's way: 

wine %>% filter(!(country %in% selected_countries)) %>%
  group_by(country) %>% 
  summarize(median = median(points)) %>% 
  arrange(desc(median))

# I followed Dojo's step, which is to prepare for the next step of using intersect
# but the code essentially is just the code I created to find countries wiht high points
# but changed the data set to a character vector  

top <- wine %>% group_by(country) %>% 
  summarize(median = median(points)) %>% 
  arrange(desc(median))
class(top)
top <- as.character(top$country)
top



