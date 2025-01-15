library(tidyverse)
super_bowls <- read_csv("super_bowls.csv")
tv <- read_csv("tv.csv")

#Q1. Do large point differences result in lost viewers? 

#plot a histogram of point differences 
ggplot(super_bowls, aes(difference_pts))+
  geom_histogram(binwidth = 2)+
  labs(x = "Point Difference", y = "Number of super bowls")

#display the closes game and largest point difference 
super_bowls %>% filter(difference_pts == min(difference_pts)|
                         difference_pts == max(difference_pts))
#merge the game data and tv data
games_tv <- tv %>% inner_join(super_bowls, by = "super_bowl")

#create a scatter plot with a linear regression model 
ggplot(games_tv, aes(difference_pts, share_household))+
  geom_point()+geom_smooth(method = "lm")+
  labs(x = "point difference", y = "viewership (household share)")
 score_impact = "weak"

 #Q2: How has the number of viewers and TV ratings trended 
 #alonside the advertisement costs? 
 games_tv_plot_avg_us_viewers <- games_tv %>% 
   select(super_bowl, avg_us_viewers) %>% 
   mutate(category = "Average Number of US viewers", 
          value = avg_us_viewers) %>% 
   select(super_bowl, category, value)
head(games_tv_plot_avg_us_viewers) 

games_tv_plot_rating_household <- games_tv %>%
  select(super_bowl, rating_household) %>%
  mutate(category = "Household rating", value = rating_household) %>%
  select(super_bowl, category, value)

games_tv_plot_ad_cost <- games_tv %>%
  select(super_bowl, ad_cost) %>%
  mutate(category = "Advertisement cost (USD)", value = ad_cost) %>%
  select(super_bowl, category, value)

games_tv_plot <- bind_rows(games_tv_plot_avg_us_viewers, 
                           games_tv_plot_rating_household, 
                           games_tv_plot_ad_cost)

ggplot(games_tv_plot) +
  geom_line(data = games_tv_plot %>% filter(category == "Average number of US viewers"),
            aes(x = super_bowl, y = value / max(value), color = "Average number of US viewers")) +
  geom_line(data = games_tv_plot %>% filter(category == "Household rating"),
            aes(x = super_bowl, y = value / max(value), color = "Household rating")) +
  geom_line(data = games_tv_plot %>% filter(category == "Advertisement cost (USD)"),
            aes(x = super_bowl, y = value / max(value), color = "Advertisement cost (USD)")) +
  labs(x = "Super Bowl", y = "Scaled Value (0–1)", color = "Category")

# Optional: here is a fast way to plot the data using facet_wrap()
# ggplot(games_tv_plot, aes(super_bowl, value)) +
#  geom_line() +
#  facet_wrap(~ category, scales = "free", nrow = 3) + 
#  labs(x = "Super Bowl", y = "")

# Interpret the visualization and store your answer to the question
# Interpretation: The ratings increased before advertisement costs did, but viewers were increasing quickly as well. Perhaps TV networks didn't know how to use this data to demand higher advertisement costs sooner, or they needed more evidence before they could justify the increase.
first_to_increase = "ratings"

