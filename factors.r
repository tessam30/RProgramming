# Factors - Data Science Book

library(tidyverse)
library(forcats)

x1 <- c("Dec", "Apr", "Jan", "Mar")
sort(x1)


x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x2)

# Create list of valid levels
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y1 <- factor(x1, levels = month_levels)
sort(y1)

y2 <- factor(x2, levels = month_levels)

# Factors are always sorted in alphabetical order
factor(x1)

# To make factors match the order of appearance in data
f2 <- x1 %>% factor() %>% fct_inorder()
levels(f2)

# Use forcats package to sort and relevel
gss_cat
gss_cat %>% count(race)
ggplot(gss_cat, aes(race)) + geom_bar() + scale_x_discrete(drop = FALSE)

# Filter the regligion based on Protestant
ggplot(filter(gss_cat, relig == "Protestant"), aes(relig)) + geom_bar() + facet_wrap(~denom)

# Exploring TV viewing patterns
  gss_cat %>% 
  group_by(relig) %>% 
  summarise(
  age = mean(age, na.rm = TRUE),
  tvhours = mean(tvhours, na.rm = TRUE),
  n = n()
  ) %>% 
  ggplot(., aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()
