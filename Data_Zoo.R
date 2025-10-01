install.packages("pacman")
install.packages("tidyverse")
library(tidyverse)

pacman::p_load(tidyverse, ggplot2, ggridges, gapminder, GGally, patchwork)


cps <- read_csv("/Users/Claire/OneDrive - Allen Economic Development Corporation/Desktop/Research + Projects/ECO 6370/cps09mar.csv")
print(cps)

cps %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_edu = mean(education, na.rm = TRUE),
    mean_earn = mean(earnings, na.rm = TRUE),
    median_earn = median(earnings, na.rm = TRUE),
    mean_hours = mean(hours, na.rm = TRUE)
  ) 

cps %>%
  mutate(age_group = case_when(
    age < 25 ~ "25- ",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 ~ "65+"
  )) %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_earnings = mean(earnings, na.rm = TRUE),
    median_earnings = median(earnings, na.rm = TRUE),
    sd_earnings = sd(earnings, na.rm = TRUE)
  ) %>%
  arrange(age_group)

#Visualization
c
#1
p_hist <- ggplot(data = cps, aes(x = earnings))  # aesthetic mappings

p_hist

#2
p_hist <- p_hist + 
  geom_histogram(bins = 50, fill = "steelblue", color = "white") # geom 

p_hist

#3
p_hist <- p_hist + 
  scale_x_continuous(labels = scales::dollar_format())

p_hist

#4 Distribution of Annual Earnings (CPS 2009)
p_hist <- p_hist + 
  labs(title = "Distribution of Annual Earnings (CPS 2009)",
       x = "Annual Earnings (USD)", y = "Count")
p_hist

#5 Distribution of Annual Earnings (CPS 2009)
p_hist <- p_hist + 
  theme_minimal()

p_hist

#6 Graph that combines everything
ggplot(data = cps, aes(x = earnings)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Distribution of Annual Earnings (CPS 2009)",
       x = "Annual Earnings (USD)", y = "Count") +
  theme_minimal()
