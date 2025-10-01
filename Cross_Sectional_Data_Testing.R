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
#Eelements of ggplot2
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

#Other Plots
#7 Earnings by education
d_plot <- cps %>%
  group_by(education) %>%
  summarise(mean_earn = mean(earnings, na.rm = TRUE)) 

print(d_plot)

#8 Average Earnings by Years of Education 
p_earn_by_edu <- ggplot(data = d_plot, aes(x = education, y = mean_earn)) +
  geom_line() + # Layer 1 with geom line
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Average Earnings by Years of Education",
       x = "Years of Education", y = "Mean Earnings (USD)") +
  theme_minimal()

p_earn_by_edu

#Graph that adds data points to 8
p_earn_by_edu + geom_point()

#Gender gap in earnings 
#Bar plit wuth mean earning by gender
d_plot <- cps %>%
  group_by(female) %>%
  summarise(mean_earn = mean(earnings, na.rm = TRUE)) %>%
  mutate(female = ifelse(female == 1, "Female", "Male"))

print(d_plot)

ggplot(data = d_plot, aes(x = female, y = mean_earn, fill = female)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Average Earnings by Gender",
       x = "", y = "Mean Earnings (USD)") +
  theme_minimal()

#Log earnings by gender
cps_w <- cps %>%
  # keep plausible values for earnings & work intensity
  filter(earnings > 0, hours > 0, week > 0, hours <= 80, week <= 52) %>%
  mutate(
    wage_hourly = earnings / (hours * week),
    log_earn    = log(earnings),
    log_wage    = log(wage_hourly),
    female_lab  = if_else(female == 1, "Female", "Male"),
    hisp_lab    = if_else(hisp == 1, "Hispanic", "Non-Hispanic"),
    # education bins (years)
    edu_bin = cut(
      education,
      breaks = c(0, 11, 12, 14, 16, Inf),
      labels = c("≤11 (<=HS-1)", "12 (HS)", "13–14 (Some college)", "15–16 (BA)", "17+ (Grad)")
    ),
    # rough labels for categorical codes (you can tweak to your codebook)
    region_lab = factor(region,
                        levels = c(1,2,3,4),
                        labels = c("Northeast","Midwest","South","West")),
    race_lab   = factor(race,
                        levels = c(1,2,3,4),
                        labels = c("White","Black","Asian","Other")),
    union_lab  = if_else(union == 1, "Union", "Non-union")
  ) %>%
  drop_na(wage_hourly, log_wage, edu_bin)

#Density plot of log earnings by gender
p_log_earn_by_gender <- ggplot(cps_w, aes(x = log_earn, fill = female_lab)) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ female_lab, nrow = 1) +
  # scale_x_continuous(labels = label_number_si()) +
  labs(title = "Log Earnings Distributions by Gender",
       x = "log(Annual earnings)", y = "Density", fill = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
p_log_earn_by_gender

#The previous two graphs, overlapped for better comparison 
p_log_earn_by_gender <- ggplot(cps_w, aes(x = log_earn, fill = female_lab)) +
  geom_density(alpha = 0.35) +
  # scale_x_continuous(labels = label_number_si()) +
  labs(title = "Log Earnings Distributions by Gender",
       x = "log(Annual earnings)", y = "Density", fill = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
p_log_earn_by_gender

#Hourly wage by education and gender 
p_wage_by_edu_and_gender <- ggplot(cps_w, aes(x = edu_bin, y = wage_hourly, fill = female_lab)) +
  geom_violin(trim = TRUE, alpha = 0.35) +
  geom_boxplot(width = 0.12, outlier.shape = NA, position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, quantile(cps_w$wage_hourly, 0.98))) +
  labs(title = "Hourly Wage by Education (with Gender Overlay)",
       x = "Education (years, binned)", y = "Hourly wage (USD)", fill = "Gender") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
p_wage_by_edu_and_gender

#Ridgelines: distribution of hourly wage by region 
# order regions by median log wage for nicer stacking
region_order <- cps_w %>%
  group_by(region_lab) %>%
  summarise(med = median(log_wage, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(region_lab)

cps_w2 <- cps_w %>% mutate(region_lab = factor(region_lab, levels = region_order))
p_wage_by_region <- ggplot(cps_w2, aes(x = log_wage, y = region_lab, fill = region_lab)) +
  ggridges::geom_density_ridges(alpha = 0.6, rel_min_height = 0.01, scale = 1.2) +
  labs(title = "Ridgelines of log(Hourly Wage) by Region",
       x = "log(Hourly wage)", y = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
p_wage_by_region
