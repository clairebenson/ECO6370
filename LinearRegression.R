#code for linear regression lesson

library(ggplot2)
library(readr)
library(gridExtra)
install.packages("sandwich")
library(sandwich)
library(lmtest)
library(dplyr) 

browser <- read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\web-browsers.csv")

#Histogram illustrates marginal distribution of log spending
# Create histogram data to match base R exactly
hist_data <- data.frame(spend = log(browser$spend))
ggplot(hist_data, aes(x = spend)) +
  geom_histogram(bins = 30, fill = "#354CA1", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(log(browser$spend)), color = "#CC0035", size = 1) +
  labs(x = "log(browser$spend)", y = "Frequency") +
  theme_classic()

#boxplot illustrates conditional distribution
# conditional distribution by broadband access
ggplot(data.frame(spend = log(browser$spend), broadband = factor(browser$broadband)), 
       aes(y = spend, x = broadband, fill = broadband)) +
  geom_boxplot(alpha = 0.8) +
  labs(y = "log(spend)", x = "broadband") +
  scale_fill_manual(values = c("#CC0035", "#354CA1")) +
  theme(legend.position = "none") +
  theme_classic()
#How to read a boxplot:
#Thick middle line: median
#The box: interquartile range (IQR) (1st quartile and 3rd quartile)
#Thin lines outside the box (whiskers): the borderline of outliers (1.5*IQR)
#dots outside of whiskers: outliers

#Linear Regression
set.seed(0)
n <- 500
X <- rpois(n, lambda=1)
eps <- rnorm(n)
Y <- 1 + 3*X + eps
ggplot(data.frame(X = X, Y = Y), aes(x = X, y = Y)) +
  geom_point(color = "#354CA1", alpha = 0.7) +
  geom_abline(intercept = 1, slope = 3, color = "#CC0035", size = 1) +
  labs(x = "X", y = "Y") +
  theme_classic()

mean(Y[X==2]) - mean(Y[X==1])

#Example 1: Orange Juice Application 
oj <- read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\oj.csv")
head(oj)

#categorical variable, in r factor data type is used for categorical 
oj$brand <- factor(oj$brand) # turn into factor data
levels(oj$brand)

#Data visualization
smu_colors <- c("#354CA1", "#CC0035", "#F9C80E") 
brandcol <- smu_colors
names(brandcol) <- levels(oj$brand)

# Boxplot of log price by brand
p1 <- ggplot(oj, aes(x = brand, y = log(price), fill = brand)) +
  geom_boxplot() +
  scale_fill_manual(values = smu_colors) +
  labs(x = "brand", y = "log(price)") +
  theme_classic() +
  theme(legend.position = "none")

# Scatterplot of log sales vs log price
p2 <- ggplot(oj, aes(x = log(price), y = log(sales), color = brand)) +
  geom_point(size = 0.1, show.legend = TRUE) +
  scale_color_manual(values = smu_colors, guide = guide_legend(override.aes = list(size = 3))) +
  labs(x = "log(price)", y = "log(sales)") +
  theme_classic()

# Display plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

#without logs 
ggplot(oj, aes(x = price, y = sales, color = brand)) +
  geom_point(size = 0.65, alpha = 0.5) +
  scale_color_manual(values = smu_colors, guide = guide_legend(override.aes = list(size = 3))) +
  labs(x = "price", y = "sales") +
  theme_classic()

#Orange Juice Regression 
OLS_model <- glm(log(sales) ~ log(price) + brand, data=oj)
summary(OLS_model)

head(model.matrix(OLS_model))

#Alternative factor specifications 
summary(glm(log(sales) ~ log(price) + brand - 1, data=oj))

oj$brand[1:10]

oj$brand <- relevel(oj$brand, "tropicana") # set baseline to tropicana
oj$brand[1:10]

summary(glm(log(sales) ~ log(price) + brand, data=oj))

oj$brand <- relevel(oj$brand, "dominicks") # set baseline back to dominicks
oj$brand[1:10]

oj$brand <- relevel(oj$brand, "dominicks") # set baseline back to dominicks
oj$brand[1:10]

#Heteroskedasticity
#simple example to illustrate heteroskedasticity
set.seed(123)
n <- 500
X <- sample(0:3, n, replace = TRUE)
# Heteroskedastic errors: variance increases with X

eps <- sapply(sqrt(0.5 + X), function(s) rnorm(1, 0, s))
Y <- 0.5 + 3 * X + eps

# Boxplot of Y by X
ggplot(data.frame(x =X, y = Y), aes(x = factor(X), y = Y)) +
  geom_boxplot(fill = "#354CA1", alpha = 0.6) +
  labs(x = "X", y = "Y", title = "Boxplot of Y by X") +
  theme_classic()

# install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)

OLS_model_mc <- lm(Y ~ X)
summary(OLS_model_mc)

coeftest(OLS_model_mc, vcov = sandwich)

#Logged outcomes 
(exp(coef(OLS_model)['brandtropicana']) - 1) * 100

#Heterogenous effects
OLS_interactions_model <- glm(log(sales) ~ log(price)*brand, data=oj)
summary(OLS_interactions_model)

# Calculate the price elasticity (slope) for each brand from the interaction model
coefs <- coef(OLS_interactions_model)
dominicks_slope <- coefs["log(price)"]
minute_maid_slope <- coefs["log(price)"] + coefs["log(price):brandminute.maid"]
tropicana_slope <- coefs["log(price)"] + coefs["log(price):brandtropicana"]

cat("Dominick's price elasticity (slope):", dominicks_slope, "\n")

cat("Minute Maid price elasticity (slope):", minute_maid_slope, "\n")

cat("Tropicana price elasticity (slope):", tropicana_slope, "\n")

#Advertising 
OLS_full_interactions_model <- glm(log(sales) ~ log(price)*brand*feat, data=oj)
summary(OLS_full_interactions_model)

b <- coef(OLS_full_interactions_model)
cat("Dominick's elasticity w/o ads:", b["log(price)"], "\n")

cat("Minute Maid elasticity w/o ads:", b["log(price)"] + b["log(price):brandminute.maid"], "\n")

cat("Tropicana elasticity w/o ads:", b["log(price)"] + b["log(price):brandtropicana"], "\n")

cat("Dominick's elasticity w/ ads:", b["log(price)"] + b["log(price):feat"], "\n")

cat("Minute Maid elasticity w/ ads:", b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"], "\n")

cat("Tropicana elasticity w/ ads:", b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"], "\n")

sales_by_brand_and_ads <- tapply(oj$sales, oj[,c("feat","brand")], sum)
mosaicplot(sales_by_brand_and_ads, col=smu_colors, main="Sales by Brand and Ads", xlab="Advertising", ylab="Brand")

#Prediction 
new_dat = data.frame(price=c(3,3.5,2), brand=factor(c("tropicana","minute.maid","dominicks")), feat=c(0,0,1)) # data set with a made-up X_{n+1}
exp(predict(OLS_model, newdata=new_dat)) # predicted number of units moved

#Example 2: Housing Data Example
redfin <- read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\redfinLA3.csv")
dim(redfin)

head(redfin)

#relationship between price and sf
ggplot(redfin, aes(x = SQUARE_FEET, y = PRICE)) +
  geom_point(color = "#354CA1", alpha = 0.7) +
  labs(x = "Square Feet", y = "Price ($)") +
  theme_classic()

#relationship between price and number of baths 
ggplot(redfin, aes(x = BATHS, y = PRICE)) +
  geom_point(color = "#354CA1", alpha = 0.7) +
  labs(x = "Baths", y = "Price ($)") +
  theme_classic()

#simple model using only bathrooms as predictor 
fit_baths <- lm(PRICE ~ BATHS, data = redfin)
summary(fit_baths)

#Adding sf to the model
fit_size <- lm(PRICE ~ BATHS + SQUARE_FEET, data = redfin)
summary(fit_size)

#check relationship between baths and sf 
cor(redfin$BATHS, redfin$SQUARE_FEET)
#2 variables are highly correlated, leads to multicollinearity issues 

#categorical predictors 
redfin %>% 
  count(LOCATION)

#fitting model with location as the only predictor 
fit_location <- lm(PRICE ~ LOCATION, data = redfin)
summary(fit_location)

#location with sf
fit_location_size <- lm(PRICE ~ SQUARE_FEET + LOCATION, data = redfin)
summary(fit_location_size)

#Interactions
#price per sf might vary by location, model this with the following interaction:
fit_interaction <- lm(PRICE ~ SQUARE_FEET * LOCATION, data = redfin)
summary(fit_interaction)

# Create a data frame for plotting regression lines
price_range <- seq(min(redfin$SQUARE_FEET), max(redfin$SQUARE_FEET), length.out = 100)
locations <- unique(redfin$LOCATION)

#visualizing different relationships 
#interaction model shows that different locations have different price perf sf relationships 
# Calculate predicted values for each location
pred_data <- expand.grid(SQUARE_FEET = price_range, LOCATION = locations)
pred_data$PRICE <- predict(fit_interaction, newdata = pred_data)

ggplot(redfin, aes(x = SQUARE_FEET, y = PRICE, color = LOCATION)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_data, aes(x = SQUARE_FEET, y = PRICE, color = LOCATION), size = 1) +
  scale_color_manual(values = smu_colors) +
  labs(x = "Square Feet", y = "Price ($)", color = "Location") +
  theme_classic()
