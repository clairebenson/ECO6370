#Code for Sampling&Bootstrap class

install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
install.packages("gridExtra")
library(gridExtra)

set.seed(0)

x <- seq(0, 10, length.out = 1000)
df <- data.frame(x = x, y = dexp(x))
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Exponential Distribution Density Function",
       x = "Value (in tens of dollars)", 
       y = "Density") +
  theme_minimal()

pexp(1) - pexp(0)

n <- 20
sample <- rexp(n)
sample

#Sample distribution of spending 
df_hist <- data.frame(sample = sample)
ggplot(df_hist, aes(x = sample)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "lightblue", color = "black") +
  xlim(0, 10) + ylim(0, 1) +
  labs(title = "Sample Distribution of Spending",
       x = "Spending (in tens of dollars)", 
       y = "Density") +
  theme_minimal()

mean(sample <= 1)

mean(sample)

#Density funciton 
xgrid <- seq(0,10,length=1000)
df_hist <- data.frame(sample = sample)
df_curve <- data.frame(x = xgrid, y = dexp(xgrid))

ggplot(df_hist, aes(x = sample)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "lightblue", color = "black") +
  geom_line(data = df_curve, aes(x = x, y = y), color = "red", size = 1) +
  xlim(0, 10) + ylim(0, 1) +
  labs(title = "Sample Distribution vs Population Distribution",
       x = "Spending (in tens of dollars)", 
       y = "Density") +
  theme_minimal()

#Repeating the experiment with larger samples 
sample_sizes <- c(20, 100, 500, 2500)
plots <- list()

for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  sample <- rexp(n)
  
  # Create data frames
  df_hist <- data.frame(sample = sample)
  xgrid <- seq(0, 10, length = 1000)
  df_curve <- data.frame(x = xgrid, y = dexp(xgrid))
  
  # Create plot
  p <- ggplot(df_hist, aes(x = sample)) +
    geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "lightblue", color = "black") +
    geom_line(data = df_curve, aes(x = x, y = y), color = "red", size = 1) +
    xlim(0, 10) + ylim(0, 1) +
    labs(title = paste("n =", n),
         x = "Spending (in tens of dollars)", 
         y = "Density") +
    theme_minimal()
  
  plots[[i]] <- p
  
  # Print statistics
  cat("For n =", n, ", the fraction is", mean(sample <= 1), ", and the mean is", mean(sample), ".\n")
}

# Arrange plots in a 2x2 grid
grid.arrange(grobs = plots, ncol = 2)

#Monte Carlo analysis 
n <- 100 # number of samples
B <- 1000 # choose a large number of repetitions
means <- rep(0, B) # placeholder vector of zeros we will fill up with means

for (b in 1:B) {
  sample <- rexp(n)
  means[b] = mean(sample)
}

# Create data frames for plotting
df_means <- data.frame(means = means)
xgrid <- seq(0.6, 1.4, length = 1000)
df_normal <- data.frame(x = xgrid, y = dnorm(xgrid, 1, 1/sqrt(n)))

ggplot(df_means, aes(x = means)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
  geom_line(data = df_normal, aes(x = x, y = y), color = "red", size = 1) +
  xlim(0.6, 1.4) +
  labs(title = "Distribution of Sample Means (n = 100)",
       x = "Sample Mean", 
       y = "Density") +
  theme_minimal()

cat("Mean of means:", mean(means), "\n")

cat("Variance of means:", var(means), "\n")

n <- 500 # number of samples
for (b in 1:B) {
  sample <- rexp(n)
  means[b] <- mean(sample)
}

# Create data frames for plotting
df_means <- data.frame(means = means)
xgrid <- seq(0.6, 1.4, length = 1000)
df_normal <- data.frame(x = xgrid, y = dnorm(xgrid, 1, 1/sqrt(n)))

#increase size of n 
ggplot(df_means, aes(x = means)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
  geom_line(data = df_normal, aes(x = x, y = y), color = "red", size = 1) +
  xlim(0.6, 1.4) +
  labs(title = "Distribution of Sample Means (n = 500)",
       x = "Sample Mean", 
       y = "Density") +
  theme_minimal()

#Illustration in R - CLT illustration 
n <- 500
sample <- rexp(n)
df_hist <- data.frame(sample = sample)
ggplot(df_hist, aes(x = sample)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
  xlim(0, 10) + ylim(0, 1) +
  labs(title = "Sample Distribution (n = 500)",
       x = "Spending (in tens of dollars)", 
       y = "Density") +
  theme_minimal()

#One bootstrap sample or one "bootstrap draw" 
boot_sample_indices <- sample.int(n, replace=TRUE) # n draw indices from 1:n with replacement
sort(boot_sample_indices)[1:10] # just to see what's going on

boot_sample <- sample[boot_sample_indices] # bootstrap sample

#Repeat for large B to get a distribution of mean(X)
B <- 1000
bootstrap_means = rep(0, B)
for (b in 1:B) {
  boot_sample_indices <- sample.int(n, replace=TRUE)
  boot_sample <- sample[boot_sample_indices]
  bootstrap_means[b] = mean(boot_sample)
}

#Plot bootstrap and compare to CLT
# Create data frames for bootstrap and true distributions
df_bootstrap <- data.frame(means = bootstrap_means, type = "Bootstrap")
xgrid <- seq(0.8, 1.2, length = 1000)

# Repeat CLT illustration for true distribution
for (b in 1:B) {
  new_sample <- rexp(n)
  means[b] <- mean(new_sample)
}

df_true <- data.frame(means = means, type = "True")
df_combined <- rbind(df_bootstrap, df_true)

# Create normal curves for overlay
df_normal_bootstrap <- data.frame(x = xgrid, y = dnorm(xgrid, mean(sample), sd(sample)/sqrt(n)), type = "Bootstrap")
df_normal_true <- data.frame(x = xgrid, y = dnorm(xgrid, mean(means), sqrt(var(means))), type = "True")
df_normal_combined <- rbind(df_normal_bootstrap, df_normal_true)

# Create side-by-side comparison
ggplot(df_combined, aes(x = means)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
  geom_line(data = df_normal_combined, aes(x = x, y = y), color = "red", size = 1) +
  facet_wrap(~ type, ncol = 2) +
  xlim(0.8, 1.2) +
  labs(x = "Sample Mean", 
       y = "Density") +
  theme_minimal()

mean(sample)

#bootstrap standard error
sd(bootstrap_means)
#vs. usual standard error 
sd(sample) / sqrt(n)

#bootstrap CI using standard error
mean(sample) + c(-2,2)*sd(bootstrap_means)
#bootstrap CI using bootstrap distribution directly
mean(sample) + c(-1,1)*quantile(abs(bootstrap_means-mean(sample)),.95)

#Online spending application 
browser <- read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\web-browsers.csv")
str(browser)
summary(browser)

# Create data frames for plotting
df_spend <- data.frame(spend = browser$spend, log_spend = log(browser$spend))

# Create side-by-side histograms
p1 <- ggplot(df_spend, aes(x = spend)) +
  geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Spending",
       x = "Spending", 
       y = "Count") +
  theme_minimal()

p2 <- ggplot(df_spend, aes(x = log_spend)) +
  geom_histogram(bins = 30, alpha = 0.7, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Log Spending",
       x = "Log Spending", 
       y = "Count") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)

#bootstrap CI for the mean of spending distribution
B <- 1000
n <- nrow(browser)
bootstrap_spend_means = rep(0, B)
for (b in 1:B) {
  boot_sample_indices <- sample.int(n, replace=TRUE)
  boot_sample <- browser$spend[boot_sample_indices]
  bootstrap_spend_means[b] = mean(boot_sample)
}
#boostrap CI
sd(bootstrap_spend_means)

#Usual standard error
sd(browser$spend)/sqrt(nrow(browser))

#boostrap CI using boostrap standard error
mean(browser$spend) + c(-2,2)*sd(bootstrap_spend_means)

#bootstrap CI using bootstrap distribution directly
mean(browser$spend) + c(-1,1)*quantile(abs(bootstrap_spend_means-mean(browser$spend)), .95)
