
  
  ## 0. Setup
  
  ```{r, message=FALSE}
set.seed(123)  # Please don't change this seed.

# You may load packages you plan to use (optional):
library(tidyverse)
library(stats)
library(MASS)
``` 



## 1. OLS Estimation

Recall that we implemented the OLS estimator in the lecture ([link](https://zhan-gao.github.io/ECOx370/lectures/02-R-adv.html#example-ols-estimation)). 
Please write a function `ols_est(X, y)` that implements the OLS estimator and returns both the estimate dcoefficients and the t-values.
                                                                    
# TODO: finish the function ols_est(X, y).
                                                                    
ols_est <- function(X, y) {
bhat <- qr.solve(t(X) %*% X, t(X) %*% y)
yhat <- X %*% bhat
residuals <- y-yhat
                                                                      
n <- nrow(X)
 k <- ncol(X)
df = n-k
sighat2 = sum(residuals^2)/df
var_bhat <- sighat2 * solve(t(X) %*% X)
se_bhat <- sqrt(diag(var_bhat))
#debugging
t_vals <- as.vector(bhat)/se_bhat
return(list(coefficients = as.vector(bhat),
t_vals =t_vals)
)
}
```
Please generate a simple test dataset to check if your function is working properly by comparing with the result from `lm()`. 
```{r q1-test}
# TODO: Test if ols_est() is working properly
# Generate a simple test dataset
# Compare the result from ols_est() and lm()
#set.seed(123)
n <- 50
x <- rnorm(n, mean=10, sd=3)
y <- 10 + 4*x + rnorm(n, sd=6)
X <- cbind(1, x)
ols_test <- ols_est(X, y)
lm_test <- lm(y~X)
summary(lm_test)
                                                                    
coef(lm_test)        # Estimated coefficients
summary(lm_test)$coefficients[, "t value"] # t-values
                                                                    
 print(ols_test$coefficients)
print(ols_test$t_vals)
```
                                                                    
## 2. Monte Carlo Simulation
Let's apply the skills learned in the class to implement a Monte Carlo simulation. 

In such experiments, we sample data from a specified statistical model and examine the finite sample performance of estimation/inference procedures.

For different data generating processes, different primitive parameter values, and different sample sizes $n$, we simulate data and estimate the model for many times and summarized the results (in most cases) by measures like (empirical) Bias, RMSE, size and empirical power functions, or empirical density plots, to examine the finite sample behavior of the estimation/inference procedures.

In this exercise, we will focus on the finite sample estimation accuracy of the OLS estimator in linear regression models. The accuracy can be measured by bias and root mean square error (RMSE).

Generically, bias and root mean square error (RMSE) are calculated by $$bias = R^{-1}\sum_{r=1}^R \left( \hat{ \theta}^{(r)} - \theta_0 \right),$$ $$RMSE = \left(R^{-1}\sum_{r= 1}^R \left( \hat{\theta}^{(r)} -\theta_0 \right)^2\right)^{1/2},$$ for true parameter $\theta_0$ and its estimate $\hat{\theta}^{(r)}$, and $R$ is the number of replications.

### Model

Consider a linear regression model 
$$y_i = \alpha + x_{i1}\beta_1 + x_{i2}\beta_2 + u_i$$
for $i = 1, 2, \ldots, n$, where $n = 100$.
$(y_i, x_i)$ are independently and identically distributed (i.i.d.) with $$u_i\sim i.i.d.N(0,1), \quad (x_{i1}, x_{i2})^\prime \sim i.i.d. N\left(\begin{pmatrix}0 \\ 1 \end{pmatrix}, \begin{pmatrix} \sigma_1^2 & \rho\sigma_1\sigma_2 \\  \rho\sigma_1\sigma_2 & \sigma_2^2 \end{pmatrix} \right).$$ True parameters are $a = 0.11$, $\beta = (0.22, 0.33)^\prime$, $\rho = 0.5$, $\sigma_1 = 1$, and $\sigma_2 = 4$.

### Step 1: Data generating function

Pleaes write a function `dgp(...)` that takes the sample size $n$ and model parameters as inputs and returns the simulated data $(y_i, x_{i1}, x_{i2})$ for $i = 1, 2, \ldots, n$.

```{r q2-dgp}
# TODO: simulate data
#installed (MASS) package
dgp <- function(n, a, B1, B2, rho, sigma1, sigma2) {
  #using a covariance matrix to account for when independent variables are correlated
  #if we assumed independent variables are uncorrelated, dgp function would be slightly difference 
  cov_x1x2 <- rho * sigma1 *sigma2
  Sigma <- matrix(c(sigma1^2, cov_x1x2, cov_x1x2, sigma2^2), nrow=2, ncol=2)
  
  X <- mvrnorm(n=n, mu=c(0,0), Sigma = Sigma)
  x1 <- X[, 1]
  x2 <- X[, 2]
  
  #error
  u <- rnorm(n)
  
  y <- a + B1*x1 + B2*x2 +u
  
  return(data.frame(y=y, x1=x1, x2=x2))
}
```


### Step 2: Setup Primitive Parameters


```{r q2-setup}
# TODO: setup primitive parameters: sample size, true parameters, etc.

n <- 100
a <- 0.11
B1 <- 0.22
B2 <- 0.33
rho <- 0.5
sigma1 <- 1
sigma2 <- 4

```


### Step 3: Run Simulation


```{r q2-run}
# TODO: Run the simulation (generate data - estimation - save results) for 1000 replications

reps <- 1000

#ncol=3 bc intercept, coeff x1 B1, coeff x2 B2
coefficient_matrix <- matrix(NA, nrow=reps, ncol=3)
tval_matrix <- matrix(NA, nrow=reps, ncol=3)

#set.seed(123)

for (i in 1:reps){
  sim_data <- dgp(n=n, a=a, B1=B1, B2=B2, rho=rho, sigma1=sigma1, sigma2=sigma2)
  
  X <- cbind(1, sim_data$x1, sim_data$x2)
  y <- sim_data$y
  
  #use ols  function created in part1
 #debugging
   result <- tryCatch(ols_est(X, y), error = function(e) NULL)
   
   if(!is.null(result)){
  coefficient_matrix[i, ] <- result$coefficients
  tval_matrix[i, ] <- result$t_vals
   }
 
}



```

### Step 4: Summarize Results

```{r q2-summarize}
# TODO: Write a function to calculate bias and RMSE
sum_results <- function(est_vals, true_vals) {
  #treat est_vals as matrix
  #remove missing vals
  est_means <- colMeans(est_vals, na.rm=TRUE)
  
  bias <- est_means - true_vals
  
  rmse <- sqrt(colMeans((est_vals - matrix(true_vals, nrow(est_vals), ncol(est_vals), byrow = TRUE))^2, na.rm = TRUE))
  
  return(data.frame(Parameter = names(true_vals),
                    True = true_vals,
                    Mean = est_means,
                    Bias = bias,
                    RMSE = rmse))
  
}

# TODO: Summarize and report the bias and RMSE
true_vals <- c(Intercept=a, Beta1=B1, Beta2=B2)

results_summary <- sum_results(coefficient_matrix, true_vals)

print(results_summary)

# TODO: plot the empirical density of the estimated coefficient beta_1 across replications
B1_estimates <- as.numeric(coefficient_matrix[, 2])
#B1_estimates_clean <- B1_estimates[!is.na(B1_estimates)]
#testing 215 to get rid of errors

#if(length(B1_estimates_clean)>=2){
plot(density(B1_estimates, na.rm=TRUE),
     main = expression(paste("Empiral density of ", hat(beta)[1])),
     xlab = expression(hat(beta)[1]),
     ylab = "Density",
     col = "pink", 
     lwd = 2)

abline(v=B1, col="blue", lty=2)
legend("topright", 
       legend = expression(paste("True beta1", beta[1])), 
       col = "blue", 
       lty = 2, 
       bty = "n")

#} else{
 # print("Not enough vals")
#}

```


### Step 5: Interpret your results
```{r}
#Please interpret your results and discuss the findings.
cat("Bell shape indicates B1 is normally distributed\n
    The peak of the curve is close to 0.2, which shows our functions are 
    running correctly as the primitive parameter for beta1 is B1=0.22 \n
    Bias is close to 0 and RMSE is small")

```

### Step 6: Run simulation with different sample sizes

Let's investigate how the estimation accuracy of the OLS estimator changes as the sample size increases.

```{r q2-run-different-n}
# TODO: Run the simulation with different sample sizes
sample_sizes <- c(100, 200, 500, 1000)
results_list <- list()

for(n in sample_sizes){
  reps <- 1000
  
  coefficient_matrix <- matrix(NA, nrow=reps, ncol=3)
  tval_matrix <- matrix(NA, nrow=reps, ncol=3)
  
  for(i in 1:reps){
    sim_data <- dgp(n=n, a=a, B1=B1, B2=B2, rho=rho, sigma1=sigma1, sigma2=sigma2)
    
    X <- cbind(1, sim_data$x1, sim_data$x2)
    y <- sim_data$y
    
    #use ols  function created in part1
    #debugging
    result <- tryCatch(ols_est(X, y), error = function(e) NULL)
    
    if(!is.null(result)){
      coefficient_matrix[i, ] <- result$coefficients
      tval_matrix[i, ] <- result$t_vals
    }
  }
  true_vals <- c(Intercept=a, Beta1=B1, Beta2=B2)
  results_summary <- sum_results(coefficient_matrix, true_vals)
  results_summary$SampleSize <- n
  
  results_list[[as.character(n)]] <- results_summary
}

all_results <- do.call(rbind, results_list)
print(all_results)

```

```{r}
#What do you observe? Why?
cat("The true paramteres and means are unchanged\n
    There is still almost no bias- doesn't change based on sample size\n
    Notably, RMSE decreases and sample size increases, meaning the beta hat gets closer 
    to the true beta as sample size increases")
```

### Step 7: Run simulation with misspecified model

Now, we re-do the simulation with the same data generating process.

However, when we run the regression, we only include the first regressor $x_{i1}$ and the intercept while omitting the second regressor $x_{i2}$.


```{r q2-run-misspecified-model}
# TODO: Run the simulation with misspecified model
run_misspecified_sim <- function(n, reps = 1000) {
  coefficient_matrix <- matrix(NA, nrow = reps, ncol = 2)  # Intercept and Beta1 only
  
  for (i in 1:reps) {
    sim_data <- dgp(n=n, a=a, B1=B1, B2=B2, rho=rho, sigma1=sigma1, sigma2=sigma2)
    X <- cbind(1, sim_data$x1)
    y <- sim_data$y
    
    result <- tryCatch(ols_est(X, y), error = function(e) NULL)
    
    if (!is.null(result)) {
      coefficient_matrix[i, ] <- result$coefficients
    }
  }
  
  true_vals <- c(Intercept = a, Beta1 = B1)
  summary_stats <- sum_results(coefficient_matrix, true_vals)
  return(summary_stats)
}
```

```{r q2-run-misspecified-model-result}
# TODO: Check the bias and RMSE for beta_1 with the misspecified model
misspecified_results <- run_misspecified_sim(n = 100)
print(misspecified_results)
```

```{r}
#What do you observe? Why?
cat("The values of the true/primative parameters are unchanged\n
    The means are differe and there is now noticable bias. while I expected the bias to be higher,
    it is still very different from the bias seen in step 6")
```
