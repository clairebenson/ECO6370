#code for logistic regression lesson 

library(ggplot2)
library(readr)
library(gridExtra)
library(sandwich)
library(lmtest)
library(dplyr) 
library(pROC)
library(Matrix)

#logit
set.seed(0)
n <- 500
X <- rnorm(n)
Y <- ( runif(n) <= 1/(1+exp(-(2+3*X))) )
model <- glm(Y~X, family="binomial")
betas = coef(model)

# Create data for the logistic curve
xgrid <- seq(-3, 2.5, length = 1000)
logistic_curve <- data.frame(
  X = xgrid,
  Y = 1/(1+exp(-betas[1]-betas[2]*xgrid))
)

ggplot(data.frame(X = X, Y = Y), aes(x = X, y = Y)) +
  geom_point(color = "#354CA1", alpha = 0.7) +
  geom_line(data = logistic_curve, aes(x = X, y = Y), color = "#CC0035", size = 1) +
  labs(x = "X", y = "Y") +
  xlim(-3, 2.5) +
  ylim(-0.1, 1.2) +
  theme_classic()

#Spam example 
email <- read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\spam.csv")
dim(email)

head(email)

#maximum likeligood estimate of logit model
logit <- glm(spam ~ ., data=email, family='binomial')

#coefficient of word_george, translated into a percentage effect on odds
(exp(coef(logit)["word_george"])-1)*100
#The coefficient is negative and large in magnitude, indicating that it is a very good predictor of an email not being spam
#Check it another way 
table(email$spam, email$word_george) # 8 out of 1813 spam e-mails contain 'george'

#contrast with the coefficeint on word_free
(exp(coef(logit)["word_free"])-1)*100

#predict spam probvaility for observations 1 and 12
predict(logit, newdata=email[c(1,12),], type="response")

#deviance is called "residual deviance in this command
summary(logit)

#Classification
#evaluate classification performance of spam model
#get predictions and create confusion matrix using a 0.5 threshold
# Get predicted probabilities for all observations
spam_probs <- predict(logit, type = "response")

# Create binary predictions using 0.5 threshold
spam_pred <- ifelse(spam_probs > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix <- table(Actual = email$spam, Predicted = spam_pred)
print(confusion_matrix)

#calculate accuracy
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Overall Accuracy:", round(accuracy, 3), "\n")

# Calculate sensitivity (true positive rate)
sensitivity <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
cat("Sensitivity (recall):", round(sensitivity, 3), "\n")

# Calculate specificity (true negative rate)
specificity <- confusion_matrix[1,1] / sum(confusion_matrix[1,])
cat("Specificity:", round(specificity, 3), "\n")

# Calculate precision
precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
cat("Precision:", round(precision, 3), "\n")

#create an ROC curve to visualize the trade-off between sensitivity (true positive) 
#and specificity (false positive) across different thresholds
# Create ROC curve object
library(pROC)
roc_spam <- roc(email$spam, spam_probs)

# Extract coordinates for ggplot
roc_coords <- coords(roc_spam, "all", ret = c("threshold", "specificity", "sensitivity"))

# Create data frame for plotting
roc_plot_df <- tibble::tibble(
  one_minus_spec = 1 - roc_coords$specificity,
  sensitivity = roc_coords$sensitivity,
  model = "Spam Model"
) %>%
  bind_rows(tibble::tibble(
    one_minus_spec = 0:100 / 100,
    sensitivity = 0:100 / 100,
    model = "Random"
  )) %>%
  tidyr::pivot_longer(cols = c(sensitivity), names_to = "metric", values_to = "value")

# Plot ROC curve using ggplot2
roc_plot_df %>% 
  ggplot(aes(x = one_minus_spec, y = value, colour = model)) + 
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "1-specificity", y = "sensitivity", title = "ROC Curve for Spam Detection") +
  coord_fixed() +
  geom_line(size = 1) +
  scale_color_manual(values = c("#CC0035", "#354CA1"))

# Calculate and display AUC
auc_spam <- auc(roc_spam)
cat("Area Under Curve (AUC):", round(auc_spam, 3))

#Hotel Reviews Example 
library(Matrix)  # Needed for sparse matrix operations
load("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\tripadvisor.RData")

y_train[1:10]

X_train[1:10, 1:5]

# Distribution of ratings
rating_data <- data.frame(rating = y_train)
ggplot(rating_data, aes(x = rating)) +
  geom_bar(fill = "#354CA1", alpha = 0.8) +
  labs(x = "Rating", y = "Count", title = "Distribution of Hotel Ratings") +
  theme_classic()

# Word frequency distribution
word_freq_data <- data.frame(word_freq = Matrix::colMeans(X_train))
ggplot(word_freq_data, aes(x = word_freq)) +
  geom_histogram(fill = "#354CA1", alpha = 0.8, bins = 30) +
  labs(x = "Word Frequency", y = "Count", 
       title = "Distribution of Word Frequencies") +
  theme_classic()

# Filtered word frequency distribution
word_freq_filtered <- word_freq_data %>% 
  filter(word_freq > 0.01)
ggplot(word_freq_filtered, aes(x = word_freq)) +
  geom_histogram(fill = "#354CA1", alpha = 0.8, bins = 30) +
  labs(x = "Word Frequency (>1%)", y = "Count", 
       title = "Distribution of Word Frequencies (>1%)") +
  theme_classic()

# Most frequent words
word_means <- Matrix::colMeans(X_train)
word_means_df <- data.frame(word = names(word_means), frequency = word_means) %>%
  arrange(desc(frequency))
head(word_means_df, 10)

# Keep only words with frequency > 5%
keep <- which(Matrix::colMeans(X_train) > 0.05)
X_train1 <- X_train[, keep]
X_train1 <- as.matrix(X_train1)

cat("Original matrix size:", format(object.size(X_train), "Mb"), "\n")

cat("Filtered matrix size:", format(object.size(X_train1), "Mb"), "\n")

cat("Number of words kept:", length(keep))

#Fitting the model
# Prepare training data
dat_train <- as_tibble(X_train1) %>% 
  mutate(is_4or5 = y_train >= 4)

# Prepare test data
dat_test <- as_tibble(as.matrix(X_test[, keep])) %>% 
  mutate(is_4or5 = y_test >= 4)

# Clean up
rm(X_train1, X_test)

# Fit logistic regression model
hotel_model <- glm(is_4or5 ~ ., family = "binomial", data = dat_train)
summary(hotel_model)

# Top coefficients by absolute value
coef_df <- data.frame(
  word = names(hotel_model$coefficients),
  coefficient = hotel_model$coefficients
) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10)

print(coef_df)

# Effect of "fantastic" word
fantastic_odds <- exp(hotel_model$coefficients["fantastic"])
fantastic_pct <- 100 * (fantastic_odds - 1)

cat("Odds multiplier for 'fantastic':", round(fantastic_odds, 3), "\n")

cat("Percentage increase:", round(fantastic_pct, 1), "%")

# Effect of "ok" word
ok_odds <- exp(hotel_model$coefficients["ok"])
ok_pct <- 100 * (1 - ok_odds)

cat("Odds multiplier for 'ok':", round(ok_odds, 3), "\n")

cat("Percentage decrease:", round(ok_pct, 1), "%")

#Model evaluations 
# Predictions on test set
pred <- predict(hotel_model, newdata = dat_test, type = "response")

# Boxplot of predictions by true class
pred_data <- dat_test %>% 
  mutate(pred = pred)

ggplot(pred_data, aes(x = factor(is_4or5), y = pred)) +
  geom_boxplot(fill = "#354CA1", alpha = 0.8) +
  labs(x = "True Rating (4+ stars)", y = "Predicted Probability", 
       title = "Predicted Probabilities by True Class") +
  theme_classic()

# Histogram of predictions by class
ggplot(pred_data, aes(x = pred, fill = factor(is_4or5))) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_grid(is_4or5 ~ .) +
  scale_fill_manual(values = c("#354CA1", "#CC0035"), 
                    labels = c("Not 4+", "4+ stars")) +
  labs(x = "Predicted Probability", y = "Count", 
       title = "Distribution of Predicted Probabilities by Class") +
  theme_classic() +
  theme(legend.title = element_blank())

# Compute ROC curve
roc_obj <- roc(dat_test$is_4or5, pred)

# Plot ROC curve
plot(roc_obj, col = "#354CA1", main = "ROC Curve for Hotel Rating Prediction")

# Get AUC (Area Under Curve)
auc_value <- auc(roc_obj)
cat("Area Under Curve (AUC):", round(auc_value, 3))

#Data Analysis 
#no "train_small.csv" for this example, perhaps email
dat <-  read_csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\train_small.csv")
