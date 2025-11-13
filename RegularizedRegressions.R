#code for regularized regression lesson 

library(ggplot2)
library(readr)
library(gridExtra)
library(sandwich)
library(lmtest)
library(dplyr) 
library(pROC)
library(Matrix)

#illustrating solution paths of LARS and LARS-LASSO
#doesn't run because nothing defined, but check online notes to see visualization 
library(lars)
data(diabetes)
par(mfrow=c(1,2))
object <- lars(diabetes$x, diabetes$y)
plot(object)
object2 <- lars(diabetes$x, diabetes$y,type="lar")
plot(object2)

# read data on spending for each machine id (household)
browser_spend <- read.csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\browser-totalspend.csv") # machine id, spending
yspend <- browser_spend$spend
cat('number of households:', length(yspend))

web <- read.csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\browser-domains.csv") 
dim(web)

head(web) # machine id, site id, # of visits

sitenames <- scan("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\browser-sites.txt", what="character") #read website names
cat('number of websites:', length(sitenames))

web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames) # relabel sites with names, turn into factor
web$id <- factor(web$id, levels=1:length(unique(web$id))) # turn into factor
head(web)

# compute total visits per machine and then % of time on each site using tapply(), where tapply(a,b,c) outputs c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]
head(visitpercent)

#Illustration of Lasso
library(Matrix) # needed for sparseMatrix function

xweb <- sparseMatrix(i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent, dims=c(nlevels(web$id),nlevels(web$site)), dimnames=list(id=levels(web$id), site=levels(web$site)))
dim(xweb) # accords with our description of the dataset

head(xweb[1, xweb[1,]!=0]) # sites visited by household 1

#run lasso regression
install.packages("glmnet")
library(glmnet)

# compute lasso regularization path
lasso_model <- glmnet(xweb, log(yspend), family = "gaussian")  # Default creates a path of 100 lambdas; can change this with the nlambda argument.

#gives us a plot of regression coefficients as a function of y
plot(lasso_model, xvar = "lambda", ylab="estimated betas")

set.seed(0)
cv.lasso_model <- cv.glmnet(xweb, log(yspend), family = "gaussian")
plot(cv.lasso_model) # the leftmost dotted vertical line is lambda.min, the largest lambda within 1 standard error of the minimum is lambda.1se

head(coef(cv.lasso_model, s = "lambda.min")) # final lasso estimates for the optimal lambda; just getting the first few for illustration to avoid a huge output dump

predict(cv.lasso_model, xweb[1:3,], s = "lambda.min") # select the lambda that minimizes CV error

#regularization and reparameterization 
oj <- read.csv("C:\\Users\\Claire\\OneDrive - Allen Economic Development Corporation\\Desktop\\Research + Projects\\ECO6370\\oj.csv")
oj$brand <- factor(oj$brand)

# By default, model.matrix drops one dummy for identifiability, but for lasso we may wish to keep all!
oj_dummies <- model.matrix(~ brand + price - 1, data=oj) # -1 removes the intercept and keeps all dummies
head(oj_dummies) # now all levels of brand are present

library(glmnet)
sales_model <- glmnet(x = oj_dummies, y = log(oj$sales), family = "gaussian")
coef(sales_model) # coefficients with default lambda
