#inclusion of required libraries
library(ggplot2)
library(tidyverse)
library(dplyr) 
library(caret)
library(mlbench)
library (purrr)
library(resample)
library(glmnet)
library(MASS)
library(sfsmisc)

# Get content into a data frame 
data1 <- read.table("data1.txt",header = TRUE, sep = "\t")

# removing missing values and form a new data frame
data1_without_NA <- na.omit(data1)

# selecting the columns corresponding to y and x1
data1_y_x1 <- data1_without_NA %>% dplyr::select(2, 4)


#removing outliers
#1. run this code to determine iqr and upper/lower ranges for independent variable x1
x <-data1_y_x1$x1
Q <- quantile(x,probs=c(.25,.75),na.rm=TRUE)
iqr <- IQR(x,na.rm=TRUE)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
#2. run this code to select only the data that's between the upper and lower ranges
removed_x1 <- subset(data1_y_x1, data1_y_x1$x1 > (Q[1] - 1.5*iqr) & data1_y_x1$x1 < (Q[2]+1.5*iqr))

X <- c(removed_x1$x1)
Y <- c(removed_x1$y)
# Apply simple linear regression using only y (dependent) and x1 (independent) variables
challenge1_a <- lm(Y ~ X)

#show regression results Challenge 1)a
summary(challenge1_a)

#show the figure
plot(X, Y, pch = 16, cex = 1.3, col = "blue", main = "Y AGAINST X1", xlab = "X1", ylab = "Y")
abline(lm(Y ~ X),col="red",lwd=c(5, 7))


# selecting the columns corresponding to y, y_w and x1
data1_y_x1_y_w <- data1_without_NA %>% dplyr::select(2:4)


#removing outliers
#1. run this code to determine iqr and upper/lower ranges for independent variable x1
x <-data1_y_x1_y_w$x1
Q <- quantile(x,probs=c(.25,.75),na.rm=TRUE)
iqr <- IQR(x,na.rm=TRUE)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
#2. run this code to select only the data that's between the upper and lower ranges
removed_x1_2 <- subset(data1_y_x1_y_w, data1_y_x1_y_w$x1 > (Q[1] - 1.5*iqr) & data1_y_x1_y_w$x1 < (Q[2]+1.5*iqr))

# min-max normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#weight normalization
removed_x1_2$y_w <- normalize(removed_x1_2$y_w) 


#define weights to use
wt <- removed_x1_2$y_w

X <- c(removed_x1_2$x1)
Y <- c(removed_x1_2$y)

#perform weighted least squares regression
challenge1_b <- lm(Y ~ X, data = removed_x1_2, weights=wt)

#show regression results Challenge 1)b
summary(challenge1_b)

anova(challenge1_a, challenge1_b)


#perform robust weighted least squares regression
challenge1_c <- rlm(Y ~ X, data = removed_x1_2, weights=wt)

#show regression results Challenge 1)c
summary(challenge1_c)

anova(challenge1_b, challenge1_c)
