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
library(equatiomatic)

# Get data1 into a data frame 
data1 <- read.table("data1.txt",header = TRUE, sep = "\t")

# removing missing values and form a new data frame for data1
data1_without_NA <- na.omit(data1)

col1 <- ncol(data1_without_NA)

# Get data2 into a data frame 
data2 <- read.table("data2.txt",header = TRUE, sep = "\t")

# removing missing values and form a new data frame for data2
data2_without_NA <- na.omit(data2)


combined_data<-merge(data1_without_NA, data2_without_NA, by.x=c("cancer_sample_ID"),by.y=c("cancer_sample_ID"))

# min-max normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#weight normalization
combined_data$y_w <- normalize(combined_data$y_w)

#remove columns containing all zeros mostly
combined_data <- combined_data[, colSums(combined_data != 0) > 1]
col2 <- ncol(combined_data)


# selecting the columns corresponding to gene
G <- combined_data %>% dplyr::select((col1+1): (col2))
col3 <- ncol(G)


Y <- combined_data$y
X1 <- combined_data$x1
output <- data.frame() 
#define weights to use
wt <- combined_data$y_w


for(i in 1:col3)
{
  X2 <- G[,i]
  #perform robust regression
  challenge2_b <- rlm(Y ~ X1+X2)
  #fetching t_scores of coefficients
  t_scores <- summary(challenge2_b)[["coefficients"]][, "t value"]
  #fetching degree of freedom
  deg_freedom <- summary(challenge2_b)$df[2]
  #getting p-values from t_scores
  p_value<-pt(q=t_scores[3], df=deg_freedom, lower.tail=FALSE)
  output[i,1] <- colnames(G[i])
  output[i,2] <- p_value
}

challenge2b_ranked_genes <-output[order(-output$V2),]

output2 <- data.frame() 

  for(i in 1:col3)
  {
    X2 <- G[,i]
    #perform robust regression
    challenge2_c <- rlm(Y ~ X1+X2+X1*X2)
    #fetching t_scores of coefficients
    t_scores <- summary(challenge2_c)[["coefficients"]][, "t value"]
    #fetching degree of freedom
    deg_freedom <- summary(challenge2_c)$df[2]
    #getting p-values from t_scores
    p_value<-pt(q=t_scores[3], df=deg_freedom, lower.tail=FALSE)
    output2[i,1] <- colnames(G[i])
    output2[i,2] <- p_value
  }

challenge2c_ranked_genes <-output2[order(-output2$V2),]





######## Random forest regression ##############################################

library(randomForest)

data_for_random_forest <- combined_data[ c(2,4:col2) ]

Y <- data_for_random_forest$y
X <- data_for_random_forest[, colnames(data_for_random_forest) != "y"]

# Create random forest for regression using X1
challenge2_d.rf <- randomForest(X,Y,classwt=wt)


# Print regression model using X1
print(challenge2_d.rf)

###"feature importance" measurement to rank the genes
output3<-challenge2_d.rf$importance

rownames(output3)
output3[,1]

data_for_feature_importance<- list(gene_name=c(rownames(output3)),importance1=c(output3[,1]))

dataFrame_for_feature_importance <- as.data.frame(data_for_feature_importance)


challenge2d_ranked_genes <-dataFrame_for_feature_importance[order(-dataFrame_for_feature_importance$importance1),]