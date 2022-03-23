library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(rpart)
library(gplots)
library(ggpubr)

#Question 2.11 part a
toy.df <- read.csv("/Users/cjgil/OneDrive/SCMA 854/ToyotaCorolla.csv")
toy_cor <- subset(toy.df, select = -c(Model, Fuel_Type, Color))
ggcorr(toy_cor, hjust = 1)

toy_cor_matrix <- data.frame(cor(toy_cor))

# Price and Year, Age and Year, Boardcomputer and Year, and ID and Age
# are the pairs of highly correlated values

toy.plot <- ggplot(toy.df)+geom_point(aes(toy.df$Mfg_Year, toy.df$Price))
toy.plot2 <- ggplot(toy.df)+geom_point(aes(toy.df$Mfg_Year, toy.df$Age_08_04))
toy.plot3 <- ggplot(toy.df)+geom_col(aes(toy.df$Mfg_Year, toy.df$Boardcomputer))
toy.plot4 <- ggplot(toy.df)+geom_point(aes(toy.df$Id, toy.df$Age_08_04))
ggarrange(toy.plot, toy.plot2, toy.plot3, toy.plot4, ncol = 2, nrow = 2)

#Question 2.11 part b
#part i
set.seed(1)
library(dummies)
toy.df$Model <- as.character(toy.df$Model)
toy.df.dum <- dummy.data.frame(toy.df, sep = ".", dummy.class = "factor")
toy.df.dum <- toy.df.dum[, -c(10,13)]
head(t(t(names(toy.df.dum))),22)

#The color and fuel type are removed and added back into the dataframe as categories.
#Color would be listed by individual as would fuel type if they were not turned into categories.

#Question 2.11 part b
#part ii
set.seed(0)
train.rows <- sample(rownames(toy.df.dum), dim(toy.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(toy.df.dum), train.rows),
                     dim(toy.df)[1]*0.3)
test.rows <- setdiff(rownames(toy.df.dum), union(train.rows, valid.rows))
train.data <- toy.df[train.rows, ]
valid.data <- toy.df[valid.rows, ]
test.data <- toy.df[test.rows, ]

#The data is split 50% training, 30% validation, and 20% test 
#the random sample of 50% pulled from row IDs for training
#The data is pulled in a way so duplicate data is not included in the different sets

