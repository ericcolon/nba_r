library(tidyverse)
library(dplyr)
library(boot)

# Any relationship between scoring and the height/weight?
# If you calculate the BMI, given weight and height can you tell if higher or lower BMI could have an effect on scores
# For multiple regression, we'd like to know how all these inputs contribute to average scorespergame  



# https://www.datacamp.com/community/tutorials/linear-regression-R#what
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization#add-mean-line-and-density-plot-on-the-histogram
# http://www.rookie-manager.com/agri-value-chain/
# 

# read the data
nba <- read_csv("data/bbal.csv") %>%
  mutate(weight_kgs = weight_pounds / 2.204)
summary(nba)


# Discrete data is data that has a certain number of values whereas continous data is data that can take any value between 
# a certain range.
# Discrete variables have a range whereas continous doesn't.
# examples of discrete is Number of siblings, number of typos in a book, continous can be temparature, age..e.t.c.



# plot distrinution plots for each variable
# histograms
ggplot(nba, aes(height_feet)) + geom_histogram(binwidth = 0.09)
ggplot(nba, aes(percentageofsuccessfullfieldgoals)) + geom_histogram(bins = 14)
ggplot(nba, aes(weight_kgs)) + geom_histogram(bins = 18, fill="green", alpha=0.4)


# add a mean line using geom_vline()
p <- ggplot(nba, aes(percentageofsuccessfullfreethrows)) + geom_histogram(fill="lightblue") 
p + geom_vline(aes(xintercept = mean(percentageofsuccessfullfreethrows)), color = "red", linetype = "dashed", size = 1)

# Add a density plot to the histogram
ggplot(nba, aes(percentageofsuccessfullfieldgoals)) + geom_density()
ggplot(nba, aes(averagescorespergame)) + geom_histogram(binwidth = 1.6,  aes(y=..density..), color = "#ff4444", fill = "#a0db8e", alpha = 0.3) + 
  geom_density(alpha=.2, fill="#ffe99b")


# using generalised linear models
# linear regression,  
# can higher field goals contribute to av scores?
# can height contribute to higher freeshots?

# plot regression lines
ggplot(nba, aes(height_feet,averagescorespergame)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))

