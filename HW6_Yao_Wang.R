
##################################################################
# Use this block comment at the top of each R code file you submit
# Homework 6 – Submitted by Yao Wang on October 10, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.
# IST 687. Due is October 11, 2018

# step A: load and merge datasets
# re-use the code from HW3
readStates <- function()
{
  states <- raw_data
  # Read form from outside of R. Create a new dataframe "dfStates", then reserve the form into "dfStates"
  # remover rows that not needed 
  # -- first row is the total for the US, we do not need that 
  states <- states[-1,]
  #-- last row is Puerto Rico, it is not a states
  num.row <- nrow(states)
  states <- states[-num.row,]
  
  # remover the first for coclumns
  states <- states[,-1:-4]
  rownames(states) <- c(1:51) 
  # change names for remaining coclumns
  colnames(states) <- c("stateName", "population","popOver18", "percentOver18")
  
  # return the results
  return(states)
}

states <- readStates()  
str(states)
# 2. re-use the code from HW2
arrests <- USArrests 
str(arrests)
# create a column with "stateName", then can merage with the other dataframe "states"
arrests <- cbind("stateName" =row.names(arrests), arrests)

# 3. Create a merged dataframe
Newarrests <- merge(arrests, states, by=c("stateName"))

# Step B: Explore the Data – Understanding distributions
#load the libraries
library(ggplot2)

# 4.1 a histogram of the population
hist1 <- ggplot(Newarrests,aes(x=population)) + geom_histogram(binwidth = 5000000, color="white", fill="orange")
# 4.2 a histogram of the murder rate
hist2 <- ggplot(Newarrests,aes(x=Murder)) + geom_histogram(binwidth = 0.5, color="white", fill="orange")
# 4.3 a histogram of the Assault
hist3 <- ggplot(Newarrests,aes(x=Assault)) + geom_histogram(binwidth = 20, color="white", fill="orange")
# 4.4 a histogram of the UrbanPop
hist4 <- ggplot(Newarrests,aes(x=UrbanPop)) + geom_histogram(binwidth = 10, color="white", fill="orange")
# 4.5 a histogram of the Rape
hist5 <- ggplot(Newarrests,aes(x=Rape)) + geom_histogram(binwidth = 10, color="white", fill="orange")

# What parameter will you have to adjust to make the other histograms look right? Geohist in the book
# Change binwidth
# 5.1 a boxplot for the population
boxplot1 <- ggplot(Newarrests, aes(factor(0), y=population)) + geom_boxplot()
# 5.2 a boxplot for the murder rate
boxplot2 <- ggplot(Newarrests, aes(factor(0), y=Murder)) + geom_boxplot()

# 6 Create a block comment explaining which visualization (boxplot or histogram) 
# you thought was more helpful (explain why)

# It depends on different situation. 
# If we want to show the frequency of population, the histogram is the best way.
# If we want to calculate the varables, the boxplot is a good choice.

# Step C: Which State had the Most Murders – bar charts
# 7	Calculate the number of murders per state
# per 100,000 residents
murderPerState <- (Newarrests$population*Newarrests$Murder)/100000
NewTotal <- cbind(Newarrests, murderPerState)
  
# 8 Generate a bar chart, with the number of murders per state
# Hint: use the geom_col function
ggplot(NewTotal, aes(x=stateName, y=murderPerState, group=1)) + geom_col()

# 9 a bar chart, with the number of murders per state
# Rotate text (on the X axis), so we can see x labels, also add a title named “Total Murders”.
ggplot(NewTotal, aes(x=stateName, y=murderPerState))+geom_col()+
  theme(axis.text.x =element_text(angle=90,hjust=1)) + ggtitle ("Total Murders")

# 10 Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
ggplot(NewTotal, aes(x=reorder(stateName,murderPerState), y=murderPerState))+
  geom_col()+
  theme(axis.text.x =element_text(angle=90,hjust=1)) + ggtitle ("Total Murders")

# 11 Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
ggplot(NewTotal, aes(x=reorder(stateName,murderPerState), y=murderPerState, fill = percentOver18))+
  geom_col()+
  theme(axis.text.x =element_text(angle=90,hjust=1)) + ggtitle ("Total Murders")

# Step D: Explore Murders – scatter chart
# 12 a scatter plot 
# have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate
ggplot(NewTotal, aes(x = population , y =percentOver18 , size = murderPerState , col =murderPerState)) + geom_point()
