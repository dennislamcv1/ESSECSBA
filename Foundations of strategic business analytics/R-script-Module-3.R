

############################################################

#     Foundation to Strategic Business Analytics           #

#       Module 3 - Understanding causes and consequences	 #

#                                                  				 #

#                                                          #

# 	Author: Nicolas Glady & Pauline Glikman                #

#                   ESSEC BUSINESS SCHOOL                  #

############################################################



############################################################

# Disclaimer: this script is used to produce the examples  #

#  presented during the course Strategic Business          #

#  Analytics. The author is not responsible in any way     #

#  for any problem encountered during this code execution. #

############################################################



############################################################

####     EXAMPLE N°1 - CREDIT SCORING                   ####

############################################################



# Set your directory to the folder where you have downloaded the Credit Scoring dataset



# To clean up the memory of your current R session run the following line

rm(list=ls(all=TRUE))



# Let's load our dataset and call it data

data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files



# Now let's have a look at our variables and see some summary statistics

str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains

summary(data) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles



hist(data$Rating) # Produce a histogram of the credit scores

cor(data[,c(1:5,10)]) # Compute the correlation between all the numerical variables of the sample



linreg=lm(Rating~.,data=data) # Estimate a linear regression model of Rating as a function of everything else.



cor(linreg$fitted.values,data$Rating) # Computes the correlation between the fitted values and the actual ones

plot(data$Rating,linreg$fitted.values) # Plot the fitted values vs. the actual ones



summary(linreg) # Reports the results of the regression



plot(data$Balance,data$Rating) # Allows to visualize the relationship between Balance and Rating

plot(data$Income,data$Rating) # Allows to visualize the relationship between Income and Rating



############################################################

####        EXAMPLE N°2 - HR ANALYTICS 2                ####

############################################################

# Set your directory to the folder where you have downloaded the HR Analytics 2 dataset



# To clean up the memory of your current R session run the following line

rm(list=ls(all=TRUE))



# Let's load our dataset and call it datatot

datatot=read.table('DATA_3.02_HR2.csv', header = T,sep=',')



# Now let's have a look at our variables and see some summary statistics

str(datatot) # The str() function shows the structure of your dataset and details the type of variables that it contains

summary(datatot) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles



table(datatot$left) # look at the frequencies for the left variable

table(datatot$left)/nrow(datatot) # look at percentages for the left variable

hist(datatot$left) # alternatively, plot a histogram



cor(datatot) # Let's check out the correlations



logreg = glm(left ~ ., family=binomial(logit), data=datatot) # Estimate the drivers of attrition



hist(logreg$fitted.values) # See the proportion of employee attrition according to the model

cor(logreg$fitted.values,datatot$left) # Assess the correlation between estimated attrition and actual







cutoff=.3 # Cutoff to determine when P[leaving] should be considered as a leaver or not. Note you can play with it...

sum((logreg$fitted.values<=cutoff)&(datatot$left==0))/sum(datatot$left==0) # Compute the percentage of correctly classified employees who stayed

sum((logreg$fitted.values>cutoff)&(datatot$left==1))/sum(datatot$left==1) # Compute the percentage of correctly classified employees who left

mean((logreg$fitted.values>cutoff)==(datatot$left==1)) # Compute the overall percentage of correctly classified employees



summary(logreg) # Report the results of the logistic regression



# Let's use a more visual way to see the effect of one of the most important driver: TIC



plot(datatot$TIC,datatot$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")



# An aggregated plot

tempdata=datatot

aggbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC

plot(aggbTimeRank$TIC,aggbTimeRank$left,main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent")



# An even better one!

cntbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbTimeRank$TIC,aggbTimeRank$left,circles=cntbTimeRank$left, inches=.75, fg="white", bg="red",main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent") # we

# (See Addendum A for a more rigorous approach)


# Let's use a more visual way to see the effect of the most important driver: Satisfaction

tempdata=datatot

tempdata$rankSatis = round(rank(-tempdata$S)/600) # We create categories of employee satisfaction ranking. We create 20 groups (because it will work well later...)

aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) # We compute the average attrition rate for each category

cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbSatisRank$rankSatis,aggbSatisRank$left,circles=cntbSatisRank$left, inches=.2, fg="white", bg="red",main= "Satisfaction and Employee Attrition", ylab="Average Attrition Rate", xlab= "Rank of Satisfaction")

# (See Addendum B for a more rigorous approach)


################################################################
##                         Addendum                           ##
##                Contributed by Stefan Avey                  ##
## https://github.com/stefanavey/strategic-business-analytics ##
################################################################

## The Bubble Charts in the last 2 examples can be made more rigorously by adjusting

## the area rather than the radius.


#######

## Addendum A ##

#######

## Human perceive area of shapes like circles. So if some value is twice as large,

## we want the area fo the circle to be twice as large, not the radius. The symbols() function

## takes the radius of the circles by default, so we need to compute the radius in order to end up with the desired size of circles.

size = cntbTimeRank$left

radius = sqrt(size / pi)

symbols(x = aggbTimeRank$TIC, y = aggbTimeRank$left,

        circles = radius, inches = .75, fg = "white", bg = "red",

        main =  "Time and Employee Attrition",

        ylab = "Average Attrition Rate", xlab =  "Time spent")


#######

## Addendum B ##

#######

## Instead of creating roughly equal size groups of 20 by rank,

## we create 20 bins of equal size between 0 and 1 and assign each

## employee to 1 bin based on Satisfaction

bins = 20

breakPoints = seq(0, 1, length.out = (bins+1))

tempdata$rankSatis = (bins+1) - as.numeric(cut(tempdata$S, breaks = breakPoints))


## Visually, these are the bins we are choosing:

hist(tempdata$S, breaks = breakPoints)

abline(v = breakPoints, col = "red", lty = 2)

## Note that the first bin 0-0.05 has no employees (so the circle should be size 0)


aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) # We compute the average attrition rate for each category

cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC


## Again here, we want to size the circles by their area, not radius

size <- cntbSatisRank$left

radius <- sqrt(size / pi)

symbols(x = aggbSatisRank$rankSatis, y = aggbSatisRank$left,

        circles = radius, inches = 0.2, fg = "white", bg = "red",

        main =  "Satisfaction and Employee Attrition",

        ylab = "Average Attrition Rate", xlab =  "Rank of Satisfaction")