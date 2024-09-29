#import data
y <- c ( 105 , 69 , 86 , 100 , 82 , 111 , 104 , 110 , 87 , 108 , 87 , 90 , 94 , 113 , 112 , 98 , 80 , 97 , 95 , 111 , 114 , 89 , 95 , 126 , 98 )
#Calculating the confidence interval usually requires taking into account the mean, standard deviation, standard error, and critical value of the sample, so the above results must be obtained first
#Calculate sample mean
y_mean <- mean(y)
#Calculate standard deviation
y_sd <- sd(y)
#Calculate sample size
n <- length(y)
#The critical value can be found by looking up the statistical table or using code
a1 <- 0.1
t1_quantile <- qt(1-a1/2, df = n - 1)
#Calculate the error
error <- t1_quantile*(y_sd/sqrt(n))
#Construct the Confidence Interval
lower_bound <- y_mean - error
upper_bound <- y_mean + error
#the consequence is (lower_bound,upper_bound)
paste("The confidence interval is: (", lower_bound, ", ", upper_bound, ")", sep="")
#Q1.2
#The mean and standard deviation have been calculated in the previous question
#national_data <- 100
national_data <- 100
#Calculate the t-statistic
t_statistic <- (y_mean - national_data) / (y_sd / sqrt(n))
t_statistic
#Because this test already has a clear hypothesis about one direction of the population parameter, and only has one tail to look for extreme values, it is therefore a one-tailed test.
#This is often used to test whether the mean is significantly greater than some hypothesized value
a2 <- 0.05
t2_quantile <- qt(1-a2, df=n-1, lower.tail = TRUE)
t2_quantile
if(t_statistic > t2_quantile){
  print('The average IQ of students in this school is higher than the national average.')
}else{
  print('The average IQ of students in this school is not higher than the national average.')
}
#Q2.1
#import datasets
expenditure <- read.table('/Users/zach/Downloads/expenditure.txt',header = TRUE)
head(expenditure)
#Check and clean data
is.na(expenditure)
class(expenditure)
str(expenditure)
#Calculate correlation coefficient
expenditure_correlation <- cor(expenditure[,c("Y", "X1", "X2", "X3")])
expenditure_correlation
library(ggplot2)
#Draw scatter plots and regression lines
# Draw a scatter plot of Y versus X1  
Y_vs_X1 <- ggplot(data = expenditure, aes(x = X1, y = Y)) +      
  geom_point() +      
  geom_smooth(method = "lm", se = TRUE, color = "blue") +      
  ggtitle("The scatter plot of Y versus X1") +      
  xlab('per capita personal income in state (X1)') +      
  ylab('per capita expenditure on shelters assistance in state (Y)')+
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("Y_vs_X1_scatterplot.pdf", plot = Y_vs_X1, width = 8, height = 6, units = "in")   
#Draw a scatter plot of Y versus X2  
Y_vs_X2 <- ggplot(data = expenditure, aes(x = X2, y = Y)) +      
  geom_point() +      
  geom_smooth(method = "lm", se = TRUE, color = "yellow") +      
  ggtitle("The scatter plot of Y versus X2") +      
  xlab('pNumber of residents per 100,000 that are financially insecure in state (X2)') +      
  ylab('per capita expenditure on shelters assistance in state (Y)')+
  theme(plot.title = element_text(hjust = 0.5))    
ggsave("Y_vs_X2_scatterplot.pdf", plot = Y_vs_X2, width = 8, height = 6, units = "in")
#Draw a scatter plot of Y versus X3
Y_vs_X3 <- ggplot(data = expenditure, aes(x = X3, y = Y)) +    
  geom_point() +    
  geom_smooth(method = "lm", se = TRUE, color = "green") +    
  ggtitle("The scatter plot of Y versus X3") +    
  xlab('Number of people per thousand residing in urban areas in state (X3)') +    
  ylab('per capita expenditure on shelters assistance in state (Y)')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Y_vs_X3_scatterplot.pdf", plot = Y_vs_X3, width = 8, height = 6, units = "in")
#Draw a scatter plot of X1 versus X2
X1_vs_X2 <- ggplot(data = expenditure, aes(x = X1, y = X2)) +    
geom_point() +    
  geom_smooth(method = "lm", se = TRUE, color = "purple") +    
  ggtitle("The scatter plot of X2 versus X1") +    
  xlab('per capita personal income in state (X1)') +    
  ylab('Number of residents per 100,000 that are financially insecure in state (X2)')+
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("X1_vs_X2_scatterplot.pdf", plot = X1_vs_X2, width = 8, height = 6, units = "in")
#Draw a scatter plot of X1 versus X3
X1_vs_X3 <- ggplot(data = expenditure, aes(x = X1, y = X3)) +    
  geom_point() +    
  geom_smooth(method = "lm", se = TRUE, color = "orange") +    
  ggtitle("The scatter plot of X3 versus X1") +    
  xlab('per capita personal income in state (X1)') +    
  ylab('Number of people per thousand residing in urban areas in state (X3)')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("X1_vs_X3_scatterplot.pdf", plot = X1_vs_X3, width = 8, height = 6, units = "in")
#Draw a scatter plot of X2 versus X3
X2_vs_X3 <- ggplot(data = expenditure, aes(x = X2, y = X3)) +    
  geom_point() +    
  geom_smooth(method = "lm", se = TRUE, color = "red") +    
  ggtitle("The scatter plot of X3 versus X2") +    
  xlab('Number of residents per 100,000 that are financially insecure in state (X2)') +    
  ylab('Number of people per thousand residing in urban areas in state (X3)')+  
  theme(plot.title = element_text(hjust = 0.5))  
ggsave("X2_vs_X3_scatterplot.pdf", plot = X2_vs_X3, width = 8, height = 6, units = "in")
par(mfrow = c(2, 2))  
# launch pdf device  
pdf(file = "scatterplot_matrix.pdf", width = 8, height = 6)  
pairs(~ X1 + X2 + X3 + Y, data = expenditure, main = "X1,X2,X3,Y scatter plot matrix")
dev.off()
#In order to obtain a more accurate relationship, regression analysis can be performed and the linear regression formula can be obtained
#Regression of X1 and X2  
model_X1_X2 <- lm(X1 ~ X2, data = expenditure)  
summary(model_X1_X2)
paste0("X1 = ", round(model_X1_X2$coefficients[1],3), " + ", round(model_X1_X2$coefficients["X2"],3), "*X2")
#Regression of X1 and X3  
model_X1_X3 <- lm(X1 ~ X3, data = expenditure)  
summary(model_X1_X3)
paste0("X1 = ", round(model_X1_X3$coefficients[1],3), " + ", round(model_X1_X3$coefficients["X3"],3), "*X3")
#Regression of X2 and X3  
model_X2_X3 <- lm(X2 ~ X3, data = expenditure)  
summary(model_X2_X3)
paste0("X2 = ", round(model_X2_X3$coefficients[1],3), " + ", round(model_X2_X3$coefficients["X3"],3), "*X3")
#Regression of Y and X1  
model_Y_X1 <- lm(Y ~ X1, data = expenditure)  
summary(model_Y_X1)
paste0("Y = ", round(model_Y_X1$coefficients[1],3), " + ", round(model_Y_X1$coefficients["X1"],3), "*X1")
#Regression of Y and X2  
model_Y_X2 <- lm(Y ~ X2, data = expenditure)  
summary(model_Y_X2)
paste0("Y = ", round(model_Y_X2$coefficients[1],3), " + ", round(model_Y_X2$coefficients["X2"],3), "*X2")
#Regression of Y and X3  
model_Y_X3 <- lm(Y ~ X3, data = expenditure)  
summary(model_Y_X3)
paste0("Y = ", round(model_Y_X3$coefficients[1],3), " + ", round(model_Y_X3$coefficients["X3"],3), "*X3")
#Regression of Y and X1,X2,X3  
model_Y_multiple <- lm(Y ~ X1+X2+X3, data = expenditure)  
summary(model_Y_multiple)
paste0("Y = ", round(model_Y_multiple$coefficients[1],3), " + ", round(model_Y_multiple$coefficients["X1"],3), "*X1"," + ", round(model_Y_multiple$coefficients["X2"],3), "*X2"," + ", round(model_Y_multiple$coefficients["X3"],3), "*X3")
#Q2.2
#Extract the region column and Y column, and form a new dataset.
Y_region <- expenditure[, c("Y", "Region")]
Y_region
Region1_vs_Y1 <- ggplot(Y_region, aes(x = factor(Region), y = Y, fill = factor(Region))) +  
     geom_bar(stat = "identity", position = "dodge") +  
     labs(title = "Per capita housing assistance expenditure by region", x = "Region", y = "per capita expenditure on sheltersa ssistance in state")+
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("Region1_vs_Y1_barchart.pdf", plot = Region1_vs_Y1, width = 8, height = 6, units = "in")
#Regional variables are discrete data. To analyze the relationship between discrete data and continuous data, regression analysis is usually used.
Y_region_correlation <- cor(Y_region[,c("Y","Region")])
Y_region_correlation
#Region is a categorical variable, and regression analysis can better determine the correlation.
Y_region$Region <- as.factor(Y_region$Region)
# use linear  regression  
model <- lm(Y ~ Region, data = Y_region)  
# check the model  
summary(model) 
#The scatter plot of Region and Y
Region2_vs_Y2 <- ggplot(data = expenditure, aes(x = Region, y = Y)) +    
     geom_point() +    
     ggtitle("The scatter plot of Region versus Y") +    
     xlab('Region') + 
     ylab('per capita expenditure on shelters assistance in state (Y)')+
     theme(plot.title = element_text(hjust = 0.5))
ggsave("Region2_vs_Y2_scatterplot.pdf", plot = Region2_vs_Y2, width = 8, height = 6, units = "in")
#Calculate housing support expenditure per capita in each region
# Group by Region  
region_split <- split(Y_region, Y_region$Region)  
# Calculate the mean of Y in each group  
Y_averages <- lapply(region_split, function(x) mean(x$Y, na.rm = TRUE)) 
#Q2.3
# scatter plot between X1 and Y
X1_Y_Region <- expenditure[, c("X1","Y", "Region")]
X1vsY_Region_scatterplot <- ggplot(X1_Y_Region, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +  
  geom_point(size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")) + # set color  
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18, "4" = 19)) + # set shape  
  labs(title = "Y vs X1 Relationship by Region",  
       x = "X1",  
       y = "Y",  
       color = "Region",  
       shape = "Region")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("X1vsY_Region_scatterplot.pdf", plot = X1vsY_Region_scatterplot, width = 8, height = 6, units = "in")
X1_Y_Region_correlation <- cor(X1_Y_Region[,c("Y", "X1", "Region")])
X1_Y_Region_correlation
#When calculating correlation by category, we need to import the dplyr package for processing
#library(dplyr)
#region_correlations <- expenditure %>%  
#  group_by(Region) %>%  
#  summarise(correlation = cor(Y, X1), .groups = 'drop') 
#region_correlations
