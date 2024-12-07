#Q1.1
#The X-squared statistic is calculated by subtracting the square of the expected value from the observed value and dividing by the expected value.
#The question gives a 2*3 contingency table. 
resulting_data <- matrix(c(14, 6, 7,  
                          7, 7, 1),  
                        nrow = 2, byrow = TRUE,  
                        dimnames = list(c("Upper class", "Lower class"),  
                                        c("Not Stopped", "Bribe requested", "Stopped/given warning")))  
# Calculate summary rows  
row_sums <- rowSums(resulting_data) 
col_sums <- colSums(resulting_data)
total_sum <- sum(resulting_data)    
# Create a new matrix including raw data and summary rows  
resulting_table <- rbind(resulting_data, c(col_sums, total_sum))
resulting_table <- cbind(resulting_table, c(row_sums, total_sum))
rownames(resulting_table) <- c("Upper class", "Lower class", "Total")
colnames(resulting_table) <- c("Not Stopped", "Bribe requested", "Stopped/given warning", "Total")
resulting_table
#Calculate expected frequency
Ex_Upper_NotStopped <- (27*21)/42
Ex_Upper_Bribe <- (13*27)/42
Ex_Upper_Stopped <- (8*27)/42
Ex_Lower_NotStopped <- (21*15)/42
Ex_Lower_Bribe <- (13*15)/42
Ex_Lower_Stopped <- (8*15)/42
#In order to be more intuitive, make an expected frequency table
Expected_table <- matrix(c(Ex_Upper_NotStopped, Ex_Upper_Bribe, Ex_Upper_Stopped,  
                           Ex_Lower_NotStopped, Ex_Lower_Bribe, Ex_Lower_Stopped),  
                         nrow = 2, byrow = TRUE,  
                         dimnames = list(c("Upper class", "Lower class"),  
                                         c("Not Stopped", "Bribe requested", "Stopped/given warning")))
Expected_table
#Calculate X^2
X_squared_statistic <- (14-13.5)^2/13.5 + (6-8.357143)^2/8.357143 + 
  (7-5.142857)^2/5.142857 + (7-7.5)^2/7.5 + 
  (7-4.642857)^2/4.642857 + (1-2.857143)^2/2.857143
X_squared_statistic
#Q1.2 
#Calculate degrees of freedom
df <- (2-1)*(3-1)
df
# Calculate p_value
p_value <- pchisq(X_squared_statistic,df,lower.tail = FALSE)
p_value
#1.3 
#Formula for standardized residuals:(obeserved-expected)/sqrt(expected(1-row prop.)*(1-column prop.))
#row prop = row total/grand total
#column prop = column/grand total
sr_Upper_NotStopped <- (14-13.5)/sqrt(13.5*(1-27/42)*(1-21/42))
sr_Upper_Bribe <- (6-8.357143)/sqrt(8.357143*(1-27/42)*(1-13/42))
sr_Upper_Stopped <- (7-5.142857)/sqrt(5.142857*(1-27/42)*(1-8/42))
sr_Lower_NotStopped <- (7-7.5)/sqrt(7.5*(1-15/42)*(1-21/42))
sr_Lower_Bribe <- (7-4.642857)/sqrt(4.642857*(1-15/42)*(1-13/42))
sr_Lower_Stopped <- (1-2.857143)/sqrt(2.857143*(1-15/42)*(1-8/42))
sr_table <- matrix(c(sr_Upper_NotStopped, sr_Upper_Bribe, sr_Upper_Stopped,  
                     sr_Lower_NotStopped, sr_Lower_Bribe, sr_Lower_Stopped),  
                         nrow = 2, byrow = TRUE,  
                         dimnames = list(c("Upper class", "Lower class"),  
                                         c("Not Stopped", "Bribe requested", "Stopped/given warning")))
sr_table
sr_Upper_NotStopped
sr_Upper_Bribe
sr_Upper_Stopped
sr_Lower_NotStopped
sr_Lower_Bribe
sr_Lower_Stopped
#2.1
#H0:The reservation policy has no effect on the number of new or repaired drinking water facilities in the villages.
#H1:The reservation policy has effect on the number of new or repaired drinking water facilities in the villages.
#2.2
#read data
women_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
women_data
#inspect data
inspect_null <- sum(is.na(women_data))
inspect_null
str(women_data)   
class(women_data)
#bivariate regression
model1 <- lm(women_data$irrigation ~ women_data$reserved,data = women_data)
summary(model1)
model2 <- lm(women_data$water ~ women_data$reserved,data = women_data)
summary(model2)

