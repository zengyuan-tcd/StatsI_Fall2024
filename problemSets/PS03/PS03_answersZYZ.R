#Q1.1
#import data
election_data <- read.csv('/Users/zach/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/incumbents_subset.csv')
#check the data
head(election_data)
paste("There are ",sum(is.na(election_data)),"na value in the data set")
#Running the regression model between difflog and voteshare
difflog_voteshare_regression <- lm(voteshare ~ difflog,data = election_data)
#view model
summary(difflog_voteshare_regression)
#Q1.2
# draw a scatter plot between difflog and voteshare
library(ggplot2)
difflog_voteshare_scatterplot <- ggplot(data = election_data, aes(x = difflog, y = voteshare))+
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE, color = 'yellow')+
  ggtitle('The scatter plot between difflog and voteshare')+
  xlab('difflog(X)')+
  ylab('voteshare(Y)')+
  theme(plot.title = element_text(hjust = 0.5))
difflog_voteshare_scatterplot
ggsave("difflog_voteshare_scatterplot.pdf", plot = difflog_voteshare_scatterplot, width = 8, height = 6, units = "in")
#Q1.3
residuals_object01 <- residuals(difflog_voteshare_regression)
head(residuals_object01)
#Q1.4
#Extract coefficients from the model
intercept1 <- coef(difflog_voteshare_regression)[1]
coef_difflog <- coef(difflog_voteshare_regression)[2]
paste("voteshare =", intercept1, "+", coef_difflog, "* difflog")
#Q2.1
#Running the regression model between difflog and presvote
difflog_presvote_regression <- lm(presvote ~ difflog,data = election_data)
#view model
summary(difflog_presvote_regression)
#Q2.2
# draw a scatter plot between difflog and presvote
difflog_presvote_scatterplot <- ggplot(data = election_data, aes(x = difflog, y = presvote))+
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE, color = 'blue')+
  ggtitle('The scatter plot between difflog and presvote')+
  xlab('difflog(X)')+
  ylab('presvote(Y)')+
  theme(plot.title = element_text(hjust = 0.5))
difflog_presvote_scatterplot
ggsave("difflog_presvote_scatterplot.pdf", plot = difflog_presvote_scatterplot, width = 8, height = 6, units = "in")
#Q2.3
residuals_object02 <- residuals(difflog_presvote_regression)
head(residuals_object02)
#Q2.4
#Extract coefficients from the model
intercept2 <- coef(difflog_presvote_regression)[1]
coef_difflog2 <- coef(difflog_presvote_regression)[2]
paste("presvote =", intercept2, "+", coef_difflog2, "* difflog")
#Q3.1
#Running the regression model between presvote and voteshare
presvote_voteshare_regression <- lm(voteshare ~ presvote,data = election_data)
#view model
summary(presvote_voteshare_regression)
#Q3.2
# draw a scatter plot between voteshare and presvote
voteshare_presvote_scatterplot <- ggplot(data = election_data, aes(x = presvote, y = voteshare))+
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE, color = 'red')+
  ggtitle('The scatter plot between voteshare and presvote')+
  xlab('presvote(X)')+
  ylab('voteshare(Y)')+
  theme(plot.title = element_text(hjust = 0.5))
voteshare_presvote_scatterplot
ggsave("voteshare_presvote_scatterplot.pdf", plot = voteshare_presvote_scatterplot, width = 8, height = 6, units = "in")
#Q3.3
#Extract coefficients from the model
intercept3 <- coef(presvote_voteshare_regression)[1]
coef_presvote<- coef(presvote_voteshare_regression)[2]
paste("voteshare =", intercept3, "+", coef_presvote, "* presvote")
#Q4.1
#Running the regression model between residuals1 and residuals2
residuals_regression <- lm(residuals_object01 ~ residuals_object02,data = election_data)
#view model
summary(residuals_regression)
#Q4.2
# draw a scatter plot between residuals1 and residuals2
residuals1_residuals2_scatterplot <- ggplot(data = election_data, aes(x = residuals_object02, y = residuals_object01))+
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE, color = 'green')+
  ggtitle('The scatter plot between residuals1 and residuals2')+
  xlab('residuals2(X)')+
  ylab('residuals1(Y)')+
  theme(plot.title = element_text(hjust = 0.5))
residuals1_residuals2_scatterplot
ggsave("residuals1_residuals2_scatterplot.pdf", plot = residuals1_residuals2_scatterplot, width = 8, height = 6, units = "in")
#Q4.3
#Extract coefficients from the model
intercept4 <- coef(residuals_regression)[1]
coef_residuals <- coef(residuals_regression)[2]
paste("residuals1 =", intercept4, "+", coef_residuals, "* residuals2")
#Q5.1
#Regression analysis of voteshare with difflog and presvote
voteshare_difflog_presvote_regression <- lm(voteshare ~ difflog + presvote,data = election_data)
#view model
summary(voteshare_difflog_presvote)
#Q5.2
intercept5 <- coef(voteshare_difflog_presvote_regression)[1]
coef_difflog3 <- coef(voteshare_difflog_presvote_regression)[2]
coef_presvote2 <- coef(voteshare_difflog_presvote_regression)[3]
paste("voteshare =", intercept5, "+", coef_difflog3, "* difflog", "+",coef_presvote2,"* presvote")