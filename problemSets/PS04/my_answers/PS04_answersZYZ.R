install.packages("car")
library(car)
data(Prestige)
help(Prestige)
#Q1.1
#check data
paste('There is :',sum(is.na(Prestige)),'na value in dataset')
#Add a professional column and assign a value
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige,n = 30)
#Q1.2
#Linear model between prestige,income,professional and intercation.
model_interaction <- lm(prestige ~ income*professional, data = Prestige)
#check the model
summary(model_interaction)
#Q1.3
#Get the coefficient and the intercept
Intercept <- coef(model_interaction)[1]
income_coef <- coef(model_interaction)[2]
professional_coef <- coef(model_interaction)[3]
income_professional_coef <- coef(model_interaction)[4]
#Write the prediction equation
paste('prestige = ',Intercept,'+',income_coef,'* income','+',
      professional_coef,'* professional',income_professional_coef,
      '* income * professional')
#Q1.6
#income change value is 1000
income_increase <- 1000
income_margin_effect <- (income_coef + income_professional_coef) * 1000
paste("When professional is 1 and income increases", income_increase, ", the marginal effect of income is", income_margin_effect)
#Q1.7
income <- 6000
#Calculate the prestige when people is professional
is_professional <- 1
is_professional_prestige <- Intercept + income_coef * income + professional_coef * is_professional + income_professional_coef * income * is_professional
#Calculate the prestige when people is not professional
no_professional <- 0
no_professional_prestige <- Intercept + income_coef * income + professional_coef * no_professional + income_professional_coef * income * no_professional
#Calculate the  change in y
margin_effect_prestige <- is_professional_prestige - no_professional_prestige
paste("When a person's income is",income,"switching from a nonprofessional to a professional occupation will result in an increase in her prestige of approximately",margin_effect_prestige, "units.")
#Q2.1
# we can get the coefficient, standard error and a.
assigned_signs_coef <- 0.042
assigned_signs_se <- 0.016
alpha <- 0.05
# Calculate t value
assigned_signs_t <- assigned_signs_coef / assigned_signs_se
# Calculate critical value
critical_value <- qt(1 - alpha/2,df = 128)
# Comparing t-values to critical values
if (assigned_signs_t > critical_value) {
  paste("At a significance level of ", alpha, ", the null hypothesis is rejected.So, there is sufficient evidence that put these yard signs in a precinct has an effect on the vote share.")
} else {
  paste("At a significance level of ", alpha, ", the null hypothesis can't be rejected.So, There is no sufficient evidence that There is sufficient evidence that put these yard signs in a precinct has an effect on the vote share has an effect on the vote share.")
}
#Q2.2
adjacent_signs_coef <- 0.042
adjacent_signs_se <- 0.013
# Calculate t value
adjacent_signs_t <- adjacent_signs_coef / adjacent_signs_se
# Calculate critical value
critical_value <- qt(1 - alpha/2,df = 128)
# Comparing t-values to critical values
if (adjacent_signs_t > critical_value) {
  paste("At a significance level of ", alpha, ", the null hypothesis is rejected.So, there is sufficient evidence that being next to precincts with these yard signs has an effect on the vote share.")
} else {
  paste("At a significance level of ", alpha, ", the null hypothesis can't be rejected.So, there is no sufficient evidence that  being next to precincts with these yard signs has an effect on the vote share.")
}