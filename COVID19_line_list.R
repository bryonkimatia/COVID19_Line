rm(list=ls()) # removes all variables stored previously
library(Hmisc) #import

data <- COVID19_line_list_data <- read.csv("D:/Portfolio/R PROGRAMMING/COVID19_line_list_data.csv")
describe(data) # Hmisc command

#cleaned uo death column
data$death_dummy <- as.integer(data$death != 0)

#death rate
sum(data$death_dummy) / nrow(data)

#AGE
#claim: people who die are older dead 
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#The difference between dead and alive is 20 year, but is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)

#normally, if p-value < 0.05, we reject null hypothesis
#Here, p-value ~ 0, so we reject the null hypothesis and conclude that this is statistically significant


#GENDER
#claim: gender has no effect
#claim: people who die are older dead 
men = subset(data, gender == "male") 
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)       #8.5%
mean(women$death_dummy, na.rm = TRUE)     #3.7

#Is this significant
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
#99% Confidence: men have from 0.8% to 8.8% higher chance of dying than women

# p-value = 0.002105, so we reject the null hypothesis and conclude that this is statistically significant