#1
model1 <- lm(data$salary ~ data$years)
confint(model1)

#2
model2 <- lm(data$salary ~ data$source)
confint(model2)

#3
model3 <- lm(data$salary ~ data$years + data$courses) 
anova(model1, model3)
anova(model2, model3)

#4
model3.z = lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
confint(model3.z)

#6
set.seed(1)
sample = data[sample(nrow(data), 15), ]


#7
data.subset = data[51:70,]
model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses) 
summary(model3.subset)

#8

model1.subset = lm(data.subset$salary ~ data.subset$years) 
model2.subset = lm(data.subset$salary ~ data.subset$courses) 
model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses) 
summary(model1.subset)
summary(model2.subset) 
summary(model3.subset)
anova(model1.subset, model3.subset)
anova(model2.subset, model3.subset)

#9
data.subset$predicted = fitted(model3.subset)
cor(data.subset$predicted, data.subset$salary)

#10
data.subset$error = resid(model3.subset) 
cor(data.subset$predicted, data.subset$error)