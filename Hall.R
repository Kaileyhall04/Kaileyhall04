###### Set Working Directory ######
setwd("H:/sta215")

###### Load Data ######
data <- read.csv("raw_data.csv")

###### Load Packages ######
library(readr)
library(dplyr)
library(haven) 
library(psych)
library(ggplot2)


###### TABLE 1: Descriptive Statistics ######
mean(data$conflict) 
mean(data$violence)
mean(data$involvement)
mean(data$rating)
mean(data$guest)

sd(data$conflict) 
sd(data$violence)
sd(data$involvement)
sd(data$rating) 
sd(data$guest)

table(data$conflict)
table(data$violence)
table(data$involvement)
table(data$rating)
table(data$guest)

describe(data$conflict)
describe(data$violence)
describe(data$involvement)
describe(data$rating)
describe(data$guest)

summary(data$conflict) 
summary(datasviolence)
summary(dataSinvolvement)
summary(data$rating)
summary(data$guest)



###### TABLE 2: Contingency Table (conflict & violence) ######
table(data$conflict , data$violence)

# chi-squared test
chisq.test(data$conflict, data$violence)
result <- chisg.test(data$conflict, data$violence)
print(result)



###### FIGURE 1: Box Plo（conflict & involvement）######

# ANOVA
aov(conflict ~ involvement, data = data)
anova_results <- aov(conflict ~ involvement, data = data)
summary(anova_results)

# box plot
ggplot(data, aes(x = conflict, y = involvement)) +
  geom_boxplot() +
  labs(title = "Box Plot of Conflict Type and Character Involvement",
    x = "Conflict Type",
    y = "Character Involvement") +
  theme_minimal()



###### FIGURE 2: Scatter Plot (rating & guest) ######

# calculate mean line
mean_x <- mean(data$rating) 
mean_y <- mean(data$guest)

# scatter plot
ggplot(data, aes(x = rating, y = guest)) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Scatter Plot for Rating and The Number of Guests") + 
  xlab("Rating") +
  ylab("Number of Guests") +
  theme_minimal() +
  geom_vline(xintercept = mean_x, color = "red", lwd = 2, lty = 2) +
  geom_hline(yintercept = mean_y, color = "purple", lwd = 2, lty = 2) +
  geom_smooth(method = "lm", color = "green", lw = 2)

# calculate the correlation coefficient
cor(data$rating, data$guest, method = "pearson")
cor.test(data$rating, data$guest)

# calculate the intercept
model <- lm(guest ~ rating, data = data)
intercept <- coefficients(model)["(Intercept)"]

# calculate the r squared
model <- lm(guest ~ rating, data = data)
r_squared <- summary(model)$r.squared
print(r_squared)

# linear regression
model <- lm(guest ~ rating, data = data)
summary(model)



###### Figure 3: Residual Plot（rating & guest）######
linear_relationship <- lm(data$rating ~ guest, data = data)
summary(linear_relationship)

plot(data$rating, residuals(linear_relationship))
abline(h = 0)
