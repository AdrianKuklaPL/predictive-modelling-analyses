# Load the dataset and perform initial exploratory analysis to understand its structure and key distributions.
fifa = read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/predictive-modelling-analyses/main/data/fifa.csv", header = TRUE)
ncol(fifa)  # Check number of columns
colnames(fifa)  # View column names
summary(fifa)  # Summary statistics for numeric variables

# Analyze categorical variables to explore player characteristics and distributions.
table(fifa$position)  # Frequency of positions
table(fifa$intl_rep)  # Frequency of international reputation levels
prop.table(table(fifa$intl_rep)) * 100  # Proportion of international reputation
prop.table(table(fifa$pref_foot)) * 100  # Proportion of preferred foot
prop.table(table(fifa$position)) * 100  # Proportion of positions

# Perform correlation analysis to understand relationships between numeric variables.
fifa2 = data.frame(fifa$overall, fifa$age, fifa$height, fifa$weight, fifa$pace, fifa$dribbling, 
                   fifa$shooting, fifa$passing, fifa$defending, fifa$physicality)
round(cor(fifa2), 2)  # Correlation matrix

# Visualize data to inspect distributions and relationships between variables.
par(mfrow = c(1, 1))  # Set up plot layout
plot(x = fifa$age, fifa$overall)  # Scatterplot of age vs overall rating
hist(fifa$overall, freq = FALSE)  # Histogram of overall ratings
lines(density(fifa$physicality), lwd = 2, col = "blue")  # Density plot for physicality
boxplot(fifa$overall, main = "P")  # Boxplot of overall ratings

# Examine relationships between categorical and numeric variables using visualizations.
boxplot(fifa$overall ~ fifa$position)  # Boxplot of overall ratings by position

# Visualize relationships between variables and explore distributions to uncover patterns and trends in the data.

# Set up the plotting area for individual visualizations.
par(mfrow = c(1, 1))

# Scatterplot to examine the relationship between player height and overall rating.
plot(x = fifa$height, fifa$overall)

# Histogram and density plot to visualize the distribution of overall ratings.
hist(fifa$overall, freq = FALSE)
lines(density(fifa$overall))

# Boxplot to explore the distribution of player ages.
boxplot(fifa$age, main = "Age")

# Boxplot to compare overall ratings across different levels of international reputation.
boxplot(fifa$overall ~ fifa$intl_rep)

# Density plots with highlighted areas to visualize distributions of key numeric variables.
plot(density(fifa$height), main = "Density Plot: Height")
polygon(density(fifa$height), col = "red")

plot(density(fifa$weight), main = "Density Plot: Weight")
polygon(density(fifa$weight), col = "red")

plot(density(fifa$pace), main = "Density Plot: Pace")
polygon(density(fifa$pace), col = "red")

plot(density(fifa$age), main = "Density Plot: Age")
polygon(density(fifa$age), col = "red")


# Perform linear regression and check assumptions for hypothesis testing.
reg <- lm(overall ~ height, data = fifa)
summary(reg)  # Regression summary

# Visualize regression line and residuals to assess model fit and normality.
plot(x = fifa$height, y = fifa$overall)
abline(reg, col = "red")
par(mfrow = c(1, 1))
boxplot(residuals(reg), main = "Residuals")
plot(density(residuals(reg)), main = "Density Plot: Residuals")
polygon(density(residuals(reg)), col = "red")
plot(x = residuals(reg), y = fifa$height)  # Residual plot

# Ensure residuals are approximately normally distributed for valid F and t-tests.

# Perform linear regression for overall rating vs age and vs weight, and assess assumptions for hypothesis testing.
# Regression analysis: overall rating vs age
reg2 <- lm(overall ~ age, data = fifa)
summary(reg2)
plot(x = fifa$age, y = fifa$overall)
abline(reg2, col = "red")
par(mfrow = c(1, 1))
boxplot(residuals(reg2), main = "Residuals")
plot(density(residuals(reg2)), main = "Density Plot: Residuals")
polygon(density(residuals(reg2)), col = "red")
plot(x = residuals(reg2), y = fifa$age)

# Regression analysis: overall rating vs weight
reg3 <- lm(overall ~ weight, data = fifa)
summary(reg3)
plot(x = fifa$weight, y = fifa$overall)
abline(reg3, col = "red")
par(mfrow = c(1, 1))
boxplot(residuals(reg3), main = "Residuals")
plot(density(residuals(reg3)), main = "Density Plot: Residuals")
polygon(density(residuals(reg3)), col = "red")
plot(x = residuals(reg3), y = fifa$weight)

#Model Building
#p=12 predictor variables
#(2^p)-1=2^12-1 regression models
2^12-1 # (excluding the empty model) - possible regression models

# Perform variable selection using forward stepwise regression and evaluate all possible regression models.

# Forward stepwise regression to iteratively select variables for predicting overall ratings.
library("olsrr")
model = lm(overall ~ age + position + height + weight + intl_rep + pace + dribbling + shooting + 
             passing + pref_foot + defending + physicality, data = fifa)

fifa3 <- fifa  # Copy of the dataset for modifications
fitnull <- lm(overall ~ 1, data = fifa3)  # Null model with no predictors
fitfull <- lm(overall ~ age + position + height + weight + intl_rep + pace + dribbling + shooting + 
                passing + pref_foot + defending + physicality, data = fifa3)  # Full model with all predictors
reg10 = step(fitnull, scope = list(lower = fitnull, upper = fitfull), direction = "forward")  # Forward stepwise selection

# Evaluate all possible regression models to identify the best subset of predictors.
library(leaps)
leaps <- regsubsets(overall ~ age + position + height + weight + intl_rep + pace + dribbling + 
                      shooting + passing + pref_foot + defending + physicality, data = fifa, nbest = 10)
summary(leaps)  # Summary of all subsets

# Identify models with the highest adjusted R-squared, lowest Mallows' Cp, and lowest BIC for comparison.
which.max(summary(leaps)$adjr2)  # Model with maximum adjusted R-squared
which.min(summary(leaps)$cp)  # Model with minimum Cp
which.min(summary(leaps)$bic)  # Model with minimum BIC

# View the selected variables for a specific subset (e.g., 71st model).
summary(leaps)$which[71,]

# Ensure variables are correctly treated as categorical in the dataset.
is.factor(fifa3$intl_rep)  # Check if 'intl_rep' is a factor
fifa3$intl_rep = as.factor(fifa3$intl_rep)  # Convert 'intl_rep' to a factor if not already

# Analyze interaction effects between variables to explore relationships beyond main effects.

# Interaction between physicality and international reputation in predicting overall ratings.
library(car)
reg16 = lm(overall ~ physicality * intl_rep, data = fifa3)
summary(reg16)

# Interaction between height and weight in predicting overall ratings.
reg16 = lm(overall ~ height * weight, data = fifa3)
summary(reg16)

# Model including main effects and interaction between international reputation and height.
reg12 = lm(overall ~ intl_rep + passing + position + dribbling + shooting + 
             physicality + pace + age + defending + weight + height + intl_rep * height, 
           data = fifa3)
summary(reg12)

# Assess and address multicollinearity and evaluate influential observations in the regression model.

# Forward stepwise regression to select predictors and check for multicollinearity.
library(car)
reg10 = step(fitnull, scope = list(lower = fitnull, upper = fitfull), direction = "forward")
summary(reg10)
vif(reg10)  # Variance Inflation Factors (VIF) to detect multicollinearity

# Correlation analysis between passing and dribbling as potential sources of multicollinearity.
cor.test(fifa3$passing, fifa3$dribbling)

# Build regression models to analyze multicollinearity and its impact on predictor significance.
reg14 = lm(overall ~ intl_rep + passing + position + physicality + pace + shooting + defending, data = fifa3)
vif(reg14)  # Check multicollinearity
summary(reg14)

reg15 = lm(overall ~ intl_rep + passing + position + physicality + pace + shooting, data = fifa3)
vif(reg15)  # Reduced model to address multicollinearity
summary(reg15)

# Correlation analysis between shooting and pace to investigate relationships.
cor.test(fifa3$shooting, fifa3$pace)

# Final model with selected predictors and VIF assessment.
reg16 = lm(overall ~ intl_rep + passing + position + physicality + pace, data = fifa3)
summary(reg16)
vif(reg16)

# Diagnostics for influential observations and leverage.
plot(reg16)
par(mfrow = c(1, 1))
lev = hat(model.matrix(reg16))  # Leverage values
plot(lev)  # Plot leverage values
which(lev > 0.03)  # Identify high-leverage points
points(836, lev[836], col = 'red')
points(837, lev[837], col = 'green')
points(838, lev[838], col = 'pink')
points(839, lev[839], col = 'blue')

# Cook's distance to identify observations with high influence.
cook = cooks.distance(reg16)
which(cook > 0.004)  # Identify influential observations
plot(cook, ylab = "Cook's distances")
points(1226, cook[1226], col = 'red')
points(1234, cook[1234], col = 'blue')
points(1714, cook[1714], col = 'green')

# Influence plot for visualizing leverage and Cook's distance.
influencePlot(reg16, main = "Influence Plot", 
              sub = "Leverage vs. Standardized Residuals with Cook's Distance")


