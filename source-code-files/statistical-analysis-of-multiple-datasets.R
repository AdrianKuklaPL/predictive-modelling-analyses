# --------------------------------------------
# DATA LOADING AND EXPLORATION
# --------------------------------------------
# Load the South African Heart Disease data from a remote source
# 'read.delim' reads tabular data, specifying ',' as the separator
sa <- read.delim('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', sep=',') 

# Preview the dataset structure and summary statistics
head(sa)      # View the first few rows of the dataset
summary(sa)   # Generate summary statistics for each variable

# Analyze the relationship between family history of heart disease and CHD (coronary heart disease)
table(sa$famhist, sa$chd)  # Contingency table of family history vs CHD

# --------------------------------------------
# FEATURE ENGINEERING AND CONTINGENCY ANALYSIS
# --------------------------------------------
# Create a categorical feature based on LDL levels and assess its relationship with CHD
test <- rep("<=6", length(sa$chd))
test[sa$ldl > 6] = ">6"
table(x = test, y = sa$chd)  # Contingency table for LDL levels and CHD

# Create a binary feature based on age and tobacco consumption thresholds
test2 <- rep(0, length(sa$chd))
test2[sa$age >= 51 & sa$tobacco > 7.5] = 1
table(test2, sa$chd)  # Contingency table for new binary feature and CHD

# Create another binary feature combining age and obesity thresholds
test5 <- rep(0, length(sa$chd))
test5[sa$age >= 35 & (sa$obesity > 15 | (sa$obesity <= 15 & sa$ldl > 5))] = 1

# --------------------------------------------
# DATA VISUALIZATION
# --------------------------------------------
# Visualize relationships between variables using boxplots
boxplot(sa$age ~ sa$chd, xlab = 'chd (1=yes)', ylab = 'Age (years)', main = 'Boxplot of age vs chd')
boxplot(sa$tobacco ~ sa$chd, xlab = 'chd (1=yes)', ylab = 'Tobacco', main = 'Boxplot of Tobacco vs chd')

# Scatter plots to explore correlations between variables
plot(sa$age, sa$tobacco, xlab = 'Age', ylab = 'Tobacco (kg)', main = 'Scatter plot of age vs tobacco consumption')
plot(sa$age, sa$tobacco, xlab = 'Age', ylab = 'Tobacco (kg)', main = 'Scatter plot of age vs tobacco consumption', type = 'n') 
points(sa$age[sa$chd == 0], sa$tobacco[sa$chd == 0], col = 'red')  # Points for non-CHD
points(sa$age[sa$chd == 1], sa$tobacco[sa$chd == 1], col = 'blue')  # Points for CHD
legend('topleft', c('No chd', 'chd'), col = c('red', 'blue'), pch = 1)  # Add a legend

# Pair plot to explore relationships between selected variables
pairs(sa[, c('age', 'tobacco', 'obesity')])  # Matrix of scatter plots

# Additional scatter plots for pairwise relationships
par(mfrow = c(2, 2))  # Set up 2x2 plot layout
plot(sa$age, sa$tobacco, xlab = 'Age', ylab = 'Tobacco (kg)', main = 'Scatter plot of age vs tobacco consumption', type = 'n')
points(sa$age[sa$chd == 0], sa$tobacco[sa$chd == 0], col = 'red')
points(sa$age[sa$chd == 1], sa$tobacco[sa$chd == 1], col = 'blue')

plot(sa$age, sa$obesity, xlab = 'Age', ylab = 'Obesity score', main = 'Scatter plot of age vs obesity score', type = 'n')
points(sa$age[sa$chd == 0], sa$obesity[sa$chd == 0], col = 'red')
points(sa$age[sa$chd == 1], sa$obesity[sa$chd == 1], col = 'blue')

plot(sa$obesity, sa$tobacco, xlab = 'Obesity score', ylab = 'Tobacco (kg)', main = 'Scatter plot of obesity score vs tobacco consumption', type = 'n')
points(sa$obesity[sa$chd == 0], sa$tobacco[sa$chd == 0], col = 'red')
points(sa$obesity[sa$chd == 1], sa$tobacco[sa$chd == 1], col = 'blue')

# Boxplot with customized colors
boxplot(sa$age ~ sa$chd, xlab = c('chd (1=yes)'), ylab = 'Age (years)', main = 'Boxplot of age vs chd', col = c('red', 'blue'))

par(mfrow = c(1, 1))  # Reset to single plot layout

# --------------------------------------------
# STATISTICAL SAMPLING AND BOOTSTRAPPING
# --------------------------------------------
# Load sample dataset (Cars) and visualize its distribution
data <- cars
View(data)
hist(data$speed, breaks = 20)  # Histogram of car speeds

# Perform random sampling
samp = sample(data$speed, size = 30, replace = TRUE)  # Random sample of size 30
samp
mean(samp)  # Mean of the sample

# Bootstrap sampling to calculate sample means
sample_means = rep(0, 1000)
for (i in 1:1000) {
  sample_means[i] = mean(sample(data$speed, size = 30, replace = TRUE))
} 
sample_means

# Visualize the distribution of bootstrap sample means
hist(sample_means, breaks = 20)
abline(v = mean(data$speed), col = 'red', lwd = 3)  # Add a vertical line for the true mean

# Calculate standard deviation of sample means (standard error)
sd(sample_means)  # Standard deviation of bootstrap sample means
sd(data$speed) / sqrt(30)  # Standard error using the formula

##### PROBABILITY DISTRIBUTIONS AND SIMULATIONS #####
# Simulating a normal distribution (X ~ N(mu, sigma^2)) with default parameters
x <- rnorm(1000)  # Generate 1000 samples from N(0, 1)
hist(x, breaks = 20)  # Histogram to visualize the distribution

# Simulating a normal distribution with specified mean and standard deviation
x <- rnorm(1000, mean = 4, sd = 3)  # Generate samples from N(4, 9)

# Simulating a binomial distribution (X ~ Bin(n, p))
x <- rbinom(100, size = 10, prob = 1/6)  # Generate 100 samples from Bin(10, 1/6)
barplot(table(x))  # Bar plot of frequencies of outcomes

# Distribution function: Compute cumulative probability P(X <= x) for N(2, 25)
pnorm(3, mean = 2, sd = 5) - pnorm(2, mean = 2, sd = 5)

# Density function: Compute probability P(X = x) for Bin(10, 0.2)
dbinom(3, size = 10, prob = 0.2)

# Compute the probability P(X > 10) for X ~ Bin(50, 0.1)
1 - pbinom(10, size = 50, prob = 0.1)  # Complement rule
pbinom(10, size = 50, prob = 0.1, lower.tail = FALSE)  # Using lower.tail argument

# Quantile calculations from probability distributions
round(qnorm(0.95), 2)  # 95th percentile of the standard normal distribution
round(qnorm(0.975), 2)  # 97.5th percentile of the standard normal distribution
qt(0.95, df = 6)  # 95th percentile of the t-distribution with 6 degrees of freedom

##### SIX NATIONS DATA SET: HYPOTHESIS TESTING AND CONFIDENCE INTERVALS #####

# Load and explore the Six Nations dataset
sixnations <- read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/predictive-modelling-analyses/main/data/sixnations.csv", header = TRUE)
str(sixnations)  # View the structure of the dataset

# Hypothesis testing for BMI of English players
bmi_irel <- sixnations$BMI[sixnations$Team == "England"]
t.test(bmi_irel, alternative = "greater", mu = 28.5, conf.level = 0.95)  # One-tailed t-test
t.test(bmi_irel, alternative = "two.sided", mu = 28.5, conf.level = 0.95)  # Two-tailed t-test

# Comments on t-test outputs:
# - Provides the test statistic (t), degrees of freedom (df), and p-value.
# - For one-tailed tests, R may return infinite confidence intervals.
# - For larger samples, t-tests approach z-tests in performance.

# Perform a two-sample t-test: Compare BMI of Irish and Italian players
bmi_irel <- sixnations$BMI[sixnations$Team == "Ireland"]
bmi_ita <- sixnations$BMI[sixnations$Team == "Italy"]
t.test(bmi_irel, bmi_ita, alternative = "greater", mu = 0, var.equal = TRUE)  # One-tailed test
t.test(bmi_irel, bmi_ita, alternative = "two.sided", mu = 0, var.equal = TRUE)  # Two-tailed test

##### CATEGORICAL DATA AND CHI-SQUARED TESTS #####

# Working with categorical variables: Converting strings to factors
string <- c("brown", "blue", "brown", "black", "green",
            "black", "brown", "green", "blue", "blue")
eyeCol <- factor(string)  # Convert string to factor (categorical variable)
levels(eyeCol)  # Extract unique categories
nlevels(eyeCol)  # Count the number of unique categories

# Chi-squared distribution overview:
# - Used in hypothesis testing for categorical data.
# - X ~ Chi^2(k), where k = degrees of freedom, mean = k, variance = 2k.
# Applications:
# 1. Test of independence: Check if two categorical variables are independent.
# 2. Goodness-of-fit test: Compare observed data to a hypothetical distribution.

# Chi-squared independence test is useful for analyzing contingency tables,
# while goodness-of-fit tests check whether observed distributions align with expected distributions.

##### DATA LOADING AND EXPLORATION #####

# Load the Titanic dataset
titanic <- read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/predictive-modelling-analyses/main/data/titanic.csv", header = TRUE)

# Explore the structure of the dataset
str(titanic)  # View variable types and dataset dimensions

##### DESCRIPTIVE STATISTICS AND PROPORTION ANALYSIS #####

# Analyze the survival of children
cond <- titanic$Age == "Child"  # Create a condition for selecting children
tab <- table(titanic$Survived[cond])  # Frequency table for children who survived and who didn’t
tab
round(prop.table(tab) * 100, 2)  # Convert frequencies to percentages of the total

# Two-way table of survival rates by age group (Child vs Adult)
tab <- table(titanic$Age, titanic$Survived)
round(prop.table(tab, margin = 1) * 100, 2)  # Row-wise percentages (proportions within each age group)

# Analyze survival rates for adult females
cond1 <- titanic$Age == "Adult"  # Condition for adults
cond2 <- titanic$Sex == "Female"  # Condition for females
tab <- table(titanic$Survived[cond1 & cond2])  # Frequency table for adult females who survived
tab
prop.table(tab) * 100  # Convert frequencies to percentages

##### CONTINGENCY TABLE ANALYSIS #####

# Survival rates by gender for adults
cond <- titanic$Age == "Adult"  # Condition for adults
tab <- table(titanic$Sex[cond], titanic$Survived[cond])  # Two-way table for Sex and Survived (for adults only)
tab
prop.table(tab, 1) * 100  # Row-wise percentages (Survival rates by gender)
prop.table(tab, 2) * 100  # Column-wise percentages (Gender distribution among survivors and non-survivors)

##### HYPOTHESIS TESTING AND ASSOCIATION ANALYSIS #####

# Analyze the relationship between economic class and survival status
tab <- table(titanic$Survived, titanic$Class)  # Contingency table for Survived and Class
tab

# Chi-squared test of independence
chisq.test(tab)  
# - Null hypothesis (H0): Survival is independent of economic class.
# - p-value < 0.05 indicates rejection of H0, suggesting evidence of association between economic class and survival.

# Proportional analysis of survival rates by class
round(prop.table(tab, 2), 2)  # Column-wise proportions to compare survival rates by class
# Observation: 1st Class is the only class with a higher proportion of survivors compared to non-survivors.

##### SIMPLE LINEAR REGRESSION #####

# Load and explore the football dataset
football <- read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/predictive-modelling-analyses/main/data/football.csv", header = TRUE)
str(football)       # Structure of the dataset
colnames(football)  # List column names
dim(football)       # Dimensions of the dataset

# Bivariate data exploration: Scatter plot and correlation
plot(x = football$ShotsTarg, y = football$Goals,
     main = "Shots on target and goals",
     xlab = "Shots on target", ylab = "Goals",
     col = "darkblue", pch = 15)  # Scatter plot of shots on target vs goals
cor(football$ShotsTarg, football$Goals)  # Correlation between shots on target and goals

plot(x = football$Corn, y = football$Goals,
     main = "Corners and goals",
     xlab = "Corners", ylab = "Goals",
     col = "red", pch = 17)  # Scatter plot of corners vs goals
cor(football$Corn, football$Goals)  # Correlation between corners and goals

# Fit a linear regression model
reg <- lm(Points ~ Goals, data = football)  # Response variable (Points) ~ Explanatory variable (Goals)
reg

# Visualize the regression line
plot(x = football$Goals, y = football$Points,
     main = "Regression of Points on Goals",
     xlab = "Goals", ylab = "Points")
abline(reg, col = "red")  # Add regression line

# Residual analysis: Checking model fit
e <- residuals(reg)  # Residuals (difference between observed and predicted values)
y_hat <- fitted(reg)  # Predicted values by the regression model
football$Points - y_hat  # Compare observed values and predictions
round((football$Points - y_hat) - e, 0)  # Verify residuals calculation

# Residual plot
plot(x = football$Goals, y = e,
     xlab = "Goals", ylab = "Residuals",
     main = "Residual plot")

# Residual standard error (RSE): Measure of prediction error
sig <- sigma(reg)  # Standard deviation of residuals
n <- nrow(football)
sig <- sqrt(deviance(reg) / (n - 2))  # RSE calculation using residual sum of squares
sig

# Add confidence bands to the residual plot
abline(h = c(-sig, +sig), col = "blue")  # 68% confidence bands (1 standard deviation)
abline(h = c(-2 * sig, +2 * sig), col = "green")  # 95% confidence bands (2 standard deviations)

# Statistical inference: Assessing model parameters and fit
summary(reg)
# - Regression coefficients: Intercept and slope (beta)
# - Residual standard error: Measure of model fit
# - R-squared: Proportion of variance explained by the model
# - p-value: Tests if slope coefficient is significantly different from zero

# Confidence intervals for regression coefficients
confint(reg, level = 0.90)  # 90% confidence intervals for all coefficients
confint(reg, parm = "Goals", level = 0.90)  # Specific coefficient (Goals)

# Prediction intervals
# Confidence interval for the mean points for teams scoring 75 goals
predict(reg, newdata = list(Goals = 75), interval = "confidence", level = 0.90)
# Prediction interval for points of a single team scoring 75 goals
predict(reg, newdata = list(Goals = 75), interval = "prediction", level = 0.90)

##### ANOVA: ONE-FACTOR ANALYSIS OF VARIANCE #####

# Hypothesis tests and confidence intervals for statistical analysis
# Normal distribution critical values
qnorm(0.05, lower.tail = FALSE)  # Compute the critical value (upper tail)

# p-value for a given test statistic
pnorm(1.65, lower.tail = FALSE)

# T-distributions for hypothesis testing
qt(0.01 / 2, df = 30, lower.tail = FALSE)  # Two-tailed critical value for t-distribution
pt(1.65, df = 20, lower.tail = FALSE)  # p-value for t-distribution with 20 degrees of freedom

# Convergence of t-distribution to normal distribution as sample size increases
pt(1.65, df = 100, lower.tail = FALSE)
pt(1.65, df = 200, lower.tail = FALSE)
pt(1.65, df = 1000, lower.tail = FALSE)

##### DATA LOADING AND EXPLORATION #####

# Load the diet dataset
dietData <- read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/predictive-modelling-analyses/main/data/diet.csv", header = TRUE)

# Explore the structure of the dataset
str(dietData)  # View variable types and dataset dimensions
table(dietData$diet)  # Frequency table for the categorical variable 'diet'

##### DESCRIPTIVE STATISTICS #####

# Compute summary statistics for the response variable by levels of the categorical factor (diet)
# Calculate the mean of weight differences grouped by diet
aggregate(weightDiff ~ diet, data = dietData, FUN = "mean")  # Sample mean by diet

# Calculate the variance of weight differences grouped by diet
aggregate(weightDiff ~ diet, data = dietData, FUN = "var")  # Sample variance by diet

##### ANALYSIS OF VARIANCE (ANOVA) #####

# Perform one-way ANOVA
# Analyze whether the mean weight difference (response variable) is affected by diet type (categorical explanatory variable)
ANOVA <- aov(weightDiff ~ diet, data = dietData) 
summary(ANOVA)

# Derive p-values and F-statistics manually
pf(6.197, 2, 75, lower.tail = FALSE)  # p-value for the F-statistic
qf(0.00323, 2, 75, lower.tail = FALSE)  # Quantile of the F-distribution
qf(0.05, 2, 75, lower.tail = FALSE)  # 5% critical value for the F-distribution

# Hypothesis Testing:
# - Null Hypothesis (H0): There is no difference in mean weight differences among the diet groups.
# - Alternative Hypothesis (HA): At least one diet group differs significantly in mean weight difference.
# The small p-value from the ANOVA test suggests rejecting H0, indicating significant differences between groups.

##### POST-HOC TESTING: TUKEY-KRAMER METHOD #####

# Perform pairwise comparisons using Tukey's Honest Significant Difference (HSD) method
TukeyHSD(ANOVA)
# Interpretation:
# - `diff`: Difference in sample means between pairs of diets
# - `lwr` and `upr`: 95% confidence interval for the difference
# - `p adj`: Adjusted p-value for pairwise comparisons
# At the 5% significance level, diet C is significantly different from the other diets.

##### INTERPRETATION OF MEAN DIFFERENCES #####

# Recompute the mean weight difference grouped by diet for comparison
aggregate(weightDiff ~ diet, data = dietData, FUN = "mean")
# This shows the difference in mean weight change across the three diet groups.

##### Law of Large Numbers #####
# Law of large numbers in statistics state that an observed sample mean from a large enough sample will be close to the true population mean and it will converge
# to the population mean as the sample grows larger.
# Moreover, central tendency theorem states that a large sample size should converge to the population's distribution.

##### Central Limit Theorem #####
# The central limit theorem states that under specific conditions the sum of a large number of random variables is approximately normal, no matter
# what the distribution of the random variables is.
# For example, X ~ Bernoulli(p) -> Coin toss

##### Probability Distributions ##### 

# A random variable is a function that maps the result of a random process to a numeric value such as coin toss, rolling dice, height,
# IQ scores, length of phone call, number of people who are entering a shop 

# Uniform distribution (continuous), all outcomes in a specified interval have the same probability of occurrence. Let X ~ Unif(a,b)
# Mean = (a+b)/2, Variance = (b-a)^2/12
# Random event: rolling a fair die, random number generators

# Normal (Gaussian) distribution, continuous symmetrical distribution and bell-shaped curve. Let X be a random variable such that X ~ N(mu, sigma^2)
# In this case the mean and the variance are the parameters of the distribution.
# Physical quantities of a sum of independent processes/events often have distributions that are approximately normal.
# Random event: height, daily returns of stock, measurement errors in experiments

# Chi-square distribution (continuous), uses for variance estimation, analysis of variance (ANOVA), hypothesis testing. Let X ~ Chi(k), where k = df
# Mean = k (degrees of freedom), Variance= 2k
# Random event: distribution of sample variances, sum of squared standard normal variables, outcome of chi-squared goodness of fit tests

# t-distribution (continuous), similar to normal distribution but with heavier tails, used for estimating the mean of a normally distributed population
# when the sample size is small (n<30) and the population standard deviation is unknown. Let X ~ t(v), where v is the degrees of freedom.
# No need to specify mean as it's assumed to be zero for a standard t distribution and the variance is constant and unknown.
# Mean = 0 (for a standard t-distribution)
# Variance = v/(v-2), for v > 2, where v is df
# Random event: estimation of a mean of a small sample size, ANOVA where the number of groups/levels is 2 and hypothesis testing for small samples.

# Bernoulli distribution (discrete) where a single trial is performed with exactly two outcomes. X ~ Bernoulli(p)
# Mean = p, Variance = p(1-p)
# Random event: 1 coin toss, whether one likes a TV show (i.e., an indicator function), whether an even number was the outcome of a dice roll

# Binomial distribution, (discrete) describes the number of successes in a fixed number of independent Bernoulli trials with same probability
# of success. Let X ~ Bin(n,p)
# Mean = np, Variance = np(1-p)
# Random event: number of heads when flipping a coin 20 times, number of defective items in an inventory, number of correct answers in an exam

# Geometric distribution (discrete) describes number of trials required to get the first success in a sequence of independent Bernoulli trials.
# Let X ~ Geo(p), there's two different ways of describing the distribution, either k number of successes or k number of failures.
# Mean = 1/p, Variance = (1-p)/p^2
# Random event: number of coin flips required to get the first tail, number of defective items until the first non-defective item was found.

# Negative binomial distribution (discrete) which is an extension to the geometric distribution which shows the number of of trials required
# to get a certain number of successes.Let X ~ NB(r, p), where r is the number of successes
# Mean = r/p, Variance = r(1-p)/p^2
# Random event: number of sales calls required to close 10 sales, number of breakdowns before achieving a specific number of machine repairs

# Poisson distribution (discrete), number of events occurring in an interval of time/space. Assumes independent events and a constant mean rate.
# Mean = rate (lambda), Variance = rate (lambda)
# Random event: Number of emails received in an hour, number of customers arriving in a store in a day, number of accidents during a week,
# Number of claims reported to an insurer during a week.

# Exponential distribution (continuous), describes the time between events in a Poisson process.Let X ~ Exp(rate = lambda) or X ~ Exp(scale = Beta = 1/lambda)
# Note that the exponential distribution can be described using a scale parameter (how quickly the probability of longer times or larger values decreases).
# The scale parameter is the mean of the distribution. It most often represents the average time until an event occurs.
# While the rate parameter shows the average rate of occurrences of an event in a specified time interval (i.e, Poisson rate parameter lambda)
# Mean = 1/lambda = Beta, Variance = 1/lambda^2 = Beta^2
# Random event: time until the next customer arrives at the shop, time between failures of machinery, time between weather events like a tornadoes etc.,

# Gamma distribution (continuous), it generalizes the exponential distribution, commonly used to model waiting times. Let X ~ Gamma(alpha, lambda)
# Mean = alpha/lambda, Variance = alpha / lambda^2
# Random event: time required for multiple events to occur, waiting time until the nth event in a Poisson process.

# Pareto distribution (continuous), Pareto distribution is a continuous probability distribution that is used to model distributions with a heavy tail,
# Resembles the idea of the Pareto Principle, whereby 80% of the effects is due to 20% of the causes. Let X ~ Pa(alpha = shape, Beta = scale)
# Mean = alpha * Beta / (alpha - 1), Variance = alpha * Beta ^2 / [(alpha - 1)^2 * ( alpha - 2)]
# Random event: The Pareto distribution is commonly used to model wealth distribution (small % of humans control most wealth), size of cities
# distribution of natural resources such as crude oil, natural gas etc.,

# Beta distribution (continuous), defined on interval [0,1] and most often used to model probabilities or proportions. Let X ~ beta(alpha, Beta)
# Mean = alpha / (alpha + Beta), Variance = (alpha * Beta) / [(alpha + Beta)^2 * (alpha + Beta +1)] 
# Random event: used in distribution of probabilities in Bayesian statistics (prior distribution used as a baseline for the posterior distribution),
# or percentage of votes received by a candidate, probability of success in a a given process.

