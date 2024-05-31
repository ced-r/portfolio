install.packages("bootstrap")
library(tidyverse)
library(bootstrap)
library(ggplot2)


#### Question 2 - JUST TO CHECK MY ANSWERS ####
q2 <- c(251.3, 250.6, 250.9, 249.8)
median(q2, na.rm = TRUE)
jk.samples.median <- jackknife(q2, theta = median, na.rm = TRUE)
jk.samples.median$jack.values 
mean(jk.samples.median$jack.values)  

jk.samples.median$jack.se
jk.samples.median$jack.bias


#### Question 3 ####
# Import the dataset Melbourne House Prices
# https://www.kaggle.com/anthonypino/melbourne-housing-market
# Price: Price in Australian dollars

setwd(r"(A:\OneDrive\OneDrive - Kaplan\1. Murdoch (2022-2023)\Sem3\ICT513 Data Analytics)")
hdata <- read.csv("Melbourne_House_Price_Subset.csv")

str(hdata)
names(hdata)
glimpse(hdata)
unique(hdata$Suburb)

hdata$Price

hdata$Suburb <- as.factor(hdata$Suburb)

## There are many N/A values for Price variable
sum(is.na(hdata$Price)) # 217 missing values

## Data Visualisation ##
boxplot(hdata$Price,
        ylab = "Price in Australian Dollars",
        main="Boxplot of Melbourne House Prices")

boxplot.stats(hdata$Price)$out

boxplot(hdata$Price ~ hdata$Suburb,
        ylab = "Price in Australian Dollars",
        xlab = "Suburbs",
        main="Boxplot of comparison of Melbourne House Prices between Suburbs")

## ggplot2
ggplot(data = hdata, aes(x = "", y = Price)) + 
  geom_boxplot() + 
  labs(title="Boxplot of Melbourne House Prices",
       y = "Price in Australian Dollars",
       x = "Houses")

## Histogram
hist(hdata$Price, xlab = "Price in Australian Dollars", 
     main="Histogram of Melbourne House Prices")

hist(hdata$Price, freq = FALSE,
     xlab = "Price in Australian Dollars", 
     main="Histogram and density plot of Melbourne House Prices")

points(density(hdata$Price, na.rm=T), type="l", col="red", lwd = 2)


c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(144,238,144, max = 255, alpha = 80, names = "lt.green")

hist(hdata$Price[hdata$Suburb == "Brunswick"], 
     xlab = "Price in Australian Dollars", 
     main="Histogram of Melbourne House Prices", 
     col = c1)

hist(hdata$Price[hdata$Suburb == "Brunswick East"], 
     col = c2, add = TRUE)

hist(hdata$Price[hdata$Suburb == "Brunswick West"], 
     col = c3, add = TRUE)

hdata %>% count(Suburb)


qqnorm(hdata$Price, main = "Normal Q-Q plot of prices of houses")
qqline(hdata$Price, col="red", lwd = 2)


## Measures
hdata %>%  
  group_by(Suburb) %>% 
  summarise("Median" = median(Price, na.rm=T))

hdata %>%  
  group_by(Suburb) %>% 
  summarise("Interquartile Ranges" = IQR(Price, na.rm=T))

summary(hdata$Price, na.rm=T)


# Measures of dispersion 

# Range
min(hdata$Price, na.rm=T)
max(hdata$Price, na.rm=T)
max(hdata$Price, na.rm=T) - min(hdata$Price, na.rm=T)
range(hdata$Price, na.rm=T)

# sample variance and sample standard deviation
var(hdata$Price, na.rm=T)
sd(hdata$Price, na.rm=T)

quantile(hdata$Price, probs=c(0, 0.25, 0.50, 0.75, 1), na.rm=T)
IQR(hdata$Price, na.rm=TRUE) 

##### Q3b #####

## Use of median
hdata %>%  
  group_by(Suburb) %>% 
  summarise("Median House Prices" = median(Price, na.rm=T))

median(hdata$Price, na.rm=T)

## Jackknife
## Median
jk.samples.median <- jackknife(hdata$Price, theta = median, na.rm = TRUE)
jk.samples.median$jack.values 
mean(jk.samples.median$jack.values)  

jk.samples.median$jack.se
jk.samples.median$jack.bias

plot(density(jk.samples.median$jack.values), 
     xlab="Median of prices",
     main="Density plot of sampling distribution of median housing prices (Jackknife samples)")

# Bias
median(hdata$Price, na.rm = TRUE) - median(jk.samples.median$jack.values) 

## IQR
jk.samples.IQR <- jackknife(hdata$Price, theta = IQR, na.rm = TRUE)
jk.samples.IQR$jack.values 
mean(jk.samples.IQR$jack.values)  

jk.samples.IQR$jack.se
jk.samples.IQR$jack.bias

plot(density(jk.samples.IQR$jack.values), 
     xlab="Median of prices",
     main="Density plot of sampling distribution of IQR of housing prices (Jackknife samples)")
##Bootstrap

# Median
bs.samples.median <- bootstrap(hdata$Price, nboot = 10000, theta = median, na.rm=TRUE)
bs.samples.median$thetastar
mean(bs.samples.median$thetastar)

plot(density(bs.samples.median$thetastar), 
     xlab="Median of prices",
     main="Density plot of sampling distribution of median housing prices using 10,000 bootstrap samples")

## Obtain the standard deviation of bootstrap replicates
sd(bs.samples.median$thetastar)
var(bs.samples.median$thetastar)

median(hdata$Price, na.rm=TRUE) - mean(bs.samples.median$thetastar) # Bias for bootstrap samples

# IQR
bs.samples.IQR <- bootstrap(hdata$Price, nboot = 10000, theta = IQR, na.rm=TRUE)
bs.samples.IQR$thetastar
mean(bs.samples.IQR$thetastar)

plot(density(bs.samples.IQR$thetastar), 
     xlab="IQR of prices",
     main="Density plot of sampling distribution of IQR of housing prices using 10,000 bootstrap samples")

## Obtain the standard deviation of bootstrap replicates
sd(bs.samples.IQR$thetastar) #Bootstrap estimate of standard error
var(bs.samples.IQR$thetastar)

IQR(hdata$Price, na.rm=TRUE) - mean(bs.samples.median$thetastar) # Bias for bootstrap samples


### Computation of confidence interval 
# Compute 95% confidence interval for bootstrap estimator
quantile(bs.samples.median$thetastar, probs = c(0.025,0.975), na.rm = TRUE)
quantile(bs.samples.IQR$thetastar, probs = c(0.025,0.975), na.rm = TRUE)


#### Question 4 ####
# install.packages("gcookbook")
library(gcookbook)
?heightweight

glimpse(heightweight)
min(heightweight$heightIn)
min(heightweight$weightLb)

library(jtools)
library(gridExtra)

# Linearity check
ggplot(data = heightweight, aes(x = heightIn, y = weightLb)) + 
  geom_point(colour = "deeppink",
             na.rm = TRUE,
             size = 3) +
  geom_smooth(method = "lm", colour = "deepskyblue4", se = FALSE) # line of best fit

## After checking for linearity, we decide to go ahead with the model fitting

model.1 <- lm(heightIn ~ weightLb, data = heightweight)
summary(model.1)
summ(model.1, confint = TRUE)

names(model.1)

model.1$residuals


# 2. Normality check - Assessing the normality assumption (check for the normality of residuals)

# Create QQ plot
qqnorm(model.1$residuals, main = "QQ Plot of Residuals for Fit of weightLb on heightIn")
qqline(model.1$residuals, col="red", lw=2)

ggplot(data = data.frame(model.1$residuals), aes(sample = model.1$residuals)) +
  geom_qq(colour = "blue", size = 2, alpha = 0.25) + 
  geom_qq_line(colour = "red", size = 1) +
  labs(y = "Sample Quantiles",
       x = "Theoretical Quantiles")

# 3. Homocedasticity (equal variance) Assumption 
# Scatterplot with residuals against fitted values
plot(model.1$fitted.values, model.1$residuals, 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, col = 2)

plot(model.1, 1) # 2 for Q-Q plot

# 4. Independence of observation assumption 


## 
pairs(heightIn ~ weightLb + log(weightLb), data = heightweight, lower.panel = NULL)
pairs(log(heightIn) ~ weightLb + log(weightLb), data = heightweight, lower.panel = NULL)


# Fit a linear regression model 
model.1 <- lm(heightIn ~ weightLb, data = heightweight)
summary(model.1)

# Check for normality
qq.plot.1 <- ggplot(data = data.frame(model.1$residuals), 
                    aes(sample = model.1$residuals)) +
  geom_qq(colour = "red") +
  geom_qq_line(colour = "blue") +
  labs(x = "Theoretical quantile",
       y = "Sample quantile",
       title = "QQ Plot before any log-transformation")

# Homocedasticity check
residfit.1 <- ggplot(data = data.frame(model.1$residuals, 
                                       model.1$fitted.values)) + 
  geom_point(aes(x = model.1$fitted.values, y = model.1$residuals)) +
  geom_hline(yintercept = 0, colour = "blue") +
  labs(title = "Scatterplot of residuals against fitted values before any log-tranformation",
       x = "fitted values",
       y = "residuals")



# Fit a linear regression model 
model.2 <- lm(log(heightIn) ~ weightLb, data = heightweight)
summary(model.2)

# Check for normality
qq.plot.2 <- ggplot(data = data.frame(model.2$residuals), 
                    aes(sample = model.2$residuals)) +
  geom_qq(colour = "red") +
  geom_qq_line(colour = "blue") +
  labs(x = "Theoretical quantile",
       y = "Sample quantile",
       title = "QQ Plot for log(heightIn), weightLb")

# Homocedasticity check
residfit.2 <- ggplot(data = data.frame(model.2$residuals, 
                                       model.2$fitted.values)) + 
  geom_point(aes(x = model.2$fitted.values, y = model.2$residuals)) +
  geom_hline(yintercept = 0, colour = "blue") +
  labs(title = "Scatterplot of residuals against fitted values, log(heightIn), weightLb",
       x = "fitted values",
       y = "residuals")


# Fit a linear regression model 
model.3 <- lm(heightIn ~ log(weightLb), data = heightweight)
summary(model.3)

# Check for normality
qq.plot.3 <- ggplot(data = data.frame(model.3$residuals), 
                    aes(sample = model.3$residuals)) +
  geom_qq(colour = "red") +
  geom_qq_line(colour = "blue") +
  labs(x = "Theoretical quantile",
       y = "Sample quantile",
       title = "QQ Plot for heightIn, log(weightLb)")

# Homocedasticity check
residfit.3 <- ggplot(data = data.frame(model.3$residuals, 
                                       model.3$fitted.values)) + 
  geom_point(aes(x = model.3$fitted.values, y = model.3$residuals)) +
  geom_hline(yintercept = 0, colour = "blue") +
  labs(title = "Scatterplot of residuals against fitted values, heightIn, log(weightLb)",
       x = "fitted values",
       y = "residuals")


# Fit a linear regression model 
model.4 <- lm(log(heightIn) ~ log(weightLb), data = heightweight)
summary(model.4)

# Check for normality
qq.plot.4 <- ggplot(data = data.frame(model.4$residuals), 
                    aes(sample = model.4$residuals)) +
  geom_qq(colour = "red") +
  geom_qq_line(colour = "blue") +
  labs(x = "Theoretical quantile",
       y = "Sample quantile",
       title = "QQ Plot, log(heightLn), log(weightLb)")

# Homocedasticity check
residfit.4<- ggplot(data = data.frame(model.4$residuals, 
                                       model.4$fitted.values)) + 
  geom_point(aes(x = model.4$fitted.values, y = model.4$residuals)) +
  geom_hline(yintercept = 0, colour = "blue") +
  labs(title = "Scatterplot of residuals against fitted values, log(heightLn), log(weightLb)",
       x = "fitted values",
       y = "residuals")




library(gridExtra)

grid.arrange(qq.plot.1, qq.plot.2, qq.plot.3, qq.plot.4 )
grid.arrange(residfit.1, residfit.2, residfit.3, residfit.4)

