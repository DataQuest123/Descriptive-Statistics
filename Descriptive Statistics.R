## Load the packages
library(tidyverse)
library(readxl)

## Load the dataset
data = read_excel("C:/Users/ADMIN/Desktop/Data Science/Mastering Data Analysis with R/cardiovascular.xlsx")

## Inspect the data
glimpse(data)
anyNA(data)
any(duplicated(data))

## Data preprocessing
## Data type conversion
data = data %>% mutate_if(is.character, as.numeric)
data$male = as.factor(data$male)
data$TenYearCHD = as.factor(data$TenYearCHD)
data$education = as.factor(data$education)
data$currentSmoker = as.factor(data$currentSmoker)
data$prevalentStroke = as.factor(data$prevalentStroke)
data$prevalentHyp = as.factor(data$prevalentHyp)
data$diabetes = as.factor(data$diabetes)

## Remove missing values
data = drop_na(data)

## Descriptive Statistics
## Measures of Central tendency
mean(data$age, na.rm = T)
median(data$cigsPerDay, na.rm = T)

# Mode function
get_mode = function(x) {
  uniq = unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}
get_mode(data$sysBP)

## Measures of Dispersion
range(data$age, na.rm = T) # MIN & MAX
diff(range(data$age, na.rm = T)) # Range (max - min)
sd(data$age, na.rm = T)
var(data$age, na.rm = T)
IQR(data$age, na.rm = T)

## Shape of Distribution
library(e1071)
# Skewness
skewness(data$age, na.rm = T)
## A skewness of 0.23 is close to zero, which suggests that the distribution of age is approximately symmetric.
## Since it is positive, it indicates a slight right (positive) skew — meaning:
## There are a few older individuals who slightly pull the tail of the distribution to the right.
## However, the skew is small and not significant, so the distribution is close to normal.

# Kurtosis
kurtosis(data$age, na.rm = T)

## A kurtosis of -0.99 (i.e., less than 0) indicates a platykurtic distribution.
## The distribution has flatter tails and less peakedness than a normal distribution.
## There are fewer extreme values (outliers) than you'd expect in a normal distribution.

## Skewness tells you whether your data is symmetrical or leans to one side. In a perfectly symmetrical dataset, the left and right sides look like mirror images.
## If skewness is around 0, the distribution is roughly symmetrical.
## If skewness is positive, the right tail is longer. This means there are a few unusually high values, and most data are clustered to the left.
## If skewness is negative, the left tail is longer. This indicates some low extreme values, with most data grouped to the right.

## In practical terms:
## Values between -0.5 and +0.5 are generally considered fairly symmetrical.
## Values from ±0.5 to ±1 indicate moderate skewness.
## Values beyond ±1 suggest the data is strongly skewed and not well-balanced.

## Kurtosis – Understanding the Peakedness and Tails of Data
## Kurtosis measures how sharp or flat the distribution's peak is, and how heavy or light the tails are compared to a normal distribution.
## If kurtosis is close to 0 (this is excess kurtosis), your data has a normal peak and tails—nothing unusual.
## If kurtosis is greater than 0, your distribution has a sharper peak and fatter tails. This means more values are close to the mean, but also more extreme values (outliers) exist in the tails. This is called leptokurtic.
## If kurtosis is less than 0, your distribution has a flatter peak and thinner tails. This suggests values are more spread out and fewer outliers exist. This is called platykurtic.

## Real-World Implications:
## A positively skewed dataset (right skew) might represent something like income, where most people earn less but a few earn a lot.
## A negatively skewed dataset (left skew) might appear in test scores where most students perform well but a few score very low.
## A leptokurtic distribution may suggest higher risk due to more frequent extreme events (e.g., in finance).
## A platykurtic distribution usually indicates a more consistent dataset with fewer extreme values.

## Distributions
table(data$male)
prop.table(table(data$male))*100
table(data$TenYearCHD)
prop.table(table(data$TenYearCHD))*100

## Summary table
summary(data)

## Basic plots using base R
# Histogram
hist(data$totChol, col = "blue", main = "Histogram of Cholestral Levels", xlab = "Cholestral levels", ylab = "Frequency")

# Density plot
plot(density(data$age), xlab = "X", ylab = "Y", main = "Scatter plot", col = "red")

# Scatter plot
plot(data$sysBP, xlab = "X", ylab = "Y", main = "Scatter plot", col = "red")

# Bar plot
barplot(table(data$education), main = "Bar plot", col = "orange", xlab = "Skin color", ylab = "count")

# Boxplot
boxplot(data$age)

# Histogram
ggplot(data, aes(x = age))+
  geom_histogram(bins = 15, fill = "green", color = "black")+
  theme_bw()+
  labs(title = "Histogram of Age", x = "Age", y = "Frequency", caption = "cardiovascular (2025)")

# Plot histogram with normal curve
# Calculate mean and sd of age
mean_age <- mean(data$age, na.rm = TRUE)
sd_age <- sd(data$age, na.rm = TRUE)
ggplot(data, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean_age, sd = sd_age), 
                color = "red", size = 1) +
  theme_bw() +
  labs(title = "Histogram of Age with Normal Curve",
       x = "Age", y = "Density",
       caption = "Cardiovascular (2025)")

# Density Plots
ggplot(data, aes(x = age))+
  geom_density(fill = "green", color = "black")+
  theme_bw()+
  labs(title = "Density plot of Age", x = "Age", y = "Frequency", caption = "cardiovascular (2025)")

## Histogram + Density Plot
ggplot(data, aes(x = age))+
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightgreen", color = "black", alpha = 0.6) +
  geom_density(color = "blue", size = 1) +
  theme_bw() +
  labs(title = "Histogram and Density Plot of Age",
       x = "Age", y = "Density",
       caption = "Cardiovascular (2025)")

## Bar Chart
## Education
ggplot(data, aes(x = education))+
  geom_bar(color = "blue", fill = "steelblue")+
  theme_bw()+
  labs(title = "Bar Chart for Education Levels",
       x = "Education", y = "Frequency", caption = "cardiovascular(2025)")

## Education vs 
ggplot(data, aes(x = education, fill = TenYearCHD))+
  geom_bar(position = "dodge")+
  theme_bw()+
  labs(title = "Bar Chart for Education by TenYearCHD",
       x = "Education", y = "Frequency", caption = "cardiovascular(2025)")

## Box Plots
## Age
ggplot(data, aes(age)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot for Age", caption = "Cardiovascular(2025)") +
  theme_minimal()

## Stroke vs age
ggplot(data, aes(x = education, y = age, fill = education)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Age by Education", x = "Education", y = "Age") +
  theme_minimal()


## Summarize education counts
education_summary <- data %>%
  count(education) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label = paste0(education, ": ", percent, "%"))

## Create pie chart
ggplot(education_summary, aes(x = "", y = n, fill = education)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Distribution of Education Level")
glimpse(data)

## Violin Plot
## Use for: Distribution + density (good alternative to boxplots)
ggplot(data, aes(x = education, y = age)) + 
  geom_violin(color = "blue")+
  theme_bw()+
  labs(title = "Age Distribution Across Education Levels",
       x = "Education Levels", y = "Age")

## Line graphs
tb = read_excel("C:/Users/ADMIN/Desktop/Data Science/Datasets/Time Series/TB Incidence China.xlsx")
glimpse(tb)
colnames(tb) = make.names(colnames(tb))
ggplot(tb, aes(y= TB.incidence.per.100.000.populations., x = Time))+
  geom_line(color = "red")+
  theme_bw()+
  labs(x = "Year", y = "Incidence", title = "TB Incidence")


