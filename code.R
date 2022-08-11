install.packages("ggplot2")
install.packages("pastecs")
install.packages("corrplot")
install.packages("GGally")                 
install.packages("dplyr")
install.packages("ggpubr")
install.packages("scales")
install.packages("moments")
install.packages("ggstatsplot")
install.packages("superml")


library("GGally")  
library("readxl")
library(dplyr)
library(ggplot2)
library(pastecs)
library(corrplot)
library("ggpubr")
library(scales)
library(ggstatsplot)
library(superml)



#reading the file
library(readxl)
heyo <- read_excel("heyo.xlsx")
View(heyo)
head(heyo)

#reading a column
PriceColumn= read.table(file="clipboard", sep = "\t",header = F) 
PriceColumn



#statistics
summary(heyo)
stem(heyo$price)
stat.desc(heyo$price)
quantile(heyo$price)
mean(heyo$price)
sd(heyo$price)
print(" Price Coefficient of variation is: ") 
sd(heyo$price) / mean(heyo$price)

#Null:
colSums(is.na(heyo))
#Duplicated:
duplicated(heyo)
# Balance:
table(heyo$brand)
table(heyo$model)
table(heyo$year)
table(heyo$color)
table(heyo$status)
table(heyo$fuel)



#visualization


#1- Status

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(status), 
                      y = prop.table(stat(count)), fill = factor(status),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_x_discrete(labels = c("New", "Used"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'status', y = 'Percentage') +
  ggtitle("Distribution of Status labels") +
  common_theme

#2- Brand


common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(brand), 
                      y = prop.table(stat(count)), fill = factor(brand),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Brand', y = 'Percentage') +
  ggtitle("Distribution of Brand labels") +
  common_theme

#3- Case_Type
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(case_type), 
                      y = prop.table(stat(count)), fill = factor(case_type),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Case Type', y = 'Percentage') +
  ggtitle("Distribution of Case Types ") +
  common_theme

#4- fuel:

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(fuel), 
                      y = prop.table(stat(count)), fill = factor(fuel),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Fuel Types', y = 'Percentage') +
  ggtitle("Distribution of Fuel Types ") +
  common_theme

#5- model:

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(model), 
                      y = prop.table(stat(count)), fill = factor(model),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Model Types', y = 'Percentage') +
  ggtitle("Distribution of Model Types ") +
  common_theme

#5- year:

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(year), 
                      y = prop.table(stat(count)), fill = factor(year),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Years', y = 'Percentage') +
  ggtitle("Distribution of Model Production Year ") +
  common_theme

#5- color:

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = heyo, aes(x = factor(color), 
                      y = prop.table(stat(count)), fill = factor(color),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Color', y = 'Percentage') +
  ggtitle("Distribution of the Color of the car") +
  common_theme



# Box Plots to check Outliers: 
#1. Status:
ggplot(heyo, aes(x = factor(status), y = price)) + geom_boxplot() + 
  labs(x = 'Status', y = 'Price') +
  ggtitle("Boxplot of status and price") + common_theme


#2. Year:
ggplot(heyo, aes(x = factor(year), y = price)) + geom_boxplot() + 
  labs(x = 'year', y = 'Price') +
  ggtitle("Boxplot of year and price") + common_theme

#3. Model:

ggplot(heyo, aes(x = factor(model), y = price)) + geom_boxplot() + 
  labs(x = 'model', y = 'Price') +
  ggtitle("Boxplot of model and price") + common_theme


#4. case_type:

ggplot(heyo, aes(x = factor(case_type), y = price)) + geom_boxplot() + 
  labs(x = 'case_type', y = 'Price') +
  ggtitle("Boxplot of case_type and price") + common_theme

#5. Price:
boxplot(heyo$price)

#5. km:
boxplot(heyo$km)

# Visualization and Numerical Correlation:
#1. Price vs k  correlation : Negative correlation

ggplot(data=heyo[!is.na(heyo$price),], aes(x=km, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

cor(heyo$price[!is.na(heyo$price)], heyo$km[!is.na(heyo$km)])




#Normality Test and random sampling:

#1. Histogram

hist(heyo$price, col='steelblue', main='Price Distribution')

#2. Q-Q plot

qqnorm(heyo$price, main='Price Q-Q plot')
qqline(heyo$price)

#3. Shapiro-Wilk Test

shapiro.test(heyo$price)

#4. Kolmogorov-Smirnov Test

ks.test(heyo$price, 'pnorm')

#5. Random sampling with density and q plot distribution:

heyo = dplyr::sample_n(df,30)

ggdensity(heyo$price, 
          main = "Density plot of the data over the prices",
          xlab = "Prices")

ggqqplot(heyo$price,
         main = "Linear plot of the data over the prices",
         xlab = "Prices")
# Point estimations and confidence intervals:
# sample mean as point estimation:
mean(heyo$price, na.rm = TRUE)
# Sample size, std, margin
n <- 197
xbar <- mean(heyo$price, na.rm = TRUE)
s <- sd(heyo$price)
margin <- qt(0.975, df=n-1)*s/sqrt(n)
low <- xbar - margin
print(low)
high <- xbar + margin
print(high)
k <- sum(heyo$price < 240000)
p <- k/n
print("95% confidence interval, and estimate the percentage of cars price bellow 240,000:")
print(p)


#Hypothesis Testing:
#1. are the new car prices higher than the used ones
#2. Does the fuel affect the price of a car
#3. does the number of kilometers significantly decreases the car price of the same model
#4.Are two cars of the same model share the same price with the fuel or engine type


#Goodness of fit:

chisq <- chisq.test(df$price)
chisq
chisq$p.value
chisq$estimate
round(chisq$expected,2)
round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)


#ANOVA


#Application of non-parametric test

#Handelling the Outliers with:
lower_bound <- quantile(heyo$price, 0.01)
print(lower_bound)

upper_bound <- quantile(heyo$price, 0.99)
print(upper_bound)

outlier_ind <- which(heyo$price < lower_bound | heyo$price > upper_bound)
outlier_ind

print("Rows with Outliers are: ")
heyo[outlier_ind, ]
# replacing outliers with Q1 and Q2:
heyo[1, 10] = 620000
heyo[2, 10] = 620000
heyo[197, 10] = 267900
heyo[198, 10] = 267900


# Handelling the skewness:

# 1. Price skweness
skewness(heyo$price)
qqnorm(heyo$price)
qqline(heyo$price)

heyo$price <- log(heyo$price) 
skewness(heyo$price)
qqnorm(heyo$price)
qqline(heyo$price)



# Data Encoding: 
#1. brand:
label <- LabelEncoder$new()
print(label$fit(heyo$brand))
heyo$brand <- label$fit_transform(heyo$brand)
print(heyo$brand)

#2. model:
label <- LabelEncoder$new()
print(label$model(heyo$model))
heyo$model <- label$fit_transform(heyo$model)
print(heyo$model)

#3. fuel:
label <- LabelEncoder$new()
print(label$fuel(heyo$fuel))
heyo$fuel <- label$fit_transform(heyo$fuel)
print(heyo$fuel)

#4. gear:
label <- LabelEncoder$new()
print(label$gear(heyo$gear))
heyo$gear <- label$fit_transform(heyo$gear)
print(heyo$gear)

#5. case_type:
label <- LabelEncoder$new()
print(label$case_type(heyo$case_type))
heyo$case_type <- label$fit_transform(heyo$case_type)
print(heyo$case_type)


#6. color:
label <- LabelEncoder$new()
print(label$color(heyo$color))
heyo$color <- label$fit_transform(heyo$color)
print(heyo$color)

# standarization: 
scale(heyo)
# Data Correlation:

M = cor(heyo)
corrplot(M, method = 'shade')  


# Linear Regression: 
#Selection:
heyo = heyo[!names(heyo) %in% c("color")]


#install.packages('caTools')
library(caTools)

split = sample.split(heyo$price, SplitRatio = 0.7)
trainingset = subset(heyo, split == TRUE)
testset = subset(heyo, split == FALSE)


price.lm<-lm(price ~  + brand  + model  +  year +  status  +  fuel  +  gear + 
               + km  + case_type ,data = heyo)

summary(price.lm)

par(mfrow=c(2,2))
plot(price.lm)
par(mfrow=c(1,1))


