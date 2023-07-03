library("ggplot2")
library("dplyr")
library("gridExtra")
library("GGally")
library("memisc")
library("pander")
library("corrplot")

wine <- read.csv('wineQualityReds.csv')

#Converting Wine quality into a ordered factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new 'rating' variable into the dataframe for different quality range

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

wine$X = factor(wine$X)
#Structure of the Dataframe

str(wine)

#Summary of the dataframe

summary(wine)

#Univariate plots

#Quality and rating
ggplot(data = wine, aes(x = quality)) +
  stat_count(width = 1, color = 'black',fill = I('orange'))

ggplot(data = wine, aes(x = rating)) +
  stat_count(width = 1, color = 'black',fill = I('blue'))



summary(wine$fixed.acidity)  #Median = 7.9 but some outliers dragged the mean upto 8.32
summary(wine$volatile.acidity)
summary(wine$citric.acid)
summary(wine$residual.sugar)
summary(wine$chlorides)
summary(wine$free.sulfur.dioxide)
summary(wine$total.sulfur.dioxide)
summary(wine$density)
summary(wine$pH)
summary(wine$sulphates)
summary(wine$alcohol)


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, ncol = 4)


#Bivariate analysis
#Correlation table
c <- cor(
  wine %>%
    # first we remove unwanted columns
    dplyr::select(-X) %>%
    dplyr::select(-rating) %>%
    mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)

pandoc.table(c)


#Fixed acidity : Doesn't seem to have much effect
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_boxplot()

#Volatile Acidity : Seems to have negative effect. With increase, quality seems to go down
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_boxplot()

#Citric acid (Better wines tend to have higher citric acid)
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_boxplot()

#Residual Sugar(Almost has no effect to quality. This is contrary to previous assumption)

ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_boxplot()

#Chlorides

ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_boxplot()

#Free SO2(We see too little and we get a poor wine and too much : we get an average wine)

ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_boxplot()

#Total SO2(Just like free SO2)

ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_boxplot()

#Density(Better wines tend to have lower densities but is it due to alcohol content?)

ggplot(data=wine, aes(x=quality, y=density)) +
  geom_boxplot()

#pH(Better wines seems to be more acidic. Now let's see contribution of each acid on pH)

ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_boxplot()


#Contribution of each acid to pH(We see all of them has negative correlation on pH except 
#volatile acidity. But how's that possible! Is it possible that there is a Simson's effect?)


ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point() +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("log10(fixed.acidity)") +
  geom_smooth(method="lm")


ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point() +
  scale_x_log10() +
  xlab("log10(citric.acid)") +
  geom_smooth(method="lm")


#Sulphates(better wines seems to have higher sulphates. Although medium wines have many outliers)

ggplot(data=wine, aes(x=quality, y=sulphates)) +
  geom_boxplot()

#Alcohol(Better wines have higher alcohol)

ggplot(data=wine, aes(x=quality, y=alcohol)) +
  geom_boxplot()


#Linear model test(From R squared value, it seems alcohol contributes only 22% to the quality variance)
alcoholQualityLM <- lm(as.numeric(quality) ~ alcohol,
                       data = wine)
summary(alcoholQualityLM)
df = data.frame(wine$quality )
df$predictions <- predict(alcoholQualityLM, wine)
df$error <- (df$predictions - as.numeric(wine$quality))/as.numeric(wine$quality)

ggplot(data=df, aes(x=wine.quality, y=error)) +
  geom_boxplot()

#Putting a Cor test together

simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')

correlations


#Making the linear model

set.seed(1221)
training_data <- sample_frac(wine, .6)
test_data <- wine[ !wine$X %in% training_data$X, ]
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + sulphates)
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m2, ~ . + pH)
mtable(m1,m2,m3,m4,m5,m6)


df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data=df, aes(x=quality,y=error)) +
  geom_point()


#Final plots
ggplot(data=wine, aes(y=alcohol, x=quality)) + 
  geom_boxplot() +
  xlab("alcohol concentration (% by volume)") +
  ggtitle("Influence of alcohol on wine quality")


ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point() +
  scale_y_continuous(limits=c(0.3,1.5)) +
  ylab("potassium sulphate (g/dm3)") +
  xlab("alcohol (% by volume)") +
  scale_color_brewer() +
  ggtitle("Alcohol and sulphates over wine quality")


df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data=df, aes(x=quality,y=error)) +
  geom_point() +
  ggtitle("Linear model errors over expected quality")

