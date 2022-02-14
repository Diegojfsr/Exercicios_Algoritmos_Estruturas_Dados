library(tidyverse)
library(caret)

df <- read.csv("HousingData.csv", sep = ',', dec = ',')

# Lista de Regressao Polinomial

ggplot(df, aes(x = df$LSTAT, y = df$MEDV))+
  geom_point() +
  stat_smooth()

## Regressao Polinomial 
#MEDV = b0 + b1*LSTAT + b2*LSTAT^2

df <- read.csv("HousingData.csv")
ggplot(df, aes(x=df$LSTAT, y=df$MEDV))+
  geom_point() +
  stat_smooth()


pm_model <- lm(MEDV ~ LSTAT + (LSTAT^2), data = df)

print(pm_model)
summary(pm_model)



anova <- aov(MEDV ~ LSTAT + (LSTAT^2), data = df)
summary(anova)
