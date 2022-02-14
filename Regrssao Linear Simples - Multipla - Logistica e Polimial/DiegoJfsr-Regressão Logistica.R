library(tidyverse)
library(caret)


df <- read.csv("framingham.csv", sep = ',', dec = ',')

# Ajuste do Modelo de regressao

model <- glm(DCF ~ ., data = df, family = "binomial")
summary(model)
print(model)

df$probability <- predict(model, df,type = "response")
df$DCF_pred <- ifelse(df$probability > 0.5, 1, 0)

df$DCF_pred <- as.factor(df$DCF_pred)
df$DCF <- as.factor(df$DCF)

cm <- confusionMatrix(df$DCF_pred, df$DCF)
print(cm$table)

accuracy <- mean(df$DCF_pred == df$DCF)
print(accuracy)

# Taxa de acerto do modelo 85%
