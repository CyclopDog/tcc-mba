# Bibliotecas
library(tidyverse)
library(Boruta)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

# Importação dos dados
data <- read.csv("Churn_Modelling.csv")

# Remove colunas com dados não utilizados
data$RowNumber <- NULL
data$Surname <- NULL

# Substitui dados vazios por NA
data[data == ""] <- NA

# Substitui 1 e 0 por Yes e No
data$Exited[data$Exited == 1] <- "Yes"
data$Exited[data$Exited == 0] <- "No"
data$IsActiveMember[data$IsActiveMember == 1] <- "Yes"
data$IsActiveMember[data$IsActiveMember == 0] <- "No"
data$HasCrCard[data$HasCrCard == 1] <- "Yes"
data$HasCrCard[data$HasCrCard == 0] <- "No"

# Trata dados categóricos como factor
data$Geography = as.factor(data$Geography)
data$Gender = as.factor(data$Gender)
data$Exited = as.factor(data$Exited)
data$IsActiveMember = as.factor(data$Exited)
data$HasCrCard = as.factor(data$Exited)

# Define a seed para reprodução
set.seed(123)

# Levantamento de variáveis importantes
boruta.train <- Boruta(Exited~.-CustomerId, data = data, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
plot(final.boruta, xlab = "", xaxt = "n")

axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# Divide os dados em treinamento e teste
trainIndex <- createDataPartition(data$Exited, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- randomForest(Exited~.-CustomerId, data = trainData, ntree = 100)

# Valida a importância das variáveis
varImportance <- importance(model)
print(varImportance)

# Análise gráfica das variáveis numéricas
summary(data$Age)
hist(data$Age, xlab = "Idade", ylab = "Frequência", main = "Histograma de Idade")
summary(data$Age[data$Exited == "Yes"])
summary(data$Age[data$Exited == "No"])
boxplot(data$Age ~ data$Exited, xlab = "Churn", ylab = "Idade", main = "Comparação de Idade por Churn")

summary(data$CreditScore)
hist(data$CreditScore, xlab = "Pontuação de Crédito", ylab = "Frequência", main = "Histograma de Pontuação de Crédito")
summary(data$CreditScore[data$Exited == "Yes"])
summary(data$CreditScore[data$Exited == "No"])
boxplot(data$CreditScore ~ data$Exited, xlab = "Churn", ylab = "Pontuação de Crédito", main = "Comparação de Pontuação de Crédito por Churn")

summary(data$Balance)
hist(data$Balance, xlab = "Saldo", ylab = "Frequência", main = "Histograma de Saldo")
summary(data$Balance[data$Exited == "Yes"])
summary(data$Balance[data$Exited == "No"])
boxplot(data$Balance ~ data$Exited, xlab = "Churn", ylab = "Saldo", main = "Comparação de Saldo por Churn")

summary(data$NumOfProducts)
hist(data$NumOfProducts, xlab = "Quantidade de Produtos", ylab = "Frequência", main = "Histograma de Quantidade de Produtos")
summary(data$NumOfProducts[data$Exited == "Yes"])
summary(data$NumOfProducts[data$Exited == "No"])
boxplot(data$NumOfProducts ~ data$Exited, xlab = "Churn", ylab = "Quantidade de Produtos", main = "Comparação de Quantidade de Produtos por Churn")

summary(data$Tenure)
hist(data$Tenure, xlab = "Permanência", ylab = "Frequência", main = "Histograma de Permanência")
summary(data$Tenure[data$Exited == "Yes"])
summary(data$Tenure[data$Exited == "No"])
boxplot(data$Tenure ~ data$Exited, xlab = "Churn", ylab = "Permanência", main = "Comparação de Permanência por Churn")

summary(data$EstimatedSalary)
hist(data$EstimatedSalary, xlab = "Salário Estimado", ylab = "Frequência", main = "Histograma de Salário Estimado")
summary(data$EstimatedSalary[data$Exited == "Yes"])
summary(data$EstimatedSalary[data$Exited == "No"])
boxplot(data$EstimatedSalary ~ data$Exited, xlab = "Churn", ylab = "EstimatedSalary", main = "Comparação de Salário Estimado por Churn")

# Análise gráfica das variáveis categóricas
table(data$Geography)
table(data$Geography[data$Exited == "Yes"])
table(data$Geography[data$Exited == "No"])
ggplot(data, aes(x = Geography)) +
  geom_bar()
tabela_contingencia <- table(data$Geography, data$Exited)
df <- as.data.frame.table(tabela_contingencia)
ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Geography", y = "Count", fill = "Exited", title = "Gráfico de Barras Agrupadas") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

table(data$Gender)
table(data$Gender[data$Exited == "Yes"])
table(data$Gender[data$Exited == "No"])
ggplot(data, aes(x = Gender)) +
  geom_bar()
tabela_contingencia <- table(data$Gender, data$Exited)
df <- as.data.frame.table(tabela_contingencia)
ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "Exited", title = "Gráfico de Barras Agrupadas") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

table(data$HasCrCard)
table(data$HasCrCard[data$Exited == "Yes"])
table(data$HasCrCard[data$Exited == "No"])
ggplot(data, aes(x = HasCrCard)) +
  geom_bar()
tabela_contingencia <- table(data$HasCrCard, data$Exited)
df <- as.data.frame.table(tabela_contingencia)
ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "HasCrCard", y = "Count", fill = "Exited", title = "Gráfico de Barras Agrupadas") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

table(data$IsActiveMember)
table(data$IsActiveMember[data$Exited == "Yes"])
table(data$IsActiveMember[data$Exited == "No"])
ggplot(data, aes(x = IsActiveMember)) +
  geom_bar()
tabela_contingencia <- table(data$IsActiveMember, data$Exited)
df <- as.data.frame.table(tabela_contingencia)
ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "IsActiveMember", y = "Count", fill = "Exited", title = "Gráfico de Barras Agrupadas") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

# Define as variáveis preditoras e a variável de resposta
predictors <- c("CreditScore", "Geography", "Age", "Balance", "NumOfProducts", "HasCrCard", "IsActiveMember", "Tenure", "EstimatedSalary")

# Treina o modelo de Random Forest
rf_model <- randomForest(Exited~., data = trainData[, c(predictors, "Exited")], ntree = 100)

# Faz previsões no conjunto de teste
predictions <- predict(rf_model, newdata = testData[, predictors])

# Acurácia
accuracy <- mean(predictions == testData$Exited)
print(accuracy)

# Matriz de confusão
confusionMatrix(predictions, testData$Exited)

# Curva ROC e AUC
roc_curve <- roc(testData$Exited, as.numeric(predictions == "Yes"))
auc <- auc(roc_curve)
print(auc)

# Define o esquema de validação cruzada manualmente
folds <- createFolds(trainData$Exited, k = 5)

# Cria um vetor para armazenar as métricas de avaliação
accuracy <- numeric(length = length(folds))

# Itera sobre as partições e treina/avalia o modelo
for (i in 1:length(folds)) {
  # Divide os dados em treinamento e teste
  train_indices <- unlist(folds[-i])
  test_indices <- folds[[i]]
  train_data <- trainData[train_indices, ]
  test_data <- trainData[test_indices, ]
  
  # Treina o modelo randomForest
  rf_model <- randomForest(Exited~.-CustomerId, data = train_data, ntree = 100, mtry = 3)
  
  # Faz previsões no conjunto de teste
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calcula a acurácia no conjunto de teste
  accuracy[i] <- mean(predictions == test_data$Exited)
}

# Calcula a acurácia média da validação cruzada
mean_accuracy <- mean(accuracy)
print(mean_accuracy)

# Aplicação do modelo
# Faz previsões no conjunto de teste ou em novos dados
predictions <- predict(rf_model, newdata = testData)

# Exibe as previsões
predictions

# Adiciona as previsões ao conjunto de teste
testData$PredictedExited <- predictions

# Exibe o conjunto de teste com as previsões
testData
predicted <- testData[1:15, ]
table(predicted)
