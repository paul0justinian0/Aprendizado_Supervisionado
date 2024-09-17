# Implementação do KNN ----

fit.knn <- function(new_value, data, k = 3, target, regressor){
    
    dist <- data[, regressor] - new_value
    neighbors <- data[order(abs(dist)),target][1:k]
    knn_value <- mean(neighbors)
    
    return(knn_value)
}

# Data (cats) ----

library(MASS)
data(cats)

## Separar entre treino e teste ----

set.seed(12345)
indices_treino <- sample(1:nrow(cats), size = nrow(cats)*0.7)
treino <- cats[indices_treino,]
teste <- cats[-indices_treino,]

## Predição utilizando knn ----

knn.values <- sapply(teste[, 2], function(i){
    fit.knn(new_value = i, data = treino, target = "Hwt", regressor = "Bwt", k = 20)
})

### EQM do knn ----

EQM.knn <- mean((knn.values - teste[,3])^2)

## Comparando com RLS ----

mod1 <- lm(Hwt ~ Bwt, data = treino)

pred.RLS <- predict(mod1, teste)

### EQM da RLS ----

EQM.RLS <- mean((pred.RLS - teste[,3])^2)

## Análise gráfica ----

indices_knn <- order(teste[,2])

plot(Hwt ~ Bwt, data = cats, pch = 20)
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red")
points(teste[,2], knn.values, col = "navy", pch = 18, cex = 1.5)
lines(teste[indices_knn,2], knn.values[indices_knn])
legend("topleft", legend = c("KNN", "Base real", "Reta de regressão"), pch = c(18, 20, 4), col = c("navy", "black", "red"))

# Outra base de dados ----

library(AER)
data("CASchools")

## Separar entre treino e teste ----

set.seed(12345)
indices_treino <- sample(1:nrow(CASchools), size = nrow(CASchools)*0.7)
treino <- CASchools[indices_treino,]
teste <- CASchools[-indices_treino,]

## Ajuste dos valores do knn ----

knn.values <- sapply(teste[, 11], function(i){
    fit.knn(new_value = i, data = treino, target = 14, regressor = 11, k = 25)
})

### EQM do knn ----

EQM.knn <- mean((knn.values - teste[,14])^2)

## Comparando com RLS ----

mod1 <- lm(math ~ income, data = treino)

pred.RLS <- predict(mod1, teste)

### EQM da RLS ----

EQM.RLS <- mean((pred.RLS - teste[,14])^2)

## Análise gráfica ----

indices_knn <- order(teste[,11])

plot(math ~ income, data = CASchools, pch = 20)
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red")
points(teste[,11], knn.values, col = "navy", pch = 18, cex = 1.5)
lines(teste[indices_knn,11], knn.values[indices_knn])
legend("topleft", legend = c("KNN", "Base real", "Reta de regressão"), pch = c(18, 20, 4), col = c("navy", "black", "red"))
