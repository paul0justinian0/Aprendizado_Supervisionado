fit_knn_mult <- function(new_data, data, k, target){
    
    # Excluindo a coluna: variável resposta do dataset
    data_covars <- as.matrix(data[, colnames(data) != target])
    newdata_covars <- as.numeric(new_data[colnames(new_data) != target])
    
    # Calculando as distâncias euclidianas
    dist <- apply(data_covars, 1, function(row){
        sqrt(sum((row - newdata_covars) ^ 2))
    })
    
    # Encontrando os k vizinhos mais próximos
    neighbors <- order(dist)[1:k]
    
    # Cálculo da média do valor alvo para os k vizinhos mais próximos
    return(mean(data[neighbors, target]))
}

knn_predict_target <- function(newdata, data, k , target){
    apply(newdata, 1, function(row){
        fit_knn_mult(new_data = as.data.frame(t(row)),
                     data = data,
                     k = k,
                     target = target)
    })
}

library(AER)
data("CASchools")
dados <- CASchools[, -c(1:9)]

knn_predict_target(newdata = dados, data = dados, k = 1, target = "income")

fit_knn_mult(dados[10,], data = dados, k = 1, target = "income")

as.data.frame(t(dados[1,]))

# Holdout repetido ----

## Definição das amostras teste x treino ----

amostras <- lapply(1:100, function(x){
    idx <- sample(1:nrow(dados), 0.75*nrow(dados))
    
    return(list(treino = dados[idx,],
                test = dados[-idx,]))
})

# Regressão Linear ----

lm_mods <- lapply(amostras, function(x){
    lm(income ~ ., data =  x$treino)
})

lm_preds <- lapply(1:100, function(x){
    predict(lm_mods[[x]], newdata = amostras[[x]]$test)
})

lm_EQM <- sapply(1:100, function(x){
    Metrics::mse(lm_preds[[x]], amostras[[x]]$test$income)
})

# Média dos EQM's
mean(lm_EQM)

# Elastic net ----

library(glmnet)

alpha <- seq(0, 1, length.out = 50)
lambda <- seq(0, 10, length.out = 50)

parametros <- expand.grid(alpha = alpha, lambda = lambda)

elastic_net <- sapply(1:nrow(parametros), function(i){
    metrics <- sapply(1:length(amostras), function(j){
        X <- as.matrix(amostras[[j]]$treino[, -2])
        Y <- amostras[[j]]$treino$income
        
        elastic_net_model <- glmnet(X, Y, alpha = parametros$alpha[i], lambda = parametros$lamda[i])
        elastic_net_pred <- predict(elastic_net_model,
                                    as.matrix(amostras[[j]]$test[, -2]))
        Metrics::rmse(elastic_net_pred, amostras[[j]]$test$income)
    })
    return(mean(metrics))
})

optimal_param <- parametros[which.min(elastic_net), ]

data.frame(alpha = parametros$alpha, lambda = parametros$lambda, rmse = elastic_net) %>% 
    ggplot(aes(x = lambda, y = alpha, fill = rmse))+
    geom_tile()+
    labs(title = "RMSE vs. alpha e lambda",
         x = "lambda",
         y = "alpha",
         fill = "RMSE")+
    theme_bw()

elastic_net_models <- lapply(amostras, function(x){
    X <- as.matrix(x$treino[, -2])
    Y <- x$treino$income
    
    elastic_net_mod <- glmnet(X, Y, alpha = optimal_param$alpha, lambda = optimal_param$lamda)
    return(elastic_net_mod)
})

elastic_net_preds <- lapply(1:100, function(x){
    predict(elastic_net_models[[x]], newx = as.matrix(amostras[[x]]$test[, -2]))
})

elastic_net_rmse <- sapply(1:100, function(x){
    Metrics::rmse(elastic_net_preds[[x]], amostras[[x]]$test$income)
})

mean(elastic_net_rmse)

# Encontrando o valor de k ótimo ----

op_k_rmse <- sapply(1:30, function(k){
    knn_rmse <- sapply(1:100, function(i){
        knn_pred <- knn_predict_target(newdata = amostras[[i]]$test, data = amostras[[i]]$treino, k = k, target = "income")
        Metrics::rmse(knn_pred, amostras[[i]]$test$income)
    })
    return(mean(knn_rmse))
})

optimal_k <- c(1:30)[which.min(op_k_rmse)]

library(tidyverse)

data.frame(k = 1:30, rmse = op_k_rmse) %>% 
    ggplot(aes(x = k, y = rmse))+
    geom_line()+
    geom_point()+
    geom_point(aes(x = optimal_k, y = min(op_k_rmse)), color = "red", size = 2)+
    annotate("text", x = optimal_k, y = min(op_k_rmse), label = paste("k = ", optimal_k), vjust = -1.5)+
    labs(title = "RMSE vs. k",
         x = "k",
         y = "RMSE")+
    theme_bw()

knn_preds <- lapply(1:100, function(x){
    knn_predict_target(newdata = amostras[[x]]$test, data = amostras[[x]]$treino, k = optimal_k, target = "income")
})

knn_rmse <- sapply(1:100, function(x){
    Metrics::rmse(knn_preds[[x]], amostras[[x]]$test$income)
})

mean(knn_rmse)
