
#Напишите функцию corr.calc, которая на вход получает data.frame
#с двумя количественными переменными, рассчитывает коэффициент корреляции Пирсона 
#и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.


library(psych)
corr.calc <- function(x){
  f <- corr.test(x)
  return(c(f$r[2, 1], f$p[2,1]))
}


#Напишите функцию filtered.cor которая на вход получает data.frame 
#с произвольным количеством переменных (как количественными, так и любых других типов),
#рассчитывает коэффициенты корреляции Пирсона между всеми парами 
#количественных переменных и возвращает наибольшее по модулю значение 
#коэффициента корреляции.

setwd("D:/4_Учебное/R_lessons/Rdir")

step6 <-  read.table("step6.csv", header=TRUE, sep=',' )

filtered.cor(step6)

#sapply(list, function)
#apply(array, margin, ...)
which.max()
diag(matrix) <- n


library(psych)
filtered.cor <- function(x){
  d <- sapply(x, is.numeric)
  newdat <- x[d]
  cor_res <-  corr.test(newdat)
  diag_null <- diag(x = -1, nrow = ncol(newdat)) + 1
  cor_null <- cor_res$r * diag_null 
  fin_abs <- abs(cor_null)
  cor_null[which.max(fin_abs)]
}


library(psych)
filtered.cor <- function(x){
cor_mat <- corr.test(x[sapply(x, is.numeric)])$r
cor_mat[which.max(abs(cor_mat * (diag(x = -1, nrow = ncol(cor_mat)) + 1)))]
}


#Напишите функцию smart_cor, которая получает на вход dataframe с двумя количественными переменными. 
#Проверьте с помощью теста Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному распределению.
#Если хотя бы в одном векторе распределение переменной отличается от нормального (p - value меньше 0.05), 
#то функция должна возвращать коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).
#Если в обоих векторах распределение переменных от нормального значимо не отличается, 
#то функция должна возвращать коэффициент корреляции Пирсона.


library(psych)


test_data <- as.data.frame(list(x = c(0.4, -0.7, -1.7, 1.05, -2.1, -0.97, 0.12, -0.05, 0.47, 1.24, 0.6, 0.48, -0.6, 1.2, 0.85, 1.05, -1.46, -0.1, -0.28, -1.22, -0.81, -0.87, 0.03, -0.94, 0.38, -0.04, -0.28, -0.25, -1.39, 1.28), y = c(-0.29, 0.45, 0.87, 0.03, -0.82, 1.61, -0.49, 1.04, -0.17, -0.32, 0.61, -1.47, 1.19, -0.64, -1.5, 1.24, 0.15, 0.72, -1.31, 1.55, -0.35, 0.39, -1.51, 0.69, 0.46, 0.23, -0.16, -0.92, 1.18, 0.26)))


smart_cor <- function(test_data){
  stat_test <- sapply(test_data, function(x) shapiro.test(x)$p.value)
  
    if (stat_test[1] < 0.05 | stat_test[2] < 0.05){
      Spearman_test <- corr.test(test_data, method = "spearman")
      print(Spearman_test$r[1,2])
    }
    if (stat_test[1] > 0.05 & stat_test[2] > 0.05){
      Pearson_test <- corr.test(test_data)
      print(Pearson_test$r[1,2])
    }
  }


smart_cor(test_data)


smart_cor <- function(x){
  stat_test <- sapply(x, function(x) shapiro.test(x)$p.value)
  
  if (stat_test[1] < 0.05 | stat_test[2] < 0.05){
    Spearman_test <- corr.test(x, method = "spearman")
    return(Spearman_test$r[1,2])
  }
  if (stat_test[1] > 0.05 & stat_test[2] > 0.05){
    Pearson_test <- corr.test(x)
    return(Pearson_test$r[1,2])
  }
}



