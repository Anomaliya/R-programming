# simple linear regression

setwd("D:/4_Учебное/R_lessons/Rdir")
library(psych)

#Напишите функцию corr.calc, которая на вход получает data.frame
#с двумя количественными переменными, рассчитывает коэффициент корреляции Пирсона 
#и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.


corr.calc <- function(x){
  f <- corr.test(x)
  return(c(f$r[2, 1], f$p[2,1]))
}


#Напишите функцию filtered.cor которая на вход получает data.frame 
#с произвольным количеством переменных (как количественными, так и любых других типов),
#рассчитывает коэффициенты корреляции Пирсона между всеми парами 
#количественных переменных и возвращает наибольшее по модулю значение 
#коэффициента корреляции.

step6 <-  read.table("step6.csv", header=TRUE, sep=',' )

filtered.cor(step6)

#sapply(list, function)
#apply(array, margin, ...)
which.max()
diag(matrix) <- n


filtered.cor <- function(x){
  d <- sapply(x, is.numeric)
  newdat <- x[d]
  cor_res <-  corr.test(newdat)
  diag_null <- diag(x = -1, nrow = ncol(newdat)) + 1
  cor_null <- cor_res$r * diag_null 
  fin_abs <- abs(cor_null)
  cor_null[which.max(fin_abs)]
}



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


#Скачайте набор данных - dataframe с двумя количественными переменными,
#постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая.
#В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
#Десятичный разделитель - точка. В поле для ответа введите два числа, не округляйте значения, например;
#12.434 6.2557


datset <- read.table("dataset_11508_12.txt")
res <- lm(datset[,1] ~ datset[,2])
print(res$coefficients)
coef(res)

#Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
#Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat) 
#постройте линейную регрессию, где в качестве зависимой переменной выступает price, 
#в качестве предиктора - переменная  depth. Сохраните коэффициенты регрессии в переменную fit_coef.

#> fit <- lm(mpg ~ disp + wt, mtcars)
#> fit$coefficients # коэффициенты модели

#Это задание нужно решить, не используя цикл for().

new.df <- (diamonds[which(diamonds$cut=="Ideal" & diamonds$carat==0.46),])
fit <- lm(price ~ depth, new.df)
fit_coef <- coef(fit)


data_for_model <- subset(diamonds, cut == 'Ideal' & carat == 0.46)    
fit <- lm(price ~ depth, data_for_model)    
fit_coef <- fit$coefficients



#Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
#Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), 
#то функция строит регрессионную модель, где первая переменная - зависимая, 
#вторая - независимая. Затем создает в dataframe новую переменную с назанием fit, 
#где сохраняет предсказанные моделью значения зависимой переменной. 
#В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit. 

#Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"

my_df = iris[,c(1,4)]

regr.calc(my_df)

regr.calc <- function(x){
  res_cor <- corr.test(x)
  if(res_cor$p[2, 1] < 0.05){
    korznach <- lm(x[,1] ~ x[,2])
    x$fit <- (korznach$fitted.values)
    return(x)
  } else{
    return("There is no sense in prediction")
  }
}


#Пример правильного решения:
  
  regr.calc <- function(sample_data){    
    cor_result = cor.test(~sample_data[[1]] + sample_data[[2]])    
    if (cor_result$p.value < 0.05){    
      fit_model  <- lm(sample_data[[1]] ~ sample_data[[2]])    
      sample_data$fit  <- fit_model$fitted.values    
      return(sample_data)    
    } else {    
      return('There is no sense in prediction')}}


  my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")





