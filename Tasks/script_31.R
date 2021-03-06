# simple linear regression

setwd("D:/4_�������/R_lessons/Rdir")
library(psych)

#�������� ������� corr.calc, ������� �� ���� �������� data.frame
#� ����� ��������������� �����������, ������������ ����������� ���������� ������� 
#� ���������� ������ �� ���� ��������: ����������� ���������� � p - ������� ����������.


corr.calc <- function(x){
  f <- corr.test(x)
  return(c(f$r[2, 1], f$p[2,1]))
}


#�������� ������� filtered.cor ������� �� ���� �������� data.frame 
#� ������������ ����������� ���������� (��� ���������������, ��� � ����� ������ �����),
#������������ ������������ ���������� ������� ����� ����� ������ 
#�������������� ���������� � ���������� ���������� �� ������ �������� 
#������������ ����������.

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


#�������� ������� smart_cor, ������� �������� �� ���� dataframe � ����� ��������������� �����������. 
#��������� � ������� ����� ������-�����, ��� ������ � ����� ���������� ����������� ����������� �������������.
#���� ���� �� � ����� ������� ������������� ���������� ���������� �� ����������� (p - value ������ 0.05), 
#�� ������� ������ ���������� ����������� ���������� ��������. (�������� ������ �� ������ ��������).
#���� � ����� �������� ������������� ���������� �� ����������� ������� �� ����������, 
#�� ������� ������ ���������� ����������� ���������� �������.



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


#�������� ����� ������ - dataframe � ����� ��������������� �����������,
#��������� �������� ���������, ��� - ������ ���������� - ���������, ������ - �����������.
#� ����� ������� �������� ������������� ������������� ������� intercept �����  slope.
#���������� ����������� - �����. � ���� ��� ������ ������� ��� �����, �� ���������� ��������, ��������;
#12.434 6.2557


datset <- read.table("dataset_11508_12.txt")
res <- lm(datset[,1] ~ datset[,2])
print(res$coefficients)
coef(res)

#������������� ��� ��������� ������� diamonds �� ���������� ggplot2. 
#������ ��� ����������� ������ Ideal (���������� cut) c ������ ����� ������ 0.46 (���������� carat) 
#��������� �������� ���������, ��� � �������� ��������� ���������� ��������� price, 
#� �������� ���������� - ����������  depth. ��������� ������������ ��������� � ���������� fit_coef.

#> fit <- lm(mpg ~ disp + wt, mtcars)
#> fit$coefficients # ������������ ������

#��� ������� ����� ������, �� ��������� ���� for().

new.df <- (diamonds[which(diamonds$cut=="Ideal" & diamonds$carat==0.46),])
fit <- lm(price ~ depth, new.df)
fit_coef <- coef(fit)


data_for_model <- subset(diamonds, cut == 'Ideal' & carat == 0.46)    
fit <- lm(price ~ depth, data_for_model)    
fit_coef <- fit$coefficients



#�������� ������� regr.calc, ������� �� ���� �������� dataframe c ����� �����������.
#���� ��� ���������� ������� ����������� (p - ������� ���������� ��� ������������ ���������� ������� ������ 0.05), 
#�� ������� ������ ������������� ������, ��� ������ ���������� - ���������, 
#������ - �����������. ����� ������� � dataframe ����� ���������� � �������� fit, 
#��� ��������� ������������� ������� �������� ��������� ����������. 
#� ���������� ������� ������ ���������� �������� dataframe � ����������� ����� ���������� fit. 

#���� ��� ���������� ������� �� �����������, �� ������� ���������� ������� "There is no sense in prediction"

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


#������ ����������� �������:
  
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





