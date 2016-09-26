# multiple linear regression

setwd("D:/4_Учебное/R_lessons/Rdir")

#Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
#x_1  -  числовой вектор
#x_2 - числовой вектор
#y - числовой вектор с пропущенными значениями.
#Теперь самое интересное, на первом этапе, использую только наблюдения,
#в которых нет пропущенных значений, вы должны построить регрессионную модель
#(без взаимодействий), где  y - зависимая переменная, x_1 и x_2 - независимые переменные. 
#Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
#Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y,
#в которой пропущенные значения заполнены предсказанными значениями построенной модели.


test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

is.na(test_data)

fit <- lm(y ~ x_1 + x_2, data = test_data)
summary(fit)

df <- mtcars[1:6]
df_new <- df[!names(df) %in% "cyl"]

df <- mtcars[c("wt", "mpg", "disp", "drat", "hp")]

model1 <- lm(wt ~ mpg, df)
summary(model1)



model <- lm(rating ~ complaints * critical, attitude)
summary(model)
