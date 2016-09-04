# 1.5 Описательные статистики.

df  <- mtcars

mean_qsec <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])
print(mean_qsec)

 
# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы) 
# и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 


aggregate(cbind(hp, disp) ~ am, df, sd)

# В переменную df сохранены встроенные данные airquality. 
# В новую переменную сохраните subset исходных данных, оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество наблюдений (не пропущенных) по переменной Ozone 
# в каждом месяце. Для определения количества наблюдений используйте функцию length(). 
# Воспользуйтесь записью с помощью формулы, при которой пропущенные значения не учитываются:
# aggregate(y ~ x + z , data, FUN)


df <- airquality
month_new <- subset(df, Month > 6)
aggregate(Ozone ~ Month, month_new, FUN = length)


# Примените функцию describeBy к количественным переменным данных airquality, 
# группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии (skew) переменной Wind 
# в восьмом месяце?
# В графу с ответом требуется ввести только число. Десятичный разделитель - запятая: например 12,6


str(df)
describeBy(x = df$Wind, group = df$Month, mat = T, digits = 1, na.rm = T)


# Обратимся к встроенным данным iris. Соотнесите значения стандартного отклонения переменных.

df <- iris
sd(df$Sepal.Length)
sd(df$Sepal.Width)
sd(df$Petal.Length)
sd(df$Petal.Width)

# В данных iris расположите по убыванию значения медиан количественных переменных в группе virginica.


describeBy(x = df[, -c(5)], group = df$Species, mat = T, digits = 1)

# В переменной my_vector сохранен вектор с пропущенными значениями. 
# Замените все пропущенные значения вектора на среднее значение по имеющимся наблюдениям.
# Напечатайте новый вектор при помощи функции print().

my_vector <- 1:50
my_vector[1:10] <- NA

is.na(my_vector)

mean_my_vec <- mean(my_vector, na.rm = T)
new_vec <- replace(x = my_vector, is.na(my_vector)==T, mean_my_vec)
print(new_vec)






