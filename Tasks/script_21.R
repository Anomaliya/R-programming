# Анализ номинативных данных.

dfl <- HairEyeColor

dimnames(HairEyeColor)
HairEyeColor[ , ,'Male']
m1 <- HairEyeColor[ , ,'Male']
str(HairEyeColor)
dimnames(m1)
str(m1)


tab1 <- prop.table(HairEyeColor[, , 'Male'], 2)
print(tab1[3, 2])


sum(HairEyeColor[, 'Green', 'Female'])


# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос 
# только у женщин из таблицы HairEyeColor.

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
mydata2 <- subset(mydata, Sex == 'Female')


obj <- ggplot(data = mydata2, aes(x = Hair, y = Freq)) + 
  geom_bar(aes( fill = Eye), stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


# На основе таблицы HairEyeColor создайте ещё одну таблицу, 
# в которой хранится информация о распределении цвета глаз у женщин-шатенок 
# (Hair = 'Brown'). Проведите тест равномерности распределения цвета глаз у шатенок 
# и выведите значение хи-квадрата для этого теста.

dimnames(Fem_broun)

Fem_broun <- (HairEyeColor['Brown', ,'Female'])
chisq.test(Fem_broun)

# Воспользуемся данными diamonds из библиотеки ggplot2. 
# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи качества огранки бриллианта 
# (сut) и его цвета (color). Сохраните результат выполнения теста в переменную. 
# При помощи команды print() выведите на печать значение статистики критерия Хи - квадрат. 

df <- diamonds
str(df)
rm(list=ls())


i <- table(df$cut, df$color)
chi <- chisq.test(table(df$cut, df$color))

# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов.

# first var

df <- diamonds
mean_price <- mean(df$price)
mean_carat <- mean(df$carat)
df$factor_price <- df$price >= mean(df$price)
df$factor_carat <- df$carat >= mean(df$carat)

df$factor_price <- replace(x = df$factor_price, factor_price == TRUE, values = 1)
df$factor_carat <- as.integer(df$factor_carat)

t <- chisq.test(table(df$factor_price, df$factor_carat))
print(40735)


# second var

diamonds$factor_price <- diamonds$price >= mean(diamonds$price)
diamonds$factor_carat <- diamonds$carat >= mean(diamonds$carat)
diamonds$factor_price <- replace(x = diamonds$factor_price, factor_price == TRUE, values = 1)
diamonds$factor_carat <- as.integer(diamonds$factor_carat)

diamonds$factor_carat <- as.factor(diamonds$factor_carat)
diamonds$factor_price <- as.factor(diamonds$factor_price)

t <- chisq.test(table(diamonds$factor_price, diamonds$factor_carat))
print(40735)

# third var (correct)

df <- diamonds
df$factor_price <- ifelse(df$price >= mean(df$price), 1, 0)
df$factor_carat <- ifelse(df$carat >= mean(df$carat), 1, 0)

df$factor_carat <- as.factor(df$factor_carat)
df$factor_price <- as.factor(df$factor_price)

t <- chisq.test(table(df$factor_price, df$factor_carat))
print(40735)

# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) 
# и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную.
# Выведите на печать получившийся p - уровень значимости при помощи команды print()

df <- mtcars
p <- fisher.test(table(df$am, df$vs))
print(0.4727)














