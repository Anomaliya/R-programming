# ������ ������������ ������.

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


# ��������� ���������� ��������� ������������� ����� ���� �� ����� ����� 
# ������ � ������ �� ������� HairEyeColor.

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
mydata2 <- subset(mydata, Sex == 'Female')


obj <- ggplot(data = mydata2, aes(x = Hair, y = Freq)) + 
  geom_bar(aes( fill = Eye), stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


# �� ������ ������� HairEyeColor �������� ��� ���� �������, 
# � ������� �������� ���������� � ������������� ����� ���� � ������-������� 
# (Hair = 'Brown'). ��������� ���� ������������� ������������� ����� ���� � ������� 
# � �������� �������� ��-�������� ��� ����� �����.

dimnames(Fem_broun)

Fem_broun <- (HairEyeColor['Brown', ,'Female'])
chisq.test(Fem_broun)

# ������������� ������� diamonds �� ���������� ggplot2. 
# ��� ������ �������� �� - ������� ��������� �������� � ����������� �������� ������� ���������� 
# (�ut) � ��� ����� (color). ��������� ��������� ���������� ����� � ����������. 
# ��� ������ ������� print() �������� �� ������ �������� ���������� �������� �� - �������. 

df <- diamonds
str(df)
rm(list=ls())


i <- table(df$cut, df$color)
chi <- chisq.test(table(df$cut, df$color))

# ��� ������ �������� �� - ������� ��������� �������� � ����������� ���� (price) � ������� (carat) �����������.

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

# ��� ������ ������� �������� ������ ��������� �������� � ����������� ���� ������� ������� (am) 
# � ���� ��������� (vs) � ������ mtcars. ��������� ���������� �������� ��������� � ����������.
# �������� �� ������ ������������ p - ������� ���������� ��� ������ ������� print()

df <- mtcars
p <- fisher.test(table(df$am, df$vs))
print(0.4727)














