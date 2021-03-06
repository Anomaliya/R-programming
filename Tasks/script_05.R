# 1.5 ������������ ����������.

df  <- mtcars

mean_qsec <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])
print(mean_qsec)

 
# ��� ������ ������� aggregate ����������� ����������� ���������� ���������� hp (��������� ����) 
# � ���������� disp (����������� ���������)  � ����� � �������������� � ������ �������� �������. 


aggregate(cbind(hp, disp) ~ am, df, sd)

# � ���������� df ��������� ���������� ������ airquality. 
# � ����� ���������� ��������� subset �������� ������, ������� ���������� ������ ��� ������� 7, 8 � 9.
# ��� ������ ������� aggregate ����������� ���������� ���������� (�� �����������) �� ���������� Ozone 
# � ������ ������. ��� ����������� ���������� ���������� ����������� ������� length(). 
# �������������� ������� � ������� �������, ��� ������� ����������� �������� �� �����������:
# aggregate(y ~ x + z , data, FUN)


df <- airquality
month_new <- subset(df, Month > 6)
aggregate(Ozone ~ Month, month_new, FUN = length)


# ��������� ������� describeBy � �������������� ���������� ������ airquality, 
# ��������� ���������� �� ���������� Month.  ���� ����� ����������� ���������� (skew) ���������� Wind 
# � ������� ������?
# � ����� � ������� ��������� ������ ������ �����. ���������� ����������� - �������: �������� 12,6


str(df)
describeBy(x = df$Wind, group = df$Month, mat = T, digits = 1, na.rm = T)


# ��������� � ���������� ������ iris. ���������� �������� ������������ ���������� ����������.

df <- iris
sd(df$Sepal.Length)
sd(df$Sepal.Width)
sd(df$Petal.Length)
sd(df$Petal.Width)

# � ������ iris ����������� �� �������� �������� ������ �������������� ���������� � ������ virginica.


describeBy(x = df[, -c(5)], group = df$Species, mat = T, digits = 1)

# � ���������� my_vector �������� ������ � ������������ ����������. 
# �������� ��� ����������� �������� ������� �� ������� �������� �� ��������� �����������.
# ����������� ����� ������ ��� ������ ������� print().

my_vector <- 1:50
my_vector[1:10] <- NA

is.na(my_vector)

mean_my_vec <- mean(my_vector, na.rm = T)
new_vec <- replace(x = my_vector, is.na(my_vector)==T, mean_my_vec)
print(new_vec)






