# multiple linear regression

setwd("D:/4_�������/R_lessons/Rdir")

#�������� ������� fill_na, ������� ��������� �� ���� ������ � ����� �����������:
#x_1  -  �������� ������
#x_2 - �������� ������
#y - �������� ������ � ������������ ����������.
#������ ����� ����������, �� ������ �����, ��������� ������ ����������,
#� ������� ��� ����������� ��������, �� ������ ��������� ������������� ������
#(��� ��������������), ���  y - ��������� ����������, x_1 � x_2 - ����������� ����������. 
#�����, ��������� ����������� ������, �� �������� ����������� �������� �������������� ������.
#������� ������ ���������� dataframe c ����� ����������  y_full. ��������� � ��� ���������� y,
#� ������� ����������� �������� ��������� �������������� ���������� ����������� ������.


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
