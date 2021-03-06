setwd("D:/4_�������/R_lessons/������� ������")

#������������� ����������� ������� npk, 
#��������������� ������� ���������� ��������� ��������� �� ����������� ������ (yield). 
#����� ������� ����� ��������, ����������� �� ������������� ���������� ����� (������ N) 
#� ������� (������ P). ��������� ������������� ������, ��� ����� ����������� ������� ������� 
#���������� ����� (N), ������� ������� ���������� ������� (P) � �� ��������������.
#� ������ ������� p-value ��� �������������� �������� N � P.

df <- npk
productivity <- aov(yield ~ N + P + N : P, data=df)
summary(productivity)

#������ ��������� ������������� ������������� ������,
#��� ��������� ���������� - ��� ����������� (yield), � ��� ������� - ���� ��������� (N, P, K).
#����� ���������� ������� ������� �� �������� ��� �������� p - ������ ���������� 
#(� ���������� ������� �� ��������).
#���������� �������� �������� � �������� p - ������ ����������.


productivity <- aov(yield ~ N + P + K, data=df)


#��������� ������������� ������������� ������ �� ���������� ������ iris. 
#��������� ���������� - ������ ����������� (Sepal.Width), ����������� ���������� - ��� (Species). 
#����� ��������� �������� ��������� �����.
#����� ���� ������������� ������� ����������� �� ������ �����������?

irisData <- iris
fitiris <- aov(Sepal.Width ~ Species, data=irisData)
summary(fitiris)

ggplot(irisData, aes(x = Species, y = Sepal.Width)) + 
  geom_boxplot()

TukeyHSD(fitiris)


#��������� ������������� ������������� ������ � ���������� �����������: 
#������� ���� �������� (pill) �� ����������� (temperature) � ������ ����������� (patient). 
#������ p-value ��� ������� ���� �������� �� �����������?

piluli <- read.csv('Pillulkin.csv')
str(piluli)

piluli$patient <- as.factor(piluli$patient)

pil1 <- aov(temperature ~ pill + Error(patient/pill), data = piluli)
summary(pil1)

#������ ����� ������� ����� �������� ������������� ������������� ������ � ���������� �����������: 
#������� �������� doctor, ������� ������� pill � �� �������������� �� temperature.
#������ ��� ��������������� ����������: � ��� ����, ��� ���� � ��� �� ������� ��������� ������ 
#��������, � ��� ����, ���  ���� � ��� �� ������� ������� � ������ ������!
#������ F-�������� ��� �������������� �������� ������� (doctor) � ���� �������� (pill)?


pil1 <- aov(temperature ~ doctor * pill + Error(patient/(doctor * pill)), data = piluli)
summary(pil1)


library(ggplot2)
pd = position_dodge(0.1)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, group=supp, col = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))





