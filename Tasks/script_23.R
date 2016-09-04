setwd("D:/4_Учебное/R_lessons/Скрипты уроков")

#Воспользуемся встроенными данными npk, 
#иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield). 
#Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) 
#и фосфата (фактор P). Примените дисперсионный анализ, где будет проверяться влияние фактора 
#применения азота (N), влияние фактора применения фосфата (P) и их взаимодействие.
#В ответе укажите p-value для взаимодействия факторов N и P.

df <- npk
productivity <- aov(yield ~ N + P + N : P, data=df)
summary(productivity)

#Теперь проведите трехфакторный дисперсионный анализ,
#где зависимая переменная - это урожайность (yield), а три фактора - типы удобрений (N, P, K).
#После проведения данного анализа вы получите три значения p - уровня значимости 
#(о значимости каждого из факторов).
#Соотнесите названия факторов и значения p - уровня значимости.


productivity <- aov(yield ~ N + P + K, data=df)


#Проведите однофакторный дисперсионный анализ на встроенных данных iris. 
#Зависимая переменная - ширина чашелистика (Sepal.Width), независимая переменная - вид (Species). 
#Затем проведите попарные сравнения видов.
#Какие виды статистически значимо различаются по ширине чашелистика?

irisData <- iris
fitiris <- aov(Sepal.Width ~ Species, data=irisData)
summary(fitiris)

ggplot(irisData, aes(x = Species, y = Sepal.Width)) + 
  geom_boxplot()

TukeyHSD(fitiris)


#Проведите однофакторный дисперсионный анализ с повторными измерениями: 
#влияние типа таблетки (pill) на температуру (temperature) с учётом испытуемого (patient). 
#Каково p-value для влияния типа таблеток на температуру?

piluli <- read.csv('Pillulkin.csv')
str(piluli)

piluli$patient <- as.factor(piluli$patient)

pil1 <- aov(temperature ~ pill + Error(patient/pill), data = piluli)
summary(pil1)

#Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями: 
#влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature.
#Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает разные 
#таблетки, и тот факт, что  один и тот же больной лечится у разных врачей!
#Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?


pil1 <- aov(temperature ~ doctor * pill + Error(patient/(doctor * pill)), data = piluli)
summary(pil1)


library(ggplot2)
pd = position_dodge(0.1)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, group=supp, col = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))





