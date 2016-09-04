var1 <- 12
var2 <- var1 * 10
print(var2)

number1 <- 1
number2 <- 2
number3 <- 3
var_new <- number1 + number2 > number3
print(var_new)


v1 <- 1:50
v2 <- 60:100
v3 <- c(v1, v2)
print(v3)

my_vector <- 1:20
print(my_vector[c(2,5,7,9,12,16,20)])

getwd()

setwd("D:/4_Учебное/R")
mydata <- read.csv('evals.csv')

head(mydata, 1) #отображает заданное количество наблюдений в начале массива
tail(mydata) #отображает заданное количество наблюдений в конце массива
View(mydata) #отображает массив в виде таблицы
str(mydata) #stucture
names(mydata)
summary(mydata)
nrow(mydata)
ncol(mydata)


mydata$age
b <- mydata$age
mydata$ten_point_scale <- mydata$score * 2 # создает новую переменную "ten_point_scale" в массиве "mydata", 
# которая содержит данные переменной "score", умноженные на 2 
            
summary(mydata$ten_point_scale)
mydata$new_var <- 1
mydata$number <- 1:nrow(mydata)
summary(mydata$number)

mydata$score[1:10]
mydata[1,1]
mydata[c(2, 193, 225), 1]
mydata[101:200, 1]

mydata[5,]
mydata[,1] == mydata$score
head(mydata[2:5])

mydata$gender
mydata$gender == 'female'
mydata[,1]


mydata$gender
mydata$fem <- (mydata$gender == 'female')

head(mydata[mydata$gender == 'female', 1:3])

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))


# слить наблюдения
mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

# слить по переменным
mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata5, mydata6)
