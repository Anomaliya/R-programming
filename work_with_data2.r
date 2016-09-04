# working with data and missing values
# transforming types of data
# creating and recoding variables
# sorting, comparing and separating data
# selecting and excluding variables


setwd("D:/4_Учебное/R_lessons/test.dir")
getwd()
df <- read.csv(file = "evals.csv", header = T, sep = ",")


# create a datatable

manager <- c(1:5)
date <- c("10/24/08","10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "UK", "US", "UK",  "UK")
gender <- c("male", "female", "female", "male", "female")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 5, 3, 4, 4)
q4 <- c(4, 4, 3, NA, 3)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors = F)

# count and add a new variable

attach(leadership)
leadership$new1 <- mean(age)
detach()

df$new2 <- c(1:nrow(df))

new_data <- transform(leadership, sumx = q1 + q2)

# recode variables

leadership <- within(leadership,
  {
    age.kod <- NA
    age.kod[age < 30] <- "less then 30"
    age.kod[age >= 30 & age < 45] <- "30 - 44"
    age.kod[age >= 45 & age < 60] <- "45 - 60"
    age.kod[age >= 60] <- "more then 60"
  }
)

# package: "doBy", function: recodevar(), cut()


# rename variables

# install.packages("reshape")
library(reshape)
leadership <- rename(leadership, 
                     c(age.kod = "age.new"))

names(leadership)[1] <- "score.new"
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5")

# missing values

leadership$age[leadership$age == 99] <- NA # recode missing values into NA

is.na() # check data for missing values

y <- c(1, 4, 6, NA, NA)
is.na(y)

is.na(leadership[,6:10])

ffff <- sum(y, na.rm=TRUE) # functions has a parameter na.rm=TRUE

na.omit() # remove all observation with NA

new.data <- na.omit(leadership)

# work with dates

mydates <- as.Date(c("2016-03-12", "2016-03-13", "2016-03-14", "2016-03-15"))
strDates <- c("01/05/1965", "08/16/2016")
dates <- as.Date(strDates, "%m/%d/%Y")

myformat <- "%m/%d/%y"
leadership$date <- as.Date(leadership$date, myformat)

Sys.Date() # returns present date
date() # returns present date and time

format(leadership$date, format="%Y/%m/%d")

today <- Sys.Date()
format(today, format="%d/%m/%Y")

difftime() # count 

today <- Sys.Date()
period <- as.Date("1989-09-15")
difftime(today, period, units = "days")

# sort

order()
# sort ascending
newdata <- leadership[order(leadership$age),]

# sort descending
attach(leadership)
mewdata <- leadership[order(gender, -age), ]
detach(leadership)

# merge

total <- merge(dataframeA, dataframeB, by="ID")
cbind()
rbind()

# select variables

newdata <- leadership[, c(6:10)]

myvars <- c("q1", "q2", "q3", "q4", "q5")
newdata <- leadership[myvars]

myvars <- paste("q", 1:5, sep="")
newdata <- leadership[myvars]

# excluding variables

myvars <- names(leadership) %in% c("q3", "q4")
newdata <- leadership[!myvars]

#excluding value

df1  <- subset(df, Species != "setosa")


# select cases

newdata <- leadership[1:3, ]

newdata <- leadership[which(leadership$gender=="male" & 
                        leadership$age > 30),]

attach(leadership)
ndata <- leadership[which(gender=="male" & age > 30), ]
detach(leadership)

# subset function

df1  <- subset(df, Species != "setosa")

newdata <- subset(leadership, age >= 35 | age < 24, 
                  select=c(q1, q2, q3))

newdata <- subset(leadership, gender=="male" & age > 25, 
                  select = gender:q4)

# random samples

mysample <- leadership[sample(1:nrow(leadership), 3,
                              replace=F), ]

set.seed(1234)
runif(5)



