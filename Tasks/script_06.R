# Base graphs. Library ggplot2.

dfx <- airquality

boxplot(Ozone ~ Month, dfx, ylab = "Ozone", main ="Month", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)


df <- mtcars

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp))+
  geom_point(size = 5)


# iris

dfl <- iris

ggplot(dfl, aes(x = Sepal.Length, fill = Species))+
  geom_histogram()+

ggplot(dfl, aes(x = Sepal.Length))+
  geom_histogram(aes(fill = Species))



ggplot(dfl, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_dotplot(aes(fill = Species, size = Petal.Length))

ggplot(dfl, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(col = Species, size = Petal.Length))


my_sd <- sd(df$cyl)
save(my_sd, file = "my_sd.RData")
getwd()



