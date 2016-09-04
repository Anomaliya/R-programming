# 1.3 Работа с data frame

data(mtcars)
df <- mtcars

df$even_gear <- ifelse (df$gear %% 2 == 1, 0, 1)
print(df$even_gear)


df$count <- 1:nrow(df)
df1 <- subset(df, gear %% 2 == 1)
df2 <- subset(df, gear %% 2 == 0)
df1$even_gear <- 0
df2$even_gear <- 1
df <- rbind(df2, df1)
df <- df[order(df$count, decreasing = F),]
print(df$even_gear)
