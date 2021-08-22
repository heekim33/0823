a <- 1
a
b <- 2
b
c <- 3
c
a+b
4/b

var1 <- c(1,2,3)
var1
var2 <- c(1:5)
var2

str1 <- "a"
str <- "text"
str3 <- "Hello World Is Good"
str3

mean(var2)
max(var2)

english <- c(90, 80, 60, 70)
english
mean(english)
math <- c(50, 60, 10, 20)
math
min(math)
df_midterm <- data.frame(english, math)
df_midterm
mean(df_midterm$english)
max(df_midterm$math)

sales <- data.frame(fruit = c("사과", "딸기", "수박"), price = c(1800, 1500, 3000), volume = c(24, 38, 13))
sales
mean(sales$price)

# 구동방식 별 고속도로 연비 평균 구하기
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats  # 상자그림 통계치 출력

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))


