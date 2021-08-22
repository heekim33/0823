# 패키지 설치 및 로딩
install.packages("C50")
library(C50)

# 학습 데이터 작성
pclass <- c(1, 1, 1, 1, 2,
            2, 2, 2, 2, 2)
gender <- c("M", "M", "M", "F", "M",
            "M", "M", "M", "F", "F")
survived <- c("Y", "Y", "Y", "Y", "N",
              "N", "N", "Y", "Y", "Y")
train_data <- data.frame("좌석등급" = pclass,
                         "성별"     = gender,
                         "생존여부" = survived)
train_data

str(train_data)

train_data$좌석등급 <- as.factor(train_data$좌석등급)
train_data$생존여부 <- as.factor(train_data$생존여부)
train_data$성별 <- as.factor(train_data$성별)
str(train_data)


# 테스트 데이터 작성
pclass <- c(1, 2, 2)
gender <- c("F", "F", "M")

test_data <- data.frame("좌석등급"= pclass,
                        "성별" = gender)
str(test_data)

test_data$좌석등급 <- as.factor(test_data$좌석등급)
test_data$성별 <- as.factor(test_data$성별)
str(test_data)

# 데이터 출력
model <- C5.0(생존여부 ~., data=train_data)
summary(model)

plot(model)

# 분류
results <- predict(object = model, newdata = test_data)
results

## 붓꽃 종의 분류와 예측

# 패키지 설치 및 로딩
# install.packages("C50")
library(C50)

# 파일 읽기
iris

str(iris)

nrow(iris)

# 데이터 분리를 위한 색인
set.seed(3598)
idx <- sample(1:nrow(iris), size = nrow(iris)*0.8)
idx

length(idx)

# 데이터 분리
train_data <- iris[idx, ]
train_data

test_data <- iris[-idx, ]
test_data

# (모형 1) 가지치기 없는 경우
c5_options <- C5.0Control(noGlobalPruning = T, CF = 1)
model1 <- C5.0(Species ~., data=train_data, control = c5_options)
summary(model1)

plot(model1)
