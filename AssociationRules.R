# 패키지 설치 및 로딩
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)


# sales.csv 파일 읽기

# 트랜잭션 클래스 형태의 데이터 파일(Sales.csv) 읽기
trans <- read.transactions(file.choose(), format = "basket", sep=",");
trans

# 데이터 출력
inspect(trans)

# 데이터 현황
itemFrequency(trans, type='absolute')
itemFrequency(trans)
itemFrequencyPlot(trans, type="absolute",
                  xlab="상품 아이템", ylab="거래 빈도",
                  col=1:5)
itemFrequencyPlot(trans,
                  xlab="상품 아이템", ylab="비율",
                  col=1:5, topN=5)

# 연관규칙 생성
rules <- apriori(trans, 
                 parameter = list(supp=0.4, conf=0.6, minlen=2))
# 연관규칙
inspect(rules)

# 규칙의 조건 검색
rules2 <- subset(rules, lift>1.0)  
rules3 <- sort(rules2, by="lift", decreasing=TRUE)
inspect(rules3)

# 연관규칙의 그래프 출력
plot(rules2, method="graph") 
 
