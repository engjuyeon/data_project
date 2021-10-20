# 인구 수에 미치는 영향에 따른 미래 예측
## 1. 데이터 출처 설명 및 Library 로딩
### 1-1. 데이터 출처
### 1-2. Library 설치 및 로딩
# library(xlsx)
# library(DBI)
# library(RJDBC)
# library(car)
# library(corrplot)
# library(psych)
# library(leaps)
# library(forecast)
# library(dplyr)
# library(ggplot2)
### 1-3. Seed 고정

## 2. 데이터 전처리
### 2-1. 서울시 1인 가구 데이터
### 2-2. 서울시 경제활동인구 데이터
### 2-3. 서울시 고용률 데이터
### 2-4. 서울시 실업률 데이터
### 2-5. 국내총생산 및 경제성장률 데이터
### 2-6. 서울시 출생사망 통계 데이터
### 2-7. 서울시 인구 데이터
### 2-8. 전처리 데이터 DB 저장

## 3. 데이터 모델링
### 3-1. 회귀분석으로 데이터 모델링
### 3-2. 회귀분석평가지표로 검증한 결과
### 3-3. 변수 선택법 진행

str(data.frame)
###2-1. 서울시 1인 가구 데이터
#install.packages('xlsx')
library(xlsx)
one_person <- read.xlsx2('./DATA/수집데이터/06~/서울시1인가구.xlsx', sheetIndex = 1, header = TRUE)
View(one_person)
str(one_person)

## 2-1-1. 데이터 전처리
sum(is.na(one_person))
one_person <- one_person[,c(1,2,3)]

one_person <- one_person[c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43),]
one_person <- one_person[,-2]


one_person[,] <- sapply(one_person[,], as.numeric)
one_person$합계 <- round(one_person$합계,2)

one_person <- one_person[-15,]

###2-2. 서울시 경제활동인구 데이터 
economically_active_population <- read.xlsx2('./DATA/수집데이터/06~/경제활동인구.xlsx', sheetIndex = 1, header = TRUE)
View(economically_active_population)
str(economically_active_population) # 데이터 chr

## 2-2-1. 데이터 전처리
sum(is.na(economically_active_population))

colnames(economically_active_population)
colnames(economically_active_population) <- gsub("[[:punct:]]", "", colnames(economically_active_population)) # 특수기호 제거 후 저장
colnames(economically_active_population) <- gsub("X", "", colnames(economically_active_population))
colnames(economically_active_population) <- substr(colnames(economically_active_population),1,4)
colnames(economically_active_population)

rownames(economically_active_population) <- economically_active_population[,1]
economically_active_population <- economically_active_population[,-1]

economically_active_population[,] <- sapply(economically_active_population[,], as.numeric)

str(economically_active_population)

year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
year

economically_active_population <- t(economically_active_population)
View(economically_active_population)

economically_active_population_c <- cbind(year,economically_active_population)
View(economically_active_population_c)

### 2-3. 서울시 고용률 데이터
employment_rate <- read.xlsx2('./DATA/수집데이터/06~/고용률.xlsx', sheetIndex = 1, header = TRUE)
View(employment_rate)
str(employment_rate) # 데이터 chr

## 2-3-1. 데이터 전처리
sum(is.na(employment_rate))

colnames(employment_rate)
colnames(employment_rate) <- gsub("[[:punct:]]", "", colnames(employment_rate)) # 특수기호 제거 후 저장
colnames(employment_rate) <- gsub("X", "", colnames(employment_rate))
colnames(employment_rate) <- substr(colnames(employment_rate),1,4)
colnames(employment_rate)

rownames(employment_rate) <- employment_rate[,1]
employment_rate <- employment_rate[,-1]

employment_rate[,] <- sapply(employment_rate[,], as.numeric)

str(employment_rate)

year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
year

employment_rate <- t(employment_rate)
View(employment_rate)

employment_rate_c <- cbind(year,employment_rate)
View(employment_rate_c)

### 2-4. 서울시 실업률 데이터
unemployment_rate <- read.xlsx2('./DATA/수집데이터/06~/실업률.xlsx', sheetIndex = 1, header = TRUE)
View(unemployment_rate)
str(unemployment_rate)

## 2-4-1. 데이터 전처리
sum(is.na(unemployment_rate))

colnames(unemployment_rate)
colnames(unemployment_rate) <- gsub("[[:punct:]]", "", colnames(unemployment_rate))
colnames(unemployment_rate) <- gsub("X", "", colnames(unemployment_rate))
colnames(unemployment_rate) <- substr(colnames(unemployment_rate), 1, 4)
colnames(unemployment_rate)

rownames(unemployment_rate) <- unemployment_rate[,1]
unemployment_rate <- unemployment_rate[,-1]


unemployment_rate[,] <- sapply(unemployment_rate[,], as.numeric)

str(unemployment_rate)

year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
year

unemployment_rate <- t(unemployment_rate)
View(unemployment_rate)

unemployment_rate_c <- cbind(year,unemployment_rate)
View(unemployment_rate_c)

### 2-5. 국내총생산 및 경제성장률 데이터

GDP <- read.xlsx2('./DATA/수집데이터/06~/국내총생산 및 경제성장률 (GDP).xls', sheetIndex = 1, header = TRUE)
View(GDP) 
str(GDP) # 데이터 chr

## 2-5-1. 데이터 전처리
sum(is.na(GDP))

colnames(GDP)
colnames(GDP) <- gsub("[[:punct:]]", "", colnames(GDP))
colnames(GDP) <- gsub("X", "", colnames(GDP))
colnames(GDP) <- substr(colnames(GDP), 1, 4)
colnames(GDP)

rownames(GDP) <- GDP[,1]
GDP <- GDP[,-1]

GDP[,] <- gsub(",","",GDP[,]) # 쉼표 제거
GDP[,] <- sapply(GDP[,], as.numeric)

str(GDP)


year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
year

GDP <- t(GDP)
View(GDP)

GDP_c <- cbind(year,GDP)
View(GDP_c)

### 2-6. 서울시 출생사망 통계 데이터
seoul_birth_death <- read.xlsx2('./DATA/수집데이터/06~/서울시출생사망.xls', sheetIndex = 1, header = TRUE)
View(seoul_birth_death)
str(seoul_birth_death)


## 2-6-1. 데이터 전처리
sum(is.na(seoul_birth_death))

colnames(seoul_birth_death)
colnames(seoul_birth_death) <- seoul_birth_death[1,]
seoul_birth_death <- seoul_birth_death[-1,]
seoul_birth_death <- seoul_birth_death[,-2]
seoul_birth_death <- seoul_birth_death[-16,] # 2020 사망률 데이터 제거
colnames(seoul_birth_death) <- gsub("\\(명\\)","",colnames(seoul_birth_death), )

seoul_birth_death[,] <- sapply(seoul_birth_death[,], as.numeric)


str(seoul_birth_death)
seoul_birth_death <- seoul_birth_death[-1,]

### 2-7. 서울시 인구 데이터
seoul_population <- read.xlsx2('./DATA/수집데이터/06~/서울시인구.xls', sheetIndex = 1, header = TRUE)
View(seoul_population)
str(seoul_population)

## 2-7-1. 데이터 전처리
sum(is.na(seoul_population))

colnames(seoul_population)
colnames(seoul_population) <- seoul_population[2,]
seoul_population <- seoul_population[c(-1,-2),]
seoul_population <- seoul_population[,-2]

seoul_population[,] <- sapply(seoul_population[,], as.numeric)

str(seoul_population)

seoul_population <- seoul_population[-15,]

### 2-8. 첫번째 전처리 데이터 DB 저장
library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.11')
library(RJDBC)

driv <- JDBC(driverClass = "org.mariadb.jdbc.Driver", classPath = "C:/Users/user/eclipse-workspace/driver/NEW/mariadb-java-client-2.7.3.jar", " ")

con <- dbConnect(driv, "jdbc:mariadb://ip:portnum/databasetable", "root", "password")

dbWriteTable(con, 'ori_economically_active_population' , economically_active_population_c, overwrite = T)
dbWriteTable(con, 'ori_employment_rate' , employment_rate_c, overwrite = T)
dbWriteTable(con, 'ori_unemployment_rate' , unemployment_rate_c, overwrite = T)
dbWriteTable(con, 'ori_GDP' , GDP_c, overwrite = T)
dbWriteTable(con, 'ori_seoul_birth_death' , seoul_birth_death, overwrite = T)
dbWriteTable(con, 'ori_seoul_population' , seoul_population, overwrite = T)
dbWriteTable(con, 'ori_one_person', one_person, overwrite = T)

# 데이터 전처리 진행 # 2006 ~ 2019
economically_active_population <- dbReadTable(con, 'ori_economically_active_population', row.names = 1)
economically_active_population <- economically_active_population[c(-1,-16,-17),]
employment_rate <- dbReadTable(con, 'ori_employment_rate', row.names = 1)
employment_rate <- employment_rate[c(-15,-16),]
unemployment_rate <- dbReadTable(con, 'ori_unemployment_rate', row.names = 1)
unemployment_rate <- unemployment_rate[c(-15,-16),]
GDP <- dbReadTable(con, 'ori_GDP', row.names = 1)
GDP <- GDP[c(-15,-16),]
seoul_population <- dbReadTable(con, 'ori_seoul_population', row.names = 1)
one_person <- dbReadTable(con, 'ori_one_person', row.names = 1)
seoul_birth_death <- dbReadTable(con, 'ori_seoul_birth_death', row.names = 1)

# 2006 ~ 2019
total_data <- cbind(seoul_population$기간, seoul_population$계, one_person$합계, economically_active_population$total, employment_rate$total, unemployment_rate$total, GDP$gdp, seoul_birth_death[,c(-1,-3,-5)])
# 인구는 고용률과 실업률과는 크게 관련이 없다.

colnames(total_data) <- c('year','population','one_person','economically_population','employment_rate','unemployment_rate','gdp','birth','death')

library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.11')
library(RJDBC)

driv <- JDBC(driverClass = "org.mariadb.jdbc.Driver", classPath = "C:/Users/user/eclipse-workspace/driver/NEW/mariadb-java-client-2.7.3.jar", " ")

con <- dbConnect(driv, "jdbc:mariadb://ip:portnum/databasetable", "root", "password")

dbWriteTable(con, 'economically_active_population' , economically_active_population, overwrite = T)
dbWriteTable(con, 'employment_rate' , employment_rate, overwrite = T)
dbWriteTable(con, 'unemployment_rate' , unemployment_rate, overwrite = T)
dbWriteTable(con, 'GDP' , GDP, overwrite = T)
dbWriteTable(con, 'seoul_birth_death' , seoul_birth_death, overwrite = T)
dbWriteTable(con, 'seoul_population' , seoul_population, overwrite = T)
dbWriteTable(con, 'one_person', one_person, overwrite = T)
dbWriteTable(con, 'total_data' , total_data, overwrite = T)

economically_active_population <- dbReadTable(con, 'economically_active_population', row.names = 1)
employment_rate <- dbReadTable(con, 'employment_rate', row.names = 1)
unemployment_rate <- dbReadTable(con, 'unemployment_rate', row.names = 1)
GDP <- dbReadTable(con, 'GDP', row.names = 1)
seoul_population <- dbReadTable(con, 'seoul_population', row.names = 1)
one_person <- dbReadTable(con, 'one_person', row.names = 1)
seoul_birth_death <- dbReadTable(con, 'seoul_birth_death', row.names = 1)
total_data <- dbReadTable(con, 'total_data', row.names = 1)


total_data_a <- total_data[,-1]

#normal <- function(x){
#  return((x-min(x))/(max(x)-min(x)))
#}

#total_data_a <- as.data.frame(lapply(total_data_a, normal))
#total_data_a

# Coefficients: 잔차가 최저가 되는 기울기
#(Intercept) y절편       one_person  economically_population          employment_rate  
#1.106e+07                2.739e-01                1.366e+03               -1.284e+05  
#unemployment_rate                      gdp                    birth                    death  
#-3.428e+04               -2.706e+00                3.810e+00                1.671e+00  

# 상관관계 확인
cor(economically_active_population$total, seoul_population$'계')
cor(employment_rate$total, seoul_population$'계')
cor(unemployment_rate$total, seoul_population$'계')
cor(GDP$gdp, seoul_population$'계') 

cor(seoul_birth_death$출생건수, seoul_population$'계')
cor(seoul_birth_death$조출생률, seoul_population$'계')
cor(seoul_birth_death$사망건수, seoul_population$'계')
cor(seoul_birth_death$조사망률, seoul_population$'계')

cor(unemployment_rate$total, seoul_birth_death$사망건수)
cor(employment_rate$total, seoul_birth_death$출생건수)
cor(employment_rate$total, GDP$gdp)
cor(unemployment_rate$total, GDP$gdp)

library(car)
cor(total_data_a)

library(corrplot)
corrplot(cor(total_data_a), method='shade', addCoef.col="black",addshade="all")

con <- dbConnect(driv, "jdbc:mariadb://127.0.0.1:3308/data_image", "root", "0431")

rs <- dbSendQuery(con, 
                  "CREATE TABLE IF NOT EXISTS image 
                  (image_id tinyint(3) NOT NULL AUTO_INCREMENT, 
                  image longblob NOT NULL, 
                  KEY image_id (image_id))") 

dbClearResult(rs)

png("./residualPlot01.png",width = 1000, height = 1000)
par(mfrow=c(1,1))
corrplot(cor(total_data_a), method='shade', addCoef.col="black",addshade="all")
dev.off()

plot_binary <- paste(readBin("residualPlot01.png", what="raw", n=1e6), collapse="")

rs <- dbSendQuery(con,
                  paste("INSERT INTO image
                  (image)
                  VALUES
                  ('",plot_binary,"')", sep=""))

dbClearResult(rs)

img <- dbGetQuery(con,
                  paste("SELECT image FROM image WHERE image = '",plot_binary,"'", sep=""))
img


model <- lm(population ~ ., data = total_data_a)
summary(model) # 통계적으로 유의하다. / 설명력 좋다.
plot(model)
plot(model,2)
shapiro.test(model$residuals) # 정규 분포가 아니라고 말하기 어렵다.

plot(model, 1)
durbinWatsonTest(model) # 잔차 간에 자기 상관성이 있다고 볼 수 없다.

plot(model, 3)
total <- total_data_a
total$resid_sq <- model$residuals^2

library(dplyr)
lm(resid_sq ~ ., data = total) %>%summary # 잔차의 등분산성을 만족한다고 볼 수 있다.

plot(model, 4)
cooks.distance(model) %>%  summary
total_data[cooks.distance(model)>1,] # 4번째 데이터 이상치


backward <- step(model, direction = "forward", trace=T)
backward <- step(model, direction = "backward", trace=T)  
# AIC=292.66 
# population ~ economically_population + employment_rate + unemployment_rate + gdp + birth
backward <- step(model, direction = "both", trace=T)


library(leaps)
leap <-regsubsets(population ~ ., total_data_a, nbest = 5)
summary(leap)

plot(leap) # bic birth + employment_rate + gdp + economically_population
plot(leap, scale = "adjr2") # adjr birth + employment_rate + gdp + economically_population

summary(leap)$bic

summary(leap)$adjr2 # 21번째 5  ( 1 ) " "        "*"                     "*"             "*"               "*" "*"   " "  


model2 <- lm(population ~ economically_population + employment_rate + unemployment_rate + gdp + birth, data = total_data_a)
summary(model2)

shapiro.test(model2$residuals)

plot(model2,2)
shapiro.test(model2$residuals)

library(car)
plot(model2, 1)
durbinWatsonTest(model2)

plot(model2, 3)
total <- total_data_a
total$resid_sq <- model2$residuals^2

library(dplyr)
lm(resid_sq ~ ., data = total) %>%summary

vif(model)
vif(model2)

library(forecast)
accuracy(model)
accuracy(model2)
accuracy(model3)

model3 <- lm(population ~ gdp, data = total_data_a)
summary(model3)

plot(total_data$population)
lines(model$fitted.values, col = "red")
lines(model2$fitted.values, col = "blue")
lines(model3$fitted.values, col = "green")

library(glmnet)
ridge.mod <- glmnet(x, y, alpha = 1)










# 시계열
library(tidyverse)
library(tsibble)
library(randomForest)
library(forecast)

normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

total_data_a_b <- scale(total_data_a)
# total_data.ts <- ts(total_data_a, start=c(2006,1), frequency = 1) # 스케일 x
# total_data.ts <- ts(total_data_a_b, start=c(2006,1), frequency = 1) # 스케일 o (표준화)
plot(total_data.ts)
plot(total_data.ts, plot.type = "single", lty = 1:6)


lapply(total_data.ts, levels)
fit.consMR <- tslm(population ~ economically_population + employment_rate + unemployment_rate + gdp + birth, data = total_data.ts)
summary(fit.consMR)
fit.consMR2 <- tslm(population ~ economically_population + employment_rate + unemployment_rate + gdp + birth + death + one_person, data = total_data.ts)
summary(fit.consMR2)

h <- 10
newdata <- data.frame(
  economically_population = c(1,1,1,1,1,1,1,1,1,1),
  employment_rate = c(1,1,1,1,1,1,1,1,1,1),
  unemployment_rate = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
  gdp = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
  birth = c(0,0,0,0,0,0,0,0,0,0)
)
library(forecast)
fcast.up <- forecast(fit.consMR, newdata = newdata)
newdata <- data.frame(
  economically_population = rep(-1, h),
  employment_rate = rep(-1, h),
  unemployment_rate = rep(-0.5, h), 
  gdp = rep(-0.5, h),
  birth = rep(0, h))
fcast.down <- forecast(fit.consMR, newdata = newdata)

library(ggplot2)
autoplot(total_data.ts[,1]) +
  autolayer(fcast.up, PI = TRUE, series = "증가") +
  autolayer(fcast.down, PI = TRUE, series = "감소") +
  guides(colour=guide_legend(title = "시나리오")) +
  xlab("연도")

#실제 인구 수와 예측된 인구 수의 시간 그래프
autoplot(total_data.ts[,'population'], series="데이터") +
  autolayer(fitted(fit.consMR), series="적합값") +
  xlab("연도") + ylab("") +
  guides(colour=guide_legend(title=" "))


cbind(Data = total_data.ts[,"population"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("적합값 (예측된 값)") +
  xlab("데이터 (예측된 값)") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.consMR)

# 잔차 대 각 예측변수의 산점도
df <- as.data.frame(total_data.ts)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=economically_population, y=Residuals)) +
  geom_point() + xlab("경제활동인구") + ylab("잔차")
p2 <- ggplot(df, aes(x=employment_rate, y=Residuals)) +
  geom_point() + xlab("고용률") + ylab("잔차")
p3 <- ggplot(df, aes(x=unemployment_rate, y=Residuals)) +
  geom_point() + xlab("실업률") + ylab("잔차")
p4 <- ggplot(df, aes(x=gdp, y=Residuals)) +
  geom_point() + xlab("gdp") + ylab("잔차")
p5 <- ggplot(df, aes(x=birth, y=Residuals)) +
  geom_point() + xlab("출생률") + ylab("잔차")
gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow=2)

# 잔차 대 적합값의 산점도
cbind(Fitted = fitted(fit.consMR),
      Residuals = residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point() +
  xlab("적합값") + ylab("잔차")


CV(fit.consMR) # 
CV(fit.consMR2)


# Estimate parameters
fc <- holt(popul, h=15)
# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)
#>               ME  RMSE   MAE MPE MAPE MASE  ACF1
#> Training set -0.06 0.25 0.17 -4.36 23.81 0.58 -0.07
fc2 <- holt(popul, damped=TRUE, h = 15)
autoplot(popul) +
  autolayer(fc, series="홀트 기법", PI=FALSE) +
  autolayer(fc2, series="감쇠 홀트 기법", PI=FALSE) +
  ylab("인구 수(명)") + xlab("연도") +
  guides(colour=guide_legend(title="예측값"))

e1 <- tsCV(popul, ses, h=1)
e2 <- tsCV(popul, holt, h=1)
e3 <- tsCV(popul, holt, damped=TRUE, h=1)
# Compare MSE:
mean(e1^2, na.rm=TRUE)
#> [1] 0.2018029
mean(e2^2, na.rm=TRUE)
#> [1] 0.1647765
mean(e3^2, na.rm=TRUE)
#> [1] 0.1293681
# Compare MAE:
mean(abs(e1), na.rm=TRUE)
#> [1] 0.3792275
mean(abs(e2), na.rm=TRUE)
#> [1] 0.3254149
mean(abs(e3), na.rm=TRUE)
#> [1] 0.2962289

fc <- holt(popul, damped = TRUE)
autoplot(fc) +
  xlab("연도") + ylab("인구 수(명)") +
  ggtitle("감쇠 홀트 기법으로 얻은 예측값")

fc[["model"]]


fit <- auto.arima(total_data.ts[,"population"],
                  xreg = total_data.ts[,"gdp"])
cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)
checkresiduals(fit)


fcast <- forecast(fit, xreg=rep(mean(total_data.ts[,2]),8))
autoplot(fcast) + xlab("연도") +
  ylab("백분율 변화")

