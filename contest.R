library(readxl)
flood <- read_excel("~/flood/2021 빅콘테스트_데이터분석분야_퓨처스리그_홍수ZERO_댐유입량,강우,수위데이터_210803.xlsx")
View(flood)
str(flood)

flood <- flood[c(-1),]
colnames(flood) <- c("홍수사상번호" , "연", "월", "일", "시간", "유입량", "집단1유역평균강수", "집단1강우A", "집단1강우B", "집단1강우C", "집단1강우D","집단1수위E","집단1수위D",
                     "집단2유역평균강수", "집단2강우A", "집단2강우B", "집단2강우C", "집단2강우D","집단2수위E","집단2수위D","집단3유역평균강수", "집단3강우A", "집단3강우B", "집단3강우C", "집단3강우D","집단3수위E","집단3수위D",
                     "집단4유역평균강수", "집단4강우A", "집단4강우B", "집단4강우C", "집단4강우D","집단4수위E","집단4수위D","집단5유역평균강수", "집단5강우A", "집단5강우B", "집단5강우C", "집단5강우D","집단5수위E","집단5수위D",
                     "집단6유역평균강수", "집단6강우A", "집단6강우B", "집단6강우C", "집단6강우D","집단6수위E","집단6수위D")
head(flood)

#NA값 확인
sum(is.na(flood))
sapply(flood, function(x) sum(is.na(x))/length(x))* 100

# 2018년도 값 제외
library(dplyr)
library(tidyverse)
str(flood)
flood <- as.data.frame(flood[,c(-1,-(3:5))]) # 홍수사상번호, 월, 일, 시간 제외
for(i in 1:ncol(flood)){
  flood[,i] <- as.numeric(flood[,i]) # chr로 저장된 숫자데이터 num으로 변환
}
selectData <- flood %>% filter(연 != 2018)
selectTest <- flood %>% filter(연 == 2018)
selectData <- selectData[-1]
selectTest <- selectTest[-1]


str(selectTest)
str(selectData)

d.flood <- selectData %>% group_by(유입량) %>% dplyr::summarise(n())
d.flood

plot(d.flood$유입량, d.flood$`n()`) # 데이터 값은 중복이 없다.

# 상관계수
library(mlbench)
library(MASS)
library(glmnet)
library(reshape2)
library(Rfast)
library(psych)
library(biglm)
fr <- 유입량 ~ 집단1유역평균강수+집단1강우A+집단1강우B+집단1강우C+집단1강우D+집단1수위E+집단1수위D+
  집단2유역평균강수+집단2강우A+집단2강우B+집단2강우C+집단2강우D+집단2수위E+집단2수위D+집단3유역평균강수+집단3강우A+집단3강우B+집단3강우C+집단3강우D+집단3수위E+집단3수위D+
  집단4유역평균강수+집단4강우A+집단4강우B+집단4강우C+집단4강우D+집단4수위E+집단4수위D+집단5유역평균강수+집단5강우A+집단5강우B+집단5강우C+집단5강우D+집단5수위E+집단5수위D+
  집단6유역평균강수+집단6강우A+집단6강우B+집단6강우C+집단6강우D+집단6수위E+집단6수위D
 
selectData <- selectData[,c(1,5:9,15,18,22:24,26,27,30,37,40,41,43)] # p => 0.05보다 크거나 NA값인 데이터 제외
str(selectData)
scaleData <- selectData
scaleData[,-1] <- scale(selectData[,-1])
str(scaleData)

x11()
pairs.panels(scaleData, stars = TRUE, lm = TRUE)

# 파티션 나눔
set.seed(1606)
n <- nrow(scaleData)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)

train <- scaleData[training_idx,]
validation <- scaleData[validate_idx,]
test <- scaleData[test_idx,]

# 회귀분석
library(biglm)

lm_model <- lm(유입량 ~ ., data = train)
summary(lm_model)
vif(lm_model)

lm_model2 <- lm(유입량 ~ .^2, data = train)
summary(lm_model2)
length(coef(lm_model2))

data_one <- stepAIC(lm_model, direction = "forward",
                    scope= list(upper = ~.^2,lower = ~1),
                    trace = FALSE)
data_two <- stepAIC(lm_model, direction = "backward",
                    scope= list(upper = ~.^2,lower = ~1),
                    trace = FALSE)
data_both <- stepAIC(lm_model, direction = "both",
                    scope= list(upper = ~.^2,lower = ~1),
                    trace = FALSE)

summary(data_one)
summary(data_two)
summary(data_both)

matrixmodel <- model.matrix(유입량 ~ .^2 - 1, scaleData)
x <- matrixmodel[training_idx,]
y <- train$유입량

glimpse(x)

data_fit <- cv.glmnet(x, y, alpha = 1, nfolds = 20)

x11()
plot(data_fit)

log(data_fit$lambda.min) # 최적의 예측력
log(data_fit$lambda.1se) # 평균값이 1-se이상 떨어지지 않은 경우

y_obs <- validation$유입량

yhat_full <- predict(lm_model2, newdata = validation)
yhat_one <- predict(data_one, newdata = validation)
yhat_two <- predict(data_two, newdata = validation)
yhat_both <- predict(data_both, newdata = validation)
yhat_glmnet <- predict(data_fit, s = "lambda.min", newx = matrixmodel[validate_idx,])

mse <- function(yi, yhat_i){
  (mean((yi-yhat_i)^2))
}

rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

mae <- function(yi, yhat_i){
  mean(abs(yi-yhat_i))
}

mape <- function(yi, yhat_i){
  mean(abs((yi-yhat_i)/yi))*100
}

myfcn_measures <- function(yi, yhat_i){
  c(mse(yi, yhat_i),rmse(yi, yhat_i),mae(yi, yhat_i),mape(yi, yhat_i))
}

matrix_measures <- matrix(rep(0,20), ncol=4)

colnames(matrix_measures) <- c("mse", "rmse", "mae", "mape")

rownames(matrix_measures) <- c("full", "forward", "backward", "both", "LASSO")

matrix_measures[1,] <- myfcn_measures(y_obs, yhat_full)
matrix_measures[2,] <- myfcn_measures(y_obs, yhat_one)
matrix_measures[3,] <- myfcn_measures(y_obs, yhat_two)
matrix_measures[4,] <- myfcn_measures(y_obs, yhat_both)
matrix_measures[5,] <- myfcn_measures(y_obs, yhat_glmnet)# 모델 평가

matrix_measures


colMins(matrix_measures)

myfcn_measures(test$유입량, predict(data_one, newdata = test))
myfcn_measures(test$유입량, predict(data_two, newdata = test)) 
myfcn_measures(test$유입량, predict(data_both, newdata = test)) 
myfcn_measures(test$유입량, predict(lm_model2, newdata = test))

cor( predict(data_both, newdata = test), test$유입량) # 0.9849418 [1] 137462.38446    370.75920    231.87177     33.87698   both     102302.3 319.8472 214.1968  36.80481
cor( predict(lm_model2, newdata = test), test$유입량) # 0.9864874 [1] 124000.33744    352.13682    227.39961     35.59705   full     101377.6 318.3985 215.8488  39.59140
cor( predict(data_one, newdata = test), test$유입량)  # 0.9855333 [1] 132656.70517    364.22068    226.84493     34.93515   forward  103101.5 321.0942 213.8444  39.43575

#fullmodel로 선정

pre_d <- predict(lm_model2, newdata = scaleData, interval = "confidence") 
ttmp <- cbind(pre_d,scaleData$유입량) 
ttmp

scaleTestData <- selectTest
scaleTestData[,-1] <- scale(selectTest[,-1])
str(scaleTestData)
scaleTestData2 <- scaleTestData[,c(1,5:9,15,18,22:24,26,27,30,37,40,41,43)]
str(scaleTestData2)
testMain <- scaleTestData2


predTest <- predict(lm_model2, testMain)
predTest

