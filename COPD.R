COPD <- read.csv(file = "C:/Users/ksj10/Desktop/R/data/COPD_student_dataset.csv",
                 header=TRUE, sep=',')


#FEV1 = Forced Expiratory volume at 1 : 억지로 숨을 강하게 뱉을때 1초간 호기량, 폐쇄성 폐질환에서는 나가는 통로가 좁아져 있으므로 FEV1이 감소하게 됨.
#MWT1= waking distance , 6분 걷기 거리 !!

#aim : assess the association between the walking distance and lung function in COPD patients

hist(COPD$MWT1Best)
hist(COPD$MWT1Best,main="Histogram of MWT1Best",xlab="MWTBest",breaks=12) #breaks=12 의미 = 막대 개수 12개로 지정

#subset= 부분집합,  | 는 and의 의미! | 입력법 : shift+\(역슬래시)
subset(COPD, MWT1Best > 600 | MWT1Best < 150)
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1")  

#list : 여러 타입의 통계를 한 번에 넣을 수 있다
#통계값들에 결측치가 있으면 오류가 날 수 있으므로 na.rm=TRUE 처리를 해준다 !
#IQR은 4분위값을 의미! 

list("Summary"=summary(COPD$MWT1Best),"Mean"=mean(COPD$MWT1Best,na.rm=TRUE),"Standard Deviation"=sd(COPD$MWT1Best,na.rm=TRUE),"Range"=range(COPD$MWT1Best,na.rm=TRUE),"InterQuartile Range"=IQR(COPD$MWT1Best,na.rm=TRUE))

#Scattorplot= 산점도 보기 !! -> plot(x,y)
plot(COPD$FEV1,COPD$MWT1Best,xlab="FEV1",ylab="MWT1Best")

#상관관계
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs", method="pearson")
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs", method="spearman")

#new vector name<-linear regression model
#lm(outcome~predict,data=dataframe)

MWT1Best_FEV1<-lm(MWT1Best~FEV1,data=COPD)
summary(MWT1Best_FEV1)

#TO View 95% confidence interval
confint(MWT1Best_FEV1)

par(mfrow=c(2,2))
par(mfrow=c(1,1))

#multiple regression in R
#Model name<-lm(outcome~predictor1+predictor2,data=dataframe)
# #Y = α + β1*X1 + β2*X2 + ε
# Y = 결과(즉, 종속) 변수.
# 
# X1 = 첫 번째 예측(즉, 독립) 변수.
# 
# X2 = 두 번째 예측 변수(즉, 독립) 변수.
# 
# α = 절편(X1=X2=0일 때 평균 Y). 참고: α는 단위에 따라 다릅니다.
# 
# β1 = 선의 기울기(X2가 일정하게 유지될 때 X1이 1 단위 증가할 때 Y의 변화). 참고: β1은 단위에 따라 다릅니다.
# 
# Β2 = 선의 기울기(X1이 일정하게 유지될 때 X2에서 1 단위 증가에 대한 Y의 변화). 참고: β2는 단위에 따라 다릅니다.
# 
# ε는 Y의 임의 변동, 즉 잔차입니다.


MWT1Best_FEV1_AGE<-lm(MWT1Best~FEV1+AGE,data=COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)
par(mfrow=c(2,2))
plot(MWT1Best_FEV1_AGE)

#Linear regression model 만들기 전에 step !
# 1.Inspect the dataset for missing values and outliers

#missing value 확인
install.packages("psych")
library(psych)
describe(COPD)

install.packages("gmodels")
library(gmodels)
CrossTable(COPD$copd)

sum(is.na(COPD$copd))
summary(COPD$MWT1Best)

#outlier 확인
hist(COPD$AGE)


#  2. Examine the relationship between your candidate predictor variables 

#Continuous variable
#Correlation Matrix --
my_data <- COPD[,c("AGE","PackHistory","FEV1","FEV1PRED","FVC","CAT","HAD","SGRQ")]
  #Create a new vector including the variables to be analysed
cor_matrix <- cor(my_data) #Create a correlation matrix of the variable that are to be analysed

cor_matrix #View the correlation matrix
round(cor_matrix,2) #Round the values of the correlation matrix to 2 decimal points(소수점 둘쨰자리까지 반올림!). 

pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data=COPD) #Command to produce the correlation plot

#Categorical variable
CrossTable(COPD$hypertension, COPD$IHD)

