COPD <- read.csv(file = "C:/Users/ksj10/Desktop/R/data/COPD_student_dataset.csv",
                 header=TRUE, sep=',')

install.packages(
  "rsthemes",
  repos = c(gadenbuie = 'https://gadenbuie.r-universe.dev', getOption("repos"))
)
#install.packages("devtools")
devtools::install_github("gadenbuie/rsthemes")
rsthemes::install_rsthemes()
rsthemes::install_rsthemes(include_base16 = TRUE)

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