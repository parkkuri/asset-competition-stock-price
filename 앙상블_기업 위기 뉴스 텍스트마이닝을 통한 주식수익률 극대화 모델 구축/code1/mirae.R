rm(list=ls())
setwd("C:/Users/ATIV/Desktop/미래에셋")

code<-read.csv("code_edit.csv",header=T,stringsAsFactors = F)
codename<-read.csv("codename.csv",header=T, stringsAsFactors = F)
stock01<-read.csv("stock01.csv",header=T,stringsAsFactors = F)
stock02<-read.csv('stock02.csv',header=T,stringsAsFactors = F)
stock03<-read.csv('stock03.csv',header=T,stringsAsFactors = F)
stock04<-read.csv('stock04.csv',header=T,stringsAsFactors = F)

#업종으로 업종코드와 종목명 매칭
code_f<-merge(code,codename,by="name",all.x=T)
write.csv(code_f,'code_f.csv',row.names = F)

#주어진 데이터에 종목코드를 추가하기 위한 데이터 MERGE
stock_01<-merge(stock01,code_f,by.x='종목명',by.y="company",all.x=T)
stock_02<-merge(stock02,code_f,by.x='종목명',by.y="company",all.x=T)
stock_03<-merge(stock03,code_f,by.x='종목명',by.y="company",all.x=T)
stock_04<-merge(stock04,code_f,by.x='종목명',by.y="company",all.x=T)

#종목명이 다르게 명시되어있는 경우 비교해서 전처리
no1<-stock_01[is.na(stock_01$name),]
no2<-unique(stock_02[is.na(stock_02$name),'종목명'])
no3<-unique(stock_03[is.na(stock_03$name),'종목명'])
no4<-unique(stock_04[is.na(stock_04$name),'종목명'])

#code_f 파일의 종목명 변경 후 다시 데이터 MERGE
code_f<-read.csv('code_f.csv',header=T,stringsAsFactors = F)
stock_01<-merge(stock01,code_f,by.x="종목명",by.y='company',all.x=T)
stock_02<-merge(stock02,code_f,by.x='종목명',by.y="company",all.x=T)
stock_03<-merge(stock03,code_f,by.x='종목명',by.y="company",all.x=T)
stock_04<-merge(stock04,code_f,by.x='종목명',by.y="company",all.x=T)
no1<-unique(stock_01[is.na(stock_01$name),'종목명'])
no2<-unique(stock_02[is.na(stock_02$name),'종목명'])
no3<-unique(stock_03[is.na(stock_03$name),'종목명'])
no4<-unique(stock_04[is.na(stock_04$name),'종목명'])

#모든 종목 주가 데이터: stock
stock<-rbind(stock_01,stock_02,stock_03,stock_04)
write.csv(stock,"stock.csv",row.names=FALSE)


#텍스트마이닝
library(RColorBrewer)
library(tm)
library(rJava)
library(stringr)
library(dplyr)

if(!require(devtools)) {install.packages("devtools"); library(devtools)} 
if(!require(RHINO)) {install_github("SukjaeChoi/RHINO"); library(RHINO)}
initRhino() 

#금융기사 크롤링 데이터 불러오기
data<-read.csv('data_final.csv',header=T,stringsAsFactors = F)

#기업의 위기/호재 판별을 위한 단어 분류 
word_bad<-read.csv('word_bad.csv',header=F,stringsAsFactors = F)
word_good<-read.csv('word_good.csv',header=F,stringsAsFactors = F)
word_bad<-as.vector(word_bad[,1])
word_good<-as.vector(word_good[,1])

#기사 제목 형태소 분석
head(data)
nouns<- sapply(data[,2], getMorph, USE.NAMES=F)

check<-c() #위기단어/호재단어 모두 포함
check2<-c() #위기단어/호재단어 모두 불포함
bad<-c() #위기단어만 포함 
good<-c() #호재단어만 포함 


#기사 제목으로 위기/호재인지 판별 
for (i in 1: length(nouns)){
  
  down<-str_detect(data[i,2],'↓')
  up<-str_detect(data[i,2],'↑')
    
    if (down==T & up==T) {
      check<-rbind(check,data[i,])
    }
    else if (down==T) {
      bad<-rbind(bad,data[i,])
    }
    else if (up==T) {
      good<-rbind(good,data[i,])
    }
    else {
  a<-nouns[[i]] %in% word_bad
  b<-nouns[[i]] %in% word_good
  
  if (  any(a==T) & any(b==T) ) {
    check<-rbind(check,data[i,])
  }
  else if (any(a==T)) {
    bad<-rbind(bad,data[i,])
  }
  else if (any(b==T)){
    good<-rbind(good,data[i,])
  }
  else {
    check2<-rbind(check2,data[i,])
  }
  }
  }

#분석에 필요한 위기기사만 저장 
write.csv(bad,'bad.csv',row.names = F)


#추가 크롤링 기사 분류
data_add<-read.csv('data_final.csv',stringsAsFactors = F)

check_add<-c() #위기단어/호재단어 모두 포함
check2_add<-c() #위기단어/호재단어 모두 불포함
bad_add<-c() #위기단어만 포함 
good_add<-c() #호재단어만 포함 


for (i in 1:nrow(data_add)){
  
  down<-str_detect(data_add[i,2],word_bad)
  up<-str_detect(data_add[i,2],word_good)
  
  if (any(down)==T & any(up)==T){
    check_add<-rbind(check_add,data_add[i,])
  }
  else if (any(down)==T) {
    bad_add<-rbind(bad_add,data_add[i,])
  }
  else if (any(up)==T) {
    good_add<-rbind(good_add,data_add[i,])
  }
  else {
    check2_add<-rbind(check2_add,data_add[i,])
  }
}

bad_add$check<-'bad'
good_add$check<-'good'

bad_good<-rbind(bad_add,good_add)
write.csv(bad_good,'bad_good.csv',row.names=F)

write.csv(bad_add,'bad_add.csv',row.names = F)
write.csv(good_add,'good_add.csv',row.names = F)
write.csv(check_add,'check_add.csv',row.names = F)
write.csv(check2_add,'check2_add.csv',row.names = F)


#2번째 추가 크롤링 기사 분류
data_add2<-read.csv('new_data_add_big2.csv',stringsAsFactors = F)

check_add2<-c() #위기단어/호재단어 모두 포함
check2_add2<-c() #위기단어/호재단어 모두 불포함
bad_add2<-c() #위기단어만 포함 
good_add2<-c() #호재단어만 포함 

for (i in 1:nrow(data_add2)){
  
  down<-str_detect(data_add2[i,1],word_bad)
  up<-str_detect(data_add2[i,1],word_good)
  
  if (any(down)==T & any(up)==T){
    check_add2<-rbind(check_add2,data_add2[i,])
  }
  else if (any(down)==T) {
    bad_add2<-rbind(bad_add2,data_add2[i,])
  }
  else if (any(up)==T) {
    good_add2<-rbind(good_add2,data_add2[i,])
  }
  else {
    check2_add2<-rbind(check2_add2,data_add2[i,])
  }
}


write.csv(bad_add2,'bad_add2.csv',row.names = F)
write.csv(good_add2,'good_add2.csv',row.names = F)
write.csv(check_add2,'check_add2.csv',row.names = F)
write.csv(check2_add2,'check2_add2.csv',row.names = F)


#각 위기 기사가 어느 기업의 기사인지 분류 (규리)


#처음 크롤링한 위기기사 3,374개에 대해 위기 유형 라벨링:data_final3


#중복기사 제거 및 개수 계산: uniq2 (처음 위기유형 분류 데이터)
data_final5<-read.csv('bad_add2.csv',stringsAsFactors=F)
data_final5<-arrange(data_final5,cb_all,date)
data_final5$number<-seq(1:nrow(data_final5))

data_final5$date<-as.Date(data_final5$date)
data_final5$stand<-1

for (i in 2:nrow(data_final5)){
  if (data_final5[i,'cb_all']!=data_final5[i-1,'cb_all']){
    next
  }
  else {
    if ((data_final5[i,'date']-data_final5[i-1,'date'])<=3){
      data_final5[i,'stand']<-NA
    }
  }
}

uniq3<-subset(data_final5,stand==1)
write.csv(uniq3,'uniq3.csv',row.names = F)


###데이터 분석을 위한 데이터 프레임 생성
#주식데이터 순서대로 정렬
stock<-read.csv('stock.csv')
c<-as.character(stock$날짜)
stock$날짜<-as.Date(c,format="%Y%m%d")

stock<-stock[order(stock$종목명,stock$날짜),]
stock$number<-seq(1:nrow(stock))

#위기기사 데이터에 종목코드 붙이기


#위기기사 데이터에 재무제표 MERGE: anlysis_data_new

#분석 데이터에 변수(매도의 힘, 거래량,매도의 힘 비율 등)과 y(최저점 도달 일수, 회복점 도달 일수) 계산
analysis_data<-read.csv('finance_add2.csv',stringsAsFactors = F)
colnames(analysis_data)[1]<-'stock_code'
analysis_data<-arrange(analysis_data,stock_code,date)

analysis_data$Psell<-NA
analysis_data$amount<-NA
analysis_data$Pratio<-NA

analysis_data$low<-NA
analysis_data$recovery<-NA
analysis_data$low_day<-NA
analysis_data$recovery_day<-NA


for (i in 1:nrow(analysis_data)){
  
  d<-c()
  code<-analysis_data[i,'stock_code']
  date<-analysis_data[i,'date']
  
  com<-stock[stock$stock_code==code,]
  a<-com[com$날짜==date,]
  a<-na.omit(a)
  if (nrow(a)==0){
    next
  }
  a<-a[1,]
  
  analysis_data[i,'Psell']<-a$시가-a$종가
  analysis_data[i,'amount']<-a$거래량
  analysis_data[i,'Pratio']<-(a$시가-a$종가)/a$시가
  
  d<-stock[a$number:(a$number+309),'종가']
  min<-which.min(d[1:10])
  if (min==1){
    analysis_data[i,c('low','low_day','recovery','recovery_day')]<-'increase'
  }
  else if(min!=1) {
  
    analysis_data$low[i]<-ifelse(min==10,'infinity',as.character(stock[a$number+min-1,'날짜']))
    analysis_data$low_day[i]<-ifelse(min==10,'infinity',min-1)
  
  if (analysis_data[i,'low']=='infinity'){
    analysis_data[i,c('low','low_day','recovery','recovery_day')]<-'infinity'
  }
  
  else if (a$stock_code!=stock[a$number+min-1,'stock_code']){
    analysis_data[i,c('low','low_day','recovery','recovery_day')]<-'no_data'
  }
  
  else{
  
    stand<-stock[a$number-1,'종가']
    rec<-which(d[min:(min+299)]>stand)[1]
    analysis_data$recovery[i]<-ifelse(is.na(rec),'infinity',as.character(stock[a$number+min+rec-2,'날짜']))
    analysis_data$recovery_day[i]<-ifelse(is.na(rec),'infinity',min+rec-2)
    if(a$stock_code!=stock[a$number+min+rec-2,'stock_code']){
      analysis_data[i,c('recovery','recovery_day')]<-'no_data'
    }
  }
  }
  }

write.csv(analysis_data,'analysis_data.csv',row.names = F)


#변수 합치기 (low->(increase,1-2,3-8,infinity) / recovery->(increase,1-8,9-15,16-30,infinity))
analysis_data2<-analysis_data
analysis_data2$low_day2<-NA
analysis_data2$recovery_day2<-NA

analysis_data2$low_day2<-ifelse(analysis_data$low_day==1|analysis_data$low_day==2,'1-2',ifelse(analysis_data$low_day==3|analysis_data$low_day==4|analysis_data$low_day==5|analysis_data$low_day==6|analysis_data$low_day==7|analysis_data$low_day==8,'3-8',ifelse(analysis_data$low_day=='increase','increase','infinity')))
analysis_data2$recovery_day2<-ifelse(analysis_data$recovery_day==1|analysis_data$recovery_day==2|analysis_data$recovery_day==3|analysis_data$recovery_day==4|analysis_data$recovery_day==5|analysis_data$recovery_day==6|analysis_data$recovery_day==7|analysis_data$recovery_day==8,'1-8',ifelse(
                              analysis_data$recovery_day==9|analysis_data$recovery_day==10|analysis_data$recovery_day==11|analysis_data$recovery_day==12|analysis_data$recovery_day==13|analysis_data$recovery_day==14|analysis_data$recovery_day==15,'9-15',
                              ifelse(analysis_data$recovery_day==16|analysis_data$recovery_day==17|analysis_data$recovery_day==18|analysis_data$recovery_day==19|analysis_data$recovery_day==20|analysis_data$recovery_day==21|analysis_data$recovery_day==22|analysis_data$recovery_day==23|analysis_data$recovery_day==24
                                     |analysis_data$recovery_day==25|analysis_data$recovery_day==26|analysis_data$recovery_day==27|analysis_data$recovery_day==28|analysis_data$recovery_day==29|analysis_data$recovery_day==30,'16-30',ifelse(analysis_data$recovery_day=='increase','increase','infinity'))))

analysis_data2<-na.omit(analysis_data2)
analysis_data2<-subset(analysis_data2, low_day!='no_data' & recovery_day!='no_data')

write.csv(analysis_data2,'analysis_data2.csv',row.names = F)

#업종이름 붙이기
code<-read.csv('code_ff.csv')
code<-code[,c(1,3)]

analysis_data3<-merge(analysis_data2,code,by='stock_code',all.x=T)
write.csv(analysis_data3,'analysis_data3.csv',row.names = F)


###모델 만들기###
library(ggplot2)
library(lazyeval)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(lime)) install.packages("lime"); library(lime)
if(!require(mice)) install.packages("mice"); library(mice)


#train / test 나누기
analysis_data3<-read.csv('analysis_data.csv',stringsAsFactors = F)
analysis_data3<-na.omit(analysis_data3)

analysis_data3$low_day2<-as.factor(analysis_data3$low_day2)
analysis_data3$recovery_day2<-as.factor(analysis_data3$recovery_day2)
                                   
idx<-createDataPartition(analysis_data3$low_day2, p = 0.7, list=F)
train1<-analysis_data3[idx,]
test1<-analysis_data3[-idx,]

idx<-createDataPartition(analysis_data3$recovery_day2, p = 0.7, list=F)
train2<-analysis_data3[idx,]
test2<-analysis_data3[-idx,]

#불균형 데이터 업샘플링
test_up1<- upSample(subset(train, select = -low_day2), train$low_day2)
test_up2 <- upSample(subset(train, select = -recovery_day2), train$recovery_day2)


formula1<-low_day2 ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  + Psell + amount + Pratio
formula2<-recovery_day2 ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  + Psell + amount + Pratio


###randomforest###
#최저점 예측
set.seed(87127)
control = trainControl(method='cv', search='random', number=10,verbose = TRUE)
rf.model <- train(
  Class ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  + Psell + amount + Pratio,
  data = train1,
  tuneLength = 10,
  trControl = control,
  method="rf")

rf.model
pred.rf <- predict(rf.model,test_up1)
confusionMatrix(pred.rf, as.factor(test_up1$low_day2))


#회복점 예측 
set.seed(87127)
control = trainControl(method='cv', search='random', number=10,verbose = TRUE)
rf.model2 <- train(
  Class ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  + Psell + amount + Pratio,
  data = train2,
  tuneLength = 10,
  trControl = control,
  method="rf")

rf.model
pred.rf2 <- predict(rf.model2,test_up2)
confusionMatrix(pred.rf2, as.factor(test_up2$recovery_day2))



#category 범주 변경 후 데이터 저장 
analysis_data10<-analysis_data
analysis_data2<-subset(analysis_data10,analysis_data10$category=='고점논란'|analysis_data10$category=='국내증시위기'|analysis_data10$category=='대규모매도'|analysis_data10$category=='미국이슈'|analysis_data10$category=='북한이슈'|analysis_data10$category=='비리의혹'|analysis_data10$category=='사업중단'|analysis_data10$category=='실적부진'|analysis_data10$category=='제도적위기'|analysis_data10$category=='제품불량'|analysis_data10$category=='중국이슈')
analysis_data11<-subset(analysis_data2,analysis_data2$recovery_day!='increase' & analysis_data2$recovery_day!='infinity' & analysis_data2$recovery_day!='no_data')
library(lattice)
bwplot(category ~ recovery_day, data=analysis_data11,xlim=1:60) 
analysis_data12<-subset(analysis_data11,analysis_data11$category!='제품불량'&analysis_data11$category!='제도적위기'&analysis_data11$category!='국내증시위기')

write.csv(analysis_data12,'analysis_data20.csv',row.names=F)


#범주 변경한 데이터 y의 회복점을 'infinity','no_data','increase','number' 4가지 factor로 변경해서 다시 모델 적합 
analysis_data20<-read.csv('analysis_data20.csv')
infi<-subset(analysis_data20,recovery_day=='infinity')
no_da<-subset(analysis_data20,recovery_day=='no_data')
incr<-subset(analysis_data20,recovery_day=='increase')
num<-subset(analysis_data20,analysis_data20$recovery_day!='increase' & analysis_data20$recovery_day!='infinity' & analysis_data20$recovery_day!='no_data')
num2<-subset(num,as.integer(num$recovery_day)>8)
num1<-subset(num,as.integer(num$recovery_day)<=8)
num1$infi<-'1-8'
analysis_data21<-rbind(infi,incr,num2)
analysis_data21$infi<-ifelse(analysis_data21$recovery_day=='infinity','infi',ifelse(analysis_data21$recovery_day=='increase','incr','num'))
analysis_data21<-rbind(analysis_data21,num1)
analysis_data21$infi<-as.factor(analysis_data21$infi)

idx<-createDataPartition(analysis_data21$infi, p = 0.7, list=F)
train_raw<-analysis_data21[idx,]
test<-analysis_data21[-idx,]

train <- upSample(subset(train_raw, select = -infi), train_raw$infi)

control = trainControl(method='cv', search='random', number=10,verbose = TRUE)
rf.model <- train(
  Class ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS + buildingyear + Psell + amount + Pratio,
  data = train,
  tuneLength = 10,
  trControl = control,
  method="rf")

rf.model

pred.rf <- predict(rf.model,test)
confusionMatrix(pred.rf,test$infi)

rf<-randomForest(infi ~ 매출액 + 영업이익률 + 부채비율 + EPS + Psell + amount + Pratio, data=train)
varImpPlot(rf)
pred.rf2 <- predict(rf,test)
confusionMatrix(pred.rf2,test$infi)


#위기기사/호재기사 시각화를 위한 코드 
bad_good_name<-read.csv('bad_good_name.csv')

idx<-createDataPartition(bad_good_name$check, p = 0.02, list=F)
train<-bad_good_name[idx,]
test<-bad_good_name[-idx,]

write.csv(train,'bad_good_300.csv',row.names = F)


