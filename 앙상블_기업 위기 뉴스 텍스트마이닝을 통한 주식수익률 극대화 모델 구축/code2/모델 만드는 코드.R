if(!require(devtools)) {install.packages("devtools"); library(devtools)} 
if(!require(RHINO)) {install_github("SukjaeChoi/RHINO"); library(RHINO)}

initRhino()          


library(tm)
library(stringr)
library(caret)


#training데이터 불러오기
data_clustering<-read.csv('analysis_data10.csv')

data_clustering1<-data_clustering

#제목이랑 기사 합쳐서 term matrix만들기까지 과정
data_clustering_content<-paste(data_clustering1[,5],data_clustering1[,7],sep=" ")
data_clustering_content_norms_NNG<-sapply(data_clustering_content, getMorph, USE.NAMES=F,"NNG")

#NNG
norms_NNG_corp<- Corpus(VectorSource(data_clustering_content_norms_NNG)) 
norms_NNG_corp_tdm <- TermDocumentMatrix(norms_NNG_corp)
Encoding(norms_NNG_corp_tdm$dimnames$Terms) ='UTF-8'
norms_NNG_corp_dtm<- as.DocumentTermMatrix(norms_NNG_corp_tdm)
norms_NNG_corp_dtm_matix<-as.matrix(norms_NNG_corp_dtm)
norms_NNG_corp_dtm_matix2<-as.data.frame(norms_NNG_corp_dtm_matix)
dtm_cosum<-colSums(norms_NNG_corp_dtm_matix2)
dtm_cosum_sort<-sort(dtm_cosum,decreasing = T)
ca_NNG<-cbind(norms_NNG_corp_dtm_matix2,category=data_clustering1$category)


#룰베이스를 통해 기사 카테고리 분류하기 
#비리기사 단어
na_bili<-c('압수','감사','위반','혐의','검찰', '구속','분식','감리','의혹','수색','뇌물','횡령','조사','비리','갑질','수사','검사')
data_clustering$lable_pred<-'no_pred'

#비리기사 단어가 존재하면 비리의혹 기사로 분류하라 (다른 카테고리 동일한 원리)
data_bili<-c()
for (j in 1:nrow(data_clustering)){
  if(any(str_detect(data_clustering[j,5],na_bili)==T)){
    data_bili<-rbind(data_bili,data_clustering[j,])
    data_clustering$lable_pred[j]<-'비리의혹'
    
  }
}
data_bili$lable_pred<-'비리의혹'


#실적부진
sii<-c('실적','이익','매출액','매출','부진','매출','적자','손실')

data_sii<-c()
for (j in 1:nrow(data_clustering)){
  if(any(str_detect(data_clustering[j,5],sii)==T)){
    data_sii<-rbind(data_sii,data_clustering[j,])
    data_clustering$lable_pred[j]<-'실적부진'
  }
}

data_sii$lable_pred<-'실적부진'


#미국이슈
mee<-c('트럼프','미국','美')

data_mee<-c()
for (j in 1:nrow(data_clustering)){
  if(any(str_detect(data_clustering[j,5],mee)==T)){
    data_mee<-rbind(data_mee,data_clustering[j,])
    data_clustering$lable_pred[j]<-'미국이슈'
  }
}

data_mee$lable_pred<-'미국이슈'

#북한이슈
boo_norm<-c('경협','북한','北','대북','탄도','미사일','안보','북핵')
data_boo<-c()
for (j in 1:nrow(data_clustering)){
  if(any(str_detect(data_clustering[j,5],boo_norm)==T)){
    data_boo<-rbind(data_boo,data_clustering[j,])
    data_clustering$lable_pred[j]<-'북한이슈'
  }
}

data_boo$lable_pred<-'북한이슈'


#분류된 기사 합치기
pre_data_str<-rbind(data_bili,data_mee,data_boo,data_sii)

data_clustering$lable_pred<-as.factor(data_clustering$lable_pred)
pre_data_str$category<-as.character(pre_data_str$category)
pre_data_str$category<-as.factor(pre_data_str$category)

#분류된 기사 정확도 확인  
table(real=pre_data_str$category,predict=pre_data_str$lable_pred)[c(4,5,6,8),]



#위기기사 기반으로 주식 수익률 모델 제작 
#increase는 고려하지 않는다 
data_clustering2<-subset(data_clustering,low_day2!='increase')
data_clustering2$low_day2<-as.character(data_clustering2$low_day2)

data_clustering2$low_day2<-as.factor(data_clustering2$low_day2)

#최저점 예측모데
set.seed(124121)
control = trainControl(method='cv', search='random', number=10,verbose = TRUE)
rf.model <- train(
  low_day2 ~ 매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  + Psell + amount +Pratio,
  data = data_clustering2,
  tuneLength = 10,
  trControl = control,
  method="rf")


pred.rf <- predict(rf.model,test)델


#회복점 예측 모델 
data_clustering2$recovery_day3<-ifelse(data_clustering2$recovery_day2=='increase','increase',ifelse(data_clustering2$recovery_day2                                                                                                   =='infinity','infinity',ifelse(data_clustering2$recovery_day2=='1-8','1-8','8-30'))
data_clustering2$recovery_day3<-as.factor(data_clustering2$recovery_day3)
data_clustering2_down <- downSample(subset(data_clustering2, select =-recovery_day3), data_clustering2$recovery_day3)


#회복점 예측 모델 
set.seed(12412133)
control = trainControl(method='cv', search='random', number=10,verbose = TRUE)
rf.model2 <- train(
  Class~  매출액 + 자산총계 + 영업이익률 + 부채비율 + EPS  +부채비율 +영업이익률 + Psell + amount +Pratio ,
  data = data_clustering2_down ,
  tuneLength = 10,
  trControl = control,
  method="rf")

pred.rf2 <- predict(rf.model2,test2)

#test data
analysis_data_add_pre3<-read.csv('analysis_data_add_pre3.csv')
analysis_data_add_pre4<-subset(analysis_data_add_pre3,low_day2!='increase')
analysis_data_add_pre4$low_day2<-as.character(analysis_data_add_pre4$low_day2
)
analysis_data_add_pre4$low_day2<-as.factor(analysis_data_add_pre4$low_day2)
                                              
#최저점 평가 후 matrix제작 
pred.rf4 <- predict(rf.model,analysis_data_add_pre4)
confusionMatrix(pred.rf4, analysis_data_add_pre4$low_day2)

data1_add<-as.data.frame(cbind(analysis_data_add_pre4,pre_low=pred.rf4))



#데이터 클래스 3개로 만듬 
data1_add$recovery_day3<-ifelse(data1_add$recovery_day2=='increase','increase',ifelse(data1_add$recovery_day2
                                                                              =='infinity','infinity',ifelse(data1_add$recovery_day2=='1-8','1-8','8-30')))


data1_add$recovery_day3<-as.factor(data1_add$recovery_day3)


#회복점 test셋 평가 
pred.rf5 <- predict(rf.model2,data1_add)
confusionMatrix(pred.rf5, as.factor(data1_add$recovery_day3))

nrow(analysis_data_add_pre3)



#카테고리 분류 나이브베이즈 모델
#4개의 카테고리만 부류


ca_NNG_4<-subset(ca_NNG,category=='실적부진'|category=='미국이슈'|category=='북한이슈'|category=='비리의혹')
real<-ca_NNG_4
ca_col<-colSums(ca_NNG_4[,1:ncol(ca_NNG_4)-1])
ca_col_sort<-sort(ca_col,decreasing = T)
real2<-ifelse(real[,1:ncol(real)-1]>=1,1,0)
real2<-as.data.frame(real2)
real4<-as.data.frame(sapply(real2,as.factor))

final<-c()
Accuracy_r<-c()
Kappa_r<-c()
for (i in 40:200){
  na<-names(head(ca_col_sort,i))
  test2<-real4[,na]
  test5<-as.data.frame(cbind(test2,category=ca_NNG_4$category))
  test6<-as.data.frame(sapply(test5,as.factor))
  
  for (j in 1:10){
    set.seed(151349)
    folds = createFolds(test6 $category, k = 10 )
    bayes_train<-test6[folds[[10]],]
    bayes_test<-test6[-folds[[10]],]
    
    NB.fit <- naive.bayes(bayes_train, "category") 
    
    nb.fit <- bn.fit(NB.fit, bayes_train)
    nb.pred <- predict(nb.fit, bayes_test)
    
    
    c<-confusionMatrix(nb.pred,bayes_test$category)
    number<-i
    Accuracy<-c[3][[1]][1]
    Kappa <-c[3][[1]][2]
    Accuracy_r[j]<-Accuracy
    Kappa_r[j]<-Kappa
    
  }
  
  final<-rbind(final,cbind(number = i,Accuracy = mean(Accuracy_r),Kappa = mean(Kappa_r) ))
  
}

#단어갯수 정확성 kappa 정보
final5<-as.data.frame(final)

#정확성 plot
plot(final5$number, final5$Accuracy,xlim=c(30,200),ylim=c(0.4,1), type='l', lwd = 2)
#정확성 maxㄱ
max(final5$Accuracy)
