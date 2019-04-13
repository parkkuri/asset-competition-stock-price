

if(!require(devtools)) {install.packages("devtools"); library(devtools)} 
if(!require(RHINO)) {install_github("SukjaeChoi/RHINO"); library(RHINO)}

initRhino()          






library(KoNLP)
library(arules)
library(igraph)
library(combinat)
library(lda)
library(RTextTools)
library(topicmodels)
library(tm)
library(Rcpp)
library(plyr)


#주식 데이터
stock<-read.csv('stock (2).csv')


names1<-stock[,c(1,17,10)]

names2<-c()

names2<-unique(names1[,c(1,2,3)])

names3<-as.data.frame(names2)
na1<-as.vector(names3[,1])
na2<-as.vector(names3[,2])
names3<-as.data.frame(names2)
names3<-names3[-c(1038,1352),]
#기업이름1(한글)
na1<-as.vector(names3[,1])
#기업이름2(영문)
na2<-as.vector(names3[,2])
#종목코드
na3<-as.vector(names3[,3])

#네이버 뉴스에는 종목코드가 앞에 0이 붙여있는 채로 나오므로 처리 
na3_1<-NULL
for (i in 1:length(na3)){
 gg<-6-str_length(na3[i])

 na3_1[i]<-paste("(",str_dup(0,gg),na3[i],")",sep="")

}


#크롤링 데이터
bad<-read.csv('bad_final.csv',stringsAsFactors = F)


#기업명 달기 
library(stringr)
cb_all<-c()
company_num1_all<-c()
label_all<-c()
#콤마가 있으면 콤마 앞에 있는 회사명이 회사명이고,
#콤마가 없으면 공백 앞에 있는 회사명이 외사명이다 
for (i in 1:nrow(bad)){
  bad[i,2]<-str_replace_all(bad[i,2],'"',"")
  if (str_detect(bad[i,2],",")){
    bad_spl<-str_split(bad[i,2],",")[[1]][1]
  } else {
    bad_spl<-str_split(bad[i,2]," ")[[1]][1]
  }
  
  na1_tf<-bad_spl == na1
  na2_tf<-bad_spl == na2
  #만약에 뉴스내용에 종목코드가 있다면 그것도 그 회사의 뉴스이니 담아라 
  if (any(str_detect(bad[i,4],na3_1))==T){
    b<-which(str_detect(bad[i,4],na3_1)==T)
    r<-na2[b]
    yy<-length(r)
    cb_all<-c(cb_all,r)
    i<-rep(i,yy)
    company_num1<-i
    company_num1_all<-c(company_num1_all,company_num1)
    label<-bad[i,1]
    label_all<-c(label_all,label)
    
  } else if(any(na1_tf==T)){
    b<-which(na1_tf==T)
    r<-na2[b]
    cb_all<-c(cb_all,r)
    company_num1<-i
    company_num1_all<-c(company_num1_all,company_num1)
    label<-bad[i,1]
    label_all<-c(label_all,label)
  } else if(any(na2_tf==T)){
    c<-which(na2_tf==T)
    r<-na2[c]
    cb_all<-c(cb_all,r)
    company_num1<-i
    company_num1_all<-c(company_num1_all,company_num1)
    label<-bad[i,1]
    label_all<-c(label_all,label)
  } else{
    next
  }
}


#합치기 
finall<-cbind(label_all,cb_all,bad[company_num1_all,2])

data_final<-merge(finall,data1,by.x='label_all',by.y='lable')
data_final<-data_final[,-3]


#내보내기 
write.csv(data_final,'data_final3.csv',row.names = F)







