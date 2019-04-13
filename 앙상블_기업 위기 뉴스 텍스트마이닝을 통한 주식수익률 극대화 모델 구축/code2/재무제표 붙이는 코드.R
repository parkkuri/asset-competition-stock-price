

library(dplyr)
library(stringr)
real_news_anlysis<-read.csv('real_news_anlysis.csv')
stock_code<-read.csv('code.csv')

mer_com1<-merge(real_news_anlysis,stock_code,by.x='cb_all',by.y='회사명', no.dups = TRUE)
mer_com1$year<-as.integer(str_sub(mer_com1[,5],start=1,end=4))



company_data_real_new<-read.csv('company_data_real_new.csv')


com_data<-subset(company_data_real_new,index1=='매출액'|index1=='자산총계'|index1=='부채비율'|index1=='EPS(원)'|index1=='영업이익률'|index1=='매출액
                 ')





fun1<-function(code1){
  
  real<-com_data[com_data$code==code1,]
  return(real)
}

mer_com2<-mer_com1
colnames(mer_com2)
mer_com2[,19]<-NA
mer_com2[,20]<-NA
mer_com2[,21]<-NA
mer_com2[,22]<-NA
mer_com2[,23]<-NA


fun1<-function(code1){
  
  real<-com_data[com_data$code==code1,]
  return(real)
}




for (j in 1:nrow(mer_com2)){
  code_real<-mer_com2$종목코드[j]
  
  if(mer_com2$year[j]==2018){
    mer_com2[j,19]<-fun1(code_real)[1,][7]
    mer_com2[j,20]<-fun1(code_real)[2,][7]
    mer_com2[j,21]<-fun1(code_real)[3,][7]
    mer_com2[j,22]<-fun1(code_real)[4,][7]
    mer_com2[j,23]<-fun1(code_real)[5,][7]
  }
  
  if(mer_com2$year[j]==2017){
    mer_com2[j,19]<-fun1(code_real)[1,][6]
    mer_com2[j,20]<-fun1(code_real)[2,][6]
    mer_com2[j,21]<-fun1(code_real)[3,][6]
    mer_com2[j,22]<-fun1(code_real)[4,][6]
    mer_com2[j,23]<-fun1(code_real)[5,][6]
  }
  
  if(mer_com2$year[j]==2016){
    mer_com2[j,19]<-fun1(code_real)[1,][5]
    mer_com2[j,20]<-fun1(code_real)[2,][5]
    mer_com2[j,21]<-fun1(code_real)[3,][5]
    mer_com2[j,22]<-fun1(code_real)[4,][5]
    mer_com2[j,23]<-fun1(code_real)[5,][5]
  }
  if(mer_com2$year[j]==2015){
    mer_com2[j,19]<-fun1(code_real)[1,][4]
    mer_com2[j,20]<-fun1(code_real)[2,][4]
    mer_com2[j,21]<-fun1(code_real)[3,][4]
    mer_com2[j,22]<-fun1(code_real)[4,][4]
    mer_com2[j,23]<-fun1(code_real)[5,][4]
  }
  
  if(mer_com2$year[j]==2014){
    mer_com2[j,19]<-fun1(code_real)[1,][3]
    mer_com2[j,20]<-fun1(code_real)[2,][3]
    mer_com2[j,21]<-fun1(code_real)[3,][3]
    mer_com2[j,22]<-fun1(code_real)[4,][3]
    mer_com2[j,23]<-fun1(code_real)[5,][3]
  }
}


colnames(mer_com2)[19:23]<-c('매출액','자산총계','영업이익률','부채비율','EPS')

mer_com2<-mer_com2[,-c(9,10,11,12,13,14,15,16,17)]

company_buliding<-read.csv('company_buliding.csv')
company_buliding$buildingyear<-as.integer(str_sub(company_buliding[,2],start=1,end=4))
stock_code<-read.csv('code.csv')
mor_com3<-merge(mer_com2,stock_code,by.x='cb_all',by.y='회사명', no.dups = TRUE)
mor_com3<-mor_com3[,-c(16,17,18,19,20,21,22)]
mer_come4<-merge(mor_com3,company_buliding,by.x='종목코드',by.y='code_compnay')
write.csv(mer_come4,'anlysis_data_new.csv',row.names=F)
