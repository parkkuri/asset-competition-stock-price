
##호황불황기사 모두 기업이름 달기 
bad_good<-read.csv('bad_good.csv')



stock<-read.csv('stock (2).csv')


names1<-stock[,c(1,17,10)]

names2<-c()

names2<-unique(names1[,c(1,2,3)])

names3<-as.data.frame(names2)
na1<-as.vector(names3[,1])
na2<-as.vector(names3[,2])
names3<-as.data.frame(names2)
names3<-names3[-c(1038,1352),]
na1<-as.vector(names3[,1])
na2<-as.vector(names3[,2])
na3<-as.vector(names3[,3])

na3_1<-NULL
for (i in 1:length(na3)){
  gg<-6-str_length(na3[i])
  
  na3_1[i]<-paste("(",str_dup(0,gg),na3[i],")",sep="")
  
}



bad<-read.csv('bad_good.csv',stringsAsFactors = F)
head(bad)

library(stringr)
cb_all<-c()
company_num1_all<-c()
label_all<-c()
for (i in 1:nrow(bad)){
  bad[i,2]<-str_replace_all(bad[i,2],'"',"")
  if (str_detect(bad[i,2],",")){
    bad_spl<-str_split(bad[i,2],",")[[1]][1]
  } else {
    bad_spl<-str_split(bad[i,2]," ")[[1]][1]
  }
  
  na1_tf<-bad_spl == na1
  na2_tf<-bad_spl == na2
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



finall<-cbind(label_all,cb_all,bad[company_num1_all,2])

data_final<-merge(finall,bad,by.x='label_all',by.y='lable')
data_final<-data_final[,-3]


write.csv(data_final,'bad_good_name.csv',row.names = F)


