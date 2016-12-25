rm(list=ls(all=T))
setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)

tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=FLASE)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=FLASE)
sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=FLASE)
#l1<-tr[,c(Application_Score)]
#l2<-tr[,c(Behavioural_Score)]
te<-te[,c("Behavioural_Score","Application_Score"):=0]

#c<-list(tr, te)
#a<- rbindlist(c)

set.seed(999)

tr$`Pre-Approved Auto Limit`<-ordered(tr$`Pre-Approved Auto Limit`,levels=
                                         c("MISSING",">480,000" ,"<360,000", "<480,000",
                                           "<240,000" ,"<120,000" ,"ZERO"),
                                     labels=c(NA,5,3,4,2,1,0))
tr$`Pre-Approved Mortgage Limit`<-ordered(tr$`Pre-Approved Mortgage Limit`,levels=
                                             c(">=5,000,000"," <2,500,000" , "<5,000,000",  "<1,000,000" ,"MISSING", "ZERO" ),labels=c(4,3,2,1,NA,0))


tr$`Pre-Approved Mortgage Limit`<-ordered(tr$`Pre-Approved Personal Limit`,levels=c("MISSING ","ZERO","<1,500,000","<1,000,000","<500,000"),labels=c(NA,0,3,2,1))


tr$Total_Bank_Products<-ordered(tr$Total_Bank_Products,levels=c(">=6","5","2","1","3","0","4"),labels=c(6,5,2,1,3,0,4))
tr$Active_Bank_Products<-ordered(tr$Active_Bank_Products,levels=c(">=5","2","1","3","0","4"),labels=c(5,2,1,3,0,4))
tr$Tenure_with_Bank_Group<-ordered(tr$Tenure_with_Bank_Group,levels=c("<=5 YRS","<=10 YRS",">10 YRS","<=2 YRS", "<=1 YRS"),labels=c(2,4,5,1,0))
tr$Education<-ordered(tr$Education,levels=c("University","Graduate and Higher","Intermediate","High School","Unknown","No Education","Primary School" ),labels=c(5,4,3,2,NA,0,1))


tr[,c("Industry_Domain","Metropolitan_City"):=NULL]
tr$Gender<-as.numeric(tr$Gender)-1

te$`Pre-Approved Auto Limit`<-ordered(te$`Pre-Approved Auto Limit`,levels=
                                          c("MISSING",">480,000" ,"<360,000", "<480,000",
                                            "<240,000" ,"<120,000"),
                                      labels=c(NA,5,3,4,2,1))
te$`Pre-Approved Mortgage Limit`<-ordered(te$`Pre-Approved Mortgage Limit`,levels=
                                              c(">=5,000,000"," <2,500,000" , "<5,000,000",  "<1,000,000" ,"MISSING" ),labels=c(4,3,2,1,NA))


te$`Pre-Approved Personal Limit`<-ordered(te$`Pre-Approved Personal Limit`,levels=c("MISSING ","<1,500,000","<1,000,000","<500,000"),labels=c(NA,3,2,1))


te$Total_Bank_Products<-ordered(te$Total_Bank_Products,levels=c(">=6","5","2","1","3","4"),labels=c(6,5,2,1,3,4))
te$Active_Bank_Products<-ordered(te$Active_Bank_Products,levels=c(">=5","2","1","3","4"),labels=c(5,2,1,3,4))
te$Tenure_with_Bank_Group<-ordered(te$Tenure_with_Bank_Group,levels=c("<=5 YRS","<=10 YRS",">10 YRS","<=2 YRS"),labels=c(2,4,5,1))
te$Education<-ordered(te$Education,levels=c("University","Graduate and Higher","Intermediate","High School","Unknown","No Education","Primary School" ),labels=c(5,4,3,2,NA,1))


te$Gender<-as.numeric(te$Gender)-1

#dim(a[complete.cases(a),]) too many NA's
#sparse_matrix <- sparse.model.matrix("Pre-Approved Personal Limit"~.-1, data = a)
#head(sparse_matrix)

library(dummies)
a1<-dummy.data.frame(names=c("Occupation"
                             ,"Insurance_Product_type"
                             ,"Indutry_Groups"
                             ,"Marital_Status"
                             ,"Customer_Segment"
                             ,"Residence"
                             ,"Insurance_Product_type"
                             ,"Insurance_Acquisition_Channel"
),data=a,sep="::")

a1$Gender<-as.numeric(a1$Gender)-1
a1<-as.data.table(a1)

##################################################################
#library(h2o)
#localH2O <- h2o.init(nthreads = -1)

###############################################################

train <- a1[1:nrow(tr),]
test <- a1[-(1:nrow(tr)),]

m1<-glm(Application_Score~.-Identifier,data=train)
m2<-glm(Behavioural_Score~.-Identifier,data=train)



train <- a[1:nrow(tr),]
test <- a[-(1:nrow(tr)),]

Dum<-dummyVars("~as.factor(c(Occupation,Insurance_Product_type,Indutry_Groups,Marital_Status,Customer_Segment,Residence,Insurance_Product_type,Insurance_Acquisition_Channel))"
               ,data=train,fullRank = T)

c.train<-data.table(predict(Dum,newdata = train))
c.test<-data.table(predict(Dum,newdata = test))
m1<-train(Application_Score~.-Identifier,data=c.train,method="lm")
m2<-train(Behavioural_Score~.-Identifier,data=c.train,method="lm")

p1<-predict(m1,newdata = test)
p2<-predict(m2,newdata = test)


#sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
#write.csv(sol,"Solution.csv",row.names=FALSE)
