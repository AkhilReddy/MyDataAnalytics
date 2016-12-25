rm(list=ls(all=T))

set.seed(999)

setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)
library(mice)
library(dummies)
library(xgboost)
library(Matrix)
library(car)
library(leaps)
library(xlsx)
require('vcd')

source("funplot.R")

tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=F)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=F)

sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=T)
te<-te[,c("Behavioural_Score","Application_Score"):=0]

c<-list(tr, te)
a<- rbindlist(c)

a$`Pre-Approved Auto Limit`<-factor(a$`Pre-Approved Auto Limit`,levels=
                                        c("MISSING",">480,000" ,"<360,000", "<480,000",
                                          "<240,000" ,"<120,000" ,"ZERO"),
                                    labels=c(NA,5,3,4,2,1,0))
a$`Pre-Approved Mortgage Limit`<-factor(a$`Pre-Approved Mortgage Limit`,levels=
                                            c(">=5,000,000"," <2,500,000" , "<5,000,000",  "<1,000,000" ,"MISSING", "ZERO" ),labels=c(4,3,2,1,NA,0))


a$`Pre-Approved Personal Limit`<-factor(a$`Pre-Approved Personal Limit`,levels=c("MISSING ","ZERO","<1,500,000","<1,000,000","<500,000"),labels=c(NA,0,3,2,1))


a$Total_Bank_Products<-factor(a$Total_Bank_Products,levels=c(">=6","5","2","1","3","0","4"),labels=c(6,5,2,1,3,0,4))
a$Active_Bank_Products<-factor(a$Active_Bank_Products,levels=c(">=5","2","1","3","0","4"),labels=c(5,2,1,3,0,4))
a$Tenure_with_Bank_Group<-factor(a$Tenure_with_Bank_Group,levels=c("<=5 YRS","<=10 YRS",">10 YRS","<=2 YRS", "<=1 YRS"),labels=c(2,4,5,1,0))
a$Education<-factor(a$Education,levels=c("University","Graduate and Higher","Intermediate","High School","Unknown","No Education","Primary School" ),labels=c(5,4,3,2,NA,0,1))


#a<-a[,":="(Salary_Amount=Data.imp$Salary_Amount,
#                      Credit_Limit=Data.imp$Credit_Limit,
#                     `Pre-Approved Mortgage Limit`=Data.imp$`Pre-Approved Mortgage Limit`,
#                     `Pre-Approved Auto Limit`=Data.imp$`Pre-Approved Auto Limit`,
#`Pre-Approved Personal Limit`=Data.imp$`Pre-Approved Personal Limit`)]

a_train <- a[,":="(Identifier =as.numeric(Identifier),
                   Age=as.numeric(Age),
                   Education=as.numeric(as.factor(Education)),
                   Indutry_Groups=as.numeric(as.factor(Indutry_Groups)),
                   Industry_Domain=as.numeric(as.factor(Industry_Domain)),
                   Marital_Status=as.numeric(as.factor(Marital_Status)),
                   Occupation=as.numeric(as.factor(Occupation)),
                   Customer_Segment=as.numeric(as.factor(Customer_Segment)),
                   Gender=ifelse(Gender=="Male",1,0),
                   Metropolitan_City=as.numeric(as.factor(Metropolitan_City)),
                   Residence=as.numeric(as.factor(Residence)),
                   Insurance_Product_type=as.numeric(as.factor(Insurance_Product_type)),
                   Insurance_Acquisition_Channel=as.numeric(as.factor(Insurance_Acquisition_Channel)),
                   Active_Bank_Products=as.numeric(Active_Bank_Products),
                   Total_Bank_Products=as.numeric(Total_Bank_Products),
                   Commercial_Loan=as.numeric(as.factor(Commercial_Loan)),
                   Consumer_Auto_Loan=as.numeric(as.factor(Consumer_Auto_Loan)),
                   Mortgage_Loan=as.numeric(as.factor(Mortgage_Loan)),
                   Personal_Loan=as.numeric(as.factor(Personal_Loan)),
                   Credit_Card=as.numeric(as.factor(Credit_Card)),
                   Deposit=as.numeric(as.factor(Deposit)),
                   `Pre-Approved Auto Limit`=as.numeric(`Pre-Approved Auto Limit`),
                   `Pre-Approved Mortgage Limit`=as.numeric(`Pre-Approved Mortgage Limit`),
                   `Pre-Approved Personal Limit`=as.numeric(`Pre-Approved Personal Limit`),
                   Tenure_with_Bank_Group=as.numeric(Tenure_with_Bank_Group),
                   Tenure_of_Insurance=as.numeric(Tenure_of_Insurance),
                   Salary_Amount=as.numeric(Salary_Amount),
                   Credit_Limit=as.numeric(Credit_Limit),
                   Total_Asset_Under_Mngmnt=as.numeric(as.factor(Total_Asset_Under_Mngmnt)),
                   Avg_Monthly_Balance=as.numeric(Avg_Monthly_Balance),
                   Application_Score=as.numeric(Application_Score),
                   Behavioural_Score=as.numeric(Behavioural_Score)
)]


Dat1 <- subset(a_train, select=c(Salary_Amount,Credit_Limit,Avg_Monthly_Balance,Tenure_of_Insurance,
                                 `Pre-Approved Mortgage Limit`,`Pre-Approved Auto Limit`,Tenure_with_Bank_Group,
                                 `Pre-Approved Personal Limit`,Active_Bank_Products,Total_Bank_Products,Age)) 
imp <- mice(Dat1, m=3, maxit=10,method="pmm", seed=999)
Data.imp<- complete(imp,3)

X_train <- a_train[,":="(Salary_Amount=Data.imp$Salary_Amount,
                         Credit_Limit=Data.imp$Credit_Limit,
                         `Pre-Approved Mortgage Limit`=Data.imp$`Pre-Approved Mortgage Limit`,
                         `Pre-Approved Auto Limit`=Data.imp$`Pre-Approved Auto Limit`,
                         `Pre-Approved Personal Limit`=Data.imp$`Pre-Approved Personal Limit`)][1:6924,]
X_test <- a_train[,":="(Salary_Amount=Data.imp$Salary_Amount,
                        Credit_Limit=Data.imp$Credit_Limit,
                        `Pre-Approved Mortgage Limit`=Data.imp$`Pre-Approved Mortgage Limit`,
                        `Pre-Approved Auto Limit`=Data.imp$`Pre-Approved Auto Limit`,
                        `Pre-Approved Personal Limit`=Data.imp$`Pre-Approved Personal Limit`)][6925:9231,]

X_targeta <- as.numeric(X_train$Application_Score)
X_targetb <- as.numeric(X_train$Behavioural_Score)

X_train1<-X_train[X_train$Customer_Segment %in% c(1,5)]
X_train2<-X_train[X_train$Customer_Segment %in% c(2,3,4)]

X_targeta1 <- as.numeric(X_train1$Application_Score)
X_targetb1 <- as.numeric(X_train1$Behavioural_Score)
X_targeta2 <- as.numeric(X_train2$Application_Score)
X_targetb2 <- as.numeric(X_train2$Behavioural_Score)


X_train<-X_train[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]
X_train1<-X_train1[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]
X_train2<-X_train2[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]

#X_test <- a_train[,c("Behavioural_Score","Application_Score"):=0][6925:9231,]
X_test1<-X_test[X_test$Customer_Segment %in% c(1,5)]
X_test2<-X_test[X_test$Customer_Segment %in% c(2,3,4)]
X_test1 <- X_test1[, .SDcols=names(X_train)]
X_test2 <- X_test2[, .SDcols=names(X_train)]

ma1<-lm((X_targeta1)~.,data = X_train1)
mb1<-lm((X_targetb1)~.,data = X_train1)
ma2<-lm((X_targeta2)~.,data = X_train2)
mb2<-lm((X_targetb2)~.,data = X_train2)


plot(m1)
dia(m1)
summary(mb1)

pa1<-predict(ma1,X_test)
pa2<-predict(ma2,X_test)
pb1<-predict(mb1,X_test)
pb2<-predict(mb2,X_test)


X_target1<-X_target1[]
X_target2<-X_target2[X_target2$Customer_Segment %in% c(2,3,4)]
X_train1<-X_train[X_train$Customer_Segment %in% c(1,5)]
X_train2<-X_train[X_train$Customer_Segment %in% c(2,3,4)]

model_xgb_cv1 <- xgb.cv(data=as.matrix(X_train1), nfold=10,
                        label=as.matrix(X_targeta1), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 20,
                        nrounds = 35,
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)
model_xgb1 <- xgboost(data=as.matrix(X_train), label=as.matrix(X_target1), 
                      booster = "gblinear", 
                      objective = "reg:linear", 
                      max.depth = 40, 
                      nround = 60, 
                      lambda = 0, 
                      lambda_bias = 0, 
                      alpha = 0)

model_xgb_cv2 <- xgb.cv(data=as.matrix(X_train), nfold=10,label=as.matrix(X_target2), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 20, 
                        nround = 30, 
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)
model_xgb2 <- xgboost(data=as.matrix(X_train), label=as.matrix(X_target2), 
                      booster = "gblinear", 
                      objective = "reg:linear", 
                      max.depth = 20, 
                      nround = 30, 
                      lambda = 0, 
                      lambda_bias = 0, 
                      alpha = 0)

p1<-predict(model_xgb1,as.matrix(X_test))
p2<-predict(model_xgb2,as.matrix(X_test))

sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)





dumy<-dummy.data.frame(names=c("Occupation"
                               ,"Insurance_Product_type"
                               ,"Indutry_Groups"
                               ,"Marital_Status"
                               ,"Customer_Segment"
                               ,"Residence"
                               ,"Insurance_Product_type"
                               ,"Insurance_Acquisition_Channel"
                               ,"Education"
                               ,"Active_Bank_Products"
                               ,"Total_Bank_Products"
                               ,"Industry_Domain"
                               ,"Metropolitan_City"
                               
),data=a,sep="")



tr.i<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=T)
te.i<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=T)

sol.i<-fread("BFSI - Solution submission template.csv",stringsAsFactors=T)
te.i<-te.i[,c("Behavioural_Score","Application_Score"):=0]

xg.1<-model.matrix(Application_Score~Age+Education               
                   +Marital_Status               
                   +Occupation             
                   +Gender            
                   +Residence+Insurance_Product_type       
                   +Insurance_Acquisition_Channel+Active_Bank_Products         
                   +Total_Bank_Products+Commercial_Loan              
                   +Consumer_Auto_Loan+Mortgage_Loan                
                   +Personal_Loan+Credit_Card+Deposit+
                       +Tenure_with_Bank_Group+Tenure_of_Insurance
                   +Salary_Amount+Credit_Limit
                   +Total_Asset_Under_Mngmnt+Avg_Monthly_Balance-1,data=tr.i)
tr.i<-tr.i[!c(923,8214,7237,6732,7004),]

App_S<-tr.i[,Application_Score][1:6819]
Bh_S<-tr.i[,Behavioural_Score][1:6819]

xg.1<-sparse.model.matrix(Application_Score~Age+Education+Marital_Status+Occupation+Personal_Loan+
                              Credit_Card+Commercial_Loan+Gender+Consumer_Auto_Loan
                          +Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence
                          +Indutry_Groups+Mortgage_Loan+Deposit+Tenure_of_Insurance+
                              Salary_Amount+Credit_Limit+Avg_Monthly_Balance-1,data=tr.i)
xg.2<-sparse.model.matrix(Behavioural_Score~Age+Education+Marital_Status+Occupation+Personal_Loan+Credit_Card+
                              Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type
                          +Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+
                              Active_Bank_Products+Mortgage_Loan+Deposit+Tenure_with_Bank_Group+Tenure_of_Insurance+
                              Salary_Amount+Credit_Limit+Avg_Monthly_Balance-1,data=tr.i)
#xg_test<-sparse.model.matrix(~Age+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=te.i)


xg_test<-sparse.model.matrix(~Age+Education+Marital_Status+Occupation+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+
                                 Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ 
                                 Marital_Status+ Residence+Indutry_Groups+
                                 Active_Bank_Products+Mortgage_Loan+Deposit+Tenure_with_Bank_Group+Tenure_of_Insurance+
                                 Salary_Amount+Credit_Limit+Avg_Monthly_Balance-1,data=te.i)
xg.1<-sparse.model.matrix(Application_Score~Age+Education+Occupation+Marital_Status+Personal_Loan+Credit_Card
                          +Active_Bank_Products+Avg_Monthly_Balance+Total_Asset_Under_Mngmnt+Tenure_of_Insurance+
                              Mortgage_Loan+Deposit+Tenure_with_Bank_Group+ Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type
                          +Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups -1,data=tr.i)
xg.2<-sparse.model.matrix(Behavioural_Score~Age+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)



xg.1<-sparse.model.matrix(Application_Score~Age+Education+Occupation+Marital_Status+Personal_Loan+Credit_Card
                          +Active_Bank_Products+Avg_Monthly_Balance+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups -1,data=tr.i)


xg_test<-sparse.model.matrix(~Age+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=te.i)xg_test<-sparse.model.matrix(~Age+Education+Occupation+Marital_Status+Personal_Loan+Credit_Card
                                                                                                                                                                                                                                                                              +Active_Bank_Products+Avg_Monthly_Balance+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups-1,data=te.i)



bm1_cv2<-xgb.cv(data = xg.1,label = App_S , nfold=10,
                booster = "gblinear", 
                objective = "reg:linear", 
                max.depth = 35, 
                nround = 5, 
                lambda = 0, 
                lambda_bias = 0, 
                alpha = 0)

bm1<-xgboost(data = xg.1,label = App_S, 
             booster = "gblinear", 
             objective = "reg:linear", 
             max.depth = 35, 
             nround = 3, 
             lambda = 0, 
             lambda_bias = 0, 
             alpha = 0)


bm2_cv2<-xgb.cv(data = xg.2,label = Bh_S, nfold=10,
                booster = "gblinear", 
                objective = "reg:linear", 
                max.depth = 45, 
                nround = 25, 
                lambda = 0, 
                lambda_bias = 0, 
                alpha = 0)

bm2<-xgboost(data = xg.2,label = Bh_S, 
             booster = "gblinear", 
             objective = "reg:linear", 
             max.depth = 45, 
             nround = 25, 
             lambda = 0, 
             lambda_bias = 0, 
             alpha = 0)

p1.i<-predict(bm1,xg_test)
p2.i<-predict(bm2,xg_test)

p1.f<-rowMeans(cbind(p1,p1.i), na.rm=TRUE)
p2.f<-p2

library(xlsx)
sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1.i,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)
