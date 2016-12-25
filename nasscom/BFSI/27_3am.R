rm(list=ls(all=T))

set.seed(999)

setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)
library(mice)
library(dummies)
library(xgboost)
library(Matrix)
library(car)
library(caret)
library(leaps)
library(xlsx)
library(randomForest)

require('vcd')

source("funplot.R")
#1.Data Preparation 

tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=F)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=F)

sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=T)
te<-te[,c("Behavioural_Score","Application_Score"):=0]

c<-list(tr, te)
a<- rbindlist(c)

a$`Pre-Approved Auto Limit`<-ordered(a$`Pre-Approved Auto Limit`,levels=
                                         c("MISSING",">480,000" ,"<360,000", "<480,000",
                                           "<240,000" ,"<120,000" ,"ZERO"),
                                     labels=c(NA,5,3,4,2,1,0))
a$`Pre-Approved Mortgage Limit`<-ordered(a$`Pre-Approved Mortgage Limit`,levels=
                                             c(">=5,000,000","<2,500,000" , "<5,000,000", 
                                               "<1,000,000" ,"MISSING", "ZERO" ),labels=c(4,3,2,1,NA,0))
a$`Pre-Approved Personal Limit`<-ordered(a$`Pre-Approved Personal Limit`,
                                         levels=c("MISSING ","ZERO","<1,500,000","<1,000,000","<500,000"),
                                         labels=c(NA,0,3,2,1))
a$Total_Bank_Products<-ordered(a$Total_Bank_Products,levels=c(">=6","5","2","1","3","0","4"),labels=c(6,5,2,1,3,0,4))
a$Active_Bank_Products<-ordered(a$Active_Bank_Products,levels=c(">=5","2","1","3","0","4"),labels=c(5,2,1,3,0,4))
a$Tenure_with_Bank_Group<-ordered(a$Tenure_with_Bank_Group,levels=c("<=5 YRS","<=10 YRS",">10 YRS","<=2 YRS", "<=1 YRS"),labels=c(2,4,5,1,0))
a$Education<-ordered(a$Education,levels=c("University","Graduate and Higher","Intermediate","High School","Unknown","No Education","Primary School" ),labels=c(5,4,3,2,NA,0,1))

colSums(is.na(a))
a[,.(.N),.(`Pre-Approved Auto Limit`)]
a[,.(.N),.(`Pre-Approved Mortgage Limit`)]
a[,.(.N),.(`Pre-Approved Personal Limit`)]

a_train<-a[,":="(`Pre-Approved Mortgage Limit`=NULL,
                       `Pre-Approved Auto Limit`=NULL,
                       `Pre-Approved Personal Limit`=NULL)]

a_train <- a[,":="(Identifier =as.numeric(Identifier),
                        Age=as.numeric(Age),
                        Education=(Education),
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
                        Active_Bank_Products=(Active_Bank_Products),
                        Total_Bank_Products=as.numeric(Total_Bank_Products),
                        Commercial_Loan=as.numeric(as.factor(Commercial_Loan)),
                        Consumer_Auto_Loan=as.numeric(as.factor(Consumer_Auto_Loan)),
                        Mortgage_Loan=as.numeric(as.factor(Mortgage_Loan)),
                        Personal_Loan=as.numeric(as.factor(Personal_Loan)),
                        Credit_Card=as.numeric(as.factor(Credit_Card)),
                        Deposit=as.numeric(as.factor(Deposit)),
                        Tenure_with_Bank_Group=(Tenure_with_Bank_Group),
                        Tenure_of_Insurance=as.numeric(Tenure_of_Insurance),
                        Salary_Amount=as.numeric(Salary_Amount),
                        Credit_Limit=as.numeric(Credit_Limit),
                        Total_Asset_Under_Mngmnt=as.numeric(as.factor(Total_Asset_Under_Mngmnt)),
                        Avg_Monthly_Balance=as.numeric(Avg_Monthly_Balance),
                        Application_Score=as.numeric(Application_Score),
                        Behavioural_Score=as.numeric(Behavioural_Score)
)]


Dat1 <- subset(a_train, select=c(Salary_Amount,Credit_Limit,Avg_Monthly_Balance,Tenure_of_Insurance,
Active_Bank_Products,Total_Bank_Products,Age,Total_Asset_Under_Mngmnt)) 
imp <- mice(Dat1, m=3, maxit=10,method="pmm", seed=999)
Data.imp<- complete(imp,3)

X_train <- a_train[,":="(Salary_Amount=Data.imp$Salary_Amount,
                         Credit_Limit=Data.imp$Credit_Limit)][1:6924,]
X_test <- a_train[,":="(Salary_Amount=Data.imp$Salary_Amount,
                         Credit_Limit=Data.imp$Credit_Limit)][6925:9231,]
X_targeta <- as.numeric(X_train$Application_Score)
X_targetb <- as.numeric(X_train$Behavioural_Score)

ma<-lm(X_targeta~.-(Application_Score+Behavioural_Score+Identifier),data = X_train)
summary(ma)
plot(ma)
dia(ma)

mb<-lm(X_targetb~.-(Application_Score+Behavioural_Score+Identifier),data = X_train)
summary(mb)
plot(mb)
dia(mb)

X_train1<-X_train[X_train$Customer_Segment %in% c(1,5)]
X_train2<-X_train[X_train$Customer_Segment %in% c(2,3,4)]
X_targeta1 <- as.numeric(X_train1$Application_Score)
X_targetb1 <- as.numeric(X_train1$Behavioural_Score)
X_targeta2 <- as.numeric(X_train2$Application_Score)
X_targetb2 <- as.numeric(X_train2$Behavioural_Score)

I<-X_train$Identifier
I1<-X_train1$Identifier
I2<-X_train2$Identifier

X_train<-X_train[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]
X_train1<-X_train1[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]
X_train2<-X_train2[,":="(Application_Score=NULL,Behavioural_Score=NULL,Identifier=NULL)]

X_test1<-X_test[X_test$Customer_Segment %in% c(1,5)]
X_test2<-X_test[X_test$Customer_Segment %in% c(2,3,4)]
X_test1 <- X_test1[, .SDcols=names(X_train)]
X_test2 <- X_test2[, .SDcols=names(X_train)]

ma1<-lm((X_targeta1)~.,data = X_train1)
summary(ma1)
plot(ma1)
dia(ma1)

mb1<-lm((X_targetb1)~.,data = X_train1)
summary(mb1)
plot(mb1)
dia(mb1)

ma2<-lm((X_targeta2)~.,data = X_train2)
summary(ma2)
plot(ma2)
dia(ma2)

mb2<-lm((X_targetb2)~.,data = X_train2)
summary(mb2)
plot(mb2)
dia(mb2)

pa1<-predict(ma1,X_test1)
pa2<-predict(ma2,X_test2)
pb1<-predict(mb1,X_test1)
pb2<-predict(mb2,X_test2)


tr_a1 <- xgb.DMatrix(data = data.matrix(X_train1), label = as.matrix(X_targeta1))
watchlist <- list(train=X_train1, test=X_test1)

xgb_cva1 <- xgb.cv(data=data.matrix(X_train), nfold=10,
                        label=as.matrix(X_targetb), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 20,
                        nrounds = 35,
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)

xgb_cva2 <- xgb.cv(data=data.matrix(X_train), nfold=10,
                   label=as.matrix(X_targeta), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 20, 
                        nround = 30, 
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)

#p1<-predict(model_xgb1,as.matrix(X_test))
#p2<-predict(model_xgb2,as.matrix(X_test))

#one-hot-encoding categorical features
ohe_feats = c('Insurance_Acquisition_Channel','Insurance_Product_type','Residence',
              'Metropolitan_City','Customer_Segment')
a0<-rbindlist(c)
dummies <- dummyVars(~ (Insurance_Acquisition_Channel+Insurance_Product_type
                        +Residence+Metropolitan_City+Customer_Segment+Identifier),sep=':',
                     fullRank=T,sparse=T,data = a0)
a_ohe <- as.data.table(predict(dummies, newdata =a0))
#a_cbd <- merge(a[,-c(which(colnames(a) %in% ohe_feats))],a_ohe,
#                        by='Identifier',all=TRUE)
a_cbd <- cbind(a[,-c(which(colnames(a0) %in% ohe_feats)),with=F],a_ohe)
a_cbd <-(a_cbd[,":="(Application_Score=NULL,
                     Behavioural_Score=NULL)])
X_train0 = a_cbd[a_cbd$Identifier %in% I,2:49,with=FALSE]
X_train01 = a_cbd[a_cbd$Identifier %in% I1,2:49,with=FALSE]
X_train02 = a_cbd[a_cbd$Identifier %in% I2,2:49,with=FALSE]
ma01<-lm((X_targeta1)~.,data = X_train01)
summary(ma01)

rf1<-randomForest((X_targeta1)~.,data = X_train1)
p_rf1<-predict(rf1,X_train1)
RMS(p_rf1,X_targeta1,36)[1]
varImpPlot(rf1)
res_plot2(X_targeta1,p_rf1)
plot(rf1)
rf1<-randomForest((X_targeta1)~.-(Gender+Occupation+Indutry_Groups+
                                      Consumer_Auto_Loan+Commercial_Loan+Credit_Card+
                                      Customer_Segment+Education+Mortgage_Loan),importance=F,ntree =100,
                                        data = X_train1)
p_rf1<-predict(rf1,X_train1)
RMS(p_rf1,X_targeta1,36)[1]
res_plot2(X_targeta1,p_rf1)


rf2<-train((X_targeta1)~.,data = X_train1)
p_rf2<-predict(rf2,X_train1)
RMS(p_rf2,X_targeta1,36)[1]


sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)
