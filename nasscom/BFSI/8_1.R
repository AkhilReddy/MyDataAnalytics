rm(list=ls(all=T))

set.seed(999)

setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)
library(mice)
library(dummies)
library(xgboost)
library(Matrix)
library(corrplot)
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
te<-te[,c("Behavioural_Score","Application_Score"):= NA]

c<-list(tr, te)
a<- rbindlist(c)

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

a0<-rbindlist(c)
cp<-cor(a0[,c(2,16,17,18,19,20,21,26:32),with=F],use="complete.obs")
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
corrplot.mixed(cp,order="AOE",col=col1(100))

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
imp <- mice(Dat1, m=5, maxit=10,method="pmm", seed=999)
Data.imp<- complete(imp,4)

X_train <- a_train[,":="(Salary_Amount=Data.imp$Salary_Amount,
                         Credit_Limit=Data.imp$Credit_Limit)][1:6924,]
X_train<-X_train[-c(5255,which(X_train$Application_Score==0)),]

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

ma2<-lm((X_targeta2)~.,data = X_train2)
summary(ma2)
plot(ma2)
dia(ma2)

mb1<-lm((X_targetb1)~.,data = X_train1)
summary(mb1)
plot(mb1)
dia(mb1)

mb2<-lm((X_targetb2)~.,data = X_train2)
summary(mb2)
plot(mb2)
dia(mb2)



xgb_cva1 <- xgb.cv(data=data.matrix(X_train1), nfold=10,
                   label=as.matrix(X_targeta1), booster = "gblinear", 
                   objective = "reg:linear", 
                   max.depth = 20,
                   nrounds = 35,
                   lambda = 0, 
                   lambda_bias = 0, 
                   alpha = 0)


#one-hot-encoding categorical features
ohe_feats = c('Insurance_Acquisition_Channel','Insurance_Product_type','Residence',
              'Metropolitan_City','Customer_Segment')

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

rfa1<-randomForest((X_targeta1)~.,data = X_train1)
p_rf1<-predict(rfa1,X_train1)
RMS(p_rf1,X_targeta1,36)[1]
varImpPlot(rfa1)
res_plot2(X_targeta1,p_rf1)
plot(rfa1)
rfa1<-randomForest((X_targeta1)~.-(Gender+Occupation+Indutry_Groups+
                                      Consumer_Auto_Loan+Commercial_Loan+Credit_Card+
                                      Customer_Segment+Education+Mortgage_Loan),
                                      importance=F,ntree =100,data = X_train1)
p_rf1<-predict(rfa1,X_train1)
RMS(p_rf1,X_targeta1,36)[1]
res_plot2(X_targeta1,p_rf1)

#xx<-randomForest((X_targetb)~.,data = X_train)
#px<-predict(xx,X_train)
#RMS(px,X_targetb,38)[1]


pa1<-predict(rfa1,X_test1)
pa2<-predict(ma2,X_test2)

da1<-as.data.table(cbind(X_test1$Identifier,pa1),check.names=T)[,.(P_Application_Score=pa1,Identifier=V1)]
sol<-merge(x = sol, y = da1, by = "Identifier", all.x = TRUE)
da2<-as.data.table(cbind(X_test2$Identifier,pa2),check.names=T)[,.(P_Application_Score=pa2,Identifier=V1)]
sol<-merge(x = sol, y = da2, by = "Identifier", all.x = TRUE)
sol$P_Application_Score<- rowSums(sol[,c("P_Application_Score.y","P_Application_Score"),with=F], na.rm=T)


p1<-sol$P_Application_Score
#p2<-predict(xx,X_test)
p2<-predict(mb,X_test)

sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=T)
sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)
