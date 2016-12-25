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
require('vcd')

tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=F)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=F)
#tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=T)
#te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=T)

sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=T)
te<-te[,c("Behavioural_Score","Application_Score"):=0]

c<-list(tr, te)
a<- rbindlist(c)
#,"Pre-Approved Auto Limit",#     "Pre-Approved Mortgage Limit","Pre-Approved Personal Limit"

a[Marital_Status=="UNKNOWN",`:=`(Marital_Status=NA)]
a[Occupation=="Unknown",`:=`(Occupation=NA)]
a[Education=="Unknown",`:=`(Education=NA)]
a[Residence=="Unknown",`:=`(Residence=NA)]
a[,`:=`(`Pre-Approved Personal Limit`=as.character(`Pre-Approved Personal Limit`))]
a[`Pre-Approved Personal Limit`=="MISSING",`:=`(`Pre-Approved Personal Limit`=NA)]


a_c<-complete.cases(a)
tra<-a_c[1:nrow(tr)]
tes<-a_c[nrow(tr)+1:nrow(a)]

a_full<-a[a_c,]
a_par<-a[a_i,]

a_train <- a_full[,":="(Identifier =NULL,
                   Age=as.numeric(Age),
                   Education=as.numeric(as.factor(Education)),
                   Indutry_Groups=NULL,
                   Industry_Domain=NULL,
                   Marital_Status=as.numeric(as.factor(Marital_Status)),
                   Occupation=as.numeric(as.factor(Occupation)),
                   Customer_Segment=as.numeric(as.factor(Customer_Segment)),
                   Gender=ifelse(Gender=="Male",1,0),
                   Metropolitan_City=NULL,
                   Residence=NULL,
                   Insurance_Product_type=as.numeric(as.factor(Insurance_Product_type)),
                   Insurance_Acquisition_Channel=NULL,
                   Active_Bank_Products=as.numeric(as.factor(Active_Bank_Products)),
                   Total_Bank_Products=as.numeric(as.factor(Total_Bank_Products)),
                   Commercial_Loan=as.numeric(as.factor(Commercial_Loan)),
                   Consumer_Auto_Loan=as.numeric(as.factor(Consumer_Auto_Loan)),
                   Mortgage_Loan=as.numeric(as.factor(Mortgage_Loan)),
                   Personal_Loan=as.numeric(as.factor(Personal_Loan)),
                   Credit_Card=as.numeric(as.factor(Credit_Card)),
                   Deposit=as.numeric(as.factor(Deposit)),
                   `Pre-Approved Auto Limit`=as.numeric(as.factor(`Pre-Approved Auto Limit`)),
                   `Pre-Approved Mortgage Limit`=as.numeric(as.factor(`Pre-Approved Mortgage Limit`)),
                   `Pre-Approved Personal Limit`=as.numeric(as.factor(`Pre-Approved Personal Limit`)),
                   Tenure_with_Bank_Group=as.numeric(as.factor(Tenure_with_Bank_Group)),
                   Tenure_of_Insurance=as.numeric(Tenure_of_Insurance),
                   Salary_Amount=as.numeric(Salary_Amount),
                   Credit_Limit=as.numeric(Credit_Limit),
                   Total_Asset_Under_Mngmnt=as.numeric(Total_Asset_Under_Mngmnt),
                   Avg_Monthly_Balance=as.numeric(Avg_Monthly_Balance),
                   Application_Score=as.numeric(Application_Score),
                   Behavioural_Score=as.numeric(Behavioural_Score)
)]

X_target1 <- as.numeric(a_train$Application_Score)
X_target1<-X_target1[a_c[1:nrow(tr)]]
X_target2 <- as.numeric(a_train$Behavioural_Score)
X_target2<-X_target2[a_c[1:nrow(tr)]]

X_test <- a_train[,c("Behavioural_Score","Application_Score"):=0]
X_test <-X_test[tes]
X_train <- a_train[,":="(Application_Score=NULL,Behavioural_Score=NULL)]
X_train <-X_train[tra]


X_test <- X_test[, .SDcols=names(X_train)]

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

model_xgb_cv1 <- xgb.cv(data=as.matrix(X_train), nfold=10,label=as.matrix(X_target1), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 8,
                        nrounds = 10,
                        nfold=10,
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)
model_xgb1 <- xgboost(data=as.matrix(X_train), label=as.matrix(X_target1), 
                      booster = "gblinear", 
                      objective = "reg:linear", 
                      max.depth = 20, 
                      nround = 10, 
                      lambda = 0, 
                      lambda_bias = 0, 
                      alpha = 0)
model_xgb_cv2 <- xgb.cv(data=as.matrix(X_train), nfold=10,label=as.matrix(X_target2), booster = "gblinear", 
                        objective = "reg:linear", 
                        max.depth = 15, 
                        nround = 30, 
                        lambda = 0, 
                        lambda_bias = 0, 
                        alpha = 0)
model_xgb2 <- xgboost(data=as.matrix(X_train), label=as.matrix(X_target2), 
                      booster = "gblinear", 
                      objective = "reg:linear", 
                      max.depth = 15, 
                      nround = 30, 
                      lambda = 0, 
                      lambda_bias = 0, 
                      alpha = 0)

p1<-predict(model_xgb1,as.matrix(X_test))
p2<-predict(model_xgb2,as.matrix(X_test))

library(xlsx)
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


df <- data.table(dumy, keep.rownames = F)
tr <- df[1:6924,]

tr <- df[1:6924,-c("Application_Score","Behavioural_Score"),with=F]
te <- dumy[6925:9231,]

tr.aic<-tr.i[,Education]


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
                   +Total_Asset_Under_Mngmnt+Avg_Monthly_Balance-1,data=tr)

App_S<-train[,Application_Score][1:6924]
Bh_S<-tr.i[,Behavioural_Score][1:6924]

xg.1<-sparse.model.matrix(Application_Score~Age+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups -1,data=train)
xg.2<-sparse.model.matrix(Behavioural_Score~Age+Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
xg_test<-sparse.model.matrix(~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=te.i)

bm1<-xgboost(data = xg.1,label = App_S, 
             booster = "gblinear", 
             objective = "reg:linear", 
             max.depth = 15, 
             nround = 10, 
             lambda = 0, 
             lambda_bias = 0, 
             alpha = 0)
bm2<-xgboost(data = xg.2,label = Bh_S, 
             booster = "gblinear", 
             objective = "reg:linear", 
             max.depth = 10, 
             nround = 2, 
             lambda = 0, 
             lambda_bias = 0, 
             alpha = 0)

p1<-predict(bm1,xg_test)
p2<-predict(bm2,xg_test)

library(xlsx)
sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)


a_train <- a[,":="(Identifier =NULL,
                   Age=as.numeric(Age),
                   Education=as.numeric(as.factor(Education)),
                   Indutry_Groups=NULL,
                   Industry_Domain=NULL,
                   Marital_Status=as.numeric(as.factor(Marital_Status)),
                   Occupation=NULL,
                   Customer_Segment=as.numeric(as.factor(Customer_Segment)),
                   Gender=ifelse(Gender=="Male",1,0),
                   Metropolitan_City=NULL,
                   Residence=NULL,
                   Insurance_Product_type=as.numeric(as.factor(Insurance_Product_type)),
                   Insurance_Acquisition_Channel=NULL,
                   Active_Bank_Products=as.numeric(as.factor(Active_Bank_Products)),
                   Total_Bank_Products=NULL,
                   Commercial_Loan=as.numeric(as.factor(Commercial_Loan)),
                   Consumer_Auto_Loan=as.numeric(as.factor(Consumer_Auto_Loan)),
                   Mortgage_Loan=as.numeric(as.factor(Mortgage_Loan)),
                   Personal_Loan=as.numeric(as.factor(Personal_Loan)),
                   Credit_Card=as.numeric(as.factor(Credit_Card)),
                   Deposit=as.numeric(as.factor(Deposit)),
                   `Pre-Approved Auto Limit`=NULL,
                   `Pre-Approved Mortgage Limit`=NULL,
                   `Pre-Approved Personal Limit`=NULL,
                   Tenure_with_Bank_Group=as.numeric(as.factor(Tenure_with_Bank_Group)),
                   Tenure_of_Insurance=as.numeric(Tenure_of_Insurance),
                   Salary_Amount=as.numeric(Salary_Amount),
                   Credit_Limit=as.numeric(Credit_Limit),
                   Total_Asset_Under_Mngmnt=as.numeric(Total_Asset_Under_Mngmnt),
                   Avg_Monthly_Balance=as.numeric(Avg_Monthly_Balance),
                   Application_Score=as.numeric(Application_Score),
                   Behavioural_Score=as.numeric(Behavioural_Score)
)]
