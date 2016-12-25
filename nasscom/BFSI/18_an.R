rm(list=ls(all=T))

set.seed(999)

setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)
library(mice)
library(dummies)
library(xgboost)
library(Matrix)

tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=F)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=F)
tr.i<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=T)
te.i<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=T)

sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=F)
te<-te[,c("Behavioural_Score","Application_Score"):=0]

c<-list(tr, te)
a<- rbindlist(c)
a[,c("Industry_Domain","Metropolitan_City","Identifier","Pre-Approved Auto Limit",
     "Pre-Approved Mortgage Limit","Pre-Approved Personal Limit"):=NULL]
a$Gender<-as.numeric(as.factor(a$Gender))-1
#te$Gender<-as.numeric(as.factor(te$Gender))-1
a[Marital_Status=="UNKNOWN",`:=`(Marital_Status=NA)]
a[Occupation=="Unknown",`:=`(Occupation=NA)]
a[Education=="Unknown",`:=`(Education=NA)]
a[Residence=="Unknown",`:=`(Residence=NA)]


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
                               ,"Tenure_with_Bank_Group"
),data=a,sep="::")


df <- data.table(dumy, keep.rownames = F)
tr <- df[1:6924,-c("Application_Score","Behavioural_Score"),with=F]
te <- dumy[6925:9231,]

tr.aic<-tr.i[,Education]


xg.1<-sparse.model.matrix(Application_Score~Age+Education               
                                             +Marital_Status               
                                             +Occupation             
                                             +Gender            
                                             +Residence+Insurance_Product_type       
                                             +Insurance_Acquisition_Channel+Active_Bank_Products         
                                             +Total_Bank_Products+Commercial_Loan              
                                             +Consumer_Auto_Loan+Mortgage_Loan                
                                             +Personal_Loan+Credit_Card+Deposit+`Pre-Approved Personal Limit`
                                             +Tenure_with_Bank_Group+Tenure_of_Insurance+Salary_Amount+Credit_Limit
                                             +Total_Asset_Under_Mngmnt+Avg_Monthly_Balance-1,data=tr.i)

App_S<-tr.i[,Application_Score][1:6924]
Bh_S<-tr.i[,Behavioural_Score][1:6924]

xg.1<-sparse.model.matrix(Application_Score~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
xg.2<-sparse.model.matrix(Behavioural_Score~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
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
