rm(list=ls(all=T))

set.seed(999)

setwd("C:/Users/dell/Dropbox/compitition/nasscom/BFSI")
library(data.table)
library(mice)
tr<-fread("BFSI Stage 1 Train data.csv",stringsAsFactors=F)
te<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=F)
sol<-fread("BFSI - Solution submission template.csv",stringsAsFactors=F)
te<-te[,c("Behavioural_Score","Application_Score"):=0]

c<-list(tr, te)
a<- rbindlist(c)
a[,c("Industry_Domain","Metropolitan_City","Identifier","Pre-Approved Auto Limit",
     "Pre-Approved Mortgage Limit","Pre-Approved Personal Limit"):=NULL]

a[`Pre-Approved Personal Limit`== "MISSING",`:=`(`Pre-Approved Auto Limit`=NA,
                                                  `Pre-Approved Mortgage Limit`=NA,
                                                  `Pre-Approved Personal Limit`=NA)]

a$Gender<-as.numeric(as.factor(a$Gender))-1
#te$Gender<-as.numeric(as.factor(te$Gender))-1
a[Marital_Status=="UNKNOWN",`:=`(Marital_Status=NA)]
a[Occupation=="Unknown",`:=`(Occupation=NA)]
a[Education=="Unknown",`:=`(Education=NA)]
a[Residence=="Unknown",`:=`(Residence=NA)]
a[Industry_Domain=="Unknown",`:=`(Industry_Domain=NA)]


tr <- a[1:nrow(tr),]
te <- a[-(1:nrow(tr)),]

###########

tr[,c("Industry_Domain","Metropolitan_City","`Pre-Approved Auto Limit`","`Pre-Approved Mortgage Limit`",
      "`Pre-Approved Personal Limit`"):=NULL]
tr[,c("`Pre-Approved Auto Limit`","`Pre-Approved Mortgage Limit`",
      "`Pre-Approved Personal Limit`"):=NULL]



#a[,c(`Pre-Approved Auto Limit`,`Pre-Approved Mortgage Limit`,
#      `Pre-Approved Personal Limit`,
#      Industry_Domain,Metropolitan_City):=NULL,with=F]
#te[,c("Industry_Domain","Metropolitan_City"):=NULL]

##############

library(dummies)
dumy<-dummy.data.frame(names=c("Occupation"
                               ,"Insurance_Product_type"
                               ,"Indutry_Groups"
                               ,"Marital_Status"
                               ,"Customer_Segment"
                               ,"Residence"
                               ,"Insurance_Product_type"
                               ,"Insurance_Acquisition_Channel"
),data=a,sep="::")

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


dumy[,c("Identifier","Pre-Approved Auto Limit",
        "Pre-Approved Mortgage Limit","Pre-Approved Personal Limit"):=NULL]
df <- data.table(dumy, keep.rownames = F)
App_S<-df[,Application_Score][1:6924]
Bh_S<-df[,Behavioural_Score][1:6924]

tr <- df[1:6924,-c("Application_Score","Behavioural_Score"),with=F]
te <- dumy[6925:9231,]

bm1<-xgboost(data = tr,label = App_S, 
        booster = "gblinear", 
        objective = "binary:logistic", 
        max.depth = 5, 
        nround = 2, 
        lambda = 0, 
        lambda_bias = 0, 
        alpha = 0)

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 10,objective = "binary:logistic")





Tr<-data.table(tr[,.(`Pre-Approved Auto Limit`,
                     `Pre-Approved Mortgage Limit`,
                     `Pre-Approved Personal Limit`)])
#Tr[`Pre-Approved Personal Limit`== "MISSING",`:=`(`Pre-Approved Auto Limit`=NA,
#                                                  `Pre-Approved Mortgage Limit`=NA,
#                                                  `Pre-Approved Personal Limit`=NA)]
tr.i<-complete(mice(Tr))
################################################

m1<-lm(Application_Score~.,data=tr)
m2<-lm(Behavioural_Score~.-Identifier,data=tr)

library(caret)
Dum<-dummyVars("~.",data=tr,fullRank = T)

c.tr<-data.table(predict(Dum,newdata = tr))
c.te<-data.table(predict(Dum,newdata = te))

m1<-train(Application_Score~.-Identifier,data=c.tr,method="lm")
m2<-train(Behavioural_Score~.-Identifier,data=c.tr,method="lm")
system.time(m1<-train(Application_Score~.-Identifier,data=c.tr,method="rf"))

p1<-predict(m1,newdata = te)
p2<-predict(m2,newdata = c.te)


m1<-lm(Application_Score~Avg_Monthly_Balance+Total_Asset_Under_Mngmnt
          +Credit_Limit+Salary_Amount+Tenure_of_Insurance
          +Tenure_with_Bank_Group+`Pre-Approved Personal Limit`
       +`Pre-Approved Mortgage Limit`+Deposit+Credit_Card+Personal_Loan+Mortgage_Loan+Consumer_Auto_Loan
       +Commercial_Loan+Active_Bank_Products+Insurance_Acquisition_Channel
       +Insurance_Product_type+Residence+Gender+Customer_Segment+Occupation
       +Marital_Status+Indutry_Groups+Education+Age,data=tr)

m2<-lm(Behavioural_Score~Avg_Monthly_Balance+Total_Asset_Under_Mngmnt
       +Credit_Limit+Salary_Amount+Tenure_of_Insurance
       +Tenure_with_Bank_Group+`Pre-Approved Personal Limit`
       +Deposit+Credit_Card+Personal_Loan+Mortgage_Loan+Consumer_Auto_Loan
       +Commercial_Loan+Active_Bank_Products+Insurance_Acquisition_Channel
       +Insurance_Product_type+Residence+Gender+Customer_Segment+Occupation
       +Marital_Status+Indutry_Groups+Education+Age,data=tr)

p1<-predict(m1,newdata=te)
p2<-predict(m2,newdata=te)

library(xlsx)
sol[,c("P_Application_Score","P_Behavioural_Score"):=list(p1,p2)]
write.xlsx(sol,"BFSI - Solution submission template.xlsx",row.names=FALSE)
