library(corrplot)

cp<-cor(tr[,c(2,16,17,18,19,20,21,26:32),with=F],use="complete.obs")
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
corrplot.mixed(cp,order="AOE",col=col1(100))


principle.comp<-prcomp(tr[,c(2,16,17,18,19,20,21,26:32),with=F],
                       retx=F,center=T,scale. = T)

amelia_fit <- amelia(tr.i, m=5, parallel = "multicore", ords=c("Indutry_Groups","Industry_Domain",
                                                               "Marital_Status", "Occupation", "Customer_Segment"
                                                               , "Gender", "Metropolitan_City", "Residence", 
                                                               "Insurance_Product_type","Active_Bank_Products",
                                                               "Total_Bank_Products", "Insurance_Acquisition_Channel")
                                                                ,noms = c("Education","Pre-Approved Auto Limit",
                                                                "Pre-Approved Mortgage Limit", "Pre-Approved Personal Limit", 
                                                                "Tenure_with_Bank_Group"))
te.i<-fread("BFSI Stage 1 Test data.csv",stringsAsFactors=T)

xg.1<-sparse.model.matrix(Application_Score~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
xg.2<-sparse.model.matrix(Behavioural_Score~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
xg_test<-sparse.model.matrix(~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=te.i)

Identifier                    Age                          
Education                     Indutry_Groups               
Industry_Domain               Marital_Status               
Occupation                    Customer_Segment             
Gender                        Metropolitan_City            
Residence                     Insurance_Product_type       
Insurance_Acquisition_Channel Active_Bank_Products         
Total_Bank_Products           Commercial_Loan              
Consumer_Auto_Loan            Mortgage_Loan                
Personal_Loan                 Credit_Card                  
Deposit                       Pre-Approved Auto Limit      
Pre-Approved Mortgage Limit   Pre-Approved Personal Limit  
Tenure_with_Bank_Group        Tenure_of_Insurance          
Salary_Amount                 Credit_Limit                 
Total_Asset_Under_Mngmnt      Avg_Monthly_Balance          
Application_Score             Behavioural_Score   

(Age+Education+Indutry_Groups               
+Industry_Domain+Marital_Status               
+Occupation+Customer_Segment             
+Gender+Metropolitan_City            
+Residence+Insurance_Product_type       
+Insurance_Acquisition_Channel+Active_Bank_Products         
+Total_Bank_Products+Commercial_Loan              
+Consumer_Auto_Loan+Mortgage_Loan                
+Personal_Loan+Credit_Card+Deposit+`Pre-Approved Auto Limit`+`Pre-Approved Mortgage Limit` +`Pre-Approved Personal Limit`+Tenure_with_Bank_Group+Tenure_of_Insurance+Salary_Amount+Credit_Limit+Total_Asset_Under_Mngmnt+Avg_Monthly_Balance+Application_Score+Behavioural_Score)   

Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain


Industry_DomainConstruction
Industry_DomainPublic Services   
Industry_DomainTextile  
Industry_DomainTransport   
`Pre-Approved Mortgage Limit`MISSING 
`Pre-Approved Personal Limit`MISSING            

tr[,`:=`(`Pre-Approved Personal Limit`=as.character(`Pre-Approved Personal Limit`))]
tr[`Pre-Approved Personal Limit`=="MISSING",`:=`(`Pre-Approved Personal Limit`=NA)]
tr[,`:=`(`Pre-Approved Personal Limit`=as.factor(`Pre-Approved Personal Limit`))]
tr.na<-tr[complete.cases(tr),]

mc2<-lm(Application_Score~.-(Behavioural_Score+Identifier),data=tr.na)
m4<-stepAIC(mc2,direction="backward")

indx <- sapply(tr.na, is.factor)
tr.na[indx] <- lapply(tr.na[indx], function(x) as.numeric(as.numeric(x)))

xg.1<-sparse.model.matrix(Application_Score ~ Age + Education + Marital_Status + Occupation + 
                              Gender + Active_Bank_Products + Mortgage_Loan + Personal_Loan + 
                              Deposit + `Pre-Approved Auto Limit` + `Pre-Approved Mortgage Limit` + 
                              `Pre-Approved Personal Limit` + Tenure_with_Bank_Group + 
                              Salary_Amount-1,data=tr.na)
xg.2<-sparse.model.matrix(Behavioural_Score~Personal_Loan+Credit_Card+Total_Asset_Under_Mngmnt+Commercial_Loan+Gender+Consumer_Auto_Loan+Insurance_Product_type+Total_Bank_Products+ Marital_Status+ Residence+Indutry_Groups+ Industry_Domain-1,data=tr.i)
