
#Read and load Loan Dataset. Read all categorical variables as Strings
#---------------------------------------------------------------------

loanData<-read.csv("loan.csv",stringsAsFactors = F)

str(loanData)

head(loanData)


#The independent column or the column which we want to predict is: loan_status

##Data Cleaning - Part 1: Removing unnecessary columns
##----------------------------------------------------

#Identify which columns have NA and how many
countNA<-function(x) { return(sum(is.na(x)))}

na.counts<-sapply(loanData, FUN=countNA)

na.counts<-sort(na.counts, decreasing = T)

View(na.counts)

#There are many columns where all records are NA. We will eliminate those.
#There are many columns where there are no NAs or few NAs (<100). We will keep those.
#This leaves us with two columns - mths_since_last_delinq and pub_rec_bankruptcies where 
#we still need to make a decision either to keep them or eliminate them.
#Let's explore these a little to decide wether we want to keep them or eliminate them

#View(loanData[,c("mths_since_last_delinq","loan_status")])

#There does not seem to be any relationship between mths_since_last_delinq and loan_status.
#Plus it has too many NAs. We will eliminate this column as well.

#View(loanData[,c("pub_rec_bankruptcies","loan_status")])

#Not too many NAs in pub_rec_bankruptcies. Information in this column seems to be useful
#Let's see if we can find some correlation between this field and a similar field pub_rec

cor(loanData$pub_rec,loanData$pub_rec_bankruptcies,use="complete.obs")
#There is a strong (0.85) corelation bewteen the two columsn - pub_rec and pub_rec_bankruptcies
#Since one column can provide information about the other we can keep just one - we will keep pub_rec
#We will eliminate pub_rec_bankruptcies

#So we have decided to eliminate all columns with NA counts >100 and keep the rest for now. 

subset<-loanData[,names(which(na.counts<100))]

str(subset)

#Lets see which columns have NA now

na.counts<-sapply(subset, FUN=countNA)
na.counts<-sort(na.counts, decreasing = T)
View(na.counts)

#Lets see the column -title 

#View(subset$title)
#There are some blanks and one NA. But this column does not seem to be helpful for our analysis.
#We will eliminate it.
subset<-subset[,-which(names(subset) %in% c("title"))]

#Lets explore column tax_liens
#View(subset$tax_liens)
#This columns has either 0s or NA. Not very useful. We will eliminiate it.
subset<-subset[,-which(names(subset) %in% c("tax_liens"))]

#Lets explore chargeoff_within_12_mths
table(subset$chargeoff_within_12_mths)
#This columns too has either 0s or NA. Not very useful. We will eliminiate it.
subset<-subset[,-which(names(subset) %in% c("chargeoff_within_12_mths"))]

#Lets explore collections_12_mths_ex_med
table(subset$collections_12_mths_ex_med)
#This columns too has either 0s or NA. Not very useful. We will eliminiate it.
subset<-subset[,-which(names(subset) %in% c("collections_12_mths_ex_med"))]

View(subset)
#Just by looking at the data the following fields seem not so useful because 
# either they don't make business sense or 
# they had too many zero or same values and we can eliminate them
#1. id : character field which does not influence default probability
#2. member_id : character field which does not influence default probability
#3. emp_title : character field which does not influence default probability
#4. url : character field which does not influence default probability
#5. desc : free form field, which can not be easily converted into a factor field
#6. pymnt_plan : same values in all the records
#7. policy_code : same values in all the records
#8. application_type : same values in all the records
#9. acc_now_delinq : same values in all the records
#10.delinq_amt : same values in all the records
#11.initial_list_status : same values in all the records

subset<-subset[,-which(names(subset) %in% c("id","member_id","emp_title","url","desc","pymnt_plan","policy_code","application_type","acc_now_delinq","delinq_amnt","initial_list_status"))]

View(subset)

#After applying business understanding, we can eliminate the following columns as well:
#1. funded_amnt and funded_amnt_inv - these columns are very similar to loan amount
cor(subset$funded_amnt,subset$loan_amnt) #correlation of .982
cor(subset$funded_amnt_inv,subset$loan_amnt) #correlation of .94
#2. out_prncp and out_prncp_inv - these columns are zero for most part and have values only 
# for loans that are current. We can eliminate this column
#3. total_pymnt_inv - this columns is similar to total_Pymnt
cor(subset$total_pymnt_inv,subset$total_pymnt) #correlation of .971
#4. total_rec_prncp and total_rec_int - business logic is that the values in these column
#add upto values in total_pymnt. So we can keep total_pymnt and eliminiate these two.
cor(subset$total_rec_prncp+subset$total_rec_int,subset$total_pymnt) #correlation of .997
#5. next_pymnt_d - this column is also only relevant for current loans which are not going 
# to be part of the analysis. Hence we can eliminate this column
subset<-subset[,-which(names(subset) %in% c("funded_amnt", "funded_amnt_inv","out_prncp","out_prncp_inv", "total_pymnt_inv","total_rec_prncp","total_rec_int","next_pymnt_d"))]



##Data cleaning - Part 2: Re-formatting of columns
##---------------------------------------------------

#Column term is currently a string field. Can be easily converted into numeric.
subset$term<-as.numeric(gsub(" months","",subset$term,ignore.case = T))

#Column int_rate is currently a string field as well. We will convert it into numeric.
subset$int_rate<-as.numeric(gsub("%","",subset$int_rate,ignore.case = T))

#Let's convert emp_length into numeric
#We will eliminate the word "year", we wil replace 10+ with 15 and "< 10" with 0.5
el<-gsub("10\\+ years","15",subset$emp_length,ignore.case = T)
el<-gsub("< 1 year","0.5",el,ignore.case = T)
el<-gsub("1 year","1",el,ignore.case = T)
el<-gsub("n/a",NA,el,ignore.case = T)
el<-as.numeric(gsub("years","",el,ignore.case = T))
subset$emp_length<-el
rm(el)
#There are lot of "n/a" values which give NA on numeric conversion. 
#We will assign 0 to emp_length where it is NA assuming these people are 
#either unemployed or self-employed
#subset<-subset[,-which(names(subset) %in% c("emp_length"))]
subset$emp_length[which(is.na(subset$emp_length))]<-0

#Let's convert revol_util into numeric
subset$revol_util<-as.numeric(gsub("%","",subset$revol_util,ignore.case = T))
#This introduces NAs as values in revol_util columns where revol_util was blank originally
#revol_util measures the percentage of total revolving credit limit that is being utilized
#This field has a relationship with revol_bal. We notice that for mostrecords (except one)
#where revol_util is NA, revol_bal is zero. We will subsitute NA with zeros for this field
subset$revol_util[which(is.na(subset$revol_util))]<-0




##Analysis Part1 - Univariate, Bi-variate and Multi-variate Analysis
##-------------------------------------------------------------------

#After data clean up, we are left with 31 columns listed here:
#[1] "loan_amnt"               "term"                    "int_rate"               
#[4] "installment"             "grade"                   "sub_grade"              
#[7] "emp_length"              "home_ownership"          "annual_inc"             
#[10] "verification_status"     "issue_d"                 "loan_status"            
#[13] "purpose"                 "zip_code"                "addr_state"             
#[16] "dti"                     "delinq_2yrs"             "earliest_cr_line"       
#[19] "inq_last_6mths"          "open_acc"                "pub_rec"                
#[22] "revol_bal"               "revol_util"              "total_acc"              
#[25] "total_pymnt"             "total_rec_late_fee"      "recoveries"             
#[28] "collection_recovery_fee" "last_pymnt_d"            "last_pymnt_amnt"        
#[31] "last_credit_pull_d"     


require(ggplot2)

#Let's explore how many records are there for each of type of loan_status

ggplot(subset, aes(x=loan_status))+geom_bar(stat='count')
#Most records (>30,000) are for "Fully Paid", some (>5000) for "Charged Off"
#and very few (<2000) are for "Current". We don't need records with "current" status
#for our analysis.
#We will eliminate those records

subset2<-subset[-which(subset$loan_status=="Current"),]

View(subset2)

#Lets explore each of the columns one at a time and its relationship with loan_status

#term:
ggplot(subset2,aes(x=factor(term),fill=loan_status))+geom_histogram(stat='count')
#Most records (close to 30000) are for 36 months term. <10000 for 60 months term.
ggplot(subset2,aes(x=factor(term), fill=loan_status))+ geom_bar(position = "fill")
#It seems that there are more charge offs on 60 months term loans than 36 months term.

#loan_amnt:
ggplot(subset2,aes(x=loan_amnt, fill=loan_status) ) + geom_histogram()
#Most loans range from $4000 to $10000. Loans which are multiples of 5000 are more common.
ggplot(subset2,aes(x=loan_amnt, fill=loan_status) ) + geom_histogram(position='fill')
#It seems that default rate increases with increase in loan amount

#int_rate:
ggplot(subset2,aes(x=int_rate, fill=loan_status) ) + geom_histogram()
#Interest rate ranges from ~5 to ~25% with int rate between 10% and 15% being most common
ggplot(subset2,aes(x=int_rate, fill=loan_status) ) + geom_histogram(position='fill')
#It seems that default rate increases with increase in interet rate

#installment:
ggplot(subset2,aes(x=installment, fill=loan_status) ) + geom_histogram()
#The range of installment is from ~$10 to ~>$1000. Most loans have instalments in the 
#range of $200 to $400
ggplot(subset2,aes(x=installment, fill=loan_status) ) + geom_histogram(position='fill')
#There is no obvious correlation between installment and default rate

#grade:
ggplot(subset2,aes(x=grade,fill=loan_status)) + geom_histogram(stat='count')
#Most loans are for grade B lendees then A, C, D, E, F and G in that order
ggplot(subset2,aes(x=factor(grade), fill=loan_status))+ geom_bar(position = "fill")
#Clearly, the least defaults are in A followed by B, C, D, E , F and G in that order

#sub_grade:
ggplot(subset2,aes(x=factor(sub_grade), fill=loan_status))+geom_bar(position = "fill")
#Similar trend seen with sub_grade. Lower letter grades and lower sub-grade digits have 
#relatively lower default rates

#emp_length:
ggplot(subset2,aes(x=emp_length, fill=loan_status) ) + geom_histogram()
#As a trend, the loan requests decrease with an increase in employment length
#People who are unemployed or self-employed have similar default rate as people with 
#less than 1 year of employment. 
ggplot(subset2,aes(x=emp_length, fill=loan_status) ) + geom_histogram(position='fill')
#There does not seem to be a good trend in default rate with respect to emp length

#home_ownership:
ggplot(subset2,aes(x=factor(home_ownership), fill=loan_status))+geom_histogram(stat='count')
ggplot(subset2,aes(x=factor(home_ownership), fill=loan_status))+geom_bar(position = "fill")
#There does not seem to be much variation is loan defaults with respect to home_ownership

#annual_income:
ggplot(subset2,aes(x=annual_inc,fill=loan_status))+geom_histogram()
#The annual income has a large range from $4000 to $6000,000. Most of the loans are by 
# people in lower income group.
ggplot(subset2,aes(x=log10(annual_inc),fill=loan_status))+geom_histogram(position='fill')
#Distribution with log of income is relatively more normal. Higher income is associated with 
#lower rate of default rate
summary(subset2$annual_inc)
#Most loans are in income group $40,000 to $82,000 with median income of ~$59000

#verification_status : Income verified or not, source verified. 
ggplot(subset2,aes(x=verification_status,fill=loan_status)) + geom_histogram(stat='count')
ggplot(subset2,aes(x=verification_status,fill=loan_status)) + geom_bar(position='fill')
#Interestingly, there is a slightly higher proportion of defaults for "Verified" loans 
# as compared to the those for "Not Verified" loans

#purpose: 
ggplot(subset2,aes(x=purpose,fill=loan_status)) + geom_histogram(stat='count')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
ggplot(subset2,aes(x=purpose,fill=loan_status)) + geom_bar(position='fill') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 
#It seems most common purpose for loans is debt consolidation
#Most defaults as a proporition happen for loans for the purpose of "Small Business"

#addr_state:
ggplot(subset2,aes(x=addr_state,fill=loan_status)) + geom_histogram(stat='count')
#California has by far the largest number of loans in this dataset
ggplot(subset2,aes(x=addr_state,fill=loan_status)) + geom_bar(position = 'fill')
#California has by far the largest number of loans in this dataset
#Nevada seems to have a disproportionate amount of defaulted loans.
require(dplyr)
by_state <- subset2 %>% group_by(addr_state,loan_status)
a <- by_state %>% summarise(n = n())
a[which(a$addr_state == "NE"),]
#The number of entries for Nevada are very low, so the result can be ignored for this state


#dti: debt to income ratio which measures how much debt one has as compared to one's income
#lower dti logically should result in better loan repayment probability
ggplot(subset2,aes(x=dti,fill=loan_status))+geom_histogram(position = 'fill')
#it seems dti ranges from 0 to 30 with most borrowers with dti b/w 5 and 20.
#It does seem like that the proportion of "Charged Off" is relatively greater for larger dti
summary(subset2$dti) #Median dti 13.37

#revol_util: utilization of revolving credit lines
ggplot(subset2,aes(revol_util,fill=loan_status))+geom_histogram(position = "fill")
ggplot(subset2,aes(loan_status,revol_util))+geom_boxplot()
#Default rate seems to higher for larger values of revol_util

#revol_bal: outstanding bal on revoling credit lines
ggplot(subset2,aes(revol_bal,fill=loan_status))+geom_histogram(position = "fill")
#Default rate doesn't seem to vary with the revol_bal variable

ggplot(subset2,aes(revol_bal,revol_util,color=loan_status))+geom_point() + 
    scale_x_log10()+scale_y_log10()
#There seems to be some sort of correlation between revol_util and revol_bal, 
#and it seems there are more loans associated with higher values of revol_util & 
#revol_bal.

#inq_last_6mths:
ggplot(subset2,aes(inq_last_6mths,fill=loan_status))+geom_histogram(position = "fill")
#Default rate seem to vary with inq_last_6mths variable. 


##Analysis Part2: Segmented and Derived Column Analysis
##-------------------------------------------------------------

#From business understanding , we know that there are 5 C's which are important 
#when it comes to determining credit repayment probability 
#We will explore the variables associated with these 5 C's and their relationshiop with loan_status
#1. Capacity - annual_inc, loan_amt, dti, revol_util
#2. Capital - 
#3. Colateral -
#4. Conditions - purpose, term, int_rate, 
#5. Character - grade, sub_grade, inq_last_6months, emp_length, home ownership


# emp_length by grade vs loan_status:

#Let's start with emp_length and its relationshiop w/ default rates across different grades
#We saw in earlier analysis that emp_length for full dataset didn't seem to have any 
#meaningful relationship w/ loan_status. We want to explore if emp_length is a key 
#variable for our analysis for any one or specific grades
# (Segemented bi-variate analysis)


pA<-ggplot(subset2[subset2$grade=='A',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: A")

pB<-ggplot(subset2[subset2$grade=='B',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: B")

pC<-ggplot(subset2[subset2$grade=='C',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: C")

pD<-ggplot(subset2[subset2$grade=='D',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: D")

pE<-ggplot(subset2[subset2$grade=='E',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: E")

pF<-ggplot(subset2[subset2$grade=='F',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: F")

pG<-ggplot(subset2[subset2$grade=='G',],aes(x=emp_length,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("grade: G")

require(gridExtra)

grid.arrange(pA,pB,pC,pD,pE,pF,pG,nrow=4,ncol=3)
#If there is any relationship b/w emp_length and defaul probability, 
#it is not evident from the plot we have. We will eliminate emp_length 
#from our consideration for further analysis

#But it seems irrespective of the emp_length, grade seems to be have a high coorelation
#w/ default rate

rm(pA)
rm(pB)
rm(pC)
rm(pD)
rm(pE)
rm(pF)
rm(pG)

 
#loan_amnt by purpose vs loan_status:

#We saw earlier that as loan_amnt increases default rate tends to increase too
#Let's explore that if that is the case irrespective of the purpose of the loan
#or the trend varies across purpose (purpose for which loan is taken)
#From earlier analysis, we know that most common purpose for loan is debt_consolidation
#and most defaults as a proporition of loan taken happen for small business loans
#Lets explore this further

pDC<-ggplot(subset2[subset2$purpose=='debt_consolidation',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("debt_consolidation")

pCC<-ggplot(subset2[subset2$purpose=='credit_card',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("credit_card")

pOT<-ggplot(subset2[subset2$purpose=='other',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("other")

pHI<-ggplot(subset2[subset2$purpose=='home_improvement',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("home_improvement")

pMP<-ggplot(subset2[subset2$purpose=='major_purchase',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("major_purchase")

pSB<-ggplot(subset2[subset2$purpose=='small_business',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("small_business")

#exploring top 6 most common purposes. 

grid.arrange(pDC,pCC,pOT,pHI,pMP,pSB,nrow=3,ncol=2)

#One interesting observation is that upto first $15,000 loan default rate does not seem 
#to vary much with loan amount for purposes other than small business. 
#For loans greater than $15000 it does seem that default rate increases with loan amount
#For small businesses loan defaul rate seems to increase with loan amount irrespecive 
#of the amount. 
#We will keep both loan amount and purpose in consideration for further analysis
#We may need a separate model for small business loan evaluation because default rate
#seems to vary differently with loan amount. 

rm(pDC)
rm(pCC)
rm(pOT)
rm(pHI)
rm(pMP)
rm(pSB)


#int_rate by term vs. loan_status

#From earlier analysis we know that default rate increases with increase int_rate
#We also know from earlier plots that more defaults are associated with longer term loans
#as compared to short term loans when seen as proportion of total loans
#There are two possibilities that longer term loans are associated with higher int_rate
#and that is why we see what we see or other possibility is that both int_rate and term
#have a correlation with default rate independent of each of other
#We will perform a segmented bi-variate analysis to check that.

p36<-ggplot(subset2[subset2$term==36,],aes(x=int_rate,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("36 months")

p60<-ggplot(subset2[subset2$term==60,],aes(x=int_rate,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("60 months")

grid.arrange(p36,p60,nrow=2,ncol=1)

#Two observations:
#a) default rate increases with increase in interest rate while keeping term constant
#b) default rate is relatively higher for 60 months term compared to 36 months 
#   for same interest rate
# We will keep both term and int_rate in our consideration for further analysis
rm(p36)
rm(p60)

#loan_amnt by home_ownership vs. loan_status

#Let's see if loam_amnt with respect to sub-groups of home ownership make a difference or not
pRE<-ggplot(subset2[subset2$home_ownership=='RENT',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("RENT")

pOW<-ggplot(subset2[subset2$home_ownership=='OWN',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("OWN")

pMO<-ggplot(subset2[subset2$home_ownership=='MORTGAGE',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("MORTGAGE")

pOT<-ggplot(subset2[subset2$home_ownership=='OTHER',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("OTHER")

pNO<-ggplot(subset2[subset2$home_ownership=='NONE',],aes(x=loan_amnt,fill=factor(loan_status))) + 
    geom_histogram(position='fill') + ggtitle("NONE")

grid.arrange(pRE,pOW,pMO,pOT,pNO,nrow=3,ncol=2)
#loan_amnt grouped by home_ownership doesn't seem to show anything apart from the correaltion which was
#seen from just the loan_amnt variable



#annual_inc by home_ownership vs. loan_status:

#Let's see if annual_inc with respect to sub-groups of home ownership make a difference or not
pRE<-ggplot(subset2[subset2$home_ownership=='RENT',],aes(x=annual_inc,fill=factor(loan_status))) + 
    geom_histogram(position = "fill") + ggtitle("RENT")+xlim(0,2e5)

pOW<-ggplot(subset2[subset2$home_ownership=='OWN',],aes(x=annual_inc,fill=factor(loan_status))) + 
    geom_histogram(position = "fill") + ggtitle("OWN")+xlim(0,2e5)

pMO<-ggplot(subset2[subset2$home_ownership=='MORTGAGE',],aes(x=annual_inc,fill=factor(loan_status))) + 
    geom_histogram(position = "fill") + ggtitle("MORTGAGE")+xlim(0,2e5)

pOT<-ggplot(subset2[subset2$home_ownership=='OTHER',],aes(x=annual_inc,fill=factor(loan_status))) + 
    geom_histogram(position = "fill") + ggtitle("OTHER")+xlim(0,2e5)

pNO<-ggplot(subset2[subset2$home_ownership=='NONE',],aes(x=annual_inc,fill=factor(loan_status))) + 
    geom_histogram(position = "fill") + ggtitle("NONE")+xlim(0,2e5)

grid.arrange(pRE,pOW,pMO,pOT,pNO,nrow=3,ncol=2)
#annual_inc grouped by home_ownership seems to be showing an interesting signal. 
#annual_inc seems to matter for people who are renting or have a mortgage on their home
#with increasing annual_inc showing lower default rates.

rm(pMO)
rm(pNO)
rm(pOT)
rm(pOW)
rm(pRE)

#A boxplot to further explore relationshiop between annual_inc, home_ownership and loan_status
ggplot(subset2,aes(x=home_ownership,y=log(annual_inc),col=loan_status))+geom_boxplot()
#Based on the plot, it seems that the effect of home ownership on loan status is quite weak
#We will not consider it for further analyis.

##Conclusion - Drivers of loan default
#---------------------------------------------------
#From the analysis so far we can conclude there are 10 drivers of loan default. 
#Capacity (4) - annual_inc, loan_amt, dti, revol_util
#Conditions (3)- purpose, term, int_rate, 
#Character (3)- grade, sub_grade, inq_last_6months
    

#Further Analysis using Derived Variables
#----------------------------------------------------
#dti, annual_inc & installment vs. loan_status
#dti - debt to income ratio is calculated by dividing monthly debt payments 
#(excluding mortgage and installment for requested loan)
#if we multiply dti with monthly income we should be able to find monthly debt obligations
#and if add installment for requested loand that should give total obligation
#that obligation subtracted from monthly income should give the buffer that the applicant
#has for taking care of any unforeseen contingencies. Hence theoretically, a good indicator 
#of default. Let's see if that is indeed the case or not.

subset2$buffer<-(subset2$annual_inc/12) - (subset2$dti*subset2$annual_inc/12) + 
                                           subset2$installment

ggplot(subset2,aes(x=buffer,fill=loan_status))+geom_histogram(position='fill')
#This does not produce any meaningful results. 

#Let's check if actual duration of the loan_payment shows any signal
#actual_time_period of payment (in months) = time elapsed between last_pymnt_d and issue_d
#issue_d: date of loan issue - last_Paymnt_d: date of last payment was made
a <- as.POSIXct(paste("01-", subset2$issue_d, sep = ""), format = "%d-%b-%y")
b <- as.POSIXct(paste("01-", subset2$last_pymnt_d, sep = ""), format = "%d-%b-%y")
subset2$actual_time_period <- (as.numeric(b)-as.numeric(a))/(3600*24*30)
ggplot(subset2,aes(factor(term),actual_time_period,color=loan_status))+geom_boxplot()

rm(a)
rm(b)
#Median actual_time_period looks to be noticeably lower for the defaulted loans, which is somewhat expected
#So, this doesn't seem to be showing anything unexpected.
#Based on business understanding, the acutal time period of payment is more of a result of
#loan default than a cause / driver of the default.
#We are not going consider the issue_d or last_paymnt_d for further analysis


#revol_util, inq_last_6mths vs. loan status
#revol_util - Revolving line utilization rate is an indicator how much credit a person 
#is using relative to total available credit. Higher the utilization, closer the person
#is to his/her limit. 
#inq_last_6mths - inquiries in the last 6 months. Although there seems to be correlation
#b/w this field and loan status but we don't want to discount someone just because that 
#person is doing inquiries especially when they are not yet close to their credit limit.
#If a person is high on his utilization of revolving credit and is aggresively taking on
#more loans, which is reflected by the inquiries in the last 6 months, then that should
#be point of concern from loan default perspective.
#Lets verify if the product of those two does result into a better predictor of loan status
#or our theory is only good in theory and not in reality.

subset2$util.inq<-subset2$revol_util*subset2$inq_last_6mths
    
ggplot(subset2,aes(x=util.inq, fill=loan_status))+geom_histogram(position='fill')
#It does seem that the interaction term of revol_util and inq_last_6mths has a strong relationship
#with default rate. Let's see how loans are distributed across the range of the interaction term
#Most values of the interaction term seem to fall below 200. Let's limit the x-axis values
ggplot(subset2,aes(x=util.inq, fill=loan_status))+geom_histogram()+xlim(0,300)+ylim(0,2000)
ggplot(subset2,aes(x=util.inq, fill=loan_status))+geom_histogram(position='fill')+
        xlim(0,300)
#The interaction terms seems to be strong driver of default rate. We will use the 
#interaction term in further analysis with the caveat to eliminate the outliers


##Build a training data frame of relevant columns for hypothesis testing &
# building model in future 
#------------------------------------------------------------------------

loanFinal<-subset2[,c("loan_status","annual_inc","loan_amnt","purpose","term", "home_ownership", "dti", "inq_last_6mths", "delinq_2yrs","revol_bal","revol_util", "total_acc", "emp_length", "open_acc","grade")]

loanFinal2<-loanFinal

str(loanFinal2)

#=================================LOGISTICS REGRESSION BEGIN ==================

#Process the variables for Logistics Regression

loanFinal2$loan_status<-ifelse(loanFinal2$loan_status=="Fully Paid",1,0)

#scale numeric variables
loanFinal2$annual_inc<-scale(loanFinal2$annual_inc)

loanFinal2$loan_amnt<-scale(loanFinal2$loan_amnt)

loanFinal2$dti<-scale(loanFinal2$dti)

loanFinal2$inq_last_6mths<-scale(loanFinal2$inq_last_6mths)

loanFinal2$delinq_2yrs<-scale(loanFinal2$delinq_2yrs)

loanFinal2$revol_bal<-scale(loanFinal2$revol_bal)

loanFinal2$revol_util<-scale(loanFinal2$revol_util)

loanFinal2$total_acc<-scale(loanFinal2$total_acc)

loanFinal2$emp_length<-scale(loanFinal2$emp_length)

loanFinal2$open_acc<-scale(loanFinal2$open_acc)


#convert term into a factor variable with two levels - 36 as 1 and 60 as 0
loanFinal2$term<-ifelse(loanFinal2$term==36,1,0)

#convert categorical variables purpose and grade into dummy numeric variables.

dummy1<-model.matrix(~purpose-1,data=loanFinal2)
loanFinal2<-cbind(loanFinal2,dummy1[,-1])

dummy2<-model.matrix(~grade-1,data=loanFinal2)
loanFinal2<-cbind(loanFinal2,dummy2[,-1])

dummy3<-model.matrix(~home_ownership,data=loanFinal2)
loanFinal2<-cbind(loanFinal2,dummy3[,-1])

loanFinal3<-loanFinal2[,-which(names(loanFinal2)%in% c("purpose","grade","home_ownership"))]

str(loanFinal3)

#Now that data is ready, lets split it into training and test set. 
set.seed(10)

train.index<-sample(nrow(loanFinal3),0.7*nrow(loanFinal3))

train<-loanFinal3[train.index,]
test<-loanFinal3[-train.index,]

logistic_model<-glm(loan_status~., data=train,family="binomial")

#AIC 20578

logistic_model_2<-step(logistic_model,direction="both")

summary(logistic_model_2)

#AIC: 20569 improvement in AIC

library(car)

vif(logistic_model_2)

#VIF for total_acc is >2, let's eliminate it.

logistic_model_3<-glm(formula = loan_status ~ annual_inc + loan_amnt + term + inq_last_6mths + 
                        revol_bal + revol_util + emp_length + open_acc + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + purposevacation + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipOWN + home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_3)

vif(logistic_model_3)

#AIC 20570
#VIFs are good. Let's focus on p-values.Let's eliminate purposevacation

logistic_model_4<-glm(formula = loan_status ~ annual_inc + loan_amnt + term + inq_last_6mths + 
                        revol_bal + revol_util + emp_length + open_acc + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipOWN + home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_4)

#AIC 20570 no change. 
#Let's eliminiate open_acc

logistic_model_5<-glm(formula = loan_status ~ annual_inc + loan_amnt + term + inq_last_6mths + 
                        revol_bal + revol_util + emp_length  + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipOWN + home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_5)

#AIC 20570 no change. 
#Let's eliminiate home_ownershipOWN

logistic_model_6<-glm(formula = loan_status ~ annual_inc + loan_amnt + term + inq_last_6mths + 
                        revol_bal + revol_util + emp_length  + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_6)

#AIC 20570 no change. 
#Let's eliminiate emp_length

logistic_model_7<-glm(formula = loan_status ~ annual_inc + loan_amnt + term + inq_last_6mths + 
                        revol_bal + revol_util  + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_7)

#AIC 20571 bare min increase. 
#Let's eliminiate loan_amnt

logistic_model_8<-glm(formula = loan_status ~ annual_inc + term + inq_last_6mths + 
                        revol_bal + revol_util  + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG + home_ownershipOTHER + 
                        home_ownershipRENT, family = "binomial", 
                    data = train)
summary(logistic_model_8)

#AIC 20572 bare min increase. 
#Let's eliminiate home_ownershipRENT and home_ownershipOTHER

logistic_model_9<-glm(formula = loan_status ~ annual_inc + term + inq_last_6mths + 
                        revol_bal + revol_util  + 
                        purposedebt_consolidation + purposeeducational + purposehome_improvement + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG, family = "binomial", 
                    data = train)
summary(logistic_model_9)

#AIC 20577 slight increase
#let's eliminiate purposehome_imporvement

logistic_model_10<-glm(formula = loan_status ~ annual_inc + term + inq_last_6mths + 
                        revol_bal + revol_util  + 
                        purposedebt_consolidation + purposeeducational  + 
                        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                        purposesmall_business + gradeB + gradeC + 
                        gradeD + gradeE + gradeF + gradeG, family = "binomial", 
                    data = train)
summary(logistic_model_10)

#AIC 20578
#let's eliminiate purposemedical, purposerenewable_energy, purposemoving

logistic_model_11<-glm(formula = loan_status ~ annual_inc + term + inq_last_6mths + 
                         revol_bal + revol_util  + 
                         purposedebt_consolidation + purposeeducational  + 
                         purposeother + 
                         purposesmall_business + gradeB + gradeC + 
                         gradeD + gradeE + gradeF + gradeG, family = "binomial", 
                     data = train)
summary(logistic_model_11)

#AIC 20595 some jump in AIC
#let's eliminiate revol_bal

logistic_model_12<-glm(formula = loan_status ~ annual_inc + term + inq_last_6mths + 
                         revol_util  + 
                         purposedebt_consolidation + purposeeducational  + 
                         purposeother + 
                         purposesmall_business + gradeB + gradeC + 
                         gradeD + gradeE + gradeF + gradeG, family = "binomial", 
                     data = train)
summary(logistic_model_12)

#AIC 20598 - slight increase

final_model<-logistic_model_12

train_pred_values<-predict(final_model,train[,-1],type="response")

train_pred_factors <- factor(ifelse(train_pred_values >= 0.85, 1 , 0))
train_actual_factors<-factor(train[,1])

confusionMatrix(train_pred_factors,train_actual_factors)

test_pred_values<-predict(final_model,test[,-1],type="response")

str(test_pred_values)

test_actual_factors<- factor(test[,1])

test_pred_factors <- factor(ifelse(test_pred_values >= 0.85, 1 , 0))

confusionMatrix(test_pred_factors,test_actual_factors)

#Logistics regression 64% accuracy

#============================End of Logistics Regression==========================


#=============================DECISION TREE BEGIN=================================

loanFinal4<-loanFinal

#convert categorical variables into factor variables

loanFinal4$loan_status<-as.factor(loanFinal4$loan_status)
loanFinal4$purpose<-as.factor(loanFinal4$purpose)
loanFinal4$home_ownership<-as.factor(loanFinal4$home_ownership)
loanFinal4$term<-as.factor(loanFinal4$term)
loanFinal4$grade<-as.factor(loanFinal4$grade)

str(loanFinal4)

#Now that data is ready, lets split it into training and test set. 
set.seed(20)

train.index<-sample(nrow(loanFinal4),0.7*nrow(loanFinal4))

train<-loanFinal4[train.index,]
test<-loanFinal4[-train.index,]

library(MASS)
library(rpart)
library(rpart.plot)

tree_model_1<-rpart(loan_status~.,data=train,method="class",
                    control=rpart.control(minsplit=2,minbucket=2, cp=.001))
prp(tree_model_1)
plot(tree_model_1)

test_pred_values<- predict(tree_model_1,test[,-1],type="class")

test_pred_factors<-as.factor(test_pred_values)                           
test_actual_factors<-as.factor(test[,1])                           

library(caret)
confusionMatrix(test_pred_values,test_actual_factors)

#====================================================================================

#==============================Random Forest Begin=========================================

loanFinal5<-loanFinal

#convert categorical variables into factor variables

loanFinal5$loan_status<-as.factor(loanFinal5$loan_status)
loanFinal5$purpose<-as.factor(loanFinal5$purpose)
loanFinal5$home_ownership<-as.factor(loanFinal5$home_ownership)
loanFinal5$term<-as.factor(loanFinal5$term)
loanFinal5$grade<-as.factor(loanFinal5$grade)

loanFinal5<-loanFinal5[,-which(colnames(loanFinal5) %in% c("purpose","grade","home_ownership","emp_length"))]


str(loanFinal5)

#Now that data is ready, lets split it into training and test set. 
set.seed(20)

train.index<-sample(nrow(loanFinal5),0.7*nrow(loanFinal5))

train<-loanFinal5[train.index,]
test<-loanFinal5[-train.index,]

library(randomForest)
rf_model1<-randomForest(loan_status~.,data=loanFinal5,importance=T,
                        ntree=200,
                        mtry=3,
                        do.trace=F,
                        proximity=F)

test_pred_values<- predict(rf_model1,test[,-1],type="response")

test_actual_factors<-as.factor(test[,1])                           

require(caret)
confusionMatrix(test_pred_values,test_actual_factors)

varImpPlot(rf_model1)

importance(rf_model1)

#=================================End of Random Forest==========================

#===============================Raw benchmarking of defaul rate vs. Int Rate=====

countsByGrade<-table(loanFinal$grade, loanFinal$loan_status)


defaultRateByGrade<-100*(countsByGrade[,1])/(countsByGrade[,1]+countsByGrade[,2])



intRateByGrade[,2]-defaultRateByGrade

#==================================================================================

#Read the production data.

productionData<-read.csv("productionData.csv", stringsAsFactors = F)

str(productionData)

colNames<-colnames(loanFinal5)

productionData_2<-productionData[,which(colnames(productionData) %in% colNames)]

#Let's convert emp_length into numeric
#We will eliminate the word "year", we wil replace 10+ with 15 and "< 10" with 0.5
#el<-gsub("10\\+ years","15",productionData_2$emp_length,ignore.case = T)
#el<-gsub("< 1 year","0.5",el,ignore.case = T)
#el<-gsub("1 year","1",el,ignore.case = T)
#el<-gsub("n/a",NA,el,ignore.case = T)
#el<-as.numeric(gsub("years","",el,ignore.case = T))
#productionData_2$emp_length<-el
#rm(el)


#convert categorical variables into factor variables

#productionData_2$purpose<-as.factor(productionData_2$purpose)
#productionData_2$home_ownership<-as.factor(productionData_2$home_ownership)
productionData_2$term<-as.factor(productionData_2$term)
#productionData_2$grade<-as.factor(productionData_2$grade)


productionData_3<-cbind(productionData[,1],productionData_2)

str(productionData_3)

prod_pred_values<- predict(rf_model1,productionData_3[,-1],type="response")

#prod_pred_factors<-as.factor(test_pred_values)     

productionData_4<-cbind(productionData_3,loan_status=prod_pred_values)

table(prod_pred_values)
