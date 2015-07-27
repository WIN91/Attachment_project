admissions_data<-read.csv("~/AProject/admissions_data.csv")

head(admissions_data)

#use tbl_df() to create a local data frame hence converting it to a local data frame
#we first load the plyrand dplyr packages then execute the commands
library(plyr)
library(dplyr)

admissions<-tbl_df(admissions_data)

##in order to see more rows we do the following
#print(admissions,n=1000)

#grouping the data according to age groups basing with the date of birth 
select(admissions,date_of_birth)

#I changed the date format of the date of birth using the as.Date() introducd a new
#column"date_of_birth2"
admissions$date_of_birth2<- as.Date(admissions$date_of_birth,format = '%d/%m/%Y')

class(admissions$date_of_birth2)

#changing the date format of the date_admn
admissions$date_admn <- as.Date(admissions$date_admn,format = '%d/%m/%Y')

class(admissions$date_admn)

#obtain the age by obtaining the difference between date of admissions and date of birth
#divide by 365.25 to obtain the years
admissions$age_yrs <- as.integer((admissions$date_admn- admissions$date_of_birth2)/365.25)

admissions$age_cat[admissions$age_yrs<1] <-0

admissions$age_cat[admissions$age_yrs>=1 & admissions$age_yrs<=5] <-1

admissions$age_cat[admissions$age_yrs>=6 & admissions$age_yrs<=10] <-2

admissions$age_cat[admissions$age_yrs>=11 & admissions$age_yrs<=15] <-3 

admissions$age_cat[admissions$age_yrs>=16 & admissions$age_yrs<=20] <-4 

#ordering the age_cat column
admissions$age_cat<-factor(admissions$age_cat,labels=c("Under 1yr","1-5yrs","6-10yrs","11-15yrs",
                                                       "16-20yrs"),ordered=TRUE)

table(admissions$age_cat)

#summary of the age_yrs

admissions<-subset(admissions,!is.na(age_yrs))

summary(admissions$age_yrs)

var(admissions$age_yrs)#variance

sd(admissions$age_yrs)#standard deviation
                            
#extract the months from the date the patient was admitted between 2002 and 2015 by creating
#a new column(mnth1)
library(lubridate)

admissions$mnth1 <- month(admissions$date_admn)

#ordering and labling the months
admissions$mnth1<-factor(admissions$mnth1,labels=c("Jan","Feb","Mar","Apr", "May","Jun",
                                                  "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

##making the summary on the age category and monthly admissions
# GENERAL admissions and count for all the months through out the year
table(admissions$mnth1)

library(ggplot2)

a<-ggplot(admissions, aes(factor(mnth1)))
a<-a +geom_bar()
a<-a + geom_bar(fill="blue", colour="pink")
a<-a+labs(x="MONTHS",y="ADMISSIONS COUNT")
a<-a+ggtitle("GENERAL MONTHLY ADMISSIONS")
a<-a+theme_grey()
a<-a+coord_flip()
a

#summary of the age category and monthly admissions(GENERAL)
table(admissions$age_cat)

c<-ggplot(admissions[!is.na(admissions$age_cat),],aes(factor(age_cat)))
c<-c+geom_bar()
c<-c+geom_bar(fill="limegreen",colour="blue")
c<-c+labs(x="AGE GROUP",y="ADMISSIONS COUNT")
c<-c+ggtitle("Admissions according to Age Group")
c

#To obtain the tables of the monthly admissions count and the age groups
# using a for loop control structure.
admissions$k <-month(admissions$date_admn)

for (i in 1:12) {
  
  print(table(admissions$age_cat[admissions$k==i]))
  
  mnth1<-month.name[i]
  title<- paste(mnth1,"Admission")
  
  c5<-ggplot(admissions[admissions$k==i &!is.na(admissions$age_cat),],aes(factor(age_cat))) 
  c5<-c5+geom_bar()
  c5<-c5+geom_bar(fill="brown",colour="yellow")
  c5<-c5+labs(x="AGE GROUP",y="ADMISSIONS COUNT")
  c5<-c5+ggtitle(title)
  c5<-c5+theme_bw()
  print(c5)
}
