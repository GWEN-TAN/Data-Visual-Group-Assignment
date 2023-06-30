### TAN SU RONG
### P120754
### STQD 6414 Data Mining
### Assignment 1

hrdata<-read.csv("Set1_assignment1_HRdata.csv")

require(tidyverse)

#1. Split "deptid_gender" into first column as "deptid" and second column as "gender"
hrdata<-hrdata%>%mutate(deptid=str_split_fixed(hrdata$deptid_gender,";",2)[,1])
hrdata<-hrdata%>%mutate(gender=str_split_fixed(hrdata$deptid_gender,";",2)[,2])

#To check the elements
unique(hrdata$deptid)
unique(hrdata$gender)
#The outputs of hrdata$gender are "M" ",M" ".M" "-M" "F"
#Should be "M" & "F" only, others has to be removed
hrdata$gender<-gsub("[^MF]","",hrdata$gender)


#2. Combine into a new column "birthdate"
hrdata<-hrdata%>%mutate(birthdate=str_c(hrdata$birthday," ",hrdata$birthmonth," ",hrdata$birthyear))

#Check if any missing value
sum(is.na(hrdata$birthdate))

##The output is 0. There is no missing value in the new column.


#3. Convert into date format
hrdata$birthdate<-as.Date(hrdata$birthdate,format = "%d %m %Y")

#Check for missing value
sum(is.na(hrdata$birthdate))
#The output is 2. There are two missing values after the conversion.

#Find out the location of missing value
which(is.na(hrdata$birthdate))
#The output is row 288 & 3927
#Check on the information of data rows with missing value
hrdata[c(288,3927),]

##Based on observations, both missing values happen due to the date of 29 Feb


#4. Remove rows with missing values
hrdata<-hrdata[-which(is.na(hrdata$birthdate)),]


#5. Check other date columns
str(hrdata)
#enterdate : chr, exitdate : chr
#change the format of variables to date format
hrdata$enterdate<-as.Date(hrdata$enterdate,format = "%d/%m/%Y")
hrdata$exitdate<-as.Date(hrdata$exitdate,format = "%d/%m/%Y")


#6. Create & calculate "age"
asofdate<-as.Date("2022-10-31")
hrdata<-hrdata%>%mutate(age=as.numeric(difftime(asofdate,hrdata$birthdate,units = "days"))/365.25)


#7. Create "status" column
hrdata<-hrdata%>%mutate(status=case_when(is.na(hrdata$exitdate)==TRUE~"active",TRUE~"inactive"))


#8. Create "lengthofservice" column
lengthofservice=case_when(hrdata$status=="active"~as.numeric(difftime(asofdate,hrdata$enterdate,units = "days"))/365.25,
                          TRUE~as.numeric(difftime(hrdata$exitdate,hrdata$enterdate,units = "days"))/365.25)
hrdata<-hrdata%>%mutate(lengthofservice)


#9. Discretise "lengthofservice" to three groups of equal frequency in "losgroup"
losgroup<-cut_number(lengthofservice,3)
hrdata<-hrdata%>%mutate(losgroup)
hrdata$losgroup<-factor(hrdata$losgroup,ordered = TRUE)


#10. Add "salary"
sdata<-read.csv("Set1_assignment1_FINdata.csv")

#To filter for the latest year of salary
fsdata<-as.data.frame(sdata%>%group_by(staffid)%>%filter(yearid==max(yearid)))

#fsdata - 5180 observations ; hrdata - 5145 observations
#Check duplicates in data set
sum(duplicated(fsdata))
#There are 2 duplicates
#Remove the duplicated data
fsdata<-unique(fsdata)

#Check for duplicates in staffid
sum(duplicated(fsdata$staffid))
#There are 30 duplicates.

#Check duplicate rows
which(duplicated(fsdata$staffid))
#compare & check duplicate patterns
fsdata[372:373,]
fsdata[1212:1213,]
fsdata[1943:1944,]
fsdata[3212:3213,]
fsdata[4481:4482,]
#The duplicated staffid has higher salary

#Take in the data with min salary between the duplicates
nfsdata<-as.data.frame(fsdata%>%group_by(staffid)%>%filter(salary==min(salary)))

#Combine both data to include salary for each employee
jdata<-inner_join(hrdata,nfsdata,by="staffid")


#11. Subset of data with selected variables of active employee only
jdata<-filter(jdata,jdata$status=="active")
subjdata<-jdata%>%select(staffid,deptid,status,gender,namefirst,namelast,losgroup,salary)


#12. Plot relative frequency histogram

#Since Frequency Density = Relative Frequency / Class Width   
#Thus, Relative Frequency = $density x class width
#So, $counts/sum($counts) = $density x class width
#When freq=FALSE in hist(), frequency density is plotted based on $density
#Need to change the value in $density by $counts/sum($counts)

layout(matrix(c(1:2),ncol=2))
#(a) class width = 10000
h12a<-hist(subjdata$salary,breaks=seq(0,70000,by=10000),plot = FALSE)
h12a$density<-h12a$counts/sum(h12a$counts)
plot(h12a,main="12(a) Histogram of Salary",xlab="Salary",ylab="Relative frequency",freq = FALSE)

#(b)
h12b<-hist(subjdata$salary,breaks=seq(0,70000,by=5000),plot = FALSE)
h12b$density<-h12b$counts/sum(h12b$counts)
plot(h12b,main="12(b) Histogram of Salary",xlab="Salary",ylab="Relative frequency",freq = FALSE)

##Histogram 12(b) describes the variable best.
##More number of cells plotted in 12(b) shows more detailed distribution of the data.
##The mode is more specific as it is in a smaller range due to smaller class width.
##In 12(a), the mode salary is 0-10000 ; in 12(b), the mode salary is 5000-10000.
##The outliers are more significant in 12(b) compared to 12(a).
##In 12(a), the outliers happen in 60000-70000 without gap;
##In 12(b), the outliers happen in 60000-65000 with a gap from the groups.
##Histogram 12(a) illustrates the distribution of the variable is extremely right skewed.
##However, the smaller class width in 12(b) makes the distribution less skewed compared to 12(a).
##We can say that as the class width gets smaller,
##the mode may move to the center and the distribution may approach to normal.


#13. boxplot
require(ggplot2)
ggplot(subjdata,aes(losgroup,salary))+
  geom_boxplot(aes(fill=losgroup))+
  ggtitle("Boxplots of Salary for Each Length of Service Group")


##The interquartile range (IQR) is getting wider as the level of losgroup goes higher.
##This indicates the dispersion of salary is much more spread out in higher length of service.
##There is a significant difference in median of salary,
##especially between service years of 13.5 to 25.5 and 25.5 to 46.6.
##Highest median salary is in losgroup (25.5,46.6] while the lowest median salary in losgroup [0,13.5].
##For losgroup [0,13.5], the outliers are mostly below of lower inner fence.
##For losgroup (13.5,25.5] and (25.5,46.6], the outliers are above and quite far from upper inner fence.
##The outlier of losgroup (13.5,25.5] is within the IQR of losgroup (25.5,46.6].
##Both losgroup (13.5,25.5] and (25.5,46.6] show right skewed distributions as their medians are closer to their Q1.
