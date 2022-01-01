library(readr)   #library to read our file
GHS_2013_PERSON_F1 <- read_csv("GHS_2013_PERSON_F1.csv") #reads the file and assign to the value
View(GHS_2013_PERSON_F1)   #for view our file.
#######################################################################################################

#Question1. (1)
salary <- GHS_2013_PERSON_F1$Q42aSTO       #assigning dictionary words to some common names.
age <-GHS_2013_PERSON_F1$Age
MaritalStatus <-GHS_2013_PERSON_F1$Q12aMARST
populationGroup <-GHS_2013_PERSON_F1$Race
religion <-GHS_2013_PERSON_F1$Q49RELIGIOUS
educationalLevel <-GHS_2013_PERSON_F1$Q16HIEDU
disability<-GHS_2013_PERSON_F1$disab
geotype <-GHS_2013_PERSON_F1$geotype
# we creating data frame after extracting from the file as seen above.
my.data <-cbind(salary,age,MaritalStatus,populationGroup,religion,disability,geotype,educationalLevel)
my.data = data.frame(my.data)
my.data<-my.data[(my.data$salary<=750000 & my.data$salary>=20000),]
View(my.data)

dim(my.data)  #Question 1. (2) sample size=652 
##################################################################################
#Question 1 (3)
#numerical
#For salary
summary(my.data$salary)
mean(my.data$salary)   
mean(my.data$salary,na.rm=TRUE)
median(my.data$salary,na.rm=TRUE)

sd(my.data$salary,na.rm=TRUE)
var(my.data$salary,na.rm=TRUE)
IQR(my.data$salary,na.rm=TRUE)

boxplot(my.data$salary)
hist(my.data$salary, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(my.data$salary)
qqline(my.data$salary, col = "blue", lwd = 2)
dd <- density(my.data$salary, na.rm=T)  #density plot or distribution
plot(dd) 

shapiro.test(my.data$salary)
t.test(my.data$salary, mu=  54390.12)
# For Age
summary(my.data$age)
mean(my.data$age)   
mean(my.data$age,na.rm=TRUE)
median(my.data$age,na.rm=TRUE)

sd(my.data$age,na.rm=TRUE)
var(my.data$age,na.rm=TRUE)
IQR(my.data$age,na.rm=TRUE)

boxplot(my.data$age)
hist(my.data$age, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(my.data$age)
qqline(my.data$age, col = "blue", lwd = 2)
d <- density(my.data$age, na.rm=T)  #density plot or distribution
plot(d) 

shapiro.test(my.data$age)

t.test(my.data$age, mu= 43.83129)
########################################################################################
# Categorical
#1
my.data$MaritalStatus<- factor(my.data$MaritalStatus,
                                      levels = c(1,2,3,4,5,6,7,9),
                                      labels = c("Legally married"," Living together"," Divorced"," Separated, but married"," Widowed"," Single, but together"," Single,never married"," Unspecified"))  
table(my.data$MaritalStatus)
#Question 1. (4)
pie(table(my.data$MaritalStatus))
barplot(table(my.data$MaritalStatus),horiz=F,main="marital status")
chisq.test(table(my.data$MaritalStatus))
#2
my.data$populationGroup<- factor(my.data$populationGroup,
                              levels = c(1,2,3,4),
                              labels = c(" African/Black"," Coloured", " Indian/Asian"," White"))
table(my.data$populationGroup)
#Question 1. (4)
pie(table(my.data$populationGroup))
barplot(table(my.data$populationGroup),horiz=F,main="Race")
chisq.test(table(my.data$populationGroup))
#3

my.data$religion<- factor(my.data$religion,
                                levels = c(1,2,3,4,5,6,7,8,9,91,92,93,98,99),
                                labels = c("Christian"," Muslim","traditional African","Hindu","Buddhist","Bahai","Jewish", "Atheist", "Agnostic","else", "Nothing ", "Refused", "Do not know", "Unspecified"))
table(my.data$religion)
#Question 1. (4)
pie(table(my.data$religion))
barplot(table(my.data$religion),horiz=F,main="religion")
chisq.test(table(table(my.data$religion)))
#4
my.data$educationalLevel <- factor(my.data$educationalLevel,
                                levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,98,99),
                                labels = c( "Grade R/0","Grade 1","Grade 2","Grade 3","Grade 4","Grade 5","Grade 6","Grade 7","Grade 8","Grade 9","Grade 10","Grade 11","Grade 12 (No Exemption)","Grade 12 (Exemption )","NTC 1", "NTC 2","NTC 3","N4","N5","N6","Certificate(less Grade 12)","Diploma(less Grade 12)"," Certificate(Grade 12)","Diploma(Grade 12)","Higher Diploma (Technikon)","Post Higher Diploma (Technikon) ", "Bachelors","Bachelors and PGD","Honours","Higher degree(Masters,PhD)","Other (specify below)","Do not know","No schooling","Unspecified"))
table(my.data$educationalLevel)
pie(table(my.data$educationalLevel))
barplot(table(my.data$educationalLevel),horiz=F,main="educational level")
chisq.test(table(my.data$educationalLevel))
#5

my.data$disability <- factor(my.data$disability, levels = c(0,1,9),labels = c("Not disabled","Disabled","Unspecified"))
table(my.data$disability)
#Question 1. (4)
pie(table(my.data$disability))
barplot(table(my.data$disability),horiz=F,main="disability")
chisq.test(table(my.data$disability))
#6

my.data$geotype<- factor(my.data$geotype,
                            levels = c(1,2,4,5),
                            labels = c(" Urban formal"," Urban informal"," Tribal areas"," Rural formal"))
table(my.data$geotype)
#Question 1. (4)
pie(table(my.data$geotype))
barplot(table(my.data$geotype),horiz=F,main="regions")
chisq.test(table(my.data$geotype))
chisq.test(table(my.data$disability))
############################################################################################
# Bavariate
#Question 1. (5)
# for salary and age ,i.e, numerical and numerical.
cor(my.data$salary, my.data$age, method = "spearman" ) #both numeric and atleast one is normally distributed
#for salary (numerical) and categorical
kruskal.test(my.data$salary ~ my.data$geotype)
kruskal.test(my.data$salary ~ my.data$disability)
kruskal.test(my.data$salary ~ my.data$educationalLevel)
kruskal.test(my.data$salary ~ my.data$religion)
kruskal.test(my.data$salary~my.data$populationGroup)
kruskal.test(my.data$salary ~ my.data$MaritalStatus)
# for age (numerical) and categorical
kruskal.test(my.data$age ~ my.data$geotype)
kruskal.test(my.data$age ~ my.data$disability)
kruskal.test(my.data$age ~ my.data$educationalLevel)
kruskal.test(my.data$age ~ my.data$religion)
kruskal.test(my.data$age~my.data$populationGroup)
kruskal.test(my.data$age ~ my.data$MaritalStatus)
################################################################################
#Question 1. (6)
#linear regression
N=my.data[(my.data$salary<=750000&my.data$salary>=20000),]                                                  
model_l=lm(salary~age+MaritalStatus+populationGroup+religion+disability+geotype,data=N)  
model_l                                                           
summary(model_l)   