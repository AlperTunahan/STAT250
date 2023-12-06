
library(MASS)
library(AID)
library(effectsize)
library(sjPlot)
data1   <-read.csv("dataset3.csv")
#changing the structure of the variables
data1$gender <- as.factor(data1$gender)
data1$surgical <- as.factor(data1$surgical)

attach(data1)


#model inceleme

model <- lm(Satisfaction ~ ., data=data1)
summary(model)

tab_model(model)

plot(model)

shapiro.test(Satisfaction)#Normality failed
boxcox(model) #saw that we need to take square root since our lambda is 0.5

boxcoxnc(Satisfaction) #to find the best normal data we found lambda as 0.42

#then we check the square root of the model for normality
shapiro.test(Satisfaction^0.5)


hist(Satisfaction^0.5)



#Q1

#tranformed model

modeltrans <- lm(sqrt(Satisfaction) ~., data = data1)
summary(modeltrans)
tab_model(modeltrans)
shapiro.test(sqrt(Satisfaction))


#multicollinearty yok
DAAG::vif(modeltrans)

par(mfrow= c(2,2))
plot(modeltrans)

#standardization

standardize_parameters(modeltrans)

#Q2

female <- data1[data1$gender==0,] 

male <- data1[data1$gender==1,]

var.test(female$Satisfaction, male$Satisfaction)#do not reject çýkýyor ama normality kontrol etmek gerek

#normality check
shapiro.test(female$Satisfaction)
shapiro.test(male$Satisfaction)


#normality check after transformation
shapiro.test(sqrt(female$Satisfaction))
shapiro.test(sqrt(male$Satisfaction))

#transformation
var.test(sqrt(female$Satisfaction), sqrt(male$Satisfaction))

boxplot(Satisfaction ~ gender,data = data1,
        xlab = "Gender of Patients",
        ylab = "Patients Satisfaction",
        main = "The relationship between patients satisfaction and gender",
        notch = TRUE, 
        varwidth = TRUE,
        col = c("red","green"),
        names = c("Female","Male")
        )


  t.test(sqrt(female$Satisfaction), sqrt(male$Satisfaction), var.equal = TRUE)



#Q3
slr <- lm(anxiety ~gender, data= data1)
summary(slr)

#In order to test the difference between anxiety levels according to the patients ages, z-test can be applied
#in order to apply z-test we should check the normality by using shapiro wilk test

#Normalýty test
#buraya normalite testi hipotezlerini ekle
femalepatient <- data1[data1$gender==0,] 
averagenonsurgical <- mean(femalepatient$anxiety)

malepatient <- data1[data1$gender==1,]
averagesurgical <- mean(malepatient$anxiety)

shapiro.test(femalepatient$anxiety)
shapiro.test(malepatient$anxiety)



# since both p values (for anxiety and age) are smaller than alpha which is 0.05 we reject the null hyothesis
#(paraphrase)---It is not proper to use z-test for this hypothesis
#first try to transfor the variables if that does not work then we can apply the Wilcoxon rank-sum test.
#Lets try transformation
plot(slr)


boxcoxnc(anxiety)

#since box cox transformation does not work
#now we can apply the non parametric for mof z-test -> Wilcoxon rank-sum test.
wilcox.test(Age,anxiety, alternative = "two.sided", conf.level = 0.95)



#Q4
model3 <- lm(Satisfaction~day+surgical)
summary(model3)

shapiro.test(Satisfaction)

boxplot(Satisfaction ~ surgical,data = data1,
        notch = TRUE, 
        varwidth = TRUE,
        col = c("purple","pink"),
        names = c("Nonsurgical","Surgical")
        )

plot(Satisfaction,day)


#we apply boxcox transformation
boxcoxnc(Satisfaction)

shapiro.test(sqrt(Satisfaction))
plot(model3)
boxcox(model3)

#transformation
model3 <- lm(sqrt(Satisfaction)~day+surgical) 
summary(model3)
tab_model(model3)

#no need
#alternative Q4
#tried but not succeed.
model3 <- lm(severity~day)
mean(data1$day)
dayaboveaverage <- data1[data1$day >mean(data1$day) ,]
daybelowaverega <- data1[data1$day <= mean(data1$day) ,]

severywith.dayabove <- dayaboveaverage$severity
severywith.daybelow <- daybelowaverega$severity

shapiro.test(severywith.dayabove)
shapiro.test(severywith.daybelow)
