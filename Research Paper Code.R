#############################
##Load Data
wdi<-read.csv("wdidata20.csv")
####################################
#####################################
##Rename uninformative variables
####################################
##Female Education Completion Rate
wdi$FemEdrate<-wdi$SE.PRM.CMPT.FE.ZS 
##Access to Electricity 
wdi$Access<-wdi$EG.ELC.ACCS.ZS
######################################
#####################################
###Subset data to Middle East and North African Region
MidEstNaf<-subset(wdi, region == "Middle East & North Africa")
#####################################
##Creating Variables 
####################################
##Independent variable x
AccessElec <- MidEstNaf$Access
##Dependent variable y
Femalegradrate <- MidEstNaf$FemEdrate
######################################
##Creating a Regression model and Establishing a Correlation
fit <- lm(Femalegradrate ~ AccessElec, data = MidEstNaf)
coef(fit)
cor(AccessElec, Femalegradrate, use = "pairwise")
#####################################
##Visualizing regression model
plot(x=AccessElec, y=Femalegradrate,
     ylab = "Female Primary School Completion Rate in %",
     xlab =  "A Country's The Level of Access To Electricity in %", 
     main = " Primary Education Completion Rate by Electricity Access in %",
     pch=20)
## Add regression line
abline(fit, lwd=3, col = "purple") # add line
###########################################
###########################################
##Accounting for Statistical Uncertainty
round(summary(fit)$coefficients, digits=4)



