##read csv file MechaCar_mpg.csv
M= read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors=F)

##import dplyr 
Library(dplyr)

##linear model with multiple variables predicting mpg
d= lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_Car)

##summary output for the model
summary(d)

##read suspension_coil.csv
s = read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors=F)

##summarize suspension coil with mean, median and variance
summarize_s = s ~ summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups='keep')

##lot summary for suspension  model
Lot_summary = s ~ group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups='keep')

##run t-test using log
t.test(log10(s$PSI),mu=mean(log10(s$PSI)))

##run t-test for lot 1
t.test(subset=s$Manufacturing_Lot=="Lot1",(s$PSI),mu=mean(s$PSI))

##run t-test for lot 2
t.test(subset=s$Manufacturing_Lot=="Lot2",(s$PSI),mu=mean(s$PSI))

##run t-test for lot 3
t.test(subset=s$Manufacturing_Lot=="Lot3",(s$PSI),mu=mean(s$PSI))
