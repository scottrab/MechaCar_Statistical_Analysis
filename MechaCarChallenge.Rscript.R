Mecha_Car<- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors=F)

Library(dplyr)

model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_Car)

summary(model)

Suspension<-read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors=F)

summarize_suspension <- Suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups='keep')

Lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups='keep')

t.test(log10(Suspension$PSI),mu=mean(log10(Suspension$PSI)))

t.test(subset=Suspension$Manufacturing_Lot=="Lot1",(Suspension$PSI),mu=mean(Suspension$PSI))

t.test(subset=Suspension$Manufacturing_Lot=="Lot2",(Suspension$PSI),mu=mean(Suspension$PSI))

t.test(subset=Suspension$Manufacturing_Lot=="Lot3",(Suspension$PSI),mu=mean(Suspension$PSI))
