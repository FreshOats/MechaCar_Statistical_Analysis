# Libraries
library(dplyr)



# Load the data
Mecha_df <- read.csv("MechaCar_mpg.csv")
head(Mecha_df)

# Multiple regression on all parameters as independents to observe the effects on the dependent mpg
multi <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, Mecha_df)

multi
summary(multi)

## Note: the AWD data is binary/dichotomous, and thus cannot be included in a linear regression analysis
multi_no_awd <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance, Mecha_df)
multi_no_awd
summary(multi_no_awd)


## Follow-up
no_awd <- Mecha_df[Mecha_df$AWD == 0,]$mpg
w_awd <- Mecha_df[Mecha_df$AWD == 1,]$mpg

t.test(no_awd,w_awd)


# Suspension Analysis
Suspension <- read.csv("Suspension_Coil.csv")
head(Suspension,4)


total_summary <- Suspension %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
total_summary

lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary

## T-Tests on Suspension Coils

All_Lots_t <- t.test(Suspension$PSI, mu=1500)

Lot1 <- Suspension %>% subset(Manufacturing_Lot == "Lot1")
Lot2 <- Suspension %>% subset(Manufacturing_Lot == "Lot2")
Lot3 <- Suspension %>% subset(Manufacturing_Lot == "Lot3")

t1 <- t.test(Lot1$PSI, mu=1500)
t2 <- t.test(Lot2$PSI, mu=1500)
t3 <- t.test(Lot3$PSI, mu=1500)

All_Lots_t
t1
t2
t3



## Study Design: MechaCar vs Competition

df <- Mecha_df %>% mutate(model=NA,hp=NA,qsec=NA,haul_cap=NA,moon_roof=NA, price=NA, safety=NA, mnt_cost=NA)
df <- df[,c(7,12,14,13,6,8,9,1,2,4,10,3,5,11)] # Change the order of the columns, while maintaining values
head(df)
