# BTL KDE estimation for 2 parameter and 3 parameter combinations#

setwd('U:/Scenario Tuning/2017-2018/Rapid Movement of Funds/3-Exploratory Data Analysis')

library(dplyr)
library(magrittr)
library(ks)

data <- read.csv('ind_rr_s.csv',as.is=T)

###Comprehensive list of multi parameter zones

head(data)
str(data)

data$PERC <- abs(data$DEBIT_AMOUNT/data$CREDIT_AMOUNT - 1)

#Adjust % Debit/Credit
data$PERC2 <- ifelse(data$PERC==0,0.001,data$PERC)


data2 <- data %>% select(CR_AMT,PERC2,PERC,CREDIT_COUNT,DEBIT_COUNT,Effective_flag) %>% filter(Effective_flag==1)


######################Density estimation########################

head(data2)

#Find threhsold values
Min_cr_amt <- 30000
Max_cr_amt <- 100000
Max_perc <- 0.05
Min_cr_ct <- 4
Max_cr_ct <- 8
Max_perc  <- 0.05
Min_dr_ct <- 4
Max_dr_ct <- 10

#############CREDIT AMOUNT AND DEBIT COUNT ############

fhat <- kde(x=data2[,c('CR_AMT','DEBIT_COUNT')])
plot(fhat,col=c(1,2,3))


#Cumulative probability of scaled values(4.49,4.49) is ~ 1

#values corresponding to other critical points
amtmin <-  25000
amtmax <- 105000
ctmin <- 3
ctmax <- 11


#Get required probabilities
Fhat <- kcde(x=data2[,c('CR_AMT','DEBIT_COUNT')],xmin=c(20000,1),xmax = c(110000,15))

ev.pts= rbind(   c(Min_cr_amt,Min_dr_ct), #1
                 c(Min_cr_amt,Max_dr_ct), #2
                 c(Max_cr_amt,Min_dr_ct), #3
                 c(Max_cr_amt,Max_dr_ct), #4
                 c(amtmin,Max_dr_ct),     #5 
                 c(Min_cr_amt,ctmax),     #6
                 c(amtmax,Max_dr_ct),     #7
                 c(Max_cr_amt,ctmax),     #8
                 c(amtmax,Min_dr_ct),     #9
                 c(Max_cr_amt,ctmin), #10
                 c(amtmax,ctmax)) #11    

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:11)
round(probs,5)

##Probability of effectiveness less than  Min Credit Amount & Min Debit Count
probs[1] #0.02815 # Region 2

##Probability of effectiveness less than Min Credit Amount &  greater than Max Debit Count
probs[6] - probs[2] #0.0182 #Region 3

##Probability of effectiveness greater than Max Credit Amount & less than  min Debit Count
probs[9] - probs[3] #0.00236 #Region 1

##Probability of effectiveness greater than max credit amount and greater than max debit count
round(probs[11] - (probs[7] + probs[8] - probs[4]),5) #0.00152#Region 4



#############CREDIT AMOUNT AND % Variance between Credit/Debit ############


fhat <- kde(x=data2[,c('CR_AMT','PERC')])
plot(fhat,col=c(1,2,3))

#fhat2 <- kde(x=data2[,c('CR_AMT_scaled','PERC_scaled')])
#plot(fhat2,col=c(1,2,3))


#Cumulative probability of scaled values(4,4) is ~ 1

#Scaled values corresponding to other critical points
amtmin  <- 25000
amtmax  <- 110000
percmin <- 0
percmax <- 0.06


#Get required probabilities

Fhat <- kcde(x=data2[,c('CR_AMT','PERC')],xmin=c(20000,0),xmax=c(125000,0.1))

ev.pts= rbind(   c(Min_cr_amt,Max_perc), #1
                 c(Max_cr_amt,Max_perc), #2
                 c(amtmin,Max_perc),     #3
                 c(amtmax,Max_perc),     #4
                 c(Min_cr_amt,percmax),  #5
                 c(Max_cr_amt,percmax),  #6
                 c(amtmax,percmax))      #7      

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:7)
round(probs,5)

##Probability of effectiveness  greater than  Max Credit Amount &  greater than Max % Variace
round(probs[7] - (probs[4] + probs[6] - probs[2]),5) # 0

##Probability of effectiveness less than Min Credit Amount &  greater than % variance
probs[5] - probs[1] #0.00128

##############################Credit Amount and Credit Count #########################

fhat <- kde(x=data2[,c('CR_AMT','CREDIT_COUNT')])
plot(fhat,col=c(1,2,3))


#Cumulative probability of scaled values(4.49,4.49) is ~ 1

#values corresponding to other critical points
amtmin <-  25000
amtmax <- 105000
ctmin <- 3
ctmax <- 9


#Get required probabilities
Fhat <- kcde(x=data2[,c('CR_AMT','CREDIT_COUNT')],xmin=c(20000,1),xmax = c(110000,15))

ev.pts= rbind(   c(Min_cr_amt,Min_cr_ct), #1
                 c(Min_cr_amt,Max_cr_ct), #2
                 c(Max_cr_amt,Min_cr_ct), #3
                 c(Max_cr_amt,Max_cr_ct), #4
                 c(amtmin,Max_cr_ct),     #5 
                 c(Min_cr_amt,ctmax),     #6
                 c(amtmax,Max_cr_ct),     #7
                 c(Max_cr_amt,ctmax),     #8
                 c(amtmax,Min_cr_ct),     #9
                 c(Max_cr_amt,ctmin), #10
                 c(amtmax,ctmax)) #11    

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:11)
round(probs,5)

##Probability of effectiveness less than  Min Credit Amount & Min Credit Count
probs[1] #0.02804

##Probability of effectiveness less than Min Credit Amount &  greater than Max  Credit Count
probs[6] - probs[2] #0.0089

##Probability of effectiveness greater than Max Credit Amount & less than  min Credit Count
probs[9] - probs[3] #0.00234 #Region 1

##Probability of effectiveness greater than max credit amount and greater than max Credit count
round(probs[11] - (probs[7] + probs[8] - probs[4]),5) #0.00076#Region 2



#######Debit Count and Credit Count #######


fhat <- kde(x=data2[,c('CREDIT_COUNT','DEBIT_COUNT')])
plot(fhat,col=c(1,2,3))

#Scaled values corresponding to other critical points
ctmin  <- 3
ctmax_cr <- 9
ctmax_dr <- 11

#Get required probabilities

Fhat <- kcde(x=data2[,c('CREDIT_COUNT','DEBIT_COUNT')],xmin=c(1,1),xmax=c(15,15))

ev.pts= rbind(   c(Min_cr_ct,Min_dr_ct), #1
                 c(Min_cr_ct,Max_dr_ct), #2
                 c(Max_cr_ct,Min_dr_ct), #3
                 c(Max_cr_ct,Max_dr_ct), #4
                 c(ctmin,Max_dr_ct),     #5 
                 c(Min_cr_ct,ctmax_dr),     #6
                 c(ctmax_cr,Max_dr_ct),     #7
                 c(Max_cr_ct,ctmax_dr),     #8
                 c(ctmax_cr,Min_dr_ct),     #9
                 c(Max_cr_ct,ctmin),     #10
                 c(ctmax_cr,ctmax_dr))         #11 

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:11)
round(probs,5)

##Probability of effectiveness  greater than  Max Credit Count &  greater than Max Debit COunt
probs[11] - (probs[8] + probs[7] - probs[4])#R2 # 0.0014

##Probability of effectiveness less than Min Credit Count &  greater than max Debit Count 
probs[6] - probs[2] #0.0062


##Probability of effectivness greater  than Max Credit Count and less than Min Debit Count
probs[9] - probs[3] #0.0015 #R1

##Probability of effectivness less than Min Credit Count and less than Min Debit Count
probs[1] #0.0377



#############CREDIT COUNT AND % Variance between Credit/Debit ############


fhat <- kde(x=data2[,c('CREDIT_COUNT','PERC')])
plot(fhat,col=c(1,2,3))

#fhat2 <- kde(x=data2[,c('CR_AMT_scaled','PERC_scaled')])
#plot(fhat2,col=c(1,2,3))



#Scaled values corresponding to other critical points
ctmin  <- 3
ctmax  <- 9
percmin <- 0
percmax <- 0.06


#Get required probabilities

Fhat <- kcde(x=data2[,c('CREDIT_COUNT','PERC')],xmin=c(1,0),xmax=c(15,0.1))

ev.pts= rbind(   c(Min_cr_ct,Max_perc), #1
                 c(Max_cr_ct,Max_perc), #2
                 c(ctmin,Max_perc),     #3
                 c(ctmax,Max_perc),     #4
                 c(Min_cr_ct,percmax),  #5
                 c(Max_cr_ct,percmax),  #6
                 c(ctmax,percmax))      #7      

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:7)
round(probs,5)

##Probability of effectiveness  greater than  Max Credit  Count&  greater than Max % Variace
probs[7] - (probs[4] + probs[6] - probs[2]) # 0

##Probability of effectiveness less than Min Credit Count &  greater than % variance
probs[5] - probs[1] #0.013


##############################3D Regions ############################
################################################################
library(ks)
head(data2)

#Find threhsold values
Min_cr_amt <- 30000
Max_cr_amt <- 100000
Max_perc <- 0.05
Min_cr_ct <- 4
Max_cr_ct <- 8
Max_perc  <- 0.05

Min_dr_ct <- 4
Max_dr_ct <- 10

###1) Credit Amount , %Debit/Credit and Credit Count################

fhat <- kde(x=data2[,c('CR_AMT','PERC','CREDIT_COUNT')])
palf <- colorRampPalette(c("gray80","dark red"))
plot(fhat,col.fun=palf)

amtmin <-  25000
amtmax <- 105000
ctmin <- 2
ctmax <- 10
percmin <- 0.025 
percmax <- 0.075

Fhat <- kcde(x=data2[,c('CR_AMT','PERC','CREDIT_COUNT')],xmin=c(20000,0,1),xmax = c(150000,0.1,12))


##A) Probability of effectiveness greater than Max Credit Amount & Max % Debit/credit & Max Credit Count
#Get required probabilities #R3



ev.pts <- rbind(c(amtmax,percmax,ctmax),      #1
                c(Max_cr_amt,percmax,ctmax),  #2
                c(Max_cr_amt,Max_perc,ctmax), #3
                c(amtmax,Max_perc,ctmax),     #4
                c(amtmax,percmax,Max_cr_ct),  #5
                c(Max_cr_amt,percmax,Max_cr_ct), #6
                c(Max_cr_amt,Max_perc,Max_cr_ct), #7
                c(amtmax,Max_perc,Max_cr_ct)) #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round(probs[1] - (probs[2] + probs[4] - probs[3]),5)# Probability of long block
pS <-  round(probs[5] - (probs[6] + probs[8] - probs[7]),5) # probability of short block

pL- pS # 0 

##B) Probability of effectiveness less than Min Trxn Amount, greater than Max credit Count and %Credit/Debit
#R4

ev.pts <- rbind(c(Min_cr_amt,percmax,ctmax),   #2
                c(Min_cr_amt,Max_perc,ctmax),  #3
                c(Min_cr_amt,percmax,Max_cr_ct), #6
                c(Min_cr_amt,Max_perc,Max_cr_ct)) #7



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(2,3,6,7)
round(probs,5)


pL <- probs['2'] - probs['6'] # Probability of long block
pS <- probs['3'] - probs['7'] # probability of short block

pL- pS #0

## C) Probability of effectiveness greater than Max Trxn Amount, greater than %Debit/Credit 
#and less than Min Credit Count #R2




ev.pts <- rbind(c(Max_cr_amt,Max_perc,Min_cr_ct), #1
                c(Max_cr_amt,percmax,Min_cr_ct),  #2
                c(amtmax,percmax,Min_cr_ct),      #3
                c(amtmax,Max_perc,Min_cr_ct))     #4

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:4)
round(probs,5)

pL <- probs[3] - probs[2]
pS <- probs[4] - probs[1]

pL - pS # 0

## D) Probability of effectiveness  less than Min Trxn Amount, greater than %Debit/Credit 
#and less than Min Credit Count

ev.pts <- rbind(c(Min_cr_amt,percmax,Min_cr_ct), #3
                c(Min_cr_amt,Max_perc,Min_cr_ct))  #4


probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(3,4)
round(probs,5)

pL <- probs['3'] 
pS <- probs['4'] 

pL - pS # 0.0000447 #R1


####################2) Credit Amount , %Debit/Credit and  Debit Count################


fhat <- kde(x=data2[,c('CR_AMT','PERC','DEBIT_COUNT')])
palf <- colorRampPalette(c("gray80","dark red"))
plot(fhat,col.fun=palf)

amtmin <-  25000
amtmax <- 105000
ctmin <- 3
ctmax <- 11
percmin <- 0.025 
percmax <- 0.075

Fhat <- kcde(x=data2[,c('CR_AMT','PERC','CREDIT_COUNT')],xmin=c(20000,0,1),xmax = c(150000,0.1,15))


##A) Probability of effectiveness greater than Max Credit Amount & Max % Debit/credit & Max Credit Count
#Get required probabilities #R3



ev.pts <- rbind(c(amtmax,percmax,ctmax),      #1
                c(Max_cr_amt,percmax,ctmax),  #2
                c(Max_cr_amt,Max_perc,ctmax), #3
                c(amtmax,Max_perc,ctmax),     #4
                c(amtmax,percmax,Max_dr_ct),  #5
                c(Max_cr_amt,percmax,Max_dr_ct), #6
                c(Max_cr_amt,Max_perc,Max_dr_ct), #7
                c(amtmax,Max_perc,Max_dr_ct)) #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round(probs[1] - (probs[2] + probs[4] - probs[3]),5)# Probability of long block
pS <-  round(probs[5] - (probs[6] + probs[8] - probs[7]),5) # probability of short block

pL- pS # 0 

##B) Probability of effectiveness less than Min Trxn Amount, greater than Max credit Count and %Credit/Debit
#R4

ev.pts <- rbind(c(Min_cr_amt,percmax,ctmax),   #2
                c(Min_cr_amt,Max_perc,ctmax),  #3
                c(Min_cr_amt,percmax,Max_cr_ct), #6
                c(Min_cr_amt,Max_perc,Max_cr_ct)) #7



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(2,3,6,7)
round(probs,5)


pL <- probs['2'] - probs['6'] # Probability of long block
pS <- probs['3'] - probs['7'] # probability of short block

pL- pS #0

## C) Probability of effectiveness greater than Max Trxn Amount, greater than %Debit/Credit 
#and less than Min  Debit Count #R2



ev.pts <- rbind(c(Max_cr_amt,Max_perc,Min_cr_ct), #1
                c(Max_cr_amt,percmax,Min_cr_ct),  #2
                c(amtmax,percmax,Min_cr_ct),      #3
                c(amtmax,Max_perc,Min_cr_ct))     #4

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:4)
round(probs,5)

pL <- probs[3] - probs[2]
pS <- probs[4] - probs[1]

pL - pS # 0

## D) Probability of effectiveness  less than Min Trxn Amount, greater than %Debit/Credit 
#and less than Min Credit Count

ev.pts <- rbind(c(Min_cr_amt,percmax,Min_cr_ct), #3
                c(Min_cr_amt,Max_perc,Min_cr_ct))  #4


probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(3,4)
round(probs,5)

pL <- probs['3'] 
pS <- probs['4'] 

pL - pS # 0.000425


####################3) Credit Amount , Credit Count and  Debit Count################



fhat <- kde(x=data2[,c('CR_AMT','CREDIT_COUNT','DEBIT_COUNT')])
palf <- colorRampPalette(c("gray80","dark red"))
plot(fhat,col.fun=palf)

amtmin <-  25000
amtmax <- 105000
ct_min <- 3
ct_dr_max <- 11
ct_cr_max <- 9
percmin <- 0.025 
percmax <- 0.075

Fhat <- kcde(x=data2[,c('CR_AMT','CREDIT_COUNT','DEBIT_COUNT')],xmin=c(20000,0,1),xmax = c(150000,0.1,15))


##A) Probability of effectiveness greater than Max Credit Amount &  Max Credit Count & MAX DEBIT COunt
#Get required probabilities



ev.pts <- rbind(c(amtmax,ct_cr_max,ct_dr_max),      #1
                c(Max_cr_amt,ct_cr_max,ct_dr_max),  #2
                c(Max_cr_amt,Max_cr_ct,ct_dr_max),  #3
                c(amtmax,Max_cr_ct,ct_dr_max),      #4
                c(amtmax,ct_cr_max,Max_dr_ct),      #5
                c(Max_cr_amt,ct_cr_max,Max_dr_ct),  #6
                c(Max_cr_amt,Max_cr_ct,Max_dr_ct),  #7
                c(amtmax,Max_cr_ct,Max_dr_ct))      #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round(probs[1] - (probs[2] + probs[4] - probs[3]),5)# Probability of long block
pS <-  round(probs[5] - (probs[6] + probs[8] - probs[7]),5) # probability of short block

pL- pS # 0 

##B) Probability of effectiveness less than Min Trxn Amount, greater than Max credit Count and Debit COunt


ev.pts <- rbind(c(Min_cr_amt,ct_cr_max,ct_dr_max),   #2
                c(Min_cr_amt,Max_cr_ct,ct_dr_max),  #3
                c(Min_cr_amt,ct_cr_max,Max_dr_ct), #6
                c(Min_cr_amt,Max_cr_ct,Max_dr_ct)) #7



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(2,3,6,7)
round(probs,5)


pL <- probs['2'] - probs['6'] # Probability of long block
pS <- probs['3'] - probs['7'] # probability of short block

pL- pS #0

## C) Probability of effectiveness greater than Max Trxn Amount, greater than Max Credit COunt
#and less than Min  Debit Count




ev.pts <- rbind(c(Max_cr_amt,Max_cr_ct,Min_dr_ct), #1
                c(Max_cr_amt,ct_cr_max,Min_dr_ct),  #2
                c(amtmax,ct_cr_max,Min_dr_ct),      #3
                c(amtmax,Max_cr_ct,Min_cr_ct))     #4

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:4)
round(probs,5)

pL <- probs[3] - probs[2]
pS <- probs[4] - probs[1]

pL - pS # 0

## D) Probability of effectiveness  less than Min Trxn Amount, greater than Max Credit COunt
#and less than Min  Debit Count

ev.pts <- rbind(c(Min_cr_amt,ct_cr_max,Min_dr_ct), #3
                c(Min_cr_amt,Max_cr_ct,Min_dr_ct))  #4


probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(3,4)
round(probs,5)

pL <- probs['3'] 
pS <- probs['4'] 

pL - pS # 0


##E) Probability of effectiveness greater than Max Credit Amount &  less than Min Credit Count & 
#greater than MAX DEBIT COunt
#Get required probabilities



ev.pts <- rbind(c(amtmax,Min_cr_ct,ct_dr_max),      #1
                c(Max_cr_amt,Min_cr_ct,ct_dr_max),  #2
                c(Max_cr_amt,Min_cr_ct,ct_dr_max),  #3
                c(amtmax,ct_min,ct_dr_max),         #4
                c(amtmax,Min_cr_ct,Max_dr_ct),      #5
                c(Max_cr_amt,Min_cr_ct,Max_dr_ct),  #6
                c(Max_cr_amt,Min_cr_ct,Max_dr_ct),  #7
                c(amtmax,ct_min,Max_dr_ct))      #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round(probs[1] - probs[2] ,5)# Probability of long block
pS <-  round(probs[5] - probs[6] ,5) # probability of short block

pL- pS # 0.00081 

##F) Probability of effectiveness less than  Min Credit Amount &  less than Min Credit Count & 
#greater than MAX DEBIT COunt
#Get required probabilities



ev.pts <- rbind(c(amtmin,Min_cr_ct,ct_dr_max),      #1
                c(Min_cr_amt,Min_cr_ct,ct_dr_max),  #2
                c(Min_cr_amt,ctmin,ct_dr_max),  #3
                c(amtmin,ct_min,ct_dr_max),         #4
                c(amtmin,Min_cr_ct,Max_dr_ct),      #5
                c(Min_cr_amt,Min_cr_ct,Max_dr_ct),  #6
                c(Min_cr_amt,ct_min,Max_dr_ct),  #7
                c(amtmin,ct_min,Max_dr_ct))        #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round( probs[2] ,5)# Probability of long block
pS <-  round(probs[6] ,5) # probability of short block

pL- pS # 0.00724 

##G) Probability of effectiveness less than  Min Credit Amount &  less than Min Credit Count & 
#less than Min Debit COunt
#Get required probabilities



ev.pts <- rbind(c(amtmin,ct_min,Min_dr_ct),         #1
                c(amtmin,Min_cr_ct,Min_dr_ct),      #2
                c(Min_cr_amt,Min_cr_ct,Min_dr_ct),  #3
                c(Min_cr_amt,ct_min,Min_dr_ct),     #4
                c(amtmin,ct_min,ct_min),            #5
                c(amtmin,Min_cr_ct,ct_min),         #6
                c(Min_cr_amt,Min_cr_ct,ct_min),     #7
                c(Min_cr_amt,ct_min,ct_min))        #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

probs[3] #0.0265


##H) Probability of effectiveness greater than Max Credit Amount &  less than Min Credit Count & 
#less than Min Debit COunt
#Get required probabilities



ev.pts <- rbind(
                c(Max_cr_amt,Min_cr_ct,Min_dr_ct),      #2
                c(amtmax,Min_cr_ct,Min_dr_ct)  #3
               )      



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c('2','3')
round(probs,5)

probs['3'] - probs['2'] #0



####################4) Credit Count , %Debit/Credit and  Debit Count################


fhat <- kde(x=data2[,c('CREDIT_COUNT','PERC','DEBIT_COUNT')])
palf <- colorRampPalette(c("gray80","dark red"))
plot(fhat,col.fun=palf)

amtmin <-  25000
amtmax <- 105000
ctmin <- 3
ct_dr_max <- 11
ct_cr_max <- 9
percmin <- 0.025 
percmax <- 0.075

Fhat <- kcde(x=data2[,c('CREDIT_COUNT','PERC','DEBIT_COUNT')],xmin=c(1,0,1),xmax = c(15,0.1,15))


##A) Probability of effectiveness greater than Max Credit Count & Max % Debit/credit & Max Debit Count
#Get required probabilities #R3



ev.pts <- rbind(c(ct_cr_max,percmax,ct_dr_max),      #1
                c(Max_cr_ct,percmax,ct_dr_max),      #2
                c(Max_cr_ct,Max_perc,ct_dr_max),     #3
                c(ct_cr_max,Max_perc,ct_dr_max),     #4
                c(ct_cr_max,percmax,Max_dr_ct),  #5
                c(Max_cr_ct,percmax,Max_dr_ct),  #6
                c(Max_cr_ct,Max_perc,Max_dr_ct), #7
                c(ct_cr_max,Max_perc,Max_dr_ct)) #8



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:8)
round(probs,5)

pL <-  round(probs[1] - (probs[2] + probs[4] - probs[3]),5)# Probability of long block
pS <-  round(probs[5] - (probs[6] + probs[8] - probs[7]),5) # probability of short block

pL- pS # 0 

##B) Probability of effectiveness less than Min Credit Count, greater than %Credit/Debit and Max Debit Count
#R4

ev.pts <- rbind(c(Min_cr_ct,percmax,ct_dr_max),   #2
                c(Min_cr_ct,Max_perc,ct_dr_max),  #3
                c(Min_cr_ct,percmax,Max_dr_ct), #6
                c(Min_cr_ct,Max_perc,Max_dr_ct)) #7



probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(2,3,6,7)
round(probs,5)


pL <- probs['2'] - probs['6'] # Probability of long block
pS <- probs['3'] - probs['7'] # probability of short block

pL- pS #0.00151

## C) Probability of effectiveness greater than Max Credit COunt, greater than %Debit/Credit 
#and less than Min  Debit Count #R2



ev.pts <- rbind(c(Max_cr_ct,Max_perc,Min_dr_ct),     #1
                c(Max_cr_ct,percmax,Min_dr_ct),      #2
                c(ct_cr_max,percmax,Min_dr_ct),      #3
                c(ct_cr_max,Max_perc,Min_dr_ct))     #4

probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(1:4)
round(probs,5)

pL <- probs[3] - probs[2]
pS <- probs[4] - probs[1]

pL - pS # 0

## D) Probability of effectiveness  less than Min Credit COunt, greater than %Debit/Credit 
#and less than Min Debit Count #R1

ev.pts <- rbind(c(Min_cr_ct,percmax,Min_dr_ct), #3
                c(Min_cr_ct,Max_perc,Min_dr_ct))  #4


probs <- predict(Fhat,x=ev.pts)
names(probs) <- c(3,4)
round(probs,5)

pL <- probs['3'] 
pS <- probs['4'] 

pL - pS # 0.00148
