# 3D visualization for ATL regions #

setwd('U:/Scenario Tuning/2017-2018/Rapid Movement of Funds/3-Exploratory Data Analysis')

library(dplyr)
library(magrittr)
library(rgl)

data <- read.csv('ind_rr_s.csv',as.is=T)

###Comprehensive list of multi parameter zones


head(data)
str(data)

data$PERC <- abs(data$DEBIT_AMOUNT/data$CREDIT_AMOUNT - 1)
data$PERC <- .01 * ceiling(data$PERC/.01)
#Adjust % Debit/Credit
data$PERC2 <- ifelse(data$PERC==0,0.001,data$PERC)



data2 <- data %>% select(CR_AMT,PERC2,CREDIT_COUNT,DEBIT_COUNT,Effective_flag) 

##Cut each variable into appropriate intervals
data2$CR_AMT_Int <- cut(data2$CR_AMT,seq(30000,100000,by=10000),include.lowest = T)
data2$PERC2_Int <- cut(data2$PERC2,seq(0,0.05,by=0.01),include.lowest = T)
data2$CREDIT_COUNT_Int <- cut(data2$CREDIT_COUNT,c(3,4,6,8))
data2$DEBIT_COUNT_Int <- cut(data2$DEBIT_COUNT,c(4,6,8,10),include.lowest = TRUE)

#Change levels of variable for plotting
levels(data2$CR_AMT_Int) <- seq(40000,100000,by=10000)
#data2$CR_AMT_Int <- as.numeric(data2$CR_AMT_Int)

levels(data2$PERC2_Int) <- seq(0.01,0.05,by=0.01)
#data2$PERC2_Int <- as.numeric(data2$PERC2_Int)

levels(data2$CREDIT_COUNT_Int) <- c(4,6,8)
#data2$CREDIT_COUNT_Int <- as.numeric(data2$CREDIT_COUNT_Int)

levels(data2$DEBIT_COUNT_Int) <- c(6,8,10)
#data2$CREDIT_COUNT_Int <- as.numeric(data2$CREDIT_COUNT_Int)

############################a)Credit Amount, % Debit/Credit & Credit Count###############################

##Aggregate at level of internal variables

data3 <- data2 %>% select(CR_AMT_Int,PERC2_Int,CREDIT_COUNT_Int,Effective_flag)

alerts <-data3 %>% group_by(CR_AMT_Int,PERC2_Int,CREDIT_COUNT_Int) %>% summarize( Alerts = n())
eff_alerts <- data3 %>% group_by(CR_AMT_Int,PERC2_Int,CREDIT_COUNT_Int) %>% 
                  summarize(Eff_alerts = sum(Effective_flag))

alerts_final <- left_join(alerts,eff_alerts)

alerts_final$Perc_eff <- alerts_final$Eff_alerts/alerts_final$Alerts

summary(alerts_final$Perc_eff)
#Convert to numeric for plotting
x <-  as.numeric(levels(alerts_final$CR_AMT_Int))[alerts_final$CR_AMT_Int]/10000
y <-  as.numeric(levels(alerts_final$PERC2_Int))[alerts_final$PERC2_Int]*100
z <-  as.numeric(levels(alerts_final$CREDIT_COUNT_Int))[alerts_final$CREDIT_COUNT_Int] 

r <- round(alerts_final$Perc_eff,2)
col <- ifelse(r<0.02,'green','red')
###3D visualization

rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}



rgl_init()
rgl.spheres(x,y,z,r = 3*r,color=col)
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.8,xat = unique(x),
         yat=unique(y),zat = unique(z)) 

title3d(main = 'Distribution of Effectiveness', xlab = "Credit Amt(10,000s)",
        ylab = "% Credit/Debit", zlab = "Credit Count ")



############################b)Credit Amount, % Debit/Credit & Debit Count###############################

##Aggregate at level of internal variables

data3 <- data2 %>% select(CR_AMT_Int,PERC2_Int,DEBIT_COUNT_Int,Effective_flag)

alerts <-data3 %>% group_by(CR_AMT_Int,PERC2_Int,DEBIT_COUNT_Int) %>% summarize( Alerts = n())
eff_alerts <- data3 %>% group_by(CR_AMT_Int,PERC2_Int,DEBIT_COUNT_Int) %>% 
  summarize(Eff_alerts = sum(Effective_flag))

alerts_final <- left_join(alerts,eff_alerts)

alerts_final$Perc_eff <- alerts_final$Eff_alerts/alerts_final$Alerts

summary(alerts_final$Perc_eff)
#Convert to numeric for plotting
x <-  as.numeric(levels(alerts_final$CR_AMT_Int))[alerts_final$CR_AMT_Int]/10000
y <-  as.numeric(levels(alerts_final$PERC2_Int))[alerts_final$PERC2_Int]*100
z <-  as.numeric(levels(alerts_final$DEBIT_COUNT_Int))[alerts_final$DEBIT_COUNT_Int] 

r <- round(alerts_final$Perc_eff,2)
col <- ifelse(r<0.02,'green','red')

###3D visualization
library(rgl)
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}



rgl_init()
rgl.spheres(x,y,z,r = 3*r,color=col)
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.8,xat = unique(x),
         yat=unique(y),zat = unique(z)) 

title3d(main = 'Distribution of Effectiveness', xlab = "Credit Amt(10,000s)",
        ylab = "% Credit/Debit", zlab = "Debit Count ")



############################c)Credit Amount, Credit Count & Debit Count###############################

##Aggregate at level of internal variables

data3 <- data2 %>% select(CR_AMT_Int,CREDIT_COUNT_Int,DEBIT_COUNT_Int,Effective_flag)

alerts <-data3 %>% group_by(CR_AMT_Int,CREDIT_COUNT_Int,DEBIT_COUNT_Int) %>% summarize( Alerts = n())
eff_alerts <- data3 %>% group_by(CR_AMT_Int,CREDIT_COUNT_Int,DEBIT_COUNT_Int) %>% 
  summarize(Eff_alerts = sum(Effective_flag))

alerts_final <- left_join(alerts,eff_alerts)

alerts_final$Perc_eff <- alerts_final$Eff_alerts/alerts_final$Alerts

summary(alerts_final$Perc_eff)
#Convert to numeric for plotting
x <-  as.numeric(levels(alerts_final$CR_AMT_Int))[alerts_final$CR_AMT_Int]/10000
y <-  as.numeric(levels(alerts_final$CREDIT_COUNT_Int))[alerts_final$CREDIT_COUNT_Int]
z <-  as.numeric(levels(alerts_final$DEBIT_COUNT_Int))[alerts_final$DEBIT_COUNT_Int] 

r <- round(alerts_final$Perc_eff,2)
col <- ifelse(r<0.02,'green','red')

###3D visualization
library(rgl)
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}



rgl_init()
rgl.spheres(x,y,z,r = 3*r,color=col)
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.8,xat = unique(x),
         yat=unique(y),zat = unique(z)) 

title3d(main = 'Distribution of Effectiveness', xlab = "Credit Amt(10,000s)",
        ylab = "Credit Count", zlab = "Debit Count ")




############################d)Credit Count, % Debit/Credit & Debit Count###############################

##Aggregate at level of internal variables

data3 <- data2 %>% select(CREDIT_COUNT_Int,PERC2_Int,DEBIT_COUNT_Int,Effective_flag)

alerts <-data3 %>% group_by(CREDIT_COUNT_Int,PERC2_Int,DEBIT_COUNT_Int) %>% summarize( Alerts = n())
eff_alerts <- data3 %>% group_by(CREDIT_COUNT_Int,PERC2_Int,DEBIT_COUNT_Int) %>% 
  summarize(Eff_alerts = sum(Effective_flag))

alerts_final <- left_join(alerts,eff_alerts)

alerts_final$Perc_eff <- alerts_final$Eff_alerts/alerts_final$Alerts

summary(alerts_final$Perc_eff)
#Convert to numeric for plotting
x <-  as.numeric(levels(alerts_final$CREDIT_COUNT_Int))[alerts_final$CREDIT_COUNT_Int]
y <-  as.numeric(levels(alerts_final$PERC2_Int))[alerts_final$PERC2_Int]*100
z <-  as.numeric(levels(alerts_final$DEBIT_COUNT_Int))[alerts_final$DEBIT_COUNT_Int] 

r <- round(alerts_final$Perc_eff,2)
col <- ifelse(r<0.02,'green','red')

###3D visualization
library(rgl)
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}



rgl_init()
rgl.spheres(x,y,z,r = 3*r,color=col)
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.8,xat = unique(x),
         yat=unique(y),zat = unique(z)) 

title3d(main = 'Distribution of Effectiveness', xlab = "Credit Count",
        ylab = "% Credit/Debit", zlab = "Debit Count ")






























