# p-> population
# e -> tolerance ( between 0 and 1) [.02]
# c -> confidence interval [.95]
e = 0.02;
c = 0.95;

hypsample<- function(p,e,c){
  x<-floor(p*e)
  a<-1-dhyper(0,x,p-x,1:p)
  b<- (sum(a<=c))
  ifelse (a[b]<c,  return(min(b+1,p)), return (b))
}
# p-> population
# e -> tolerance ( between 0 and 1) [.02]
# c -> confidence interval [.95]

##Business RR SEASONED
b1 <- c(264,230,364)
b_rr_s <- sapply(b1,hypsample,e,c)


##Business MR SEASONED
b2 <- c(1122,322,624,63,275)
b_mr_s <- sapply(b2,hypsample,e,c)

##Total business smaple size
bus <- sum(b_rr_s ,b_mr_s)

##IND RR NON SEASONED
i1 <- c(109)
i_rr_ns <- hypsample(i1,e,c)
i_rr_ns

#IND RR SEASONED
i2_1 <- c(494,3325,672,1436,145,958,187,1533,177,884,112,454,57,281,39)
i2_s1 <- sapply(i2_1,hypsample,e,c)
i2_s1

i2_2 <- c(513,233,151,69,163,83,42,25)
i2_s2 <- sapply(i2_2,hypsample,e,c)
i2_s2

i2_3 <- c(120,7,26,1,34,3,15)
i2_s3 <- sapply(i2_3,hypsample,e,c)
i2_s3

i2_4 <- c(370,68,274,65,58,15,45,11)
i2_s4 <- sapply(i2_4,hypsample,e,c)
i2_s4


i2_5 <- c(7,2)
i2_s5 <- sapply(i2_5,hypsample,e,c)
i2_s5

i2_6 <- c(26,5,8,1)
i2_s6 <- sapply(i2_6,hypsample,e,c)
i2_s6

i2_7 <- c(36,16,4,5)
i2_s7 <- sapply(i2_7,hypsample,e,c)
i2_s7

i2_8 <- c(2)
i2_s8 <- sapply(i2_8,hypsample,e,c)
i2_s8

i_rr_s <- sum(i2_s1,i2_s2,i2_s3,i2_s4,i2_s5,i2_s6,i2_s7,i2_s8)
i_rr_s

#IND MR SEASONED
i3_1 <- c(79,1480,6,1852,18,378,2)
i3_s1 <- sapply(i3_1,hypsample,e,c)
i3_s1

i3_2 <- c(56,4,8)
i3_s2 <- sapply(i3_2,hypsample,e,c)
i3_s2

i_mr_s <-sum(i3_s1,i3_s2)

##Total IND Sample size
ind <- i_rr_s +i_rr_ns+i_mr_s
bus+ind

