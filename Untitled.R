library('Rcmdr') #this is bs



#10/20


orange <- read.table("/Users/cpkoywk/Downloads/Orange Juice Example(1).csv",
                     header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
LinearModel.1<-lm(logmove ~ BRAND + logprice+Season+BRAND*logprice, data=orange)

summary(LinearModel.1)
#According to the summary
#5.14+1.384HH+1.312MINMAID+1.793TROP+2.96X-.007Spring-.142SUMMER-.008Winter-.25HH*X-.19Minmaid*x+.44Trop*X


LinearModel.2<-lm(logmove ~ BRAND + logprice+BRAND*logprice, data=orange)
summary(LinearModel.2)


#Hypothesis test
#compare two models
anova(LinearModel.2,LinearModel.1)
#0.003849 is the p-value



LinearModel.3<-lm(logmove ~ BRAND + logprice+BRAND*logprice + Season, data=orange)
