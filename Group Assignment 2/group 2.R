library('Rcmdr') #this is bs
#load data
orange <- 
  read.table("/Users/cpkoywk/OneDrive/SCM651/Group Assignment 2/Orange Juice Assignment 2.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


#1(a)
LinearModel.2 <- lm(logmove ~ logprice + BRAND + Season + BRAND*logprice + 
                      Feat + AGE9 + AGE60 + EDUC + ETHNIC + INCOME + NOCAR + SINGLE + POVERTY, 
                    data=orange)
summary(LinearModel.2)

#1(b)

local({
  .Hypothesis <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,0,0,1), 2, 18, byrow=TRUE)
  .RHS <- c(0,0)
  linearHypothesis(LinearModel.2, .Hypothesis, rhs=.RHS)
})

#drop age9 and ethnic column in linear model
LinearModel1.2<-lm(logmove~logprice+Feat+BRAND+Season+BRAND*logprice+AGE60+EDUC+INCOME+NOCAR+POVERTY+SINGLE,data=orange)

#compare two models
anova(LinearModel1.1, LinearModel1.2)

#1(b)(ii)

local({
  .Hypothesis <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,0,0,1), 2, 18, byrow=TRUE)
  .RHS <- c(0,0)
  linearHypothesis(LinearModel.2, .Hypothesis, rhs=.RHS)
})


#1(b)iii
local({
  .Hypothesis <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0), 1, 18, 
                        byrow=TRUE)
  .RHS <- c(0)
  linearHypothesis(LinearModel.2, .Hypothesis, rhs=.RHS)
})


#1(c)
GLM.2 <- glm(Feat ~ BRAND + Season, family=binomial(logit), data=orange)
summary(GLM.2)
exp(coef(GLM.2))  # Exponentiated coefficients ("odds ratios")

#2
test<-read.csv("Group Assignment 2/L1.csv")
predict(LinearModel1.1,interval="prediction",level=.95,newdata=test)

#1(d)(i)
local({
  .Hypothesis <- matrix(c(0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1), 3, 6, 
                        byrow=TRUE)
  .RHS <- c(0,0,0)
  linearHypothesis(GLM.2, .Hypothesis, rhs=.RHS, test="Chisq")
})

#1(d)(ii)
#1(d)(ii) Season being same, Minute Maid and Tropicana are equally likely to be on sale.

local({
  .Hypothesis <- matrix(c(0,1,0,0,0,0,0,0,1,0,0,0), 2, 6, byrow=TRUE)
  .RHS <- c(0,0)
  linearHypothesis(GLM.2, .Hypothesis, rhs=.RHS, test="Chisq")
})

#2.
LinearModel.4 <- lm(logmove ~ BRAND + Feat + logprice, data=orange)
summary(LinearModel.4)

data1 <- 
  read.table("/Users/cpkoywk/OneDrive/SCM651/Group Assignment 2/Orangejuicepredset.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

predict(LinearModel.4,interval='prediction',level=.95,newdata=data1)


#3
flight <- 
  read.table("/Users/cpkoywk/OneDrive/SCM651/Group Assignment 2/Flight Delays Ledolter with dummies.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

GLM.5 <- glm(delaynew ~ d1 + d2 + d3 + d4 + d5, family=binomial(logit), 
             data=flight)
summary(GLM.5)
exp(coef(GLM.5))  # Exponentiated coefficients ("odds ratios")

#1)
local({
.Hypothesis <- matrix(c(0,1,0,0,0,0,0,0,1,0,0,0), 2, 6, byrow=TRUE)
.RHS <- c(0,0)
linearHypothesis(GLM.5, .Hypothesis, rhs=.RHS, test="Chisq")
})
#2)
local({
  .Hypothesis <- matrix(c(0,0,0,1,0,0,0,0,0,0,1,0), 2, 6, byrow=TRUE)
  .RHS <- c(0,0)
  linearHypothesis(GLM.5, .Hypothesis, rhs=.RHS, test="Chisq")
})
#3)
local({
  .Hypothesis <- matrix(c(0,1,0,0,0,0,0,0,0,0,1,0), 2, 6, byrow=TRUE)
  .RHS <- c(0,0)
  linearHypothesis(GLM.5, .Hypothesis, rhs=.RHS, test="Chisq")
})
#4)
local({
  .Hypothesis <- matrix(c(0,0,1,1,0,0), 1, 6, byrow=TRUE)
  .RHS <- c(0)
  linearHypothesis(GLM.5, .Hypothesis, rhs=.RHS, test="Chisq")
})
#5)

colnames(orange)

