#file Q1Q2V5.csv 
df1<-read.csv("Q1Q2V6.csv", stringsAsFactors = FALSE)

LinearModel.1 <- lm(logmove ~ logprice + BRAND + Season + BRAND*logprice 
                    +Feat +AGE9 + AGE60 + EDUC + ETHNIC + INCOME + NOCAR + NWHITE + POVERTY + 
                      RETIRED + SINGLE + UNEMP, data=df1)
summary(LinearModel.1)

unique(df1$BRAND)

df2<-read.csv("Q3Submit.csv", stringsAsFactors = FALSE)

colnames(df2)

