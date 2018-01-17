
View(titanic)


library('Rcmdr')
library(neuralnet)

#build the neuralnetwork model
titanic<-read.csv("/Users/cpkoywk/OneDrive/SCM651/data/Titanic.csv")
titanicnet<-neuralnet(survived ~ gendernum+age,titanic,hidden=2,lifesign = "minimal",linear.output = FALSE,threshold = 0.01)
#everyone starts at a random starting point

#this tells us that 
titanicnet$result.matrix

plot(titanicnet)
?neuralnet


dim(titanic)

titanic[1:20,]
