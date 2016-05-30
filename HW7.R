if(!require("mlbench")) {
  install.packages("mlbench", dep=TRUE)
  library("mlbench")
}
if(!require("MASS")) {
  install.packages("MASS", dep=TRUE)
  library("MASS")
}

data(PimaIndiansDiabetes)
#head(PimaIndiansDiabetes)

PimaIndiansDiabetesC<- PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),] 
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))


PimaIndiansDiabetesC$Test<-F
PimaIndiansDiabetesC[sample(1:nrow(PimaIndiansDiabetesC), nrow(PimaIndiansDiabetesC)/3),]$Test<-T #1/3
c(sum(PimaIndiansDiabetesC$Test==F), sum(PimaIndiansDiabetesC$Test==T))

Result<-glm(formula=diabetes~., data=PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,], family="binomial")
finalFit<-stepAIC(Result,direction = "both",trace = T)
summary(finalFit)