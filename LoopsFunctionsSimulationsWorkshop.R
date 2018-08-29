## STRI R Workshop - Day 3
## Loops and functions and other fun stuff
## July 17, 2018

library(car)
library(dplyr)
library(ggplot2)

RxP.clean<-read.csv("RxP_clean.csv")

#first simple for loop example
for (i in 1:20)
{
  print(paste("I am student",i))
}

#second simple for loop example
for (i in 1:nrow(RxP.clean))
{
  print(paste("I am tadpole",i))
}

#Run a for loop to look at block effects
#create an empty matrix to store our values (pvalues)
block.pred.effects<-matrix(nrow=8, ncol=1, data=NA)

#run the for loop
for(i in 1:length(unique(RxP.clean$Block)))
{
  mod1<-lm(log(SVL.final)~Pred, data=RxP.clean[RxP.clean$Block == i,])
  #print(anova(mod1))
  block.pred.effects[i,1] <-anova(mod1)$Pr[1] #stores P values into the empty matrix
}

block.pred.effects #now can easily modify this for different purposes
# for example, another approach if you don't want to make a matrix is to concatinate new P values --> makes a vector of P values

output1<-anova(mod1)
str(output1)
output1$Pr #probability #pval #two values 
output$Pr[1]#pvalue is the first value


library(broom)
tidy(anova(mod1))
tidy(anova(mod1))[1,6]


## Let's make our loop into a function where we can vary the input predictor
#make a function for block effects
block_effects<-function(response, predictor, dataset) #can call these arguments w/e we want
{
  #create a blank dataframe of a certain size and give it useful column names
  output<-data.frame(matrix(nrow=8, ncol=2, data=NA))
  names(output) <-c("Block", "pval") #default is X1 and X2 - not very useful

  model_specs<-paste(response, predictor, sep="~") #make the arguments you feed into the function into a string of characters --> to make into a formula later
  
  #run a for loop to run the model 8 times # to calculate the p-value for each block
  for (i in 1:length(unique(dataset$Block)))
  {
    output[i,1]<-paste("Block",1) #will tell us what block is what in the first column of output
    mod1<-lm(formula(model_specs), data=dataset[dataset$Block==i,])
    output[i,2]<-anova(mod1)$Pr[1]
  }
  #return the output
  return(output)
}

block_effects("Tail.initial", "Hatch", RxP.clean) #Don't forget to put predictors in quotes!
block_effects("SVL.final", "Res", RxP.clean)


stderr <- function(vec) #std dev/sample size    ##make a function for std err
{
  return(sd(vec)/sqrt(length(vec)))
}

aggregate(SVL.initial~Res*Pred, data=RxP.clean, stderr)
stderr(c(5,234,346,34,234,235))

#consider the builtin function rbinom()
#rbinom(N, times, probability) #N = # of trials
rbinom(10, 1, 0.5) #flip 1 coin 10 times -->
rbinom(1,10,0.5) # flip 10 coins 1 time --> number of successes out of 10

rbinom(1000, 10, 0.5) #1000 predation trials # with 10 tadpoles # with avg predation rate of 50%
hist(rbinom(1000,10,0.5))
table(rbinom(1000,10,0.5))/1000 #proportions of each fate (or raw numbers if no /1000)

comparePred<-function() #notice no arguments yet - doesn't need any
{
  predA<-rbinom(5000,10,0.3) #5000 binomial trials with 10 prey in each, with probs 0.3 or 0.5
  predB<-rbinom(5000,10,0.5)
  return(list("predA"=predA, "predB"=predB)) #return a list
}

temp<-comparePred()
str(temp)
hist(temp$predA)
hist(temp$predB)

#make the function better by using a data frame
comparePred<-function() #notice no arguments yet - doesn't need any
{
  #Build a blank data frame
  simData<-data.frame(Trial=rep(1:10, times = 2), Eaten=NA) #make a column called trial; repeats 1-10 twice (vs. each, which would make 1, 1, 2, 2, 3, 3, etc.)
  simData$Eaten[1:10]<-rbinom(10,10,0.3) #adds to Eaten column
  simData$Eaten[11:20]<-rbinom(10,10,0.5)
  predA<-rbinom(5000,10,0.3) #5000 binomial trials with 10 prey in each, with probs 0.3 or 0.5
  predB<-rbinom(5000,10,0.5)
  return(simData) #return a list
}

temp<-comparePred()
temp


#make the function better changing 10 to more flexible
comparePred<-function(numTrials, predLevelA, predLevelB) #notice no arguments yet - doesn't need any
{
  #Build a blank data frame
  simData<-data.frame(Trial=rep(1:numTrials, times = 2), 
                      Pred=rep(c("PredA","PredB"), each=numTrials), 
                      Eaten=NA) #make a column called trial; repeats 1-10 twice (vs. each, which would make 1, 1, 2, 2, 3, 3, etc.)
  simData$Eaten[simData$Pred=="PredA"]<-rbinom(numTrials,10,predLevelA) #adds to Eaten column
  simData$Eaten[simData$Pred=="PredB"]<-rbinom(numTrials,10,predLevelB)
  return(simData) #return a list
}

temp<-comparePred(100, 0.2, 0.7)
temp

qplot(data=temp, x=Eaten, fill=Pred, alpha=0.5, geom="density")



#make the function better changing 10 to more flexible
predTrialSim<-function(numTrials, predLevelA, predLevelB) #notice no arguments yet - doesn't need any
{
  #Build a blank data frame
  simData<-data.frame(Trial=rep(1:numTrials, times = 2), 
                      Pred=rep(c("PredA","PredB"), each=numTrials), 
                      Eaten=NA, 
                      NotEaten=NA) #make a column called trial; repeats 1-10 twice (vs. each, which would make 1, 1, 2, 2, 3, 3, etc.)
  simData$Eaten[simData$Pred=="PredA"]<-rbinom(numTrials,10,predLevelA) #adds to Eaten column
  simData$Eaten[simData$Pred=="PredB"]<-rbinom(numTrials,10,predLevelB)
  simData$NotEaten=10-simData$Eaten
  
  #run the glm
  simGLM<-glm(cbind(Eaten,NotEaten)~Pred, data=simData, family="binomial")
  return(car::Anova(simGLM)$Pr) #requires car package
}

predTrialSim(100, 0.2, 0.7)

#now we can replicate this function many times
#replicate() #is a function that allows you to repeat a function over and over and gives the output as a vector
predSims<-replicate(1000,predTrialSim(10,0.5,0.7))
str(predSims) #these are the pvalues from the binomial glm
#how many of those pvalues are sigificant?
predSims<=0.05 #how many are trues?
sum(predSims<=0.05)/1000 #number of trues/1000 --> proportion of pvalues that we simulated that were less than 0.05, or significant

hist(predSims, breaks=20)
abline(v=0.05, col="red", lwd=2)
#if we conducted 10 paired predation trials each containing 10 prey animals, and our predators averaged 50% and 70% efficacy, we would expect to detect a significant difference between predators 83% of the time

#what if we double our sample size, from 10 to 20?
predSims<-replicate(1000,predTrialSim(20,0.5,0.7))
sum(predSims<=0.05)/1000

#if only do 5 predation trials, there's a 50% chance that you'll be able to detect a significant difference
predSims<-replicate(1000,predTrialSim(5,0.5,0.7))
sum(predSims<=0.05)/1000

#If predators are more similar in efficacy, --> larger spread of non-significant p values

#Let's add in the ability to vary the # of prey per trial (change the 10's to N)

predTrialSim<-function(numTrials, N, predLevelA, predLevelB) #notice no arguments yet - doesn't need any
{
  #Build a blank data frame
  simData<-data.frame(Trial=rep(1:numTrials, times = 2), 
                      Pred=rep(c("PredA","PredB"), each=numTrials), 
                      Eaten=NA, 
                      NotEaten=NA) #make a column called trial; repeats 1-10 twice (vs. each, which would make 1, 1, 2, 2, 3, 3, etc.)
  simData$Eaten[simData$Pred=="PredA"]<-rbinom(numTrials,N,predLevelA) #adds to Eaten column
  simData$Eaten[simData$Pred=="PredB"]<-rbinom(numTrials,N,predLevelB)
  simData$NotEaten=N-simData$Eaten
  
  #run the glm
  simGLM<-glm(cbind(Eaten,NotEaten)~Pred, data=simData, family="binomial")
  return(car::Anova(simGLM)$Pr) #requires car package
}

predSims<-replicate(1000,predTrialSim(20,5,0.5,0.7))
sum(predSims<=0.05)/1000


predSims<-function(RangeTrials, RangeN, RangePredA, RangePredB, sims) #notice no arguments yet - doesn't need any
{
  #Build a blank data frame
  simData2<-expand.grid(NumTrials=RangeTrials, 
                      NumPrey=RangeN, 
                      PredLevelA=RangePredA, 
                      PredLevelB=RangePredB,
                      Pval=NA,
                      Sim=1:sims)
  #run the simulation for each row in the data frame
  for (i in 1:nrow(simData2))
  {
    simData2$pval[i]<-predTrialSim(simData2$NumTrials)
  }
  
}

#simulatedData<-predSims(RangeTrials = 10...)

