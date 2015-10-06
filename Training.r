#### This code tests fro the presence of jumps using nueral networks


#### Training the neural network #####

trainingdata=matrix(0,nrow=5000,ncol=9)

for(i in 1:5000)
{
  
  mu=runif(1,-1,1)
  s=runif(1,0.01,0.7)
  lam=runif(1,1,252)/252
  mu_jump=runif(1,-1,1)
  s_jump=runif(1,0.01,0.5)
  delta=1/252
  TotalTime=252*5
  
  u = runif(1,0,1)
  if(u<=0.5)
  {
    data=simulateJump(mu,s,lam,mu_jump,s_jump,TotalTime,delta,FALSE,1)
    X= data[,1]#X is already log form
    jump=1
  }
  else
  {    
    data=simulateGBM(mu,s,TotalTime,delta) 
    X= data[,1]#X is already log form
    jump=0
  }

  FirstMoment= mean(diff(X)^1)
  SecondMoment= mean((diff(X)-FirstMoment)^2)
  Skewness=mean((diff(X)-FirstMoment)^3)/SecondMoment^(3/2)
  Kurtosis = (3-mean((diff(X)-FirstMoment)^4)/SecondMoment^2)/10
  FirthMoment= mean((diff(X)-FirstMoment)^5)
  SixthMoment= mean((diff(X)-FirstMoment)^6)
  SeventhMoment= mean((diff(X)-FirstMoment)^7)
  EigthMoment= mean((diff(X)-FirstMoment)^8)
  
  #Column bind the data into one variable
  trainingdata[i,]= cbind(FirstMoment,SecondMoment,Skewness,Kurtosis,
                          FirthMoment,SixthMoment,SeventhMoment,EigthMoment,jump)
  cat("Iteration Number:\t",i, "\n")
  
}
colnames(trainingdata) <- c("FirstMoment", "SecondMoment","Skewness","Kurtosis",
                            "FirthMoment", "SixthMoment","SeventhMoment","EigthMoment","jump")



library("neuralnet")

#derivatives of the error function as stopping criteria.
jumpTest<- neuralnet(jump~FirstMoment+SecondMoment+Skewness
                      +Kurtosis+FirthMoment+SixthMoment+SeventhMoment
                     +EigthMoment,trainingdata[1:3000,], hidden=c(10),
                      threshold=0.001,rep=1,stepmax=1e9)

plot(jumpTest)
