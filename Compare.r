TotalTime=252*2
sims=5000
testdata=matrix(0,nrow=sims,ncol=9)
leeResults=matrix(0,nrow=sims,ncol=TotalTime)

for(i in 1:sims)
{
  mu=runif(1,-1,1)
  s=runif(1,0.01,0.7)
  lam=runif(1,1,252)/252
  mu_jump=runif(1,-1,1)
  s_jump=runif(1,0.01,0.5)
  delta=1/252
    
  u = runif(1,0.2,1)
  if(u<=0.5)
  {
    data=simulateJump(mu,s,lam,mu_jump,s_jump,TotalTime,delta,FALSE,1)
    X= data[,1]#X is already log form
    jump=1
  }
  else
  {    
    data=simulateGBM(mu,s,TotalTime,delta) #X is already log form
    X= data[,1]#X is already log form
    jump=0
  }
  
  lee=leeTest(X,16,delta)
  leeResults[i,]=lee
  
FirstMoment= mean(diff(X)^1)
SecondMoment= mean((diff(X)-FirstMoment)^2)
Skewness=mean((diff(X)-FirstMoment)^3)/SecondMoment^(3/2)
Kurtosis = (3-mean((diff(X)-FirstMoment)^4)/SecondMoment^2)/10
FirthMoment= mean((diff(X)-FirstMoment)^5)
SixthMoment= mean((diff(X)-FirstMoment)^6)
SeventhMoment= mean((diff(X)-FirstMoment)^7)
EigthMoment= mean((diff(X)-FirstMoment)^8)

#Column bind the data into one variable
testdata[i,]= cbind(FirstMoment,SecondMoment,Skewness,Kurtosis,
                    FirthMoment,SixthMoment,SeventhMoment,EigthMoment,jump)
cat("Iteration Number:\t",i, "\n")

}
colnames(testdata) <- c("Input1", "Input2","Input3","Input4",
                        "Input5", "Input6","Input7","Input8","Output")



jumpTest.results <- compute(jumpTest, testdata[,1:8]) 
jumpsDetected=jumpTest.results$net.result
jumpsDetectedRounded=round(jumpsDetected)

leeTestJumps=seq(from=1,to=1,length=length(jumpsDetectedRounded))*(rowSums(leeResults)>0)
leeSums=rowSums(leeResults)

actualJumps=testdata[,9]

compare=cbind(jumpsDetectedRounded[17:sims],actualJumps[17:sims],leeTestJumps[17:sims],leeSums[17:sims])
print(compare)


########################################
cat("Proability of  ACTUAL dection:\t",i, "\n")

w=sum(seq(from=1,to=1,length=length(jumpsDetectedRounded[17:sims]))*(jumpsDetectedRounded[17:sims]==0)*(actualJumps[17:sims]==1))
1-w/sum(actualJumps[17:sims])

y=sum(seq(from=1,to=1,length=length(leeTestJumps[17:sims]))*(leeTestJumps[17:sims]==0)*(actualJumps[17:sims]==1))
1-y/sum(actualJumps[17:sims])

#######################################
cat("Proability of FALSE dection:\t",i, "\n")


w=sum(seq(from=1,to=1,length=length(jumpsDetectedRounded[17:sims]))*(jumpsDetectedRounded[17:sims]>=1)*(actualJumps[17:sims]==0))
w/(sims-sum(actualJumps[17:sims]))

y=sum(seq(from=1,to=1,length=length(leeTestJumps[17:sims]))*(leeTestJumps[17:sims]==1)*(actualJumps[17:sims]==0))
y/(sims-sum(actualJumps[17:sims]))

