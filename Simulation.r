
simulateJump=function(mu_,ss_,lambda_,mu2_,sigma_,TotalTime,delta,compare,size)
{
  Sn=0
  times <- c(0)
  while(Sn <= TotalTime)
  {
    n <- length(times)
    u <- runif(1)
    expon <- -log(u)/lambda_
    Sn <- times[n]+expon
    times <- c(times, Sn)
  }
  times=times[-length(times)]                  #the last time is beyond TotalTime, so i delete from the vector times
  indicator=seq(from=0,to=0,length=TotalTime)  # if there are jumps or not between two times
  jumpSize=seq(from=0,to=0,length=TotalTime)   # stores the size of the jump
  
  delta=1/252
  t=(0:(length=(TotalTime)-1))
  for(m in 2:length(times))
  {
    for(k in 2: length(t))
    {
      if( t[k-1]<=times[m] && times[m]<=t[k])
      {
        indicator[k]=1
        
        if(compare)
        {
          u = runif(1,0,1)
          signA=1          
          if(u<=0.5)
          { signA=-1}
          
          jumpSize[k]=ss_*size*signA
        }
        else
        {
          jumpSize[k]=rnorm(1,mean=mu2_,sd=sigma_)
        }
      }
    }
  }
  
  stock=seq(from=0,to=0,length=TotalTime)
  stock[1]=10
  
  for(i in 2:TotalTime)
  {
    stock[i]=stock[i-1]+(mu_-0.5*ss_^2)*delta+ss_*sqrt(delta)*rnorm(1)+jumpSize[i]*indicator[i]
  }
  
  final=cbind(stock)
  return(final)
}


simulateGBM=function(mu_,ss_,TotalTime,delta)
{
  
  
  stock=seq(from=0,to=0,length=TotalTime)
  stock[1]=10
  
  for(i in 2:TotalTime)
  {
    stock[i]=stock[i-1]+(mu_-0.5*ss_^2)*delta+ss_*sqrt(delta)*rnorm(1)
  }
  
  indicator=stock*0
  final=cbind(stock)
  return(final)
}
