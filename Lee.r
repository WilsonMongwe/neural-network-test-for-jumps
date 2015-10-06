################################### Volatility estimation ########################


sigmaf=function(W,k)
{
  result=W*0
  
  for(i in (k-1):length(W))
  {
    result[i]=0
    for(j in  (i-k+4):(i-1))
    {
      result[i]=result[i]+abs((W[j]-W[j-1]))*abs((W[j-1]-W[j-2]))
    }
  }
  
  return (sqrt(result/(k-2)))
}


muF=function(W,k)
{
  result=W*0
  
  for(i in (k-1):length(W))
  {
    result[i]=0
    for(j in  (i-k+3):(i-1))
    {
      result[i]=result[i]+((W[j]-W[j-1]))
    }
  }
  
  return ((result/(k-1)))
}


################## Calculation ##########################


leeTest=function(X,k,delta)
{
  jumpTimes=matrix(0,nrow=1,ncol=length(X))
  delta= delta
  k=k
  S=X    
  sig=sigmaf(S,k) 
  drift=muF(S,k) 
  L=S*0
  n=length(S)
  c=sqrt(2/pi)
  
  c_n = (2*log(n))^0.5/c-(log(pi)+log(log(n)))/(2*c*(2*log(n))^0.5)
  s_n = 1/(c*(2*log(n))^0.5)
  
  alpha=0.05/(length(X)-k+1)
  critical= -log(-log(1-alpha))*s_n+c_n
  
  for(i in (k-1):length(S))
  {
    L[i]= (S[i]-S[i-1]-drift[i])/sig[i]
  }
  
  jumpTimes[1,k:length(jumpTimes)] = seq(from=1,to=1,length=length(L[k:length(jumpTimes)]))*
    (abs(L[k:length(jumpTimes)])>=critical)
  
  return(jumpTimes)
}
