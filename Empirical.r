data=read.table("clipboard",header=TRUE,fill=TRUE)
attach(data)
delta=1/252
n=length(data[1,])

testdata=matrix(0,nrow=n,ncol=8)


for(i in 1:n)
{
Stock=data[,i]
p=which(!is.na(Stock))
X=  log(Stock[p])
FirstMoment= mean(diff(X)^1)
SecondMoment= mean((diff(X)-FirstMoment)^2)
Skewness=mean((diff(X)-FirstMoment)^3)/SecondMoment^(3/2)
Kurtosis = (3-mean((diff(X)-FirstMoment)^4)/SecondMoment^2)/10
FirthMoment= mean((diff(X)-FirstMoment)^5)
SixthMoment= mean((diff(X)-FirstMoment)^6)
SeventhMoment= mean((diff(X)-FirstMoment)^7)
EigthMoment= mean((diff(X)-FirstMoment)^8)


testdata[i,]= cbind(FirstMoment,SecondMoment,Skewness,Kurtosis,
                    FirthMoment,SixthMoment,SeventhMoment,EigthMoment)
}



jumpTest.results <- compute(jumpTest, testdata[,1:8]) 
jumpsDetected=jumpTest.results$net.result
jumpsDetectedRounded=round(jumpsDetected)

write.csv(jumpsDetectedRounded,file="Jumps.csv",row.names=FALSE)


######################### Clusering of the stocks ###############################

x=testdata[,3:4]
#x=x[-c(96,180),]
fit <- kmeans(x, 5) # 5 cluster solution
# get cluster means 
aggregate(x,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(x, fit$cluster)

plot(mydata[,1],mydata[,3])

library(cluster)

clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0, main="Clusters of Stocks",xlab="Skweness",ylab="Kurtosis")


write.csv(fit$cluster,"C:/Users/om60270/Desktop/Jump Test Work/Nueral Network/cluster.CSV",row.names=FALSE)
