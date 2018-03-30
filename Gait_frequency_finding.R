load("/home/xidwang/A-dong/data_env2.RData")

dim(new_sub1)

find_frequency=function(m)
{
  
  M=matrix(0,nrow(m),ncol(m))
  M[1,]=m[1,]

  j=1;count=c(1,rep(0,nrow(m)-1))
for(i in 2:nrow(m)){
  finding=apply(M,1,function(x) x==m[i,] )
  if(max(colSums(finding))==12)
  {count[which(colSums(finding)==12)]=count[which(colSums(finding)==12)]+1}
  else
  {
    j=j+1
    M[j,]=m[i,]
    count[j]=count[j]+1
  }
}
frequency=list(M,count)
return(frequency)
}

frequency_sub1=find_frequency(new_sub1)

save(frequency_sub1,file="gait_frequency")
