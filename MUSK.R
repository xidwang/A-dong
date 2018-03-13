load("/home/xidwang/A-dong/data_env.RData")

y_musk=labe
x_musk=scale_0_1(clean2[,-c(1,2,169)])   # 6598 by 166

lkratio2=function(v)
{
  l=length(v)
  MM=matrix(rep(v,l),l,l)
  dist=log(sweep(MM,2,v,"/"))
  dist[is.infinite(dist)]=10
  dist[is.na(dist)]=0
  return(dist)
}

loglkratio_musk=function(v,N)
{
  count_musk=function(v)
  { 
    result=integer()
    for(i in 1:max(y_musk))
    { result=c(result,length(which(v==i))/length(which(y_musk==i)) ) }
    return(result)    
  }
  steel_tree=hclust(dist(x_musk[,v]),method="ward.D2", members = NULL)
  steel_cut=cutree(steel_tree,N)
  # M is f(x) matrix  each column: fb(x),fg(x) 2 lables total
  M=matrix(rep(0,nrow(x_musk)*max(y_musk)),nrow(x_musk),max(y_musk))
  for(j in 1:N)
  {
    M[which(steel_cut==j),] =t(matrix(rep( count_musk(y_musk[which(steel_cut==j)]) ,table(steel_cut)[j]),max(y_musk),table(steel_cut)[j]))
  }
  distance=matrix(0,nrow(x_musk),nrow(x_musk))
  for(j in 1:max(y_musk))
  {distance=pmax(distance,abs(lkratio2(M[,j]))) }   # lkratio2
  return(distance)
}

#final_distance=loglkratio_musk(1,10)
#save(final_distance,file="MUSK_distance")


Q=100
asso_musk=matrix(0,166,166)
for(i in 1:166)
{asso_musk[i,]=apply(x_musk,2, function(x) iono_association(x_musk[,i],x))}

save(asso_musk,file="MUSK_asso")
