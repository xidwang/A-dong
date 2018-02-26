y_musk=labe
x_musk=scale_0_1(clean2[,-c(1,2,169)])   # 6598 by 166


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
  distance=matrix(0,nrow(x_musk),1)
  for(j in 1:max(y_musk))
  {distance=pmax(distance,abs(lkratio(M[,j]))) }
  return(distance)
}


heatmap(loglkratio_musk(1,10),distfun=as.dist,hclustfun=function(d) hclust(d,method = "ward.D2"),sym=TRUE)

?rainbow

