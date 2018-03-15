load("/home/xidwang/A-dong/data_env.RData")

sub1_total=rbind(sub1_trflat_walk[1:55000,],sub1_trflat_run[1:29000,],sub1_trslope_walk[1:78000,],sub1_inflat_walk[1:22000,],sub1_inflat_run[1:25000,])
dim(sub1_total)
tree_sub1_total_LFX=hclust(dist(sub1_total[,1]),method="ward.D2")
tree_sub1_total_LFY=hclust(dist(sub1_total[,2]),method="ward.D2")
tree_sub1_total_LFZ=hclust(dist(sub1_total[,3]),method="ward.D2")
tree_sub1_total_RFX=hclust(dist(sub1_total[,4]),method="ward.D2")
tree_sub1_total_RFY=hclust(dist(sub1_total[,5]),method="ward.D2")
tree_sub1_total_RFZ=hclust(dist(sub1_total[,6]),method="ward.D2")
tree_sub1_total_WAX=hclust(dist(sub1_total[,7]),method="ward.D2")
tree_sub1_total_WAY=hclust(dist(sub1_total[,8]),method="ward.D2")
tree_sub1_total_WAZ=hclust(dist(sub1_total[,9]),method="ward.D2")
tree_sub1_total_WRX=hclust(dist(sub1_total[,10]),method="ward.D2")
tree_sub1_total_WRY=hclust(dist(sub1_total[,11]),method="ward.D2")
tree_sub1_total_WRZ=hclust(dist(sub1_total[,12]),method="ward.D2")

cut_sub1_total_LFX=cutree(tree_sub1_total_LFX,10)
cut_sub1_total_LFY=cutree(tree_sub1_total_LFY,10)
cut_sub1_total_LFZ=cutree(tree_sub1_total_LFZ,10)
cut_sub1_total_RFX=cutree(tree_sub1_total_RFX,10)
cut_sub1_total_RFY=cutree(tree_sub1_total_RFY,10)
cut_sub1_total_RFZ=cutree(tree_sub1_total_RFZ,10)
cut_sub1_total_WAX=cutree(tree_sub1_total_WAX,10)
cut_sub1_total_WAY=cutree(tree_sub1_total_WAY,10)
cut_sub1_total_WAZ=cutree(tree_sub1_total_WAZ,10)
cut_sub1_total_WRX=cutree(tree_sub1_total_WRX,10)
cut_sub1_total_WRY=cutree(tree_sub1_total_WRY,10)
cut_sub1_total_WRZ=cutree(tree_sub1_total_WRZ,10)

count_musk=function(v)  
{
  result=integer(0)
  for(k in 1:10)
  {result=c(result,length(v==k))}
  return(result)
}

new_sub1_musk=matrix(0,209,12*10)
for(i in 1:209)
{
  a=(i-1)*1000+1
  b=i*1000
  new_sub1_musk[i,]=c(count_musk(cut_sub1_total_LFX[a:b]),count_musk(cut_sub1_total_LFY[a:b]),count_musk(cut_sub1_total_LFZ[a:b]),
  count_musk(cut_sub1_total_RFX[a:b]),count_musk(cut_sub1_total_RFY[a:b]),count_musk(cut_sub1_total_RFZ[a:b]),
  count_musk(cut_sub1_total_WAX[a:b]),count_musk(cut_sub1_total_WAY[a:b]),count_musk(cut_sub1_total_WAZ[a:b]),
  count_musk(cut_sub1_total_WRX[a:b]),count_musk(cut_sub1_total_WRY[a:b]),count_musk(cut_sub1_total_WRZ[a:b]) )
}

save(new_sub1_musk,file="new_data_sub1")

