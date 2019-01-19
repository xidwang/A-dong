set.seed(100)
train_index=sample(nrow(x_fastball),0.7*nrow(x_fastball),replace = FALSE)
x_fastball_train=x_fastball[train_index,]
x_fastball_valid=x_fastball[-train_index,]
y_fastball_train=y_fastball[train_index]
y_fastball_valid=y_fastball[-train_index]
color_fastball_train=color_fastball[train_index]
color_fastball_valid=color_fastball[-train_index]


# given training and validation dataset
singleton_prediction_pos_neg=function(pitcher_label,sample_index,Ncluster)
{
  new_sample_singleton0=x_fastball_valid[which(y_fastball_valid==levels(y_fastball)[pitcher_label])[sample_index],]
  new_sample_singleton=as.data.frame(matrix(rep(as.matrix(new_sample_singleton0),900),900,21,byrow=TRUE))
  colnames(new_sample_singleton)=colnames(new_sample_singleton0)
  #Ncluster=30
  count_label=rep(0,14)
  for(i in 1:91){
    a=combinatorics2[,i]
    l=levels(y_fastball)[a]
    index=which(y_fastball_train == l[1] | y_fastball_train ==  l[2] )
    x_3pitcher_scaled=scale_0_1( rbind(x_fastball_train[index,] , new_sample_singleton)  )
    #y_3pitcher=y_fastball_train[index]
    color_3pitcher=c( color_fastball_train[index],rep("white",nrow(new_sample_singleton) ) )
    # cut into white heatmap into N cluster
    prediction_cut=cutree( hclust(dist( as.matrix(x_3pitcher_scaled[,G1]) ),method = "ward.D2"), Ncluster )
    ClustNo=prediction_cut[length(prediction_cut)]
    
    pos_color=-1   # initiate pos_color
    # if entropy is larger than 0.01, then count the mixed color
    if( H(color_3pitcher[which(prediction_cut==ClustNo)]) > 0.02){
      TAB=table(color_3pitcher[which(prediction_cut==ClustNo)])
      for( x in names(TAB) ){     
        # use marjority rule
        # chose the largest number of non-white color
        if( x!="white" & TAB[x] == max(TAB[-which(names(TAB)=="white")]) )
        { 
          count_label[which(cols==x)]= count_label[which(cols==x)] + 1 
          pos_color=x   # record the positive color's name
        }
      }
      
    }
    
    # find the non-white cluster and the colors in there
    neg_set=names(table( color_3pitcher[which(prediction_cut!=ClustNo)] ))
    # make a negetive label for colors except
    for( q in neg_set[!pos_color==neg_set] ){
      count_label[which(cols==q)] = count_label[which(cols==q)] - 1
    }
  }
  return (count_label)
}




pitcher_label=6
test_size=400
result_matrix=matrix(0,test_size,14)
m= length(which(y_fastball_valid==levels(y_fastball)[pitcher_label]))
new_sample_index=sample(1:m,test_size)
for( i in 1:length(new_sample_index) )
{result_matrix[i,]=singleton_prediction_pos_neg( pitcher_label , new_sample_index[i] , 20 )}
predict_pitcher6=table(apply(result_matrix,1,which.max))

write.csv(result_matrix, file = "/Users/wangxiaodong/Desktop/predict_pitcher6.csv")

