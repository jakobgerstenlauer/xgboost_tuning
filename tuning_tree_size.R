############# regression tree ############
library(rpart)
set.seed(567)
N=10000
ran<-rnorm(N)
y<-ifelse(ran>0,"A","B")
x1<-ran-rnorm(N)
x2<-3*(ran+rnorm(N))
x3<-2*((ran-rnorm(N))-3)
training_data<-data.frame(y,x1,x2,x3)

########################################################################################################
# minsplit:	
# the minimum number of observations that must exist in a node in order for a split to be attempted.
# minbucket:	
# the minimum number of observations in any terminal <leaf> node. If only one of minbucket
#or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
########################################################################################################

rpart.ctrl <-rpart.control(cp=0.0001, xval=10, minsplit=max(N/10,50), minbucket = 20)
m1.rp <- rpart(y ~ ., method="class", data=training_data, control= rpart.ctrl)
printcp(m1.rp)
plotcp(m1.rp)

#retrieve the optimal value of cp:
index_min<-which.min(m1.rp$cptable[,4])
#standard deviation at minimum
std_min<-m1.rp$cptable[index_min,5]
cp_opt<-0.001
#If the second best cp value is within reach of the standard deviation replace cp at min with second best
if(index_min>1){
  if((min(m1.rp$cptable[,4]) + std_min) > m1.rp$cptable[index_min-1,1]){
    cp_opt<-m1.rp$cptable[index_min-1,1]
  }else{
    cp_opt<-m1.rp$cptable[index_min,1]
  }
}else{
  cp_opt<-m1.rp$cptable[1,1] 
}

m2.rp<-prune(m1.rp, cp = cp_opt)
plotcp(m2.rp)
printcp(m2.rp)
