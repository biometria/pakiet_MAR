evaluationCC<-function(CC, EC){
library(cluster)
dist_ec<-mean(daisy(EC))
dist_cc<-mean(daisy(CC))
DD <- (abs(dist_ec-dist_cc)/dist_ec)*100
if (ncol(CC)!=ncol(EC)) Cat("Erorr: Diferent numbers of traits")
cat("\n","Evaluation of core collection","\n","\n")
cat("1) dD%      ",DD,"\n","\n")
MD_b<-NULL
j<-1
for(i in 2:ncol(CC)){
MD_a=(colMeans(CC[i])-colMeans(EC[i]))/colMeans(EC[i])
MD_b[j]<-MD_a
j<-j+1
}
MD<-(sum(MD_b)/(ncol(CC)-1))*100
cat("2) MD%      ",MD,"\n","\n")
j<-1
i<-NULL
VD_b<-NULL
for(i in 2:ncol(CC)){
VD_a=(var(CC[i])-var(EC[i]))/var(EC[i])
VD_b[j]<-VD_a
j<-j+1
}
VD<-(sum(VD_b)/(ncol(CC)-1))*100
cat("3) VD%      ",VD,"\n","\n")
j<-1
i<-NULL
RR_b<-NULL
for(i in 2:ncol(CC)){
RR_a=((max(CC[i])-min(CC[i]))-(max(EC[i])-min(EC[i])))/(max(EC[i])-min(EC[i]))
RR_b[j]<-RR_a
j<-j+1
}
RR<-(sum(RR_b)/(ncol(CC)-1))*100
cat("4) RR%      ",RR,"\n","\n")


}