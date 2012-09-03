allocation<-function(data, groups, size=0.1, method="Pro"){
n<-nrow(data)*size
nj<-NULL
njn<-NULL
if (method=="Pro"){
cat("Proportional allocation method","\n")
a<-max(groups)
for (j in 1:a){
groups_a<-subset(data, groups==j) 
nj<-round(n*(nrow(groups_a)/nrow(data)))
if (nj==0) nj<-1
cat(paste(j, nj,"\n", sep=" "))
njn[j]<-nj
}
l<-1:a
njn<-cbind(l,njn, deparse.level=0)
cat(paste("Number of accessions in core colection:", sum(njn[,2]),"\n" ,sep=" "))
} else if (method=="Log"){
print("Logarytmic allocation method")
a<-max(groups)
sum_log_a<-NULL
for(j in 1:a){
groups_a<-subset(data, groups==j) 
sum_log_a[j]<-(log(nrow(groups_a))*nrow(groups_a))
sum_log<-sum(sum_log_a)}
for (j in 1:a){
groups_a<-subset(data, groups==j) 
nj<-round(n*(log(nrow(groups_a))*nrow(groups_a)/sum_log))
if (nj==0) nj<-1
cat(paste(j, nj,"\n", sep=" "))
njn[j]<-nj
}
l<-1:a
njn<-cbind(l,njn, deparse.level=0)
cat(paste("Number of accessions in core colection:", sum(njn[,2]),"\n" ,sep=" "))

} else if (method=="D2"){
cat("D2 allocation method", "\n")
library(cluster)
a<-max(groups)
sum_dist_a<-NULL
j<-NULL
for(j in 1:a){
groups_a<-subset(data, groups==j) 
sum_dist_a[j]<-(mean(daisy(groups_a))*nrow(groups_a))
sum_dist<-sum(sum_dist_a)}
j<-NULL
for (j in 1:a){
groupsb<-subset(data, groups==j) 
dist_j<-mean(daisy(groupsb))
nj<-round(n*((dist_j*nrow(groupsb))/(sum_dist)))
if (nj==0) nj<-1
cat(paste(j, nj,"\n", sep=" "))
njn[j]<-nj}
l<-1:a
njn<-cbind(l,njn, deparse.level=0)
cat(paste("Number of accessions in core colection:", sum(njn[,2]),"\n" ,sep=" "))

} else if (method=="D3"){
cat("D3 allocation method","\n")
library(cluster)
a<-max(groups)
sum_dist_a<-NULL
j<-NULL
for(j in 1:a){
groups_a<-subset(data, groups==j) 
sum_dist_a[j]<-(mean(daisy(groups_a))*log(nrow(groups_a))*nrow(groups_a))
sum_dist<-sum(sum_dist_a)}
j<-NULL
for (j in 1:a){
groupsb<-subset(data, groups==j) 
dist_j<-mean(daisy(groupsb))
nj<-round(n*((dist_j*nrow(groupsb)*log(nrow(groupsb)))/(sum_dist)))
if (nj==0) nj<-1
cat(paste(j, nj,"\n", sep=" "))
njn[j]<-nj}
l<-1:a
njn<-cbind(l,njn, deparse.level=0)
cat(paste("Number of accessions in core colection:", sum(njn[,2]),"\n" ,sep=" "))

} else print("Error: Bad name for method")
return(njn)
}