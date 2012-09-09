allocation<-function(data, groups, fraction=0.1, method="Pro"){
	
	require(cluster)
	
	if(!any(method %in% c("Pro", "Log", "D2", "D3"))
		stop("Bad name for allocation method!")
	size <- nrow(data)
	newSize <- size*fraction #n
	grpIDs <- unique(groups)
	
	grpIDs <- unique(groups)
	grpSize<-unlist(lapply(1:a, function(i, groups){sum(groups==i)}, groups))


	if (method=="Pro"){
		
		cat("Proportional allocation method\n") #method that not require cluster package
		newGrpSizes<-PRO(groups, size, newSize, grpIDs)
		
	} else if (method=="Log"){
		
		cat("Logarytmic allocation method\n")
		newGrpSizes<-LOG(groups, size, newSize, grpIDs, grpSize)


	} else if (method=="D2"){
		cat("D2 allocation method\n")
		group_a<- groupSize[j]
		sum_dist_a<-NULL
		j<-NULL
		for(j in 1:a){
			
			sum_dist_a[j]<-(mean(daisy(groups_a))*nrow(groups_a))
			sum_dist<-sum(sum_dist_a)}
			j<-NULL
			for (j in 1:a){
				groupsb<-subset(data, groups==j) 
				dist_j<-mean(daisy(groupsb))
				nj<-round(n*((dist_j*nrow(groupsb))/(sum_dist)))
				if (nj==0) nj<-1
					cat(paste(j, nj,"\n", sep=" "))
				njn[j]<-nj
			}
	

	} else if (method=="D3"){
		cat("D3 allocation method\n")
		group_a<- groupSize[j]
		sum_dist_a<-NULL
		j<-NULL
		for(j in 1:a){
			
			sum_dist_a[j]<-(mean(daisy(groups_a))*log(nrow(groups_a))*nrow(groups_a))
			sum_dist<-sum(sum_dist_a)}
			j<-NULL
			for (j in 1:a){
				groupsb<-subset(data, groups==j) 
				dist_j<-mean(daisy(groupsb))
				nj<-round(n*((dist_j*nrow(groupsb)*log(nrow(groupsb)))/(sum_dist)))
				if (nj==0) nj<-1
					cat(paste(j, nj,"\n", sep=" "))
				njn[j]<-nj
			}
			
	}
	
	l<-1:a
	njn<-cbind(l,njn, deparse.level=0)
	cat(paste("Number of accessions in core colection:", sum(njn[,2]),"\n" ,sep=" "))
			
	return(njn)
}
