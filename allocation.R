allocation<-function(data, groups, fraction=0.1, method="Pro"){
	
	#require(cluster)
	
	if(!any(method %in% c("Pro", "Log", "D2", "D3"))
		stop("Bad name for allocation method!")
	size <- nrow(data)
	newSize <- size*fraction
	
	grpIDs <- unique(groups)
	grpSize<-unlist(lapply(1:a, function(i, groups){sum(groups==i)}, groups))


	if (method=="Pro"){
		
		cat("Proportional allocation method\n")
		newGrpSizes<-PRO(groups, size, newSize, grpIDs)
		
	} else if (method=="Log"){
		
		cat("Logarytmic allocation method\n")
		newGrpSizes<-LOG(groups, size, newSize, grpIDs, grpSize)

	} else if (method=="D2"){
		
		cat("D2 allocation method\n")
		newGrpSizes <- D2(size, newSize, grpIDs, grpSize, groups, data)
		
	} else if (method=="D3"){
		
		cat("D3 allocation method\n")
		newGrpSizes <- D3(size, newSize, grpIDs, grpSize, groups, data)
	
	}
	
	results<-cbind(grpIDs,newGrpSizes, deparse.level=0)
	cat(paste("Number of accessions in core colection:", sum(results[,2]),"\n" ,sep=" "))
			
	return(results)
}
