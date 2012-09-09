PRO <- function(groups, size, newSize){
	grpIDs <- unique(groups)
	newGrpSizes<-unlist(lapply(1:length(grpIDs), function(i, groups){
		newSize <- round(newSize*(sum(groups==i)/size))
		if(newSize == 0)
			newSize <- 1
		return(newSize)
	}, groups))
	return(newGrpSizes)
}
