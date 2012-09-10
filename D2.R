D2 <- function(size, newSize, grpIDs, grpSize, groups, data){
	sumGrpDist <- unlist( lapply( 1:length(grpIds), function(i, grpIDs, grpSize, groups,data){
		return( mean( daisy(data[groups==grpIDs[i],]) * grpSize[i] )  )
	}, grpIDs, grpSize, groups, data )
	
	sumDist <- sum(sumGrpDist)
	
	newGrpSizes<- unlist (lapply( 1: length(grpIDs), function(i, grpIDs, groups, sumGrpDist, sumDist){
		newGrpSize <- round( newSize * (sumGrpDist[i] / sumDist) )
		if(newGrpSize == 0)
			newGrpSize <- 1
		return(newGrpSize)
	}, grpIDs, groups, sumGrpDist, sumDist) )
	return(newGrpSizes)
}
