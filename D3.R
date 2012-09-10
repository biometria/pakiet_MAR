D3 <- function(size, newSize, grpIDs, grpSize, groups, data){
	sumGrpDist <- unlist( lapply( 1: length(grpIDs), function(i, groups, grpIDs, grpSize, data){
		return( mean( daisy( data[groups==grpIDs[i],] ) )* log(grpSize[i]) * grpSize[i] )
	}, groups, grpIDs, grpSize, data)
	
	sumDist <- sum(sumGrpDist)

	newGrpSizes <- unlist( lapply( 1: length(grpIDs), function(i, newSize, sumGrpDist, sumDist){
		newGrpSize <- round(newSize * (sumGrpDist[i] / sumDist))
		if(newGrpSize == 0)
			newGrpSize <- 1
		return(newGrpSize)
	},  newSize, sumGrpDist, sumDist) )
	return(newGrpSizes)
}
