MD<-function(CC,EC){
	tmp <- unlist(lapply(1:ncol(CC), function(i, CC, EC){
		return(colMeans(CC[i]) - colMeans(EC[i])) / colMeans(EC[i])
	}, CC, EC))
	MD_index <- (sum(tmp) / ncol(CC)) * 100
	return(MD_index)
}
