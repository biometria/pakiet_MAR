evaluationCC<-function(CC, EC)
{
	require(cluster) # required packages should be defined in package I think!

	DD_index <- DD(CC, EC)	

	cat("\n","Evaluation of core collection","\n","\n")
	cat("1) dD%      ",DD,"\n","\n")
	
	MD_index <- MD(CC, EC)
	cat("2) MD%      ",MD,"\n","\n")

	
	VD_index <- VD(CC, EC)
	cat("3) VD%      ",VD,"\n","\n")

	RR_index <- RR(CC, EC)
	cat("4) RR%      ",RR,"\n","\n")


}
