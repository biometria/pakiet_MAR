random_cc<-function(data, size=0.1){
cc<-NULL
n<-round(nrow(data)*size)
a<- sample(1:nrow(data), n)
L<-c(1:nrow(data))
a2<-data.frame(L,dane)
cc<-data.frame(a2[L %in% a,  ])
cc<-subset(cc,select=-L)
return(cc)
}
  