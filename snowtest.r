library(snow)

cl <- makeCluster(type = "MPI")

clusterCall(cl, function(x=2){x+1}, 5)
