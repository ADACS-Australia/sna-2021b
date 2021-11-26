fun <- function(x) {
  for (i in c(1:10000))
  {
    x <- (x*x)/(x^i)
  }
  return(x)
}

library("parallel")

cluster <- makeCluster(4, type="FORK")

for (i in c(1:10))
{
  x = clusterApply(cluster, 1:100, fun)
}

stopCluster(cluster)