library(snow)

sendData.MPInode <- function(node, data)
    Rmpi::mpi.isend.Robj(data, node$rank, node$SENDTAG, node$comm)

staticClusterApply <- function(cl, fun, n, argfun) {
    checkCluster(cl)
    p <- length(cl)
    if (n > 0 && p > 0) {
        val <- vector("list", n)
        start <- 1
        while (start <= n) {
            end <- min(n, start + p - 1)
            jobs <- end - start + 1
            for (i in 1:jobs)
                sendCall(cl[[i]], fun, argfun(start + i - 1))
                # val[i] <- snow::recvResult(cl[1:jobs])
            for (i in 1:jobs)
                val[i] <- lapply(cl[i], recvResult)
            start <- start + jobs
        }
        checkForRemoteErrors(val)
    }
}
