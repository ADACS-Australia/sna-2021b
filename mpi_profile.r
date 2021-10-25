library(Rmpi)
library(snow)

if (mpi.comm.rank(0) > 0) {
    sys.load.image(".RData", TRUE)
    .First.sys()
    sink(file = "/dev/null")
    slaveLoop(makeMPImaster())
    mpi.quit()
} else {
    makeMPIcluster()
    .Last <<- function() {
        cl <- getMPIcluster()
        if (!is.null(cl)) {
              stopCluster(cl)
          }
        mpi.quit()
    }
}
