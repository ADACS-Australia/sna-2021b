# Parse script args
parser = argparse::ArgumentParser()
parser$add_argument("file", nargs = 1, help = "Setup file to run sienaBayes on")
args = parser$parse_args()

# Load setup
load(args$file)

# Add source mods here
# e.g.
# insertSource("../snow_mod.r")

clusterExport.mpi.fast <- local({
    env <- as.environment(1) ## .GlobalEnv
    gets <- function(n, v) { assign(n, v, envir = env); NULL }
    function(cl, list, envir = .GlobalEnv) {
        ## do this with only one clusterCall--loop on workers?
        for (name in list) {
            clusterCall.mpi.fast(cl, gets, name, get(name, envir = envir))
        }
    }
})

clusterCall.mpi.fast <- function(cl, fun, ...) {
    checkCluster(cl)

    # create packet and serialise
    value = list(fun = fun, args = list(...), return = TRUE, tag = NULL)
    data_ = list(type = "EXEC", data = value, tag = NULL)
    data = serialize(data_, NULL)

    # send packet to all workers
    for (i in seq(along = cl)) {
        node = cl[[i]]
        Rmpi::mpi.isend(x=data, type=4, dest=node$rank, tag=node$SENDTAG, comm=node$comm)
    }

    # recieve results from workers and return
    checkForRemoteErrors(lapply(cl, recvResult))
}


# Run sienaBayes
groupModel.ec = RSienaTest::sienaBayes(GroupsModel,
  data = my.Karen,
  effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
  priorKappa = 0.01,
  prevBayes = groupModel.e,
  nmain = 3, nrunMHBatches = 40,
  silentstart = FALSE, clusterType = "MPI"
)

# Create testfile name
N <- max(Rmpi::mpi.comm.size(0) - 1, 1)
testfile = stringr::str_replace(basename(args$file),"Karen.RData","test.reference")
testfile = paste("test_references/",testfile,".mpi",N,sep="")

# Compare with testfile
testthat::expect_known_value(
  groupModel.ec$theta,
  testfile,
  update = FALSE,
  info = NULL,
  label = NULL,
  version = 3,
  # tolerance = 1.e-3
)
