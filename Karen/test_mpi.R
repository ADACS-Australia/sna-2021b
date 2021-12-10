# Parse script args
parser = argparse::ArgumentParser()
parser$add_argument("file", nargs = 1, help = "Setup file to run sienaBayes on")
args = parser$parse_args()

# Load setup
load(args$file)

# Add source mods here
# e.g.
# insertSource("../snow_mod.r")

clusterExportPickle <- local({
    env <- as.environment(1) ## .GlobalEnv
    gets <- function(n, v) { assign(n, v, envir = env); NULL }
    function(cl, list, envir = .GlobalEnv) {
        ## do this with only one clusterCall--loop on workers?
        for (name in list) {
            print(paste("SENDING:",name))
            clusterCallPickle(cl, gets, name, get(name, envir = envir))
        }
    }
})

clusterCallPickle <- function(cl, fun, ...) {
    checkCluster(cl)

    print("Serializing")
    # serialise here
    value = list(fun = fun, args = list(...), return = TRUE, tag = NULL)
    data = list(type = "EXEC", data = value, tag = NULL)
    print("----DATA----")
    print(data)
    print("----DATA----")
    data = serialize(data, NULL)
    print("Serialized")

    # then send to all workers
    for (i in seq(along = cl))
        print(paste("Sending to",i))
        node = cl[[i]]
        Rmpi::mpi.send(data, type=4, dest=node$rank, tag=node$SENDTAG, comm=node$comm)

    print("Finished sending")

    # recieve results from workers and return
    checkForRemoteErrors(lapply(cl, recvResult))

    print("Received results")
}

x = 1

cl <- snow::makeCluster(
          type = "MPI",
          outfile = "cluster.out"
        )

clusterExportPickle(cl, list("x"), envir=environment())

print("FINISHED")


# # Run sienaBayes
# groupModel.ec = RSienaTest::sienaBayes(GroupsModel,
#   data = my.Karen,
#   effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
#   priorKappa = 0.01,
#   prevBayes = groupModel.e,
#   nmain = 3, nrunMHBatches = 40,
#   silentstart = FALSE, clusterType = "MPI"
# )

# # Create testfile name
# N <- max(Rmpi::mpi.comm.size(0) - 1, 1)
# testfile = stringr::str_replace(basename(args$file),"Karen.RData","test.reference")
# testfile = paste("test_references/",testfile,".mpi",N,sep="")

# # Compare with testfile
# testthat::expect_known_value(
#   groupModel.ec$theta,
#   testfile,
#   update = FALSE,
#   info = NULL,
#   label = NULL,
#   version = 3,
#   # tolerance = 1.e-3
# )
