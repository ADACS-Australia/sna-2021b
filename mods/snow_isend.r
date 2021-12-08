sendData.MPInode <- function(node, data) {
    Rmpi::mpi.isend.Robj(data, node$rank, node$SENDTAG, node$comm)
}
