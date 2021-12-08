mpi.send.Robj <- function(obj, dest, tag, comm=1){
    mpi.send(x=serialize(obj, NULL), type=4, dest=dest, tag=tag, comm=comm)
}
