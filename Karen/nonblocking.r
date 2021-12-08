library(snow)

sendData.MPInode <- function(node, data)
    Rmpi::mpi.isend.Robj(data, node$rank, node$SENDTAG, node$comm)


workLoop <- function(master) {
    repeat tryCatch({
        msg <- recvData(master)
	cat(paste("Type:", msg$type, "\n"))

        if (msg$type == "DONE") {
            closeNode(master)
            break;
        }
        else if (msg$type == "EXEC") {
            success <- TRUE
            ## This uses the message, rather than the exception since
            ## the exception class/methods may not be available on the
            ## master.
            handler <- function(e) {
                success <<- FALSE
                structure(conditionMessage(e),
                          class=c("snow-try-error","try-error"))
            }
            t1 <- proc.time()
            value <- tryCatch(docall(msg$data$fun, msg$data$args),
                              error = handler)
            t2 <- proc.time()
            value <- list(type = "VALUE", value = value, success = success,
                          time = t2 - t1, tag = msg$data$tag)
            msg <- NULL ## release for GC
            sendData(master, value)
            # Don't release buffer for nonblocking sends
            # value <- NULL ## release for GC
        }
    }, interrupt = function(e) NULL)
}
