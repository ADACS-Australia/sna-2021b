

sendData.SOCKnode <- function(node, data) {
  print("Im in sock")
  start <- Sys.time()
  r <- serialize(data, node$con)
  end <- Sys.time()
  diff <- end - start
  print(diff)
  r
}
