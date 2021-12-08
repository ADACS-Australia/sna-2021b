# Parse script args
parser = argparse::ArgumentParser()
parser$add_argument("file", nargs = 1, help = "Setup file to run sienaBayes on")
args = parser$parse_args()

# Load setup
load(args$file)

# Add source mods here
# e.g.
# insertSource("../snow_mod.r")

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
