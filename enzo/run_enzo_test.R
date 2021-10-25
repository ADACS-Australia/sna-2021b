### === runing estimation
library(RSienaTest)
# load data and initial estimation
load("Enzo.RData")
# run sienaBayes for timing and end-to-end purposes
# set nmain to suit your timing needs but probably not less than 10
groupModel.ec <- sienaBayes(GroupsModel,
  data = FourGroups,
  effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
  priorKappa = 0.01,
  prevBayes = groupModel.e,
  nmain = 20, nrunMHBatches = 40,
  silentstart = FALSE,
  clusterType = 'MPI'
)
