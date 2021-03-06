### === runing estimation
library(RSienaTest)
# load data and initial estimation
load('setups/Karen.RData.n10.m4')
# run sienaBayes for timing and end-to-end purposes
# set nmain to suit your timing needs but probably not less than 10
library(snow)

insertSource("../mods/snow_isend.r", package="snow")

groupModel.ec <- sienaBayes(GroupsModel, data = my.Karen,
                            effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
                            priorKappa = 0.01,
                            prevBayes = groupModel.e,
                            nmain=3, nrunMHBatches=40,
                            nbrNodes=3, silentstart=FALSE, clusterType="SOCK")
