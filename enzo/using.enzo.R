## == script: using.enzo.r
### === the function create.enzo creates data and an initial estimation
create.enzo(nbrNodes=1,nmain=20,nwarm=20,seed=123)
### === arguments
# 1. set nbrNodes to the minimum of your number of cores and 6
# 2. set nmain to something in the range 20 to 100
# 3. set nwarm to something in the range 20 to 200
# 4. use your choice of random seed - this random seed will be passed to subsequent estimations

### NB: you only have to create data once


### === runing estimation
# load data and initial estimation
load('Enzo.RData')
# run sienaBayes for timing and end-to-end purposes
# set nmain to suit your timing needs but probably not less than 10
groupModel.ec <- sienaBayes(GroupsModel, data = FourGroups,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
				prevBayes = groupModel.e,
				nmain=20, nrunMHBatches=40,
                nbrNodes=1, silentstart=FALSE)
                
                
                