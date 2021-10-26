library(RSienaTest)

load("G21.RData")

GroupsAlgo <- sienaAlgorithmCreate(projname = 'Andrea21Groups')

################################################################################
### Defining the model
################################################################################

# Construct the effects object; first without random effects.
GroupEffects <- getEffects(TwentyOne_Groups)
# Three ways to report what this is:
GroupEffects
print(GroupEffects, includeRandoms=TRUE)
print(GroupEffects, includeRandoms=TRUE, dropRates=TRUE)
# Note the randomEffects column.

# Construct a basic algorithm object.
GroupsAlgo <- sienaAlgorithmCreate(projname = 'Andrea21Groups')
# mult=5 is the default; here it is just a reminder.

# Construct the effects object.
GroupEffects <- getEffects(TwentyOne_Groups)
# Specify:
GroupEffects <- getEffects(TwentyOne_Groups)
GroupEffects <- setEffect(GroupEffects, density)
GroupEffects <- setEffect(GroupEffects, recip)
GroupEffects <- setEffect(GroupEffects, inPop)
GroupEffects <- setEffect(GroupEffects, outAct)
GroupEffects <- setEffect(GroupEffects, reciAct)
GroupEffects <- setEffect(GroupEffects, transTrip)
GroupEffects <- setEffect(GroupEffects, transRecTrip)
GroupEffects <- setEffect(GroupEffects, simX, interaction1="sex")
GroupEffects <- setEffect(GroupEffects, X, interaction1="oldties")
GroupEffects <- setEffect(GroupEffects, egoX, interaction1="delinq")
GroupEffects <- setEffect(GroupEffects, altX, interaction1="delinq")
GroupEffects <- setEffect(GroupEffects, egoXaltX, interaction1="delinq")
GroupEffects <- setEffect(GroupEffects, linear, name="delinq")
GroupEffects <- setEffect(GroupEffects, quad, name="delinq")
GroupEffects <- setEffect(GroupEffects, effFrom, name="delinq",
                       interaction1="sex")
GroupEffects <- setEffect(GroupEffects, avAlt, name="delinq",
                       interaction1="friends")
# Now define the main effect of the group variable nn.
# The main effect of a group-level variable is the egoX effect
# (test question 1: why not the altX effect?)
# (test question 2: and when should it be the effFrom effect?)
GroupEffects <- includeEffects(GroupEffects, egoX, name='friends', interaction1='nn')

################################################################################
### Specifying the random part
################################################################################

GroupEffects <- setEffect(GroupEffects, density, random=TRUE)
GroupEffects <- setEffect(GroupEffects, recip, random=TRUE)
GroupEffects <- setEffect(GroupEffects, inPop, random=TRUE)
GroupEffects <- setEffect(GroupEffects, outAct, random=TRUE)
GroupEffects <- setEffect(GroupEffects, reciAct, random=TRUE)
GroupEffects <- setEffect(GroupEffects, simX, interaction1="sex", random=TRUE)
GroupEffects <- setEffect(GroupEffects, egoX, interaction1="delinq", random=TRUE)
GroupEffects <- setEffect(GroupEffects, linear, name="delinq", random=TRUE)
GroupEffects <- setEffect(GroupEffects, quad, name="delinq", random=TRUE)


Mu <- rep(0,11)
Mu[2] <- -2  # outdegree
Mu[3] <- 1   # reciprocity
Mu[8] <- 0.4 # sex similarity

Sig <- matrix(0,11,11)
diag(Sig) <- 0.01

load("ans.RData")
print(ans)
groupModel.e <- sienaBayes(GroupsAlgo, data = TwentyOne_Groups,
				initgainGlobal=0.1, initgainGroupwise = 0.001,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
                prevAns = ans,
				nwarm=20, nmain=20, nrunMHBatches=40,
                clusterType = "MPI", silentstart=FALSE)
