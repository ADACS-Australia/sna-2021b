################################################################################
###
### ---- RscriptsienaBayes.r: a script for introduction to RSienaTest ----------
###
###                         version November 26, 2020
################################################################################
#
# This is a script for demonstrating network multilevel modeling
# using sienaBayes in RSienaTest.
# Written by Tom Snijders.
#

library(RSienaTest)

################################################################################
### Getting the data
################################################################################

# The data used is a small part of the data collected by Andrea Knecht
# for her dissertation
# Knecht, A., 2008. Friendship Selection and Friends' Influence.
# Dynamics of Networks and Actor Attributes in Early Adolescence.
# PhD dissertation, University of Utrecht.
# Various articles were published about this, e.g.,
# Andrea Knecht, Tom A. B. Snijders, Chris Baerveldt, Christian E. G. Steglich,
# and Werner Raub (2010. Friendship and Delinquency: Selection and Influence
# Processes in Early Adolescence, Social Development, 19, 494-514.
# http://dx.doi.org/10.1111/j.1467-9507.2009.00564.x.
# The selection made here is the set of 21 classrooms used in
# chapter 2 of the dissertation.

# Define data sets:
load("G21.RData")
#  http://www.stats.ox.ac.uk/~snijders/siena/G21.RData

# What is this?
ls()
# Aha. Important for us:
TwentyOne_Groups
class(TwentyOne_Groups)
length(TwentyOne_Groups)
# This is a sienaGroup object, created by
?sienaGroupCreate
# The first of the 21:
TwentyOne_Groups[[1]]

# The covariate nn is log(group size - 1), a group-level covariate
group_n <- sapply(TwentyOne_Groups, function(x){length(x$nodeSets[[1]])})
group_n

# Get the initial description
print01Report(TwentyOne_Groups, modelname = 'Andrea_21Groups')

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
print(GroupEffects, includeRandoms=TRUE, dropRates=TRUE)


################################################################################
### First estimate the multi-group model
################################################################################
ans <- siena07(GroupsAlgo, data = TwentyOne_Groups,
				effects = GroupEffects,
                useCluster=TRUE, nbrNodes=8)
ans
# nn has very precisely a parameter estimate of -0.5!
tt.ans <- sienaTimeTest(ans)
# The warning does not matter.
summary(tt.ans)
# We look at the effect-wise joint significance tests
# for the between-group variabilty of effects.
# The quickest summary is by mentioning the non-significant
# variabilities:
# oldties, egoXaltX for delinq, lin, avAlt, effect sex on delinq.
# In addition to the p-values, also the chi-square values
# give information about the variability.
# The highest chi-squared values (larger than 50) are for
# recip, inPop, outAct, transTrip, reciAct, delinq ego, simX sex.

# we shall give these, and some basic others, random effects.

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
print(GroupEffects, includeRandoms=TRUE, dropRates=TRUE)

# Note, added later:
# evidently, transTrip wasforgottein in this exercise.

################################################################################
### Estimating
################################################################################

# Now we are going to apply
 ?sienaBayes
# Prior dimensions should be 11, as shown by the print(GroupEffects, ...
# For the rates we have to give values, but they do not matter
# if we use the default priorRatesFromData=2
# (they just have to be there).
# Argued prior based on experience
# The ordering of the random effects can be seen from the
# print of the effects object (see above),
# but rate parameters are also among the random effects,
# and are not mentioned if dropRates=TRUE is used!
Mu <- rep(0,11)
Mu[2] <- -2  # outdegree
Mu[3] <- 1   # reciprocity
Mu[8] <- 0.4 # sex similarity
Mu

Sig <- matrix(0,11,11)
diag(Sig) <- 0.01
Sig

# Save what we have now (I later added #,
# not to accidentally save something again later under the same name).
#save.image("Group_c.RData")

# These prior values are not hewn in stone at all,
# because Bayesian procedures are subjectivistic.
# I think these values are reasonable;
# but more differentiation would be possible.
# It will also be reasonable to try other values,
# and check whether the differences in results are large.

groupModel.e <- sienaBayes(GroupsAlgo, data = TwentyOne_Groups,
				initgainGlobal=0.1, initgainGroupwise = 0.001,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
				prevAns = ans,
				nwarm=200, nmain=1000, nrunMHBatches=40,
                nbrNodes=7, silentstart=FALSE)
save(groupModel.e, file="groupModele_01.RData")

# If we find this was not long enough, we can prolong:

groupModel.ec <- sienaBayes(GroupsAlgo, data = TwentyOne_Groups,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
				prevBayes = groupModel.e,
				nmain=2000, nrunMHBatches=40,
                nbrNodes=7, silentstart=FALSE)
save(groupModel.ec, file="groupModelee_01.RData")

# combine it with the earlier chain:
eee <- glueBayes(groupModel.e, groupModel.ec)

################################################################################
### Studying the results
################################################################################

# Look at the first set of results:
groupModel.e
# A lot of information. You get even more information with
summary(groupModel.e)
# which also shows groupwise results.
# We can also look at the combined results
eee
summary(eee)

# The posterior groupwise means can be extracted as follows.
(pmean <- extract.posteriorMeans(eee))
# See
#
?extract.posteriorMeans
# for further options, in particular, to get posterior standard deviations.

# Make a multidimensional scaling plot of the posterior means
# of the groupwise parameters
plotPostMeansMDS(eee, pmonly=1)
# This is useful for spotting outliers.
# In this case, it is a nice scatter.
# Note (see manual): strong outliers can be real deviating groups,
# but they can also be random fluctuations
# due to having too many random parameters given the number of groups.
# Practical advice: if there are really strong outliers,
# then first try reducing the number of random parameters;
# perhaps the outliers disappear.

# Download the file  http://www.stats.ox.ac.uk/~snijders/siena/BayesPlots.r
# and then
source("BayesPlots.r")
# or directly use
# source("https://www.stats.ox.ac.uk/~snijders/siena/BayesPlots.r")
# Read the start of this file, which explains what it is meant to do.
# It is not foolproof (well, nothing of this is foolproof).
# These functions produce plots in your working directory.
# If you wish not to clutter the directory you can go to a different directory
# for the plots
# setwd("C:\\Users\\.....\\Plots") # fill in and modify as desired
# It must be an existing directory.
# On my machine I did
setwd("C:/Users/Tom.Snijders/Documents/Siena/workshops/sienaBayes2019/Plots")

# You can make some plots with the following function calls
GlobalRateParameterPlots(eee)
GlobalNonRateParameterPlots(eee, setOfEffects = 2:8, title="enet_base")
GlobalNonRateParameterPlots(eee, setOfEffects = 10:12, title="enet_del")
GlobalNonRateParameterPlots(eee, setOfEffects = c(9,13,14),
										title="enet_others")
GlobalNonRateParameterPlots(eee, setOfEffects = 16:19, title="edel")

# Study the arguments of these function calls and look at the plots produced,
# to understand why these calls produce those plots.
# Of course the names are to be chosen by you, as well as the combinations
# of effects for each plot.
# The numbering is as in the print of groupModel.e,
# without duplication of the rate parameters.
# For the numbering, you can also use this non-exported function in RSienaTest:
RSienaTest:::getNames(groupModel.e)

# For some of the parameters, such as the estimates for nn 
# (see enet_others_postEta), delinquency ego  
# (see enet_del_postMu and enet_del_postSD) and for the rate parameters
# (see eee_postMuRate and eee_postSDRate), seem to converge rather late;
# they are more stable after about run 1300.

# The density plots are given here only for the first 8 groups,
# just to keep the plots readable.
AllDensityPlots(groupModel.e, basetitle='groupModel18', groups=1:8,
						legendpos="topright")
# The 'average theta' curve refers to
# the average for all groups in this data set;
# The 'mu' curve refers to the estimation of the population mean parameter.

# If a look at the trace plots suggested that convergence took place not
# after 200 but, e.g., after about 500 runs, it would be better to request
print(groupModel.e, nfirst=500)
# You can also use
(sbr.c <- shortBayesResults(groupModel.e, nfirst=500))
# which produces a data frame, not viewed so nicely,
# but which may be useful for further use. See
?shortBayesResults

################################################################################
### Continuing estimation
################################################################################

# Another option than prolonging the first run is to make several runs.
# We will now calculate 5 further similar runs, to get a better estimate,
# and be better able to assess convergence.
# First, this is done as if the prolonged run did not exist,
# to demonstrate this alternative option.

# In the following for-loop,
# the estimation results groupModel.e are first saved and then overwritten;
# the first is done to protect against loss in case of crashes,
# the second is done to avoid problems of using too much memory.
# Our algorithm object GroupsAlgo does not specify a random number seed;
# therefore the runs will all be independent.
# An alternative would be to define the random number seed for the algorithm
# within the for-loop, different for each i.

# When you follow this whole sequence of model fits,
# it is good to know already now that the following 5 runs turn out 
# to be less good than the 5 runs after that; 
# therefore, these may be skipped. 
# They are mentioned here just for comparison purposes.

for (i in 2:5)
{
  groupModel.e <- sienaBayes(GroupsAlgo, data = TwentyOne_Groups,
				initgainGlobal=0.1, initgainGroupwise = 0.001,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
				prevAns = ans,
				nwarm=200, nmain=1000, nrunMHBatches=40,
                nbrNodes=7, silentstart=FALSE)
  if (i < 10){filename <- paste("groupModele_0",i,".RData", sep="")} else {
	filename <- paste("groupModele_",i,".RData", sep="")}
  save(groupModel.e, file=filename)
}

# Second, we make five continuations with as starting point
# the end of groupModel.ec, because the plots for eee
# (of which groupModel.ec is the second part) seemed to converge
# some time about run 1300.
# We use newProposalFromPrev = FALSE (see the help page)
# because we just want to continue from that point,
# although with different random number seeds.

# Note the difference with above:
# in the four additional runs above, groupModel.e is constructed
# just using the initialization prevAns=ans;
# in the five additional runs below, groupModel.ecx is constructed
# while using prevBayes=groupModel.ec, newProposalFromPrev=FALSE,
# which will turn out to produce better results.

for (i in 1:5)
{
  groupModel.ecx <- sienaBayes(GroupsAlgo, data = TwentyOne_Groups,
				initgainGlobal=0.1, initgainGroupwise = 0.001,
                effects = GroupEffects, priorMu = Mu, priorSigma = Sig,
				priorKappa = 0.01,
				prevBayes = groupModel.ec,
				newProposalFromPrev = FALSE,
				nmain=1000, nrunMHBatches=40,
                nbrNodes=7, silentstart=FALSE)
  if (i < 10){filename <- paste("groupModelec_0",i,".RData", sep="")} else {
	filename <- paste("groupModelec_",i,".RData", sep="")}
  save(groupModel.ecx, file=filename)
}

################################################################################
### Assessing convergence
################################################################################

# Now check whether the five results are similar and long enough.
# This uses package stan, see
# Gelman, Andrew, Carlin, John B., Stern, Hal S., and Rubin, Donald B. 2014.
# Bayesian Data Analysis. 3nd edn. Boca Raton, FL: Chapman & Hall / CRC
# The package is called after Stanislav Ulam. Wikiquotes has nice quotes from him.
# If necessary: install.packages("rstan")
library(rstan)
?monitor

# Since the objects were overwritten and saved, we have to load them again.

load("groupModelec_01.RData")
grc1 <- groupModel.ecx
load("groupModelec_02.RData")
grc2 <- groupModel.ecx
load("groupModelec_03.RData")
grc3 <- groupModel.ecx
load("groupModelec_04.RData")
grc4 <- groupModel.ecx
load("groupModelec_05.RData")
grc5 <- groupModel.ecx

# Combine them in a list.
zlist5 <- list(grc1, grc2, grc3, grc4, grc5)
# Extract the samples from the posterior.
?extract.sienaBayes
# Just to demonstrate, a variety of selections is extracted.
xvl <- extract.sienaBayes(zlist5, extracted="varying", nfirst=1)
xv <- extract.sienaBayes(zlist5, extracted="varying", sdLog=FALSE, nfirst=1)
xnv <- extract.sienaBayes(zlist5, extracted="non-varying", nfirst=1)
xo <- extract.sienaBayes(zlist5, extracted="objective", nfirst=1)
# nfirst=1 is used, because burn-in was already done in groupModel.ec
# because of the use of prevBayes=groupModel.ec.

# See RSiena manual Section 11.3.7:
# Rules of thumb given in Gelman et al. (2014, p. 287) are that,
# for all parameters of interest, Rhat <= 1.1 and n_eff >= 5m,
# where m is the number of chains; here m=5.

# See RSiena manual Section 11.3.7:
# Rules of thumb given in Gelman et al. (2014, p. 287) are that,
# for all parameters of interest, Rhat <= 1.1 and n_eff >= 5m,
# where m is the number of chains; here m=5.

mvl <- monitor(xvl, warmup=200, digits_summary=3)
mv <- monitor(xv, warmup=200, digits_summary=3)
# Whether or not the log of posterior standard deviations is taken,
# makes little difference for convergence.
mnv <- monitor(xnv, warmup=200, digits_summary=3)
mo <- monitor(xo, warmup=200, digits_summary=3)
mo <- monitor(xo, warmup=0, digits_summary=3)
# It seems the <<extracted="objective">> does not work properly:
# there still are some rate parameters here.

# Convergence is excellent.

################################################################################
### Evaluating results
################################################################################


# Now we can glue the results together to obtain the combined results.
grc <- glueBayes(grc1,grc2)
grc <- glueBayes(grc,grc3)
grc <- glueBayes(grc,grc4)
grc <- glueBayes(grc,grc5)

# Make some plots as follows
plotPostMeansMDS(grc)
# and using BayesPlots.r as above:
GlobalRateParameterPlots(grc)
GlobalNonRateParameterPlots(grc, setOfEffects = 2:8, title="grcnet_base")
GlobalNonRateParameterPlots(grc, setOfEffects = 10:13, title="grcnet_del")
GlobalNonRateParameterPlots(grc, setOfEffects = c(9,14,15),
										title="grcnet_others")
GlobalNonRateParameterPlots(grc, setOfEffects = 17:20, title="grcdel")
AllDensityPlots(grc, basetitle='grc', legendpos="topright")
# To see only the figures for Mu and average theta:
AllDensityPlots(grc, basetitle='gr0', legendpos="topright", groups=NULL)

# You can get parameter estimates from
grc
# or more extensively
summary(grc)

# The influence effect of friends on delinquency, avAlt, is medium strong,
# and some (about 12%) of the posterior probability for avAlt is less than 0.
# The 95% credibility interval is from -0.12 to 0.46;
# depending on Monte Carlo estimation randomness,
# the 2nd decimal is not totally reliable.

################################################################################
### Assessing convergence for the earlier five parallel runs
################################################################################

# For the earlier five parallel runs, stored in 
# "groupModele_01.RData" to "groupModele_05.RData", convergence is less good.


################################################################################
### How different are the results of  
### the combination of groupModel.e and groupModel.ec
### from the results of  the combination of groupModel.ecx?
################################################################################

# For this, we can use the function 
?shortBayesResults
s.grc <- shortBayesResults(grc) 
s.eee <- shortBayesResults(eee) 
# This produces two data frames, the columns are
names(s.grc)
# We take out only the end points of the credibility intervals
s.12 <- cbind(s.grc[,c("cFrom","cTo")], s.eee[,c("cFrom","cTo")])
s112 <- round(s.12[,c(1,3,2,4)],3)
s012 <- s.grc[,'effectName']
cbind(s012, s112)
# Comparing the columns shows that the differences are not important.

################################################################################
### Studying the effect of delinquency on friendship
################################################################################

# The selection tables explained in Section 13.3 of the RSiena manual can be used
# can be used to get plots of the effect of delinquency on friendship.
# These can be produced by the functions in
#  source("http://www.stats.ox.ac.uk/~snijders/siena/SelectionTables.r")
# Download this file and read its beginning.
source("SelectionTables.r")
# The selection table is obtained by requesting
selectionMatrix(grc,TwentyOne_Groups,"friends", "delinq",levls=0:5)
# The last decimals are unreliable, it is better to present
round(selectionMatrix(grc,TwentyOne_Groups,"friends", "delinq",levls=0:5), 2)

# A plot can be made by
library(ggplot2)
png(filename="selectionTable_delinq_fivepar.png", width=1000,height=800)
selectionTable.plot(grc,TwentyOne_Groups,"friends", "delinq",levls=0:5)
graphics.off()

# Look at this file and interpret what it means for this data set.
# Note that the effects of the delinquency effects on friendship
# are not 'significant'. Their joint effect can be tested by
? multipleBayesTest
multipleBayesTest(grc, 9:11)
plot(multipleBayesTest(grc, 9:11))
# and also is not 'significant'.

