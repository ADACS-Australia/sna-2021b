## ==== how to call Karen
### === the function create.enzo creates data and an initial estimation
source('data.set.Karen.R')
data.set.karen.set.up(n=10,M=4,seed=123,nbrNodes=3,nmain=20,nwarm=20,nprewarm=20,clusterType="FORK")
### === arguments
# 1. set nbrNodes to the minimum of your number of cores and 6
# 2. set nmain to something in the range 20 to 100
# 3. set nwarm to something in the range 20 to 200
# 4. use your choice of random seed - this random seed will be passed to subsequent estimations
# 5. n is the size of the network (this probably need to be greater than 10)
# 6. M is the number of groups/networks

### NB: you only have to create data once
