#Scenario: We are playing a game, where we are fighting against a boss against a 
#time limit, hence, we want to maximise the damage dealt.
#We are considering two stats, critical rate, which defines the probability of a 
#critical strike,an attack can only be a normal strike or a critical strike.
#and anoother stat, critical damage which determines the damage increase resulting from a 
#critical strike, for example, if the normal strike does 200 dmg, and 
#critical damage was 50%, a critical strike would deal 300 dmg
#We are given 300 stat points, each stat point can be allocated to either 
#crit dmg or crit rate at a rate of:
#1 stat point for 1% of CD and 2 stat points for 1% of CR

#Question:What allocation yields the greatest damage increase?

#We going to try a simulation approach, where we assume an allocation and
#simulate a single hit, many times, then we average the damage dealt across
#all simulation and consider this the expected damage increase.
#We repeat this for each allocation and find which has the highest increase

nsim=100 
#defines number of simulations for each allocation, 
#the higher the number, the more accurate the results

nhits=1
#defines the number of hits per simulation

#This for loop, repeats all simulations for each allocation, x defines the crit rate in %
#repeating for 0% CR to 100%, and putting remaining stat points in CD
for(x in 0:100){
  P=3;                
  #Represents stats allocatable.
  
  CR=0.01*x;          
  #The crit rate
  #We test for CR between 0-100%
  #We are able to change the amount of stat points allocatable but if P<2
  #we must adjust the values which x can take to fit the max crit rate achievable
  #given stats allocatable, P
  
  CD=P-0.02*x;        
  #The crit damage
  #Remaining stat points are all put into the alternative
  
  ncrit=rbinom(nsim,nhits,CR)  
  #simulates [nsim] bernoulli distributions with p=[CR] representing crit rate.
  #Bernoulli fits because there are only 2 possibilities and CR decides the chance of either
  #Where 1 means a critical strike was landed and 0 meaning a normal strike was landed
  #Output looks like:c(1,1,0,0,1,...,0,1)
  
  Edmg=1+CD*ncrit
  #Calculates the effective damage of each simulation and stores in a vector of size nsim
  
  MEAN[x+1]=mean(Edmg)
  #The mean of Edmg across all simulations
}
which.max(MEAN)-1
max(MEAN)
barplot(MEAN)

#Note that the method above is not consistent for small number of simulations
#but has longer runtimes if number of simulations is large. So, we try an
#analytic approach using probability formulas to determine the mean effective dmg
#It's quicker as there are no simulation only calculations, however the complexity of
#calculation rises with complexity of the problem.

#With an analytic approach:
#Note that effective damage is a random variable with 1-CR probability of being 1, 
#and CR probability of being 1+CD, a bernoulli dist. with p=CR where instead of 0 and 1
#we have 1 and 1+CD, we go to basics and calculate mean using the expectation formula
#So the expectation of this distribution can be given by: (1-CR)+CR*(1+CD)

for (x in 0:100){
  CR=0.01*x
  CD=P-2*CR
  MEAN[x+1]=(1-CR)+CR*(1+CD)
  #this vector represents the expected effective damage
  #for each stat allocation starting from CR=0%,CD=300% to CR=100%,CD=100%
}
which.max(MEAN)-1
#represents the optimal crit rate
max(MEAN)
#represents the expected effective damage at optimal allocation
barplot(MEAN)



#Now consider a case where if you are unable to land a critical strike, you are able
#to reset the fight and try again until you land the critical strike, this is a concept
#called "crit fishing". Of course your time is valuable so you will limit the 
#number of retries you are willing to take

nsim=10000
nhits=1

ntries=3
#The number of retries willing to take

SIM=1;prize=1;MEAN=1 
#for resetting purposes

for (x in 0:100) {
  CR=0.01*x
  CD=P-2*CR
  
  for (n in 1:nsim){
    SIM[n]=max(rbinom(ntries,nhits,CR))
    #Simulates 3 tries for 1 hit at chance of CR returns something like c(1,0,1),
    #taking the max yields 0 if all entries are 0, yields 1 otherwise meaning you
    #landed the crit in one of the tries, simultate this many times.
  }
  Edmg=1+CD*SIM
  MEAN[x+1]=mean(Edmg)
 
}
which.max(MEAN)-1
max(MEAN)
barplot(MEAN)
#A plot makes it easier to tell the distribution of allocation, and the general shape,
#allowing you to deduce if an extreme result were due to some noise and estimate 
#the real mean

#As for the analytic approach:
#We can use this formula for the pmf of a max for multiple bernoulli distributions:
#P[M=1]=(1-p)^3,P[M=1+CD]=1-(1-CR)^3
#Where M=max(X1,X2,X3), Xi~Bern(CR)
#Here, mean is given by: (1-CR)^3+(1-(1-CR)^3)*(1+CD)

for (x in 0:100){
  CR=0.01*x
  CD=P-2*CR
  MEAN[x+1]=(1-CR)^ntries+(1-(1-CR)^ntries)*(1+CD)
}
which.max(MEAN)-1
max(MEAN)
barplot(MEAN)


#Now we consider where in each try of each simulation, there are multiple equal hits,
#so now a higher crit rate is more valuable, as you want to land more critical strikes
#in each simulation without sacrificing too much of the damage increase from
#critical strikes

nsim=10000
nhits=3
ntries=5
SIM=1;prize=1;MEAN=1 #for resetting purposes

for (x in 0:100) {
  CR=0.01*x
  CD=P-2*CR
  
  for (n in 1:nsim){
    SIM[n]=max(rbinom(ntries,nhits,CR))
    #Simulates [nsim] simulations, taking the result with the most critical strikes
    #and records results in a vector, SIM
  }
  Edmg=1+CD*SIM/nhits
  #We scale the effective damage as we only want to look at the effective damage multiplier
  #so we are able to compare results of different number of hits
  MEAN[x+1]=mean(Edmg)
  
}
which.max(MEAN)-1
max(MEAN)
barplot(MEAN,ylim=c(0,max(MEAN)+0.2))
lines(seq(0,200,length.out=100),rep(max(MEAN),100),col="red")
lines(rep(which.max(MEAN),100),seq(0,4,length.out=100),col="red")
points(which.max(MEAN),max(MEAN),col='blue',pch=19)
#Lines and points make it clearer where the maximum lies


#Analytically, this is getting more complex, instead of the distribution being 
#the max of multiple bernoullis, it is now the max of i.i.d. binomial distributions, 
#which can be calculated using this formula:
#F_m(M=m)=[F_x(X=m)]^k  for m=0,1,...,n
#Where M=max(X1,X2,...,Xn) and Xi=Bin(n,p) for all i=1,2,...,k
#P_m(M=m)=[F_x(X=m)]^k-[F_x(X=m-1)]^k

for (x in 0:100){
  CR=0.01*x
  CD=P-2*CR
  for (n in 0:nhits) {
    VEC[n+1]=(pbinom(n,nhits,CR)^ntries-pbinom(n-1,nhits,CR)^ntries)*(1+n*CD/nhits)
    #This calculates m*P[M=m] and inputs it into a vector, later to be summed to calculate expectation
    #Where m is the prize amount and P is the probability of prize being said prize amount
  }
  MEAN[x+1]=sum(VEC)
}
which.max(MEAN)-1
max(MEAN)
barplot(MEAN,ylim=c(0,max(MEAN)+0.2))
lines(seq(0,200,length.out=100),rep(max(MEAN),100),col="red")
lines(rep(which.max(MEAN),100),seq(0,4,length.out=100),col="red")
points(which.max(MEAN),max(MEAN),col='blue',pch=19)


#Can we now do, one where each hit can have different scalings or weightings?
#In this case, we must find a way to be able to apply weighting to each of the unequal parts
#And in cases of the critical strike, increase the damage in proportion to each of the
#of the weightings, instead if the damage increase to be a flat increase for each hit
#Some hits you want to crit more than others, specifically the ones that hold more weighting
#We must restructure the code, as the above code cannot accomodate different weightings

#Parameters:
P=3.1
ntries=5
nsim=10000
#number of simulations to calculate expectation, higher=more accuracy and more lag

split=rep(c(0.28,0.13,rep(0.24/32,32),rep(0.35/32,32))/2,2) #Camellya
#We manually define the scaling and weightings of each hit, the sum of this vector
#must equal one, which can be done by dividing the vector by it's sum
#Our example here, has 2 big hits, represented by 0.28 and 0.13, and 2 sets
#of many small hits.

nhits=length(split)
#Automatically adjusts nhits to match number of hits in 'split'
SIM=numeric(nsim)
TRIES=numeric(ntries)
MEAN=numeric(101)
#initialise each vector used, so results from previous runs don't effect new results
#and avoid '(vector) not found' when trying to add values to a vector that doesn't
#yet exist

for (x in 0:100) {
  CR=0.01*x
  CD=P-2*CR
  
  for (n in 1:nsim){
    for (a in 1:ntries) {
      TRIES[a]=sum((rbinom(nhits,1,CR)*CD+1)*split)
      #In this small line of code, we simulate [nhit] bernoulli distributions,
      #to decide the result of each strike (normal or critical) in split
      #scale it to make the result (1,1+CD) instead of (0,1), and we apply
      #element-wise multiplication with split, to get a resulting vector
      #which represents the effective damage of each hit in a single try in a simulation
      #and we sum it all to get the total effective damage in a single try
      #and we store the result in a vector, TRIES, this is repeated [ntries] times
    }
    SIM[n]=max(TRIES)
    #We take the maximum result of the tries
  }
  
  
  MEAN[x+1]=mean(SIM)
  
}
cat('CR:',which.max(MEAN)-1,'%',' ','CD:',100*P-2*(which.max(MEAN)-1),'%');
cat('effective dmg multiplier:',max(MEAN))
barplot(MEAN,ylim=c(0,max(MEAN)+0.2),space=0)
lines(seq(0,200,length.out=100),rep(max(MEAN),100),col="red")
lines(rep(which.max(MEAN),100),seq(0,4,length.out=100),col="red")
points(which.max(MEAN),max(MEAN),col='blue',pch=19)
axis(1,at=seq(0,100,10),cex.axis=0.9)



#This is getting very complex, the distribution of the prize amount is now max(X1,X2,...,Xn)
#Where Xi ~ Σs_k*B_k  Where B_i is Bern(CR) and s_i are elements of the vector, 'split'
#which represents the weighting of each hit
#I have not yet figured out the analytic solution

#Future Ideas:
#1.Use other averages instead of just mean or expectation, such as mode, median or truncated
#  mean to discount the effect of extreme unfeasible results that arise from many simulations.
#2.Attempt an analytic solution for the version with weightings (very hard, complex calculation)
#3.Make an interactive interface so code is more user-friendly and appealing to the eyes
#4.Optimise number of simulations required to find max, so code runs faster
#5.Consider extra stat for optimisation like strength/atk? this becomes very hard,
#  as it then becomes a 3-dimensional optimisation problem, but what if we remove some
#  complexities of the previous code, what is instead of optimising between 100 allocations
#  we only compare a few builds and see which outputs the most damage? (Working on this)

#Debugging & testing:
x=28
CR=0.01*x
CD=P-2*CR
MEAN=1
VEC=1
SIM=1
prize=1
TRIES=1
MEAN

sum((rbinom(nhits,1,CR)*CD+1)*split)
((rbinom(nhits,1,CR)*CD+1)*split)
rbinom(nhits,1,CR)*CD+1
rbinom(nhits,1,CR)*CD
rbinom(nhits,1,CR)

#Camellya:split=c(0.28,0.13,rep(0.24/32,32),rep(0.35/32,32))
#Changli:
split=c(1210,rep(820,2),rep(150,7),rep(300,2),130,200,rep(100,2),rep(72,10))/sum(c(1210,rep(820,2),rep(150,7),rep(300,2),130,200,rep(100,2),rep(72,10)))
sum(split)
MEAN[68]



#Now let's consider a case, where you cannot choose your stat allocation freely, but you are
#allowed to pick between two already built stat allocation, now with the attack stat built in
#assume all attack scale directly proportionally with attack, and we want to maximise
#crit fishing damage

#stat parameters:
CD=3.30
CR=0.697
ATK=2637

#code parameters:
ntries=3
nsim=1000
scale=c()
#Add the scaling of each hit in a rotation into this vector

#initialise
TRIES=numeric(ntries)
SIM=numeric(nsim)
nhits=length(scale)

for (n in 1:nsim){
  for (a in 1:ntries) {
    TRIES[a]=sum((rbinom(nhits,1,CR)*CD+1)*scale*ATK)
  }
  SIM[n]=max(TRIES)
}

mean(SIM)



#c(0.3145,rep(0.2338,2),rep(0.2550,3),rep(0.1242,20),rep(0.2423,4)) basic attack chain



