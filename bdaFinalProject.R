# bdaFinalProject.R

# load/setup              ####
rm(list = ls())
data <- read.csv("data/fieldData2024ForBdaGenoSuspect.11192024.cvs")

# packages
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# make some EDA plots     ####
ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=FRN)) + ylab("Fruit Count")

ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=H)) + ylab("plant height")
  # this is probably confounded alot by stage 
ggplot(data) + geom_boxplot(aes(x=Stg, y=H)) + ylab("plant height")
  # yup that is a big driver of that

ggplot(data) + geom_boxplot(aes(x=genoSuspect, y=W)) + ylab("plant width")
  # oh maybe this is not just a factor of stage 

hist(data[!data$FRN %in% 0, "FRN"])
  # ok so that is quite the tail we probably want to use a poisson???

# data cleaning           ####

data[data$Stg %in% c("B", "FO", "X"), ]
data[!data$Stg %in% c("B", "FO", "X"), ]
data <- data[!data$Stg %in% c("B", "FO", "X"), ]


# model plan              ####

# FRN ~ genoSuspect + (site + stage)
   # this notation is frequentest but my brain still works this way so pretend its not freq
  # site and stage pretend they are hirec 

table(data$Stg)
# stage: could treat linearly(code R as 1, FS as 2, SO as 3), remove B, FO, X

# create a potential/max frt ct var flw + frt
data[ , c("FRN", "FLN")]

# we need to turn site into a factor then interger 
data$Site <- as.integer(factor(data$Site))
# same thing with geno suspect
data$genoSuspect<-as.integer(factor(data$genoSuspect))
# fruit count model                             ####
dataFrn <- data[!data$FRN %in% NA, ]

dataList <- list(
  FRN = dataFrn$FRN,
  J =length(unique(dataFrn$Site)),
  siteID = dataFrn$Site,
  K=length(unique(dataFrn$genoSuspect)),
  gs = dataFrn$genoSuspect,
  N = nrow(dataFrn)
)


frtMod <- "
data {
int<lower=0> N;                   // sample size
int<lower=0> FRN[N];                 // fruit number
int<lower=0> J;                   // number of unique sites
int<lower=1, upper=J> siteID[N];     // catergorical site ID var
int<lower=0> K;                   // number of unique genoSuspect options
int<lower=1, upper=K> gs[N];         // catergorical genosuspect var
}

parameters {
vector[K] beta0;                  // vector of beta0 effect of genotype
vector[J] beta1;                  // vector of beta1 effect of site
vector<lower=0>[1] sig;
}

model{
real lambda[N];                   

// GLM poisson likelihood log link
for(i in 1:N){
    lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
    FRN[i] ~ poisson(lambda[i]);
}


// priors
beta0 ~ normal(0, 20);
beta1 ~ normal(0, sig[1]);
sig ~ normal(0,10);
}

generated quantities{
int<lower=0> Yrep[N];             // replicated data
real lambda[N];                   // storage of rate

for(i in 1:N){
  lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
  Yrep[i] = poisson_rng(lambda[i]);
}
}

"

# fitFrn <- stan(model_code= frtMod, data = dataList, iter = 4000)
  # ok I want to run this model again but without the zeros

head(dataFrn)
nrow(dataFrn[dataFrn$Stg %in% c("FS", "SO"), ])
dataFrnNo0 <- dataFrn[dataFrn$Stg %in% c("FS", "SO"), ]
dataListFrnNo0 <- list(
  FRN = dataFrnNo0$FRN,
  J =length(unique(dataFrnNo0$Site)),
  siteID = dataFrnNo0$Site,
  K=length(unique(dataFrnNo0$genoSuspect)),
  gs = dataFrnNo0$genoSuspect,
  N = nrow(dataFrnNo0)
)

fitFrnNo0 <- stan(model_code= frtMod, data = dataListFrnNo0, iter = 4000)
fitFrn
plot(fitFrn)
traceplot(fitFrn)


# max fit potential fit                     ####
  # this is going to be frt + flw including 0's
# need to make column

data$max <- data$FRN + data$FLN

dataMax <- data[!data$max %in% NA, ]
  # so site and geno suspect are still good becasue we formated them before making the frt specific data frame so we are good

dataListMax <- list(
  MAX = dataMax$max,
  J =length(unique(dataMax$Site)),
  siteID = dataMax$Site,
  K=length(unique(dataMax$genoSuspect)),
  gs = dataMax$genoSuspect,
  N = nrow(dataMax)
)

maxMod <- "
data {
int<lower=0> N;                     // sample size
int<lower=0> MAX[N];                 // fruit number
int<lower=0> J;                     // number of unique sites
int<lower=1, upper=J> siteID[N];     // catergorical site ID var
int<lower=0> K;                       // number of unique genoSuspect options
int<lower=1, upper=K> gs[N];         // catergorical genosuspect var
}

parameters {
vector[K] beta0;                  // vector of beta0 effect of genotype
vector[J] beta1;                  // vector of beta1 effect of site
vector<lower=0>[1] sig;
}

model{
real lambda[N];                   

// GLM poisson likelihood log link
for(i in 1:N){
    lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
    MAX[i] ~ poisson(lambda[i]);
}


// priors
beta0 ~ normal(0, 20);
beta1 ~ normal(0, sig[1]);
sig ~ normal(0,10);
}

generated quantities{
int<lower=0> Yrep[N];             // replicated data
real lambda[N];                   // storage of rate

for(i in 1:N){
  lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
  Yrep[i] = poisson_rng(lambda[i]);
}
}

"

fitMax <- stan(model_code= maxMod, data = dataListMax, iter = 4000)
fitMax
plot(fitMax)
traceplot(fitMax)


# # petal : petal + carpel      ####
# head(data)
# # so we need to divide PL1/PL2
# 
# data$PlRatio <- data$PL1/data$PL2
# data$PlRatio
#   # ok this is a small data set
# 
# dataPL <- data[!data$PlRatio %in% NA, ]
# 
# dataListPL <- list(
#   PL = dataPL$PlRatio,
#   J =length(unique(dataPL$Site)),
#   siteID = dataPL$Site,
#   K=length(unique(dataPL$genoSuspect)),
#   gs = dataPL$genoSuspect,
#   N = nrow(dataPL)
# )
# 
# plMod <- "
# data {
# int<lower=0> N;                     // sample size
# int<lower=0> PL[N];                 // fruit number
# int<lower=0> J;                     // number of unique sites
# int<lower=1, upper=J> siteID[N];     // catergorical site ID var
# int<lower=0> K;                       // number of unique genoSuspect options
# int<lower=1, upper=K> gs[N];         // catergorical genosuspect var
# }
# 
# parameters {
# vector[K] beta0;                  // vector of beta0 effect of genotype
# vector[J] beta1;                  // vector of beta1 effect of site
# vector<lower=0>[1] sig;
# }
# 
# model{
# real lambda[N];                   
# 
# // GLM poisson likelihood log link
# for(i in 1:N){
#     lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
#     PL[i] ~ poisson(lambda[i]);
# }
# 
# 
# // priors
# beta0 ~ normal(0, 20);
# beta1 ~ normal(0, sig[1]);
# sig ~ normal(0,10);
# }
# 
# generated quantities{
# int<lower=0> Yrep[N];             // replicated data
# real lambda[N];                   // storage of rate
# 
# for(i in 1:N){
#   lambda[i] = exp(beta0[gs[i]] + beta1[siteID[i]]);
#   Yrep[i] = poisson_rng(lambda[i]);
# }
# }
# 
# "
# 
# fitPL <- stan(model_code= plMod, data = dataListPL, iter = 4000)
# fitPL
# plot(fitPL)
# traceplot(fitPL)

# ammount of herbivory        ####
  # lets try amount of herbivore damage 
  # we have a few different things happening here but I am acutally using a binomial
      # y: number of successes (leaves with damage)
      # n: number of trials (leaves total)

data$avgProDam <- data$AvgPCD/100

# data$damage <- ((data$DLN * data$avgProDam)/ data$LFN) 
# data[data$damage %in% c(NaN, NA), ]
  # add a buffer into this (whatever the smallest detectable variable) then logit transform, then normal dist, add sigma
    # real answer is beta 
# actually doing a binomial! 
cbind(data$DLN, data$LFN)
dataDam <- data[!(data$DLN %in% c(NaN, NA) | data$LFN %in% NA), ]



dataListDam <- list(
  y = dataDam$DLN,
  n = dataDam$LFN,
  # J =length(unique(dataDam$Site)),
  # siteID = dataDam$Site,
  K=length(unique(dataDam$genoSuspect)),
  gs = dataDam$genoSuspect,
  N = nrow(dataDam)
)

damMod <- "
data {
int<lower=0> N;                     // sample size
int<lower=0> y[N];                  // number of successes/ number of damaged leaves
int<lower=0> n[N];                  // number of trials/ number of leaves
// int<lower=0> J;                     // number of unique sites
// int<lower=1, upper=J> siteID[N];     // catergorical site ID var
int<lower=0> K;                       // number of unique genoSuspect options
int<lower=1, upper=K> gs[N];         // catergorical genosuspect var
}

parameters {
vector<lower=0, upper=1> [N] p;       // vector of probs
real<lower=0, upper=1> pi[3];         // mean damage by genotype
real<lower=0> K[3];                   // variablility in damage by genotype
}

model{
  // binomial likelihood
  for(i in 1:N){
  y[i] ~ binomial(n[i], p[i])
  }
  
  // conditional prior on the ps
  for(i in 1:N){
  p[i] ~ beta(pi[gs[i]] * K[gs[i]], (1-pi[gs[i]]) * K[gs[i]]);
  }
  
  // hyperpriors
  for(j in 1:2){
  pi[j] ~ beta(1, 1);
  K[j] ~ uniform(0.5, 1000);
  }
}

generated quantities{
real pdam; // difference in damage
pdam = pi[1] - pi[2]

}
"

damMod2 <- "data {
  int<lower=0> N;                   // sample size
  int<lower=0> y[N];                // successes
  int<lower=0> n[N];                // trials
  int<lower=0> K;                   // number of genotypes
  int<lower=1, upper=K> gs[N];      // genotype IDs
}

parameters {
  vector<lower=0, upper=1> [N] p;   // probabilities
  real<lower=0, upper=1> pi[K];     // mean damage by genotype
  real<lower=0> K_par[K];           // variability in damage by genotype
}

model {
  // Binomial likelihood
  for (i in 1:N) {
    y[i] ~ binomial(n[i], p[i]);
  }
  
  // Conditional prior on p
  for (i in 1:N) {
    p[i] ~ beta(pi[gs[i]] * K_par[gs[i]], (1 - pi[gs[i]]) * K_par[gs[i]]);
  }
  
  // Hyperpriors
  for (j in 1:K) {
    pi[j] ~ beta(1, 1);
    K_par[j] ~ uniform(0.5, 1000);
  }
}

generated quantities {
  real pdam;
  pdam = pi[1] - pi[2];
}
"

fitDam <- stan(model_code= damMod2, data = dataListDam, iter = 4000)
fitDam
plot(fitDam)
traceplot(fitDam, pars = c("pi", "K_par", "pdam"))
print(fitDam,pars="pi")
summary(fitDam, pars = c("pdam"))
  # ok so this is showing me the difference between the 3 groups

# I want to know more about the differences between groups

plot(fitDam, pars = c("pi"))

p <- extract(fitDam)$p
  # ok this is weird and idk what to do with it


# totals per data set         ####
table(dataFrnNo0$genoSuspect)
table(dataMax$genoSuspect)
table(dataDam$genoSuspect)
table(data$Site)

hist(dataMax$max)
dataMax[dataMax$max > 100, ]
