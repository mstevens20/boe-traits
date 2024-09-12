# cleanFieldData.R

# goals: 
  # fix site column (make into seprate columns of site transect quadrat)
  # fix problems we found and noted in captains log
  # remove any blank lines
  # combine 3 data frames
  # left and right being negative

# load/set up ####
library(dplyr); library(tidyr)
getwd()
d1 <- read.csv("data/2024FieldworkDatasheet06182024.verified.csv")
d2 <- read.csv("data/2024FieldworkDatasheet07062024.verified.csv")
d3 <- read.csv("data/2024FieldworkDatasheet07232024.verified.csv")


# fixing site column    ####
# i want 3 columns, site, transect, quadrat
head(d1)
head(d2)
head(d3)

# D1
# we have two columns site and tansect/quad
# I am going to use the seperate function to fix this

head(separate(data=d1, col=TRQ, into= c("TR", "QUAD"), sep = "\\.")
)
# d1[grepl("\\.", d1$TRQ), "TRQ"]

d1 <- separate(data=d1, col=TRQ, into= c("TR", "QUAD"), sep = "\\.")

table(d1$Site)

# d2
d2$Site <- gsub(pattern = "_", replacement = ".", x=d2$Site)
  # make it so there is only periods not a mix
d2[457, ]

head(separate(data=d2, col=Site, into= c("Site", "TR", "QUAD"), sep = "\\.")
)
  
d2[457:465, ]
d2 <- separate(data=d2, col=Site, into= c("Site", "TR", "QUAD"), sep = "\\.")

d2$TR <- gsub(pattern = "T", replacement = "", x=d2$TR)
d2$QUAD <- gsub(pattern = "Q", replacement = "", x=d2$QUAD)

table(d2$Site)
# fix tm to twm
d2$Site <- gsub(pattern = "TM", replacement = "TWM", x=d2$Site)


# d3
head(d3)
table(d3$Site)

d3$Site <- gsub(pattern = "_", replacement = ".", x=d3$Site)
# make it so there is only periods not a mix
d3[457, ]

d3 <- separate(data=d3, col=Site, into= c("Site", "TR", "QUAD"), sep = "\\.")
d3[457:465, ]

d3$TR <- gsub(pattern = "T", replacement = "", x=d3$TR)
d3$QUAD <- gsub(pattern = "Q", replacement = "", x=d3$QUAD)

table(d3$Site)
# fix tm to twm
d3$Site <- gsub(pattern = "TM", replacement = "TWM", x=d3$Site)

# fix site names ####
# twm was fixed in the previous section
# ARR <- AR
# SOV <- SO

d1$Site <- gsub(pattern = "AR", replacement = "ARR", x=d1$Site)
d2$Site <- gsub(pattern = "AR", replacement = "ARR", x=d2$Site)
d3$Site <- gsub(pattern = "AR", replacement = "ARR", x=d3$Site)

d1$Site <- gsub(pattern = "SO", replacement = "SOV", x=d1$Site)
d2$Site <- gsub(pattern = "SO", replacement = "SOV", x=d2$Site)
d3$Site <- gsub(pattern = "SO", replacement = "SOV", x=d3$Site)

table(d1$Site)
table(d2$Site)
table(d3$Site)


# fix small errors in TWM T2 ####
d1[d1$ID %in% "p525", ]
d1[d1$ID %in% "p525", 'x']
# start here after fixing the verification of the first set (hence why we don't have a notes column!)