# cleanFieldData.R

# goals: 
  # fix site column (make into seprate columns of site transect quadrat)
  # fix problems we found and noted in captains log
  # remove any blank lines
  # combine 3 data frames
  # left and right being negative

# load/set up ####
library(dplyr); library(tidyr); library(echinaceaLab)
getwd()
d1 <- read.csv("data/2024FieldworkDatasheet06182024.fullverified.csv")
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

# make L -      ####
# going to pull out the L's and then add a - in a new col
d1$x.2 <- d1$x
d1[d1$L.R %in% "L", ]
d1[d1$L.R %in% "L", "x"]
-d1[d1$L.R %in% "L", "x"]
d1[d1$L.R %in% "L", "x.2"] <- -d1[d1$L.R %in% "L", "x"]
d1[d1$L.R %in% "L", "x.2"]

# combining data sets ####

# remove empties and repeats
d1[!d1$Site %in% "", ]
d1.1 <- d1[!d1$Site %in% "", ]

d2[d2$ID %in% d1.1$ID, ]
d2.1 <- d2[!d2$ID %in% d1.1$ID & !d2$Obs %in% "", ]

d3[!d3$ID %in% d1.1$ID & !d3$Obs %in% "", ]
d3[!d3$s1.2 %in% "" & !d3$STG3 %in% "", ]
d3[d3$s1.2 %in% "", ]
d3.1 <- d3[d3$s1.2 %in% "", ]


# match cols in d1 and d2
compare2vectors(names(d1.1), names(d2.1))
names(d2.1)[21] <- "herbType" # change names
dput(names(d2.1))
# turn the stg2 into stg
d2.1 <- d2.1[ , c("ID", "Site", "TR", "QUAD", "y", "x.2", "Obs", "Geno", "genoSuspect", 
                  "GLS",  "STG2", "H", "W", "FLN", "FRN", "INH", "LFN", "DLN", 
                  "AvgPCD", "herbType", "TCO", "date", "notes")]

names(d2.1)[11] <- "Stg"
d2.1$trip <- 2
d1.1$trip <- 1
d1.1$genoSuspect <- "" # add cols to d1


d2.1$FLA <- ""
d2.1$FRL <- NA
d2.1$Nec <- ""
d2.1$PC <- ""
d2.1$PL1 <- ""
d2.1$PL2 <- ""
d2.1$SLA <- ""

# trim excess cols from d1, L.R and x
dput(names(d1.1))

d1.2 <- d1.1[ , c("ID", "Site", "TR", "QUAD", "Geno", "GLS", "SLA", "Obs", "Stg", 
                  "FLN", "FRN", "INH", "PL1", "PL2", "Nec", "TCO", "LFN", "DLN", 
                  "AvgPCD", "herbType", "y", "date", "H", "W", "FRL", 
                  "PC", "FLA", "notes", "x.2", "trip", "genoSuspect")]
compare2vectors(names(d1.2), names(d2.1))

head(merge(x=d1.2, y=d2.1, all=T))
nrow(merge(x=d1.2, y=d2.1, all=T))
nrow(d1.2) + nrow(d2.1)
d12 <- merge(x=d1.2, y=d2.1, all=T)

# ok merging 1,2 with 3

compare2vectors(names(d12), names(d3.1))

names(d3.1)[21] <- "herbType" # change names

# fix stage
names(d3.1)[11] <- "Stg"

# add cols to d3
d3.1$trip <- 3
d3.1$genoSuspect <- "" 
d3.1$FLA <- ""
d3.1$FRL <- NA
d3.1$Nec <- ""
d3.1$PC <- ""
d3.1$PL1 <- ""
d3.1$PL2 <- ""
d3.1$SLA <- ""

# cut s1.2
dput(names(d3.1))

d3.2 <- d3.1[ , c("ID", "Site", "TR", "QUAD", "y", "x.2", "Geno", "GLS", "Obs", 
                   "Stg", "H", "W", "FLN", "FRN", "FRL", "INH", "LFN", "DLN", 
                  "AvgPCD", "herbType", "TCO", "notes", "date", "trip", "genoSuspect", 
                  "FLA", "Nec", "PC", "PL1", "PL2", "SLA")]

compare2vectors(names(d12), names(d3.2))

head(merge(x=d12, y=d3.2, all=T))
nrow(merge(x=d12, y=d3.2, all=T))
nrow(d12) + nrow(d3.2)
d123 <- merge(x=d12, y=d3.2, all=T)

head(d123)

# lets take a peak        ####
table(d123$Site)
d123[d123$Site %in% "", ]
table(d123$Site, d123$trip)

# I am removing these emptyies
d123 <- d123[!d123$Site %in% "", ]

# updated genosuspect column ####
table(d123$genoSuspect)

# start with SOV t1

d123[d123$Site %in% "SOV" & d123$TR %in% 1, ]
# quadrat one is just stricta
d123[d123$Site %in% "SOV" & d123$TR %in% 1 & d123$QUAD %in% 1, "genoSuspect"] <- "str"

# qudarat 2 is a mixture of stricta and hybrids ??

# retro y>= 15
d123[d123$Site %in% "SOV" & d123$TR %in% 1 & d123$y >= 1500, ]

# fix small errors in TWM T2 ####
d1[d1$ID %in% "p525", ]
d1[d1$ID %in% "p525", 'x']
# start here after fixing the verification of the first set (hence why we don't have a notes column!)


# export data frame ####

# write.csv(d123, "data/fieldData2024Full.10022024.cvs", row.names = F)
