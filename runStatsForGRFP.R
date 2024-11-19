
rm(list= ls())

d123 <- read.csv("data/fieldData2024Full.10022024.cvs")


# eda for grfp      ####
table(d123$genoSuspect)

# hyb average number of frut and flowers
nrow(d123[d123$genoSuspect %in% c("H/R", "hyb", "R/H?", "ret/hyb", "RH", "str/hyb"), ])

mean(na.omit(d123[d123$genoSuspect %in% c("H/R", "hyb", "R/H?", "ret/hyb", "RH", "str/hyb") & !d123$Stg %in% "R", "FLN"]))
mean(na.omit(d123[d123$genoSuspect %in% c("H/R", "hyb", "R/H?", "ret/hyb", "RH", "str/hyb") & !d123$Stg %in% "R", "FRN"]))

# retro ave number of fruit and flowers
d123[d123$genoSuspect %in% c("R", "ret"), ]
nrow(d123[d123$genoSuspect %in% c("R", "ret"), ])

mean(na.omit(d123[d123$genoSuspect %in% c("R", "ret") & !d123$Stg %in% "R", "FLN"]))
mean(na.omit(d123[d123$genoSuspect %in% c("R", "ret") & !d123$Stg %in% "R", "FRN"]))

# stricta ave number of fruit and flowers
d123[d123$genoSuspect %in% c("str", "str?"), ]
nrow(d123[d123$genoSuspect %in% c("str", "str?"), ])

mean(na.omit(d123[d123$genoSuspect %in% c("str", "str?") & !d123$Stg %in% "R", "FLN"]))
mean(na.omit(d123[d123$genoSuspect %in% c("str", "str?") & !d123$Stg %in% "R", "FRN"]))


# I am going to make a mini dataframe to play around with
d123[!d123$genoSuspect %in% "", c("ID", "genoSuspect", "FLN", "FRN", "Stg")]
d123[!d123$genoSuspect %in% "" & !d123$Stg %in% "R" , c("ID", "genoSuspect", "FLN", "FRN", "Stg")]
small <- d123[!d123$genoSuspect %in% "" & !d123$Stg %in% "R" , c("ID", "genoSuspect", "FLN", "FRN", "Stg")]
table(small$genoSuspect)

small[small$genoSuspect %in% "str?", "genoSuspect"] <- "str"
small[small$genoSuspect %in% c("R/H?", "RH", "str/hyb"), "genoSuspect"] <- "hyb"
small[small$genoSuspect %in% "R", "genoSuspect"] <- "ret"

library(ggplot2)

ggplot(small, aes(x=genoSuspect, y=FRN, fill=genoSuspect)) + geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point", size = 5, shape = 1,
               color = "black") + theme_bw()


ggplot(small, aes(x=genoSuspect, y=FLN, fill=genoSuspect)) + geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point", size = 5, shape = 1,
               color = "black") + theme_bw()

ggplot(small) + geom_boxplot(aes(x=genoSuspect, y=FRN))

# caclulating sd. ####
table(small$genoSuspect)

sd(na.omit(small[small$genoSuspect %in% "hyb", "FRN"]))
sd(na.omit(small[small$genoSuspect %in% "ret", "FRN"]))
sd(na.omit(small[small$genoSuspect %in% "str", "FRN"]))


sd(na.omit(small[small$genoSuspect %in% "hyb", "FLN"]))
sd(na.omit(small[small$genoSuspect %in% "ret", "FLN"]))
sd(na.omit(small[small$genoSuspect %in% "str", "FLN"]))


# running anova     ####
small$genoSuspect <- as.factor(small$genoSuspect)
frn <- aov(FRN ~ genoSuspect,
    data = small
)
summary(frn)
  # hey there's sig difference!

fln <- aov(FLN ~ genoSuspect,
           data = small
)
summary(fln)

# post hoc for frn    ####
# Tukey HSD test:
# install.packages("multcomp")
library(multcomp)
post_test <- glht(frn,
                  linfct = mcp(genoSuspect = "Tukey")
)

summary(post_test)

small[small$FLN > 60, ]

# EXPORT DATA FRAME FOR BDA     ####
# so small has only flowering and genoSuspect

# I am going to make a dataframe that has rossets and flowering

nrow(d123[!d123$genoSuspect %in% "", ])

table(is.na(d123$Stg))


d123[d123$genoSuspect %in% "str?", "genoSuspect"] <- "str"
d123[d123$genoSuspect %in% c("R/H?", "RH", "str/hyb", "H/R", "ret/hyb"), "genoSuspect"] <- "hyb"
d123[d123$genoSuspect %in% "R", "genoSuspect"] <- "ret"

# I am going to add upper twm so that there is more evenness in the numbers
unique(d123$Site)
nrow(d123[d123$Site %in% "UTWM", ])
d123[d123$Site %in% "UTWM", "genoSuspect"] <- "str"

table(d123$genoSuspect)

genoSus <- d123[!d123$genoSuspect %in% "", ]

table(d123[!d123$genoSuspect %in% "", "Stg"])

table(is.na(genoSus$H))
d123[d123$H %in% NA, ]
# ok those are fine

table(is.na(d123$W))
d123[d123$W %in% NA, ]
# these are fine
# ok those are the important things time to export

# write.csv(genoSus, "data/fieldData2024ForBdaGenoSuspect.11192024.cvs", row.names = F)
