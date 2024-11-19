#makeAnticipatedResults.R

library(ggplot2)
library(RColorBrewer)
# install.packages("ggbiplot")
library(ggbiplot)

# aim 1 ####

# lets make a PCA to look at distinction in traits between species

# example code from chat 
data(iris)
iris_data <- iris[ , -5]

iris_scaled <- scale(iris_data)

# run pca
pca_result <- prcomp(iris_scaled, center = TRUE, scale. = TRUE)

# summarize
summary(pca_result)

# ggplot
pca_df <- as.data.frame(pca_result$x)  # Get PCA scores
pca_df$Species <- iris$Species  # Add species for coloring

ggplot(pca_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "PCA of Iris Dataset", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


  # make data for pca     ####
# traits I want to use
  # gsl, lf water content, flower angle, fruit number (this is fine for now)

# I am goign to make a data set for each and then combine them 

gslRet <- abs(rnorm(n = 40, mean = 5, sd=5))
gslhyb <- abs(rnorm(n = 40, mean = 10, sd=10))
gslstr <- abs(rnorm(n = 40, mean = 5, sd=15))

wcRet <- abs(rnorm(n = 40, mean = 15, sd=5))
wchyb <- abs(rnorm(n = 40, mean = 25, sd=10))
wcstr <- abs(rnorm(n = 40, mean = 20, sd=15))


angRet <- abs(rnorm(n = 40, mean = 180, sd=5))
anghyb <- abs(rnorm(n = 40, mean = 90, sd=10))
angstr <- abs(rnorm(n = 40, mean = 0, sd=15))

fcRet <- abs(rnorm(n = 40, mean = 11.5, sd = 11.9))
fchyb <- abs(rnorm(n = 40, mean = 11.5, sd = 10.4))
fcstr <- abs(rnorm(n = 40, mean = 3.9, sd = 2.7))

# make data frames

ret <- data.frame(
  species = "ret",
   gsl= gslRet,
   wc = wcRet,
   ang= angRet,
   fc = fcRet
)

hyb <- data.frame(
  species = "hyb",
  gsl= gslhyb,
  wc = wchyb,
  ang= anghyb,
  fc = fchyb
)

str <- data.frame(
  species = "str",
  gsl= gslstr, 
  wc = wcstr,
  ang= angstr,
  fc = fcstr
)

# make into one

allTrait <- rbind(ret, hyb)
allTrait <- rbind(allTrait, str)

nosps <- allTrait[ , -1] # remove sps column

# scale data
trt_scaled <- scale(nosps)

# run pca
pca_result <- prcomp(trt_scaled, center = TRUE, scale. = TRUE)

# summarize
summary(pca_result)

# ggplot
pca_df <- as.data.frame(pca_result$x)  # Get PCA scores
pca_df$Species <- allTrait$species  # Add species for coloring

ggplot(pca_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  labs( x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

ggbiplot(pca_result) + 
  geom_point(aes(color = allTrait$species)) +
  scale_color_brewer(palette = "Dark2") + 
  labs( x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


biplot(nosps)
?biplot
# aim 2 ####
# lets make a bar plot that is abundance and diversity



# hist of visitors
t <- 1:80
ret <- sample(1:25, size = 40, replace = T)
str <- sample(55:80, size = 40, replace = T)
hyb <- rnorm(n = 40, mean = 40, sd=10)

hybdf <- data.frame(
  group = "hyb",
  obs = hyb
)

strdf <- data.frame(
  group = "str",
  obs = str
)

retdf <- data.frame(
  group = "ret",
  obs = ret
)

big <- rbind(hybdf, retdf)
big <- rbind(big, strdf)


ggplot(big, aes(x=obs, fill=group)) + 
  geom_density(aes(y=..count..), alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2") + 
  labs( x = "Growing season day", y = "number of visitors per plant") +
  theme_minimal()

