
## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages!
packages <- c('data.table', 'kml', 'kml3d', 'ggplot2')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## data ------
dat <- readRDS('data/jointwide.RDS')
setDT(dat)


# use kml package to get data structure we need
datcld <- cld3d(traj = dat, idAll = dat$Area, timeInData = list(Food = as.vector(2:54), Feed = as.vector(55:107)))

# perform clustering
parKml3d(imputationMethod = "linearInterpol.local", scale=TRUE)

kml3d(datcld, nbClusters = 2:10)
X11(type="Xlib", choice(datcld))

# save clusters
dat$clusters10 <- getClusters(datcld, 10)
dat$clusters9 <- getClusters(datcld, 9)
dat$clusters8 <- getClusters(datcld, 8)
dat$clusters7 <- getClusters(datcld, 7)
dat$clusters6 <- getClusters(datcld, 6)
dat$clusters5 <- getClusters(datcld, 5)
dat$clusters4 <- getClusters(datcld, 4)
dat$clusters3 <- getClusters(datcld, 3)
dat$clusters2 <- getClusters(datcld, 2)

summary(dat)


clusterassignment <- dat[, .(Area, clusters2, clusters3, clusters4, clusters5, 
                             clusters6, clusters7, clusters8, clusters9, clusters10)]


longdat <- readRDS('data/moddat.RDS')
setDT(longdat)
longdat <- clusterassignment[longdat, on = "Area"]

ggplot(longdat[clusters3 == "B"], aes(x = year, y = Feed, color = Area)) +
  geom_line() + theme_minimal() + ggtitle("Feed Production in Cluster 2 when k = 3")

ggplot(longdat[clusters3 == "B"], aes(x = year, y = Food, color = Area)) +
  geom_line() + theme_minimal() + ggtitle("Food Production in Cluster 2 when k = 3")

ggplot(longdat[clusters3 == "C"], aes(x=year, y=prop, color=Area)) +
  geom_line() + theme_minimal() +
  geom_hline(yintercept = 1, lwd = 0.4, linetype = "dashed") +
  labs(title = "Proportion of feed to food in Cluster 3 when k = 3", 
       subtitle = "Dotted line indications equal amounts food and feed. Below the line, \ncountries produce more food than feed and vice versa.") +
  theme(plot.subtitle = element_text(hjust = 0))



