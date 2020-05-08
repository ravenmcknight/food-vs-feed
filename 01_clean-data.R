
## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages!
packages <- c('data.table', 'tidyr', 'dplyr', 'ggplot2', 'stringr')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)


# read data from kaggle
fao <- fread("data/FAO.csv")
maize <- fao[Item == "Maize and products"]

# reshape data
maize_long <- pivot_longer(maize, cols = names(maize[, 11:63]), names_to = "year", values_to = "amount")
setDT(maize_long)

# remove Y from year
maize_long[, year := str_remove(pattern = "Y", string = year)]
maize_long[, year := as.numeric(year)]


ggplot(maize_long[Area == "United States of America"], aes(x=year, y=amount)) + 
  geom_line() + facet_wrap(~Element) + ggtitle("Corn as food vs feed in US")

ggplot(maize_long[Area == "Yemen"], aes(x=year, y=amount)) + 
  geom_line() + facet_wrap(~Element) + ggtitle("Corn as food vs feed in Yemen")


maizedat <- maize_long %>% pivot_wider(names_from = "Element", values_from = "amount", id_cols = c("Area", "year"))
setDT(maizedat)

# get prop
maizedat <- maizedat[Food != 0]
maizedat[, prop := Feed/Food]


ggplot(maizedat[Area == "United States of America" | Area == "Mexico" | Area == "China, mainland" | Area == "Zimbabwe"], aes(x=year, y=prop, color=Area)) +
  geom_line() + ggtitle("Proportion of Feed to Food, example countires")


moddat <- na.omit(maizedat)
setDT(moddat)
moddat[prop == 0, prop := 0.01]

ggplot(moddat, aes(x=log(prop))) +
  geom_histogram() + ggtitle("distribution of response")

# we'll use log as a response

saveRDS(moddat, "data/moddat.RDS")
# wider again?

moddat[, logprop := log(prop + 0.01)]
moddatwide <- moddat %>% 
  pivot_wider(names_from = "year", values_from = "prop", id = "Area")

setDT(moddatwide)
saveRDS(moddatwide, "data/moddatwide.RDS")

moddatwidelog <- moddat %>% 
  pivot_wider(names_from = "year", values_from = "logprop", id = "Area")


setDT(moddatwidelog)
saveRDS(moddatwidelog, "data/moddatwidelog.RDS")


# this is what we can use to model both jointly
jointwide <- moddat %>%
  pivot_wider(names_from = "year", values_from = c("Feed", "Food"), id_cols = "Area")

setDT(jointwide)
saveRDS(jointwide, "data/jointwide.RDS")
