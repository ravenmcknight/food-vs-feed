
## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages!
packages <- c('data.table')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lme4)

# read data from kaggle
fao <- fread("FAO.csv")
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


# simplest mod: just by country. this is fixed effects
mod1 <- lmer(log(prop) ~ (1|Area), data = moddat)


## addition: random effects by country


summary(mod1)
