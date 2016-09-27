

##############################################################
# Code to compute the proportion of threatened species in each
# country for birds, mammals, amphibians
# Lucie Bland, The University of Melbourne
# 27 September 2016
##############################################################


# Load libraries
library(rredlist)

##############################################################
# Set up data 

# IUCN Red List, already filtered to our 25 countries and birds, mammals, amphibians
setwd("C:/9. Other projects/Psychology/Analysis")
data <- read.csv("Animals.csv")
summary(data)

# countries and ISO codes
country <- read.csv("Country_ISO.csv")

# IUCN Red List API token, LM BLAND
API <- "e6f5a8ec6b3cb6df3f360bdf1ff33793e4018e942b2a9d77ccb4ccc9a02aeabc"

################################################################
# Search for species in a country

prop <- rep(0, length(country$ISO))
sppno <- rep(0, length(country$ISO))

for (i in 1:length(country$ISO)){
  
  # Finding species in each country and filtering for mammals/birds/amphibians
  countryspp <- rl_sp_country(country = country$ISO[i], key = API)
  spp <- data[data$Species.ID %in% countryspp$result$taxonid , ]

  # Removing DD, NE, EX and EW species
  spp <- spp[spp$Red.List.status != "DD" & spp$Red.List.status != "NE" 
            & spp$Red.List.status != "EX" & spp$Red.List.status != "EW", ]

  # Calculating proportion of threatened species
  spp$RL <- factor((spp$Red.List.status == "VU")+(spp$Red.List.status == "EN") + (spp$Red.List.status == "CR"))
  prop[i] <- (summary(spp$RL)[2] / (summary(spp$RL)[1] + summary(spp$RL)[2]))
  prop[i] <- round(prop[i] * 100, 2)
  sppno[i] <- length(spp$RL)
}


# Computing results: country name, proportion threatened, extant species richness
propthr <- data.frame(cbind(as.vector(country$Country), prop, sppno))
colnames(propthr) <- c("Country", "Prop_threatened", "Species_richness")
write.csv(propthr, "Prop_Animals_Threatened.csv")



