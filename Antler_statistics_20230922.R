

# Antler Statistics
# Royer and Somerville
# Sept 16, 2023



library(tidyverse)

setwd("C:/Users/asomervi/Desktop/_____/Iowa State University/STUDENTS/Royer, Julien")
Antlers <- read.csv("Antlers_and_climate.csv")

####################################################################

# ANOVA TESTS - Are there significant differences in the means of different isotope variables between the antlers? 

#ANOVA d13Cap values
CapANOVA <- Antlers %>% 
  select(Antler, d13Cap)
res_CapANOVA <- aov(d13Cap ~ Antler, data = CapANOVA)
summary(res_CapANOVA)


#ANOVA d18Oap values
OapANOVA <- Antlers %>% 
  select(Antler, d18O)
res_OapANOVA <- aov(d18O ~ Antler, data = OapANOVA)
summary(res_OapANOVA)

#ANOVA d13Ccol values
CcolANOVA <- Antlers %>% 
  select(Antler, d13Ccol)
res_CcolANOVA <- aov(d13Ccol ~ Antler, data = CcolANOVA)
summary(res_CcolANOVA)

#ANOVA d15Ncol values
NcolANOVA <- Antlers %>% 
  select(Antler, d15N)
res_NcolANOVA <- aov(d15N ~ Antler, data = NcolANOVA)
summary(res_NcolANOVA)


############################################################################

# CORRELATIONS BETWEEN ISOTOPE DATA AND CLIMATE DATA

library(tidyverse)

# Creating the vectors to use as part of the correlation matrices 
# To what extent does seasonal variation in climate variables influence isotope variables?

Climate <- read.csv("Antlers_and_climate.csv")
Climate2 <- Climate %>% select("Antler", "TavgNOAA", "PrecipNOAA", "DIVA.GIS.Rain", "d18O", "d13Cap", "d13Ccol", "d15N")

PEL0090 <- Climate2 %>% filter(Antler == "PEL-0090")
PEL90 <- PEL0090[, -1]

PEL0114 <- Climate2 %>% filter(Antler == "PEL-0114")
PEL114 <- PEL0114[, -1]

PEL0164 <- Climate2 %>% filter(Antler == "PEL-0164")
PEL164 <- PEL0164[, -1]

PEL0165 <- Climate2 %>% filter(Antler == "PEL-0165")
PEL165 <- PEL0165[, -1]

# Correlation tables

#PEL-0090

matrix <- round(cor(PEL90),2)

# Defining the lower and upper parts of the correlation matrix
lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)

upper.tri(matrix)

# Hiding the upper triangle of the matrix
upper <- matrix
upper[upper.tri(matrix)] <- ""
upper <- as.data.frame(upper)
upper

write.table(upper, file = "PEL-0090.csv", sep = ",", quote = FALSE)


#PEL-0114

matrix <- round(cor(PEL114),2)

lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)

upper.tri(matrix)

upper <- matrix
upper[upper.tri(matrix)] <- ""
upper <- as.data.frame(upper)
upper

write.table(upper, file = "PEL-0114.csv", sep = ",", quote = FALSE)

#PEL-0164 with all data

matrix <- round(cor(PEL164),2)

lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)

upper.tri(matrix)

upper <- matrix
upper[upper.tri(matrix)] <- ""
upper <- as.data.frame(upper)
upper

write.table(upper, file = "PEL-0164.csv", sep = ",", quote = FALSE)

#PEL-0164 with d18O outlier value removed

PEL164trim <- PEL164[-5,] # this is to remove the outlier value from June (-1.5 parts per mil)

matrix <- round(cor(PEL164trim),2)

lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)

upper.tri(matrix)

upper <- matrix
upper[upper.tri(matrix)] <- ""
upper <- as.data.frame(upper)
upper

write.table(upper, file = "PEL-0164_no_outlier.csv", sep = ",", quote = FALSE)

#PEL-0165

matrix <- round(cor(PEL165),2)

lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)

upper.tri(matrix)

upper <- matrix
upper[upper.tri(matrix)] <- ""
upper <- as.data.frame(upper)
upper

write.table(upper, file = "PEL-0165.csv", sep = ",", quote = FALSE)




###############################################################################



###### LINEAR REGRESSIONS ##### 

# How meaningful are the correlations between climate and isotope variables?


setwd("C:/Users/asomervi/Desktop/_____/Iowa State University/STUDENTS/Royer, Julien")

library(tidyverse)
library(ggalt)
library(corrplot)
library(xtable)

Climate <- read.csv("Climate_data_trim.csv")
Climate2 <- Climate %>% select("Antler", "TavgNOAA", "TprecipNOAA", "DIVA.GIS.Rain", "d18O", "d13Cap", "d13Ccol", "d15N")

PEL0090 <- Climate2 %>% filter(Antler == "PEL-0090")
PEL90 <- PEL0090[, -1]

#TEMP
d13C90 <- lm(d13Ccol ~ TavgNOAA, data = PEL90)
summary(d13C90)
d15N90 <- lm(d15N ~ TavgNOAA, data = PEL90)
summary(d15N90)
d13Cap90 <- lm(d13Cap ~ TavgNOAA, data = PEL90)
summary(d13Cap90)
d18O90 <- lm(d18O ~ TavgNOAA, data = PEL90)
summary(d18O90)

#1 YR PRECIP
d13C90 <- lm(d13Ccol ~ TprecipNOAA, data = PEL90)
summary(d13C90)
d15N90 <- lm(d15N ~ TprecipNOAA, data = PEL90)
summary(d15N90)
d13Cap90 <- lm(d13Cap ~ TprecipNOAA, data = PEL90)
summary(d13Cap90)
d18O90 <- lm(d18O ~ TprecipNOAA, data = PEL90)
summary(d18O90)

# #50 year PRECIP
d13C90 <- lm(d13Ccol ~ DIVA.GIS.Rain, data = PEL90)
summary(d13C90)
d15N90 <- lm(d15N ~ DIVA.GIS.Rain, data = PEL90)
summary(d15N90)
d13Cap90 <- lm(d13Cap ~ DIVA.GIS.Rain, data = PEL90)
summary(d13Cap90)
d18O90 <- lm(d18O ~ DIVA.GIS.Rain, data = PEL90)
summary(d18O90)


PEL0114 <- Climate2 %>% filter(Antler == "PEL-0114")
PEL114 <- PEL0114[, -1]

PEL0164 <- Climate2 %>% filter(Antler == "PEL-0164")
PEL164 <- PEL0164[, -1]

PEL0165 <- Climate2 %>% filter(Antler == "PEL-0165")
PEL165 <- PEL0165[, -1]


