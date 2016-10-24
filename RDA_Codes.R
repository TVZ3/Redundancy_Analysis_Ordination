#RDA
#I. Load Libraries
library(ade4)
library(vegan)
library(MASS)
install.packages("packfor", repos="http://R-Forge.R-project.org")
library(packfor)

setwd("C:\\Users\\adminuser\\Desktop\\EVERYTHING THOMAS\\Chapter 2")

dat<-read.csv("rda.csv")
dat<-na.omit(dat)
nrow(dat)
class(dat)
dat <- as.data.frame(dat)
colnames(dat)
summary(dat)

##Load data and log-transform environmental variables that violate statistical assumptions
###Write a loop to log transform the necessary variables
lakedat <- cbind(dat[4:9])
for (i in 1:6){
    lakedat[i] <- log10(lakedat[i]+1) 
}
climdat <- cbind(dat[10:19])
fishdat <- cbind(dat[20:29])
envdat <- data.frame(lakedat, climdat)

spe.hel=decostand(fishdat, "hellinger") ### Hellinger transform species data

env.std=decostand(envdat, "standardize") ### Standardize environmental data

#RDA with all explanatory variables
fishRDA=rda(spe.hel ~ ., data=env.std)
R2adjusted=RsquareAdj(fishRDA)$adj.r.squared
R2adjusted
plot(fishRDA)
text(fishRDA, display = "species", cex=0.8, col="red")

#Forward selection of important explanatory variables
forward.sel(spe.hel, env.std)

fishRDA=rda(spe.hel ~ Surface_Area_ha+Secchi_Depth_m+July_0012_temp+July_0012_prec+
               DOC_mgL+P_ugL, data=env.std)
#Global adjusted R2
R2adjusted=RsquareAdj(fishRDA)$adj.r.squared
R2adjusted
#Assessing multicollinearity between explanatory varables
vif.cca(fishRDA) ## values over 10 indicate redundant constraints

#RDA with only significant variables
plot(fishRDA)
text(fishRDA, display = "species", cex=0.8, col="red")
anova.cca(fishRDA, step=1000)





