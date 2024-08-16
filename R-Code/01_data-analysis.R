library(vegan)
library(fossil)
library(lmerTest)
library(performance)
library(tidyverse)
library(dendextend)
#----------------------------------
#organise the data

#read in data for ordination
IV = read.table("Importance_Values.csv", header = T, sep = ",", fileEncoding="UTF-8-BOM")


#for each row, go through each column. Therefore for each cell do:
#if value isn't NA, undo the log, add 1, and then re-log
#if value is an NA, then make the value log(1)
for(r in 1:nrow(IV)){
  for(c in 2:ncol(IV)){
    if(!is.na(IV[r,c])){
      IV[r,c] = log((10^IV[r,c])+1)
    } else{
      IV[r,c] = log(1)  
    }
  }
}

#give column 1 (species) a name
colnames(IV)[1] = "Species"


#assign rownames
rownames(IV) = IV$Species

#transpose the dataframe - the ordination function wants it the other way around (so rows become columns)
IV_t = t(IV[,2:ncol(IV)])

#add column names for each species
colnames(IV_t) = IV$Species

#----------------------------------
#ordination

#do the ordination
nmds = metaMDS(IV_t)
nmds

#get the scores
scores = scores(nmds)

str(scores)

#----------------------------------------------------------------
#Splitting out different groups from within the data

#The individual plots
plots = colnames(IV)[2:ncol(IV)]
#The plantation species (i.e. prad or pmen)
sp = sapply(plots, function(X){substr(x = X, start = 1, stop = 2)})
#The site where the plots are located
site = sapply(plots, function(X){substr(x = X, start = 3, stop = 6)})

#---------------------------------------------------------------------
#Analysis of multivariate homogeneity of group dispersion

#Creating the dissimilarity matrix
dist = vegdist((IV_t[,2:ncol(IV_t)]))
#Testing dispersion between the two plantation species
sp_disp <- betadisper(dist, sp)
anova(sp_disp)
permutest(sp_disp, pairwise = TRUE, permutations = 999)

#Testing dispersion between the eight sites
site_disp <- betadisper(dist, site)
anova(site_disp)
permutest(site_disp, pairwise = TRUE, permutations = 999)

#Next we do a PERMANOVA using the same dissimilarity matrix
#Testing variance between the two plantation species
perm_sp <- adonis2(dist ~ sp)
perm_sp

#Testing variance between the eight sites
perm_site <- adonis2(dist ~ site)
perm_site

#--------------------------------------------------
#Fitting environmental vectors onto the ordination

#read in a dataframe where each row is a plot, and each column a different environmental variable
env.vars = read.csv("Plot_Environmental_Data.csv", header = T)

#Now to fit the vectors. I want to split them into environmental and vegetation attributes
#Just environmental variables
SV.envfit <- envfit(nmds ~ Elevation+Distance.Seed.Source+Slope, env.vars)
SV.envfit

#Just vegetation attributes
VG.envfit <- envfit(nmds ~ Basal.Area+Stocking+Age+Total.IV+Native.IV+Exotic.IV+Total.Species, env.vars)
VG.envfit

#The environmental variable data but the coloumns are named with letter codes
env.codes = read.csv("Plot_Environmental_Data_Codes.csv", header = T)
#All the variables with letters codes
ALL.envfit <- envfit(nmds ~ E + DS + SL + BA + S + A + TI + NI + EI + SR, env.codes)


#---------------------------------
#Hierarchical cluster analysis

#use the same dissimilarity matrix as we used in the PERMDISP and PERMANOVA to do the cluster analysis
clust = hclust(dist, method = "complete")
#Making the cluster into a dendrogram
dend <- as.dendrogram(clust)
#Set some parameters for the dendrogram plot
dend <- dend %>%
  set("branches_lwd", 2.8) %>%
  set("labels_col", "black") %>%
  set("branches_k_color", value = c("#c51b7d", "#e9a3c9", "#f1a340", "#7fbf7b", "#1b7837"), k =5)

#Finding out which plot belongs to which cluster
dendgroups <- cutree(dend, k =5)

#---------------------------------------------------------------------------------------------
#Generalised linear mixed effects models

#subsetting out the two plantation species
PMEN <- subset(env.vars, Species == "PSEMEN")
PRAD <- subset(env.vars, Species == "PINRAD")

#Effect of plantation age on native importance value in P.menziesii
#The glmm
Age_NIV_PM <- lmer(formula = Native.IV ~ Age + (1|Stand_ID), data = PMEN)
summary(Age_NIV_PM)
#Getting the marginal and conditional R2 values
r2(Age_NIV_PM)
#Working out the confidence interval
conf1 <- confint.merMod(Age_NIV_PM, level = 0.95)
conf1

#Effect of plantation stocking on native importance value in P. menziesii
Stocking_NIV_PM <- lmer(formula = Native.IV ~ Stocking + (1|Stand_ID), data = PMEN)
summary(Stocking_NIV_PM)
r2(Stocking_NIV_PM)
conf2 <- confint.merMod(Stocking_NIV_PM, level = 0.95)
conf2

#Effect of plantation stocking on native importance value in P. radiata
Stocking_NIV_PR <- lmer(formula = Native.IV ~ Stocking + (1|Stand_ID), data = PRAD)
summary(Stocking_NIV_PR)
r2(Stocking_NIV_PR)
conf3 <- confint.merMod(Stocking_NIV_PR, level = 0.95)
conf3

#Effect of plantation age on native species richness in P. menziesii
Age_NSR_PM <- lmer(formula = Native.Species ~ Age + (1|Stand_ID), data = PMEN)
summary(Age_NSR_PM)
r2(Age_NSR_PM)
conf4 <- confint.merMod(Age_NSR_PM, level = 0.95)
conf4

#Effect of plantation stocking on native species richness in P. menziesii
Stocking_NSR_PM <- lmer(formula = Native.Species ~ Stocking + (1|Stand_ID), data = PMEN)
summary(Stocking_NSR_PM)
r2(Stocking_NSR_PM)
conf5 <- confint.merMod(Stocking_NSR_PM, level = 0.95)
conf5

#Effect of plantation stocking on native species richness in P. radiata
Stocking_NSR_PR <- lmer(formula = Native.Species ~ Stocking + (1|Stand_ID), data = PRAD)
summary(Stocking_NSR_PR)
r2(Stocking_NSR_PR)
conf6 <- confint.merMod(Stocking_NSR_PR, level = 0.95)
conf6
