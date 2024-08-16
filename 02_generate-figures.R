#Code for FIGURE2
#Read in data that is importance values of canopy species by plantation species
canopy_IV = read.table("Canopy_Species_IV.csv", header = T, sep = ",", stringsAsFactors = T)

png("Canopy_SPecies_Stacked.png", width = 50, height = 50, units = "cm", res = 1000, pointsize = 26)
par(mar = c(4,6,1,3))

barplot(as.matrix(canopy_IV),
        las=2,cex.names=.8, axes = T, horiz=T, xlab = "Species Importance Value", ylab = "",
        col = c("#41ae76", "#006d2c"))
legend(x = "bottomright",
       legend = c("IV in P. radiata stands", "IV in P. menziesii stands"),
       col = c("#41ae76", "#006d2c"),
       pch = 15,
       bty = "n",
       xpd = TRUE,
       horiz = F,
       cex = 1.3)

dev.off()
#------------------------------------------------------------------------------------------
#Code for FIGURE3
#Boxplots for environmental characteristics between overstorey species
png("boxplots.png", width = 25, height = 25, units = "cm", res = 600, pointsize = 18)
par(mfrow = c(3,1), mar = c(3,6,3,1))

boxplot(env.vars$Elevation ~ env.vars$Species, xlab = "", ylab = "Elevation (m)", col = "white",
        main = '***', horizontal = TRUE)
mtext("(a)", side = 2, at = par('usr')[4], line = 5, las = 1, cex = par('cex'))

boxplot(env.vars$Slope ~ env.vars$Species, xlab = "", ylab = "Slope (°)", col="white",
        main = '***', horizontal = TRUE)
mtext("(b)", side = 2, at = par('usr')[4], line = 5, las = 1, cex = par('cex'))

boxplot(env.vars$Distance.Seed.Source ~ env.vars$Species, xlab = "", ylab = "Distance to Seed Source (m)", col = "white",
        main = '**', horizontal = TRUE)
mtext("(c)", side = 2, at = par('usr')[4], line = 5, las = 1, cex = par('cex'))

dev.off()


#----------------------------------------------------------------------------------------
#Code for FIGURE4
  
png("Ordinations_Polygons.png", width = 45, height = 50, units = "cm", res = 1200, pointsize = 26)
par(mfrow = c(3,2), mar = c(4,5,5.5,1.5))

#Simple ordination with colour and shapes differing by plantation species
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], col = c('#525252', '#bdbdbd') [as.factor(sp)] ,xlab = "NMDS1", ylab = "NMDS2")
ordiellipse(nmds, groups = sp, draw = "polygon")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
mtext("(a)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#legend for B&W
legend(x = "top",
       inset = c(0,-0.2),
       legend = c("PSEMEN", "PINRAD"),
       col = c('#525252', '#bdbdbd'),
       pch = c(17, 16),
       bty = "n",
       xpd = TRUE,
       horiz = TRUE)

#Ordination with shape by plantation species and colour by site
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], 
     col = c('#377eb8', '#e41a1c', '#984ea3', '#f781bf', '#4daf4a', '#ff7f00', '#a65628', '#666666') [as.factor(site)],
     yaxt = "n", xlab = "NMDS1", ylab = "NMDS2")
ordiellipse(nmds,groups = site, draw = "polygon")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
mtext("(b)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#legend for colours
legend(x = "top",
       inset = c(0, -0.3),
       legend = c("BEC1", "BEC2", "ALLEN", "LAKE", "LIZA", "ROCK", "TONO", "WAKA"),
       col = c('#377eb8', '#e41a1c', '#984ea3', '#f781bf', '#4daf4a', '#ff7f00', '#a65628', '#666666'),
       pch = c(16, 16, 17, 17, 17, 16, 17, 16),
       bty = "n",
       xpd = TRUE,
       ncol = 3)

#Ordination with environmental variables
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], col = c('#525252', '#bdbdbd') [as.factor(sp)],
     xlab = "NMDS1", ylab = "NMDS2")
plot(SV.envfit, col = 'black')
axis(side = 1, labels = F)
axis(side = 2, labels = F)
mtext("(c)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Ordination with vegetation parameters
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], col = c('#525252', '#bdbdbd') [as.factor(sp)],
     yaxt = "n", xlab = "NMDS1", ylab = "NMDS2")
plot(VG.envfit, col = 'black')
axis(side = 1, labels = F)
axis(side = 2, labels = F)
mtext("(d)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#cluster diagram
plot(dend)
text(x = 20, y = -0.1, 'A', xpd = T, font =2, cex = 1.8, col = c("#c51b7d"))
text(x = 46, y = -0.1, 'B', xpd = T, font = 2, cex = 1.8, col = c("#e9a3c9" ))
text(x = 64, y = -0.1, 'C', xpd = T, font = 2, cex = 1.8, col = c("#f1a340"))
text(x = 88, y = -0.1, 'D', xpd = T, font = 2, cex = 1.8, col = c("#7fbf7b"))
text(x = 108, y = -0.1, 'E', xpd = T, font = 2, cex = 1.8, col = c("#1b7837"))
mtext(side = 1, line = 2, cex = 0.8, "RECCE sampling plots")
mtext(side = 2, line = 3, cex = 0.8, "Bray-Curtis dissimilarity")
mtext("(e)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Ordination coloured by dendrogram groups
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], 
     col = c("#7fbf37", "#1b7837", "#f1a340", "#c51b7d", "#e9a3c9") [as.factor(dendgroups)],
     yaxt = "n", xlab = "NMDS1", ylab = "NMDS2")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
mtext("(f)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

dev.off()


#------------------------------------------------------------
#Code for FIGURE5

#Making the ordination plot matrix
png("Species.Ordinations.png", width = 40, height = 32, units = "cm", res = 1000, pointsize = 26)
par(mfrow = c(3,3), mar = c(2,5,2,1))

#First panel is the ordination from Fig4 with point shape and colour by plantation species with the environmental variables fitted
plot(scores$sites[,1], scores$sites[,2], pch = c(17, 16) [as.factor(sp)], col = c('#525252', '#bdbdbd') [as.factor(sp)] ,xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
plot(ALL.envfit, col = 'black', cex = 0.8)
mtext("(a)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel b is P.radiata
#Start with a blank plot where everything is white
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Pinus radiata'))))
#Add grey filled in circles for the plots
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
#Go over the plots that contain p.rad in white to remove them from the plot
points(scores$sites[,1][which(IV_t[,"PINRAD"]>0)], scores$sites[,2][which(IV_t[,"PINRAD"]>0)],col = "white", pch = 16, cex = 0.6)
#add circles over plots with p.rad, with circle size relative to abundance of p.rad
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"PINRAD"], inches = 0.25, add = T)
#label panel
mtext("(b)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel c is for P.menziesii
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Pseudotsuga menziesii'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"PSEMEN"]>0)], scores$sites[,2][which(IV_t[,"PSEMEN"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"PSEMEN"], inches = 0.25, add = T)
mtext("(c)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel d is for P. racemosa
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Pterophylla racemosa'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"WEIRAC"]>0)], scores$sites[,2][which(IV_t[,"WEIRAC"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"WEIRAC"], inches = 0.25, add = T)
mtext("(d)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel e is for F. fusca
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Fuscospora fusca'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"FUSFUS"]>0)], scores$sites[,2][which(IV_t[,"FUSFUS"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"FUSFUS"], inches = 0.25, add = T)
mtext("(e)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel f is for M. ramiflorus
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Melicytus ramiflorus'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"MELRAM"]>0)], scores$sites[,2][which(IV_t[,"MELRAM"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"MELRAM"], inches = 0.25, add = T)
mtext("(f)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel g is for C. medullaris
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Cyathea medullaris'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"CYAMED"]>0)], scores$sites[,2][which(IV_t[,"CYAMED"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"CYAMED"], inches = 0.25, add = T)
mtext("(g)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel h is for C. dealbata
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Cyathea dealbata'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"CYADEA"]>0)], scores$sites[,2][which(IV_t[,"CYADEA"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"CYADEA"], inches = 0.25, add = T)
mtext("(h)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Panel i if for K. ericoides
plot(scores$sites[,1], scores$sites[,2],xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "white", pch = 1, cex = 0.6,
     main = substitute(paste(italic('Kunzea ericoides'))))
points(scores$sites[,1], scores$sites[,2],col = "darkgrey", pch = 16, cex = 0.6)
points(scores$sites[,1][which(IV_t[,"KUNEVE"]>0)], scores$sites[,2][which(IV_t[,"KUNEVE"]>0)],col = "white", pch = 16, cex = 0.6)
symbols(scores$sites[,1], scores$sites[,2], circles = IV_t[,"KUNEVE"], inches = 0.25, add = T)
mtext("(i)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

dev.off()

#---------------------------------------------------------------------
#Code for FIGURE6

#Scatter plots for Native Importance value and species richness

png("Scatterplot_Native.png", width = 50, height = 50, units = "cm", res = 1000, pointsize = 26)
par(mfrow = c(2,2), mar = c(4,4,3,3))


#Effect of age on native IV
plot(y = env.vars$Native.IV, x = env.vars$Age,
     pch = c(16, 17) [as.factor(env.vars$Species)],
     col = c('#969696','#525252') [as.factor(env.vars$Species)],
     xlab = "", xaxt = "n", ylab = "Native Importance Value")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
abline(a = -7.5206, b = 0.3257, col = "#525252", lwd = 4, lty = 2)
lines()
lines()
text(x = 43, y = 22, label = expression(paste( "PSEMEN ", R^2, "=", 0.384)), cex = 0.7)
mtext("(a)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Legend
legend(x = "bottom",
       inset = c(0, -0.2),
       legend = c("PSEMEN", "PINRAD"),
       col = c('#525252', '#969696'),
       pch = c(17,16),
       lty = c(2,1),
       bty = "n",
       cex = 1.2,
       lwd = 3,
       xpd = TRUE,
       horiz = TRUE)

#Effect of stocking on native IV
plot(y = env.vars$Native.IV, x = env.vars$Stocking,
     pch = c(16, 17) [as.factor(env.vars$Species)],
     col = c('#969696', '#525252') [as.factor(env.vars$Species)],
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
abline(a = 11.445692, b = -0.006337,col = "#525252", lwd = 4, lty = 2)
abline(a = 12.392019, b = -0.005282,col = "#969696", lwd = 4, lty = 1)
text(x = 1010, y = 21, label = expression(paste( "PINRAD ", R^2, "=", 0.067)), cex = 0.7)
text(x = 1010, y = 22, label = expression(paste( "PSEMEN ", R^2, "=", 0.286)), cex = 0.7)
mtext("(b)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Effect of age on native species richness
plot(y = env.vars$Native.Species, x = env.vars$Age,
     pch = c(16, 17) [as.factor(env.vars$Species)],
     col = c('#969696', '#525252') [as.factor(env.vars$Species)],
     xlab = "Age (years)", ylab = "Native Species Richness")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
abline(a = -7.52121, b = 0.37080,col = "#525252", lwd = 4, lty = 2)
text(x = 47, y = 21, label = expression(paste( "PSEMEN ", R^2, "=", 0.527)), cex = 0.7)
mtext("(c)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))

#Effect of stocking on native species richness
plot(y = env.vars$Native.Species, x = env.vars$Stocking,
     pch = c(16, 17) [as.factor(env.vars$Species)],
     col = c('#969696', '#525252') [as.factor(env.vars$Species)],
     yaxt = "n", xlab = "Stocking (stems/ha)", ylab = "")
axis(side = 1, labels = F)
axis(side = 2, labels = F)
abline(a = 12.329033,b = -0.003331,col = "#525252", lwd = 4, lty = 2)
abline(a = 12.276018,b = -0.004041,col = "#969696", lwd = 4, lty = 1)
text(x = 1010, y = 20, label = expression(paste( "PINRAD ", R^2, "=", 0.075)), cex = 0.7)
text(x = 1010, y = 21, label = expression(paste( "PSEMEN ", R^2, "=", 0.058)), cex = 0.7)
mtext("(d)", side = 2, at = par('usr')[4], line = 3, las = 1, cex = par('cex'))


dev.off()