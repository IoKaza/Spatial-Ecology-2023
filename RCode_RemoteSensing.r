# αν θελω να εγκαταστήρω πακατζ το γράφω στο κάτω πλαίσιο στην Ρ όπως ε΄ναι κάτω.
install.packages("raster")

#Κώδικας 
library(raster)
library(RStoolbox)

setwd("C:/Users/liaka/Documents/Ready for DA/files to use/monitorng ecosystems")

p224r63_2011 <- brick("p224r63_2011_masked.grd")
library(raster)
p224r63_2011


plot(p224r63_2011)
cl <- colorRampPalette(c('black','grey','light grey'))(100)
plot(p224r63_2011, col=cl)
# Χρησιμοποιώ την παρακάτω εντόλη για να "καθαρίσω" τα προηγούμενα πλοτ
dev.off()
# Ξανακάνω το πλότ που με ενδιαφέρει



> par(mfrow=c(1,2))
> plot(p224r63_2011$B1_sre[[1]], col=cl)
> plot(p224r63_2011$B1_sre[[2]], col=cl)
Error in h(simpleError(msg, call)) : 
  error in evaluating the argument 'x' in selecting a method for function 'plot': not a valid subset
