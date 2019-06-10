### This script contains code used to build analyze versatility, based on PageRank, calculated in muxViz,
### or other metrics that may be extracted. It includes calculation of correlation between multilayer
### versatility with its aggregated counterpart or strength (or other you may extract) each disperser is present.

rm(list = ls())
library(ggplot2)
library(Hmisc)
library(reshape2)
library(stringr)

### Load and prepare data output from muxViz
# Set working directory
setwd("G:/Multilayer_Curso/muxViz-master_2018/muxViz-master/data/GNP/ext_edgelist_ex/output")

# load species centrality data for the multilayer and single layer
dispersers.multi <- read.table(file = "Centrality_Multilayer_Time.csv", header = T, sep = ";")
dispersers.single <- read.table(file = "Centrality_Monolayer_Time.csv", header = T, sep = ";")

# Arrange the data
dispersers.multi <- dispersers.multi[which(dispersers.multi$Layer == "1-Multi"),c(1,3,4,10)]
dispersers.multi$Layer <- as.character(dispersers.multi$Layer)
dispersers.multi$Layer[grepl("1-Multi", dispersers.multi$Layer)] <- "Multilayer"
dispersers.multi$Layer <- as.factor(dispersers.multi$Layer)
colnames(dispersers.multi)[2] <- "Species"

dispersers.single <- dispersers.single[which(dispersers.single$Layer == "Agrr"
                                             |!dispersers.single$Degree == "0"),c(1,3,7,10)]
dispersers.single$Layer <- as.character(dispersers.single$Layer)
dispersers.single$Layer[grepl("Aggr", dispersers.single$Layer)] <- "Aggregated"
dispersers.single$Layer <- as.factor(dispersers.single$Layer)
#dispersers.single <- dispersers.single[,c(1,3,7,10)]
colnames(dispersers.single)[2] <- "Species"

# Name the different time layers as you wish in the following line (or to habitat names)
levels(dispersers.single$Layer) <- c("Season 1", "Season 2", "Season 3", "Season 4", "Aggregated")
centrality.dispersers <- rbind(dispersers.single, dispersers.multi)
centrality.dispersers$Layer <-
  factor(centrality.dispersers$Layer,
         levels = c("Multilayer", "Aggregated", "Season 1", "Season 2", "Season 3", "Season 4"))
colnames(centrality.dispersers)[3] <- "Strength"
colnames(centrality.dispersers)[4] <- "Versatility"

# centrality.dispersers.wide <-
#   reshape(centrality.dispersers, v.names = c("Versatility"), idvar = "Species", 
#                                  timevar = "Layer", direction = "wide")

centrality.dispersers.wide <- reshape(centrality.dispersers,
                  idvar = "Species", timevar = "Layer", direction = "wide")
centrality.dispersers.wide$Species <- str_replace(centrality.dispersers.wide$Species, '_', ' ')

centrality.dispersers.long <- reshape(centrality.dispersers.wide, v.names = c("Versatility", "Strength"),
                                      idvar = "Species", direction = "long")
rownames(centrality.dispersers.long) <- c(1:nrow(centrality.dispersers.long))
centrality.dispersers.long$Species <- str_replace(centrality.dispersers.long$Species, '_', ' ')
colnames(centrality.dispersers.long)[3:4] <- c("Strength", "Versatility")

# centrality.dispersers.long <- reshape(zz.wide, v.names = c("Versatility"),
#                                       idvar = "Species", direction = "long")


# centrality.dispersers.long <- reshape(centrality.dispersers.wide, v.names = c("Versatility"),
#                                  idvar = "Species", direction = "long")
# 
# #
# refcols <- c("Layer")
# centrality.dispersers.long <-
#   centrality.dispersers.long[, c(refcols, setdiff(names(centrality.dispersers.long), refcols))]
# rownames(centrality.dispersers.long) <- c(1:nrow(centrality.dispersers.long))
# centrality.dispersers.long$Species <- str_replace(centrality.dispersers.long$Species, '_', ' ')
# colnames(zz.long)[3:4] <- c("Strength", "Versatility")


### Analyzis of species versatility and visualization.
# 1 - Species importance for multilayer structure
ggplot(centrality.dispersers.wide, aes(x = reorder(Species, -Versatility.Multilayer), y = Versatility.Multilayer)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Species")

# 2 - Calculate the correlation between Multilayer and Aggregated networks
cor.result <- cor.test(centrality.dispersers.wide$Versatility.Multilayer,
                       centrality.dispersers.wide$Versatility.Aggregated,
                       method = "spearman")

# Plot the data
par(mfrow = c(1,1), mar = c(4,4,0.5,0.5))
plot(centrality.dispersers.wide$Versatility.Multilayer, centrality.dispersers.wide$Versatility.Aggregated,
     xlim = c(0.2,1.0), ylim = c(0.2,1.0), las = 1, xlab = "Multilayer versatility",
     ylab = "Aggregated versatility", pch = 16, cex = 0.8, col = "darkblue")
abline(a = 0,b = 1, lty = 4, lwd = 1.0, col = "red")
text(0.4, 0.425, srt = 33, labels = "x = y", cex = 0.8)
text(0.34, 1.0, cex = 0.9, labels = paste("rho =", round(cor.result$estimate, 2),
                                          ", p =", round(cor.result$p.value, 3)))

# 3 -  Comparison and visualization of per layer versatility values (single layer) of each species.
centrality.dispersers.single <-
  centrality.dispersers.long[which(!centrality.dispersers.long$Layer == "Aggregated" &
                                     !centrality.dispersers.long$Layer == "Multilayer"),]

ggplot(centrality.dispersers.single, aes(x = reorder(Species, Versatility), y = Versatility, fill = Layer)) +
  geom_col(position = "dodge", color = NA) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("black", "darkgreen", "purple", "orange")) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "top", legend.title=element_blank()) + labs(x = "Species")

        