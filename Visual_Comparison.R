#Gilang Hardadi

library(ggplot2)
library(reshape)
library(cowplot)
library(ggpubr)

elas <- read.csv("Elastcomparison.csv")
delas <- read.csv("dElastcomparison.csv")

KK <- elas[c(1,c(2,12))]
names(KK) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(KK$Sector, levels = KK$Sector))
KK <- melt(KK, id="Sector")
names(KK) <- c("Sector","Source","Elasticity")
KK$Sector <- factor(KK$Sector,sector)

dKK <- cbind(delas[1],rep(0,95),delas[c(2)])
names(dKK) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dKK$Sector, levels = dKK$Sector))
dKK <- melt(dKK, id="Sector")
names(dKK) <- c("Sector","Source","Elasticity")
dKK$Sector <- factor(dKK$Sector,sector)

KK1 <- ggplot(KK, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Own-Price Elasticity K-K") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2, 0.5), breaks = seq(-2,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = KK$Elasticity - 2 * dKK$Elasticity, xmax = KK$Elasticity + 2 * dKK$Elasticity))

EE <- elas[c(1,c(4,24))]
names(EE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(EE$Sector, levels = EE$Sector))
EE <- melt(EE, id="Sector")
names(EE) <- c("Sector","Source","Elasticity")
EE$Sector <- factor(EE$Sector,sector)

dEE <- cbind(delas[1],rep(0,95),delas[c(14)])
names(dEE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dEE$Sector, levels = dEE$Sector))
dEE <- melt(dEE, id="Sector")
names(dEE) <- c("Sector","Source","Elasticity")
dEE$Sector <- factor(dEE$Sector,sector)

EE1 <- ggplot(EE, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Own-Price Elasticity E-E") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2, 0.5), breaks = seq(-2,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = EE$Elasticity - 2 * dEE$Elasticity, xmax = EE$Elasticity + 2 * dEE$Elasticity))

MM <- elas[c(1,c(5,30))]
names(MM) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(MM$Sector, levels = MM$Sector))
MM <- melt(MM, id="Sector")
names(MM) <- c("Sector","Source","Elasticity")
MM$Sector <- factor(MM$Sector,sector)

dMM <- cbind(delas[1],rep(0,95),delas[c(20)])
names(dMM) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dMM$Sector, levels = dMM$Sector))
dMM <- melt(dMM, id="Sector")
names(dMM) <- c("Sector","Source","Elasticity")
dMM$Sector <- factor(dMM$Sector,sector)

MM1 <- ggplot(MM, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Own-Price Elasticity M-M") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2, 0.5), breaks = seq(-2,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-2.0,0.5),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = MM$Elasticity - 2 * dMM$Elasticity, xmax = MM$Elasticity + 2 * dMM$Elasticity))

KL <- elas[c(1,c(6,13))]
names(KL) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(KL$Sector, levels = KL$Sector))
KL <- melt(KL, id="Sector")
names(KL) <- c("Sector","Source","Elasticity")
KL$Sector <- factor(KL$Sector,sector)

dKL <- cbind(delas[1],rep(0,95),delas[c(3)])
names(dKL) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dKL$Sector, levels = dKL$Sector))
dKL <- melt(dKL, id="Sector")
names(dKL) <- c("Sector","Source","Elasticity")
KL$Sector <- factor(dKL$Sector,sector)

KL1 <- ggplot(KL, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity K-L") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.5, 2), breaks = seq(-0.5,2,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.75, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.75, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.75, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 1.75, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 1.75, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = KL$Elasticity - 2 * dKL$Elasticity, xmax = KL$Elasticity + 2 * dKL$Elasticity))

KE <- elas[c(1,c(7,14))]
names(KE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(KE$Sector, levels = KE$Sector))
KE <- melt(KE, id="Sector")
names(KE) <- c("Sector","Source","Elasticity")
KE$Sector <- factor(KE$Sector,sector)

dKE <- cbind(delas[1],rep(0,95),delas[c(4)])
names(dKE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dKE$Sector, levels = dKE$Sector))
dKE <- melt(dKE, id="Sector")
names(dKE) <- c("Sector","Source","Elasticity")
dKE$Sector <- factor(dKE$Sector,sector)

KE1 <- ggplot(KE, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity K-E") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.6, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.5, 2), breaks = seq(-0.5,2,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.75, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.75, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.75, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 1.75, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 1.75, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = KE$Elasticity - 2 * dKE$Elasticity, xmax = KE$Elasticity + 2 * dKE$Elasticity))

LE <- elas[c(1,c(8,19))]
names(LE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(LE$Sector, levels = LE$Sector))
LE <- melt(LE, id="Sector")
names(LE) <- c("Sector","Source","Elasticity")
LE$Sector <- factor(LE$Sector,sector)

dLE <- cbind(delas[1],rep(0,95),delas[c(9)])
names(dLE) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dLE$Sector, levels = dLE$Sector))
dLE <- melt(dLE, id="Sector")
names(dLE) <- c("Sector","Source","Elasticity")
dLE$Sector <- factor(dLE$Sector,sector)

LE1 <- ggplot(LE, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Own-Price Elasticity L-E") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.5, 2), breaks = seq(-0.5,2,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.75, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.75, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.75, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 1.75, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 1.75, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = LE$Elasticity - 2 * dLE$Elasticity, xmax = LE$Elasticity + 2 * dLE$Elasticity))

KM <- elas[c(1,c(9,15))]
names(KM) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(KM$Sector, levels = KM$Sector))
KM <- melt(KM, id="Sector")
names(KM) <- c("Sector","Source","Elasticity")
KM$Sector <- factor(KM$Sector,sector)

dKM <- cbind(delas[1],rep(0,95),delas[c(4)])
names(dKM) <- c("Sector","Kirchner et al., 2018", "Own Results")
sector <- as.character(factor(dKM$Sector, levels = dKM$Sector))
dKM <- melt(dKM, id="Sector")
names(dKM) <- c("Sector","Source","Elasticity")
dKM$Sector <- factor(dKM$Sector,sector)

KM1 <- ggplot(KM, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity K-M") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.5, 2), breaks = seq(-0.5,2,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.5,2.0),95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.75, y = "i01.f i01.g i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.75, y = "i14.1", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.75, y = "i25", size = 4) +
  annotate("text", label = "Electricity", x = 1.75, y = "i40.11.f", size = 4) +
  annotate("text", label = "Services", x = 1.75, y = "i72", size = 4) +
  geom_errorbarh(aes(xmin = KM$Elasticity - 2 * dKM$Elasticity, xmax = KM$Elasticity + 2 * dKM$Elasticity))

plot_grid(KK1,EE1,MM1,KL1,KE1,KM1, ncol = 3, nrow = 2)
ggsave("ElastComp.tiff", width = 21.33, height = 11.3, dpi = 320)

KLKS <- elas[c(1,c(13,17,16,32))]
names(KLKS) <- c("Sector","KL", "LK", "KS", "SK")
sector <- as.character(factor(KLKS$Sector, levels = KLKS$Sector))
KLKS <- melt(KLKS, id="Sector")
names(KLKS) <- c("Sector","Source","Elasticity")
KLKS$Sector <- factor(KLKS$Sector,sector)

dKLKS <- cbind(delas[c(1,3,7,6,22)])
names(dKLKS) <- c("Sector","KL", "LK", "KS", "SK")
sector <- as.character(factor(dKLKS$Sector, levels = dKLKS$Sector))
dKLKS <- melt(dKLKS, id="Sector")
names(dKLKS) <- c("Sector","Source","Elasticity")
KLKS$Sector <- factor(dKLKS$Sector,sector)

KLKS1 <- ggplot(KLKS, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity K-L, L-K, K-S, S-K") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.3,1.2), breaks = seq(-0.3,1.2,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.05, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = 1.05, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = 1.05, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = 1.05, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = 1.05, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = KLKS$Elasticity - 2 * dKLKS$Elasticity, xmax = KLKS$Elasticity + 2 * dKLKS$Elasticity))

KEKM <- elas[c(1,c(14,22,15,27))]
names(KEKM) <- c("Sector","KE", "EK", "KM", "MK")
sector <- as.character(factor(KEKM$Sector, levels = KEKM$Sector))
KEKM <- melt(KEKM, id="Sector")
names(KEKM) <- c("Sector","Source","Elasticity")
KEKM$Sector <- factor(KEKM$Sector,sector)

dKEKM <- cbind(delas[c(1,4,12,5,17)])
names(dKEKM) <- c("Sector","KE", "EK", "KM", "MK")
sector <- as.character(factor(dKEKM$Sector, levels = dKEKM$Sector))
dKEKM <- melt(dKEKM, id="Sector")
names(dKEKM) <- c("Sector","Source","Elasticity")
dKEKM$Sector <- factor(dKEKM$Sector,sector)

KEKM1 <- ggplot(KEKM, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity K-E, E-K, K-M, M-K") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.3,1.2), breaks = seq(-0.3,1.2,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.05, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = 1.05, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = 1.05, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = 1.05, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = 1.05, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = KEKM$Elasticity - 2 * dKEKM$Elasticity, xmax = KEKM$Elasticity + 2 * dKEKM$Elasticity))

LELM <- elas[c(1,c(19,23,20,28))]
names(LELM) <- c("Sector","LE", "EL", "LM", "ML")
sector <- as.character(factor(LELM$Sector, levels = LELM$Sector))
LELM <- melt(LELM, id="Sector")
names(LELM) <- c("Sector","Source","Elasticity")
LELM$Sector <- factor(LELM$Sector,sector)

dLELM <- cbind(delas[c(1,9,13,10,18)])
names(dLELM) <- c("Sector","LE", "EL", "LM", "ML")
sector <- as.character(factor(dLELM$Sector, levels = dLELM$Sector))
dLELM <- melt(dLELM, id="Sector")
names(dLELM) <- c("Sector","Source","Elasticity")
LELM$Sector <- factor(dLELM$Sector,sector)

LELM1 <- ggplot(LELM, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity L-E, E-L, L-M, M-L") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.3,1.2), breaks = seq(-0.3,1.2,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.05, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = 1.05, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = 1.05, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = 1.05, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = 1.05, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = LELM$Elasticity - 2 * dLELM$Elasticity, xmax = LELM$Elasticity + 2 * dLELM$Elasticity))

ESMS <- elas[c(1,c(26,34,31,35))]
names(ESMS) <- c("Sector","ES", "SE", "MS", "SM")
sector <- as.character(factor(ESMS$Sector, levels = ESMS$Sector))
ESMS <- melt(ESMS, id="Sector")
names(ESMS) <- c("Sector","Source","Elasticity")
ESMS$Sector <- factor(ESMS$Sector,sector)

dESMS <- cbind(delas[c(1,16,24,21,25)])
names(dESMS) <- c("Sector","ES", "SE", "MS", "SM")
sector <- as.character(factor(dESMS$Sector, levels = dESMS$Sector))
dESMS <- melt(dESMS, id="Sector")
names(dESMS) <- c("Sector","Source","Elasticity")
ESMS$Sector <- factor(dESMS$Sector,sector)

ESMS1 <- ggplot(ESMS, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity E-S, S-E, M-S, S-M") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.3,1.2), breaks = seq(-0.3,1.2,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.05, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = 1.05, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = 1.05, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = 1.05, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = 1.05, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = ESMS$Elasticity - 2 * dESMS$Elasticity, xmax = ESMS$Elasticity + 2 * dESMS$Elasticity))

LSEM <- elas[c(1,c(21,33,25,29))]
names(LSEM) <- c("Sector","LS", "SL", "EM", "ME")
sector <- as.character(factor(LSEM$Sector, levels = LSEM$Sector))
LSEM <- melt(LSEM, id="Sector")
names(LSEM) <- c("Sector","Source","Elasticity")
LSEM$Sector <- factor(LSEM$Sector,sector)

dLSEM <- cbind(delas[c(1,11,23,15,19)])
names(dLSEM) <- c("Sector","LS", "SL", "EM", "ME")
sector <- as.character(factor(dLSEM$Sector, levels = dLSEM$Sector))
dLSEM <- melt(dLSEM, id="Sector")
names(dLSEM) <- c("Sector","Source","Elasticity")
LSEM$Sector <- factor(dLSEM$Sector,sector)

LSEM1 <- ggplot(LSEM, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Cross-Price Elasticity L-S, S-L, E-M, M-E") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-0.3,1.2), breaks = seq(-0.3,1.2,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(c(-0.3,1.2),190), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.05, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = 1.05, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = 1.05, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = 1.05, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = 1.05, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = LSEM$Elasticity - 2 * dLSEM$Elasticity, xmax = LSEM$Elasticity + 2 * dLSEM$Elasticity))

OWN <- elas[c(1,c(12,18,24,30,36))]
names(OWN) <- c("Sector","KK", "LL", "EE", "MM", "SS")
sector <- as.character(factor(OWN$Sector, levels = OWN$Sector))
OWN <- melt(OWN, id="Sector")
names(OWN) <- c("Sector","Source","Elasticity")
OWN$Sector <- factor(OWN$Sector,sector)

dOWN <- cbind(delas[c(1,2,8,14,20,26)])
names(dOWN) <- c("Sector","KK", "LL", "EE", "MM", "SS")
sector <- as.character(factor(dOWN$Sector, levels = dOWN$Sector))
dOWN <- melt(dOWN, id="Sector")
names(dOWN) <- c("Sector","Source","Elasticity")
OWN$Sector <- factor(dOWN$Sector,sector)

OWN1 <- ggplot(OWN, aes(y = Sector, x = Elasticity, fill = Source, colour = Source)) +
  labs(x="Own-Price Elasticity") + 
  theme(axis.title = element_text(size=14),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = c(0.14, 0.7),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1.5, -0), breaks = seq(-1.5,-0,0.3)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(seq(-1.5,-0,0.375), 95), y = "i06"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(seq(-1.5,-0,0.375), 95), y = "i14.5"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(seq(-1.5,-0,0.375), 95), y = "i38"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = 15) + geom_line(aes(x = rep(seq(-1.5,-0,0.375), 95), y = "i43"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = -0.15, y = "i01.f i01.g i01.h", size = 3.5) +
  annotate("text", label = "Mining", x = -0.15, y = "i14.1", size = 3.5) +
  annotate("text", label = "Manufacturing", x = -0.15, y = "i25", size = 3.5) +
  annotate("text", label = "Electricity", x = -0.15, y = "i40.11.f", size = 3.5) +
  annotate("text", label = "Services", x = -0.15, y = "i72", size = 3.5) +
  geom_errorbarh(aes(xmin = OWN$Elasticity - 2 * dOWN$Elasticity, xmax = OWN$Elasticity + 2 * dOWN$Elasticity))

plot_grid(OWN1,KLKS1,KEKM1,LELM1,ESMS1,LSEM1,ncol = 3, nrow = 2)
ggsave("ElastComp2_New.tiff", width = 19.2, height = 10.4, dpi = 320)
