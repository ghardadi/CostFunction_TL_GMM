#Gilang Hardadi

library(ggplot2)
library(reshape)
library(cowplot)
library(ggpubr)

elas <- read.csv("Elasticities.csv")
delas <- read.csv("dElasticities.csv")

KK <- elas[c(1,seq(6,106,25))]
names(KK) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(KK$Sector, levels = KK$Sector))
KK <- melt(KK, id="Sector")
names(KK) <- c("Sector","Level","Elasticity")
KK$Sector <- factor(KK$Sector,sector)

dKK <- delas[c(1,seq(6,106,25))]
names(dKK) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dKK$Sector, levels = dKK$Sector))
dKK <- melt(dKK, id="Sector")
names(dKK) <- c("Sector","Level","Elasticity")
dKK$Sector <- factor(dKK$Sector,sector)

KK$xmin <- KK$Elasticity - 2 * dKK$Elasticity
for (i in c(1:length(KK$xmin))){
  if ((KK$xmin[i] < -2.0) & (!is.na(KK$xmin[i]))){
    KK$xmin[i] = -2.0
  }
}

KK$xmax <- KK$Elasticity + 2 * dKK$Elasticity
for (i in c(1:length(KK$xmax))){
  if ((KK$xmax[i] > 0.5) & (!is.na(KK$xmax[i]))){
    KK$xmax[i] = 0.5
  }
}

KK1 <- ggplot(KK, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Own-Price Elasticity K-K") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2.0, 0.5), breaks = seq(-2.0,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = KK$xmin, xmax = KK$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

LL <- elas[c(1,seq(12,112,25))]
names(LL) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(LL$Sector, levels = LL$Sector))
LL <- melt(LL, id="Sector")
names(LL) <- c("Sector","Level","Elasticity")
LL$Sector <- factor(LL$Sector,sector)

dLL <- delas[c(1,seq(12,112,25))]
names(dLL) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dLL$Sector, levels = dLL$Sector))
dLL <- melt(dLL, id="Sector")
names(dLL) <- c("Sector","Level","Elasticity")
dLL$Sector <- factor(dLL$Sector,sector)

LL$xmin <- LL$Elasticity - 2 * dLL$Elasticity
for (i in c(1:length(LL$xmin))){
  if ((LL$xmin[i] < -2.0) & (!is.na(LL$xmin[i]))){
    LL$xmin[i] = -2.0
  }
}

LL$xmax <- LL$Elasticity + 2 * dLL$Elasticity
for (i in c(1:length(LL$xmax))){
  if ((LL$xmax[i] > 0.5) & (!is.na(LL$xmax[i]))){
    LL$xmax[i] = 0.5
  }
}

LL1 <- ggplot(LL, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Own-Price Elasticity L-L") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2.0, 0.5), breaks = seq(-2.0,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = LL$xmin, xmax = LL$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

EE <- elas[c(1,seq(18,118,25))]
names(EE) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(EE$Sector, levels = EE$Sector))
EE <- melt(EE, id="Sector")
names(EE) <- c("Sector","Level","Elasticity")
EE$Sector <- factor(EE$Sector,sector)

dEE <- delas[c(1,seq(18,118,25))]
names(dEE) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dEE$Sector, levels = dEE$Sector))
dEE <- melt(dEE, id="Sector")
names(dEE) <- c("Sector","Level","Elasticity")
dKK$Sector <- factor(dEE$Sector,sector)

EE$xmin <- EE$Elasticity - 2 * dEE$Elasticity
for (i in c(1:length(EE$xmin))){
  if ((EE$xmin[i] < -2.0) & (!is.na(EE$xmin[i]))){
    EE$xmin[i] = -2.0
  }
}

EE$xmax <- EE$Elasticity + 2 * dEE$Elasticity
for (i in c(1:length(EE$xmax))){
  if ((EE$xmax[i] > 0.5) & (!is.na(EE$xmax[i]))){
    EE$xmax[i] = 0.5
  }
}

EE1 <- ggplot(EE, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Own-Price Elasticity E-E") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2.0, 0.5), breaks = seq(-2.0,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = EE$xmin, xmax = EE$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

MM <- elas[c(1,seq(24,124,25))]
names(MM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(MM$Sector, levels = MM$Sector))
MM <- melt(MM, id="Sector")
names(MM) <- c("Sector","Level","Elasticity")
MM$Sector <- factor(MM$Sector,sector)

dMM <- delas[c(1,seq(24,124,25))]
names(dMM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dMM$Sector, levels = dMM$Sector))
dMM <- melt(dMM, id="Sector")
names(dMM) <- c("Sector","Level","Elasticity")
dMM$Sector <- factor(dMM$Sector,sector)

MM$xmin <- MM$Elasticity - 2 * dMM$Elasticity
for (i in c(1:length(MM$xmin))){
  if ((MM$xmin[i] < -2.0) & (!is.na(MM$xmin[i]))){
    MM$xmin[i] = -2.0
  }
}

MM$xmax <- MM$Elasticity + 2 * dMM$Elasticity
for (i in c(1:length(MM$xmax))){
  if ((MM$xmax[i] > 0.5) & (!is.na(MM$xmax[i]))){
    MM$xmax[i] = 0.5
  }
}

MM1 <- ggplot(MM, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Own-Price Elasticity M-M") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2.0, 0.5), breaks = seq(-2.0,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = MM$xmin, xmax = MM$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

SS <- elas[c(1,seq(30,130,25))]
names(SS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(SS$Sector, levels = SS$Sector))
SS <- melt(SS, id="Sector")
names(SS) <- c("Sector","Level","Elasticity")
SS$Sector <- factor(SS$Sector,sector)

dSS <- delas[c(1,seq(30,130,25))]
names(dSS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dSS$Sector, levels = dSS$Sector))
dSS <- melt(dSS, id="Sector")
names(dSS) <- c("Sector","Level","Elasticity")
dSS$Sector <- factor(dSS$Sector,sector)

SS$xmin <- SS$Elasticity - 2 * dSS$Elasticity
for (i in c(1:length(SS$xmin))){
  if ((SS$xmin[i] < -2.0) & (!is.na(SS$xmin[i]))){
    SS$xmin[i] = -2.0
  }
}

SS$xmax <- SS$Elasticity + 2 * dSS$Elasticity
for (i in c(1:length(SS$xmax))){
  if ((SS$xmax[i] > 0.5) & (!is.na(SS$xmax[i]))){
    SS$xmax[i] = 0.5
  }
}

SS1 <- ggplot(SS, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Own-Price Elasticity S-S") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-2.0, 0.5), breaks = seq(-2.0,0.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-2.0,0.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 0.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 0.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 0.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 0.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 0.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = SS$xmin, xmax = SS$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

#Legend Only
Legend <- as_ggplot(get_legend(
  ggplot(SS, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
    labs(x="Own-Price Elasticity S-S", colour="Aggregation Level") + 
    theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
          axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "right",
          legend.title = element_text(size = 22),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1,"cm"),
          legend.key.width = unit(1,"cm"),
          panel.background = element_rect(fill = "grey97"),
          panel.grid.major.x = element_line(colour = "black")) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1,1,0.25)) +
    scale_shape_discrete(solid=T) +
    scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
    geom_point(size = 5, shape = 15)))

# Convert to a ggplot and print
plot_grid(KK1,LL1,EE1,MM1,SS1,Legend, ncol = 3, nrow = 2)
ggsave("ElasticitiesFin.tiff", width = 24, height = 12.7, dpi = 320)

KL <- elas[c(1,seq(7,107,25))]
names(KL) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(KL$Sector, levels = KL$Sector))
KL <- melt(KL, id="Sector")
names(KL) <- c("Sector","Level","Elasticity")
KL$Sector <- factor(KL$Sector,sector)

dKL <- delas[c(1,seq(7,107,25))]
names(dKL) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dKL$Sector, levels = dKL$Sector))
dKL <- melt(dKL, id="Sector")
names(dKL) <- c("Sector","Level","Elasticity")
dKL$Sector <- factor(dKL$Sector,sector)

KL$xmin <- KL$Elasticity - 2 * dKL$Elasticity
for (i in c(1:length(KL$xmin))){
  if ((KL$xmin[i] < -1.0) & (!is.na(KL$xmin[i]))){
    KL$xmin[i] = -1.0
  }
}

KL$xmax <- KL$Elasticity + 2 * dKL$Elasticity
for (i in c(1:length(KL$xmax))){
  if ((KL$xmax[i] > 1.5) & (!is.na(KL$xmax[i]))){
    KL$xmax[i] = 1.5
  }
}

KL1 <- ggplot(KL, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Cross-Price Elasticity K-L") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1,1.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 1.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 1.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = KL$xmin, xmax = KL$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

KM <- elas[c(1,seq(9,109,25))]
names(KM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(KM$Sector, levels = KM$Sector))
KM <- melt(KM, id="Sector")
names(KM) <- c("Sector","Level","Elasticity")
KM$Sector <- factor(KM$Sector,sector)

dKM <- delas[c(1,seq(9,109,25))]
names(dKM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dKM$Sector, levels = dKM$Sector))
dKM <- melt(dKM, id="Sector")
names(dKM) <- c("Sector","Level","Elasticity")
dKM$Sector <- factor(dKM$Sector,sector)

KM$xmin <- KM$Elasticity - 2 * dKM$Elasticity
for (i in c(1:length(KM$xmin))){
  if ((KM$xmin[i] < -1.0) & (!is.na(KM$xmin[i]))){
    KM$xmin[i] = -1.0
  }
}

KM$xmax <- KM$Elasticity + 2 * dKM$Elasticity
for (i in c(1:length(KM$xmax))){
  if ((KM$xmax[i] > 1.5) & (!is.na(KM$xmax[i]))){
    KM$xmax[i] = 1.5
  }
}

KM1 <- ggplot(KM, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Cross-Price Elasticity K-M") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1,1.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 1.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 1.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = KM$xmin, xmax = KM$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

LS <- elas[c(1,seq(15,115,25))]
names(LS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(LS$Sector, levels = LS$Sector))
LS <- melt(LS, id="Sector")
names(LS) <- c("Sector","Level","Elasticity")
LS$Sector <- factor(LS$Sector,sector)

dLS <- delas[c(1,seq(15,115,25))]
names(dLS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dLS$Sector, levels = dLS$Sector))
dLS <- melt(dLS, id="Sector")
names(dLS) <- c("Sector","Level","Elasticity")
dLS$Sector <- factor(dLS$Sector,sector)

LS$xmin <- LS$Elasticity - 2 * dLS$Elasticity
for (i in c(1:length(LS$xmin))){
  if ((LS$xmin[i] < -1.0) & (!is.na(LS$xmin[i]))){
    LS$xmin[i] = -1.0
  }
}

LS$xmax <- LS$Elasticity + 2 * dLS$Elasticity
for (i in c(1:length(LS$xmax))){
  if ((LS$xmax[i] > 1.5) & (!is.na(LS$xmax[i]))){
    LS$xmax[i] = 1.5
  }
}

LS1 <- ggplot(LS, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Cross-Price Elasticity L-S") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1,1.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 1.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 1.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = LS$xmin, xmax = LS$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

EM <- elas[c(1,seq(19,119,25))]
names(EM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(EM$Sector, levels = EM$Sector))
EM <- melt(EM, id="Sector")
names(EM) <- c("Sector","Level","Elasticity")
EM$Sector <- factor(EM$Sector,sector)

dEM <- delas[c(1,seq(19,119,25))]
names(dEM) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dEM$Sector, levels = dEM$Sector))
dEM <- melt(dEM, id="Sector")
names(dEM) <- c("Sector","Level","Elasticity")
dEM$Sector <- factor(dEM$Sector,sector)

EM$xmin <- EM$Elasticity - 2 * dEM$Elasticity
for (i in c(1:length(EM$xmin))){
  if ((EM$xmin[i] < -1.0) & (!is.na(EM$xmin[i]))){
    EM$xmin[i] = -1.0
  }
}

EM$xmax <- EM$Elasticity + 2 * dEM$Elasticity
for (i in c(1:length(EM$xmax))){
  if ((EM$xmax[i] > 1.5) & (!is.na(EM$xmax[i]))){
    EM$xmax[i] = 1.5
  }
}

EM1 <- ggplot(EM, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Cross-Price Elasticity E-M") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1,1.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 1.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 1.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = EM$xmin, xmax = EM$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

MS <- elas[c(1,seq(25,125,25))]
names(MS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(MS$Sector, levels = MS$Sector))
MS <- melt(MS, id="Sector")
names(MS) <- c("Sector","Level","Elasticity")
MS$Sector <- factor(MS$Sector,sector)

dMS <- delas[c(1,seq(25,125,25))]
names(dMS) <- c("Sector","4D","3D","2D","1D","0D")
sector <- as.character(factor(dMS$Sector, levels = dMS$Sector))
dMS <- melt(dMS, id="Sector")
names(dMS) <- c("Sector","Level","Elasticity")
dMS$Sector <- factor(dMS$Sector,sector)

MS$xmin <- MS$Elasticity - 2 * dMS$Elasticity
for (i in c(1:length(MS$xmin))){
  if ((MS$xmin[i] < -1.0) & (!is.na(MS$xmin[i]))){
    MS$xmin[i] = -1.0
  }
}

MS$xmax <- MS$Elasticity + 2 * dMS$Elasticity
for (i in c(1:length(MS$xmax))){
  if ((MS$xmax[i] > 1.5) & (!is.na(MS$xmax[i]))){
    MS$xmax[i] = 1.5
  }
}

MS1 <- ggplot(MS, aes(y = Sector, x = Elasticity, fill = Level, colour = Level)) +
  labs(x="Cross-Price Elasticity M-S") + 
  theme(axis.title = element_text(size=16),axis.text.x = element_text(size=12),
        axis.text.y=element_blank(), axis.ticks.y = element_blank(), legend.position = "none",
        panel.background = element_rect(fill = "grey97"),
        panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1,1.5,0.5)) +
  scale_shape_discrete(solid=T) +
  scale_color_manual(values = c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i05"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i14.3"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i37.w.1"), color = "brown", linetype = "dotdash") +
  geom_point(size = 2.5, shape = "|") + geom_line(aes(x = rep(c(-1,1.5),400), y = "i41"), color = "brown", linetype = "dotdash") +
  annotate("text", label = "Agriculture", x = 1.25, y = "i01.h", size = 4) +
  annotate("text", label = "Mining", x = 1.25, y = "i13.20.12", size = 4) +
  annotate("text", label = "Manufacturing", x = 1.25, y = "i26.b", size = 4) +
  annotate("text", label = "Electricity", x = 1.25, y = "i40.11.i", size = 4) +
  annotate("text", label = "Services", x = 1.25, y = "i75", size = 4) +
  geom_point(size = 2.5, shape = "|") +
  geom_errorbarh(aes(xmin = MS$xmin, xmax = MS$xmax),
                 colour = c(rep("#E61709", 160), rep("#FF8C00", 160), rep("#6B8E23", 160),
                            rep("#0072CC", 160), rep("#BA55D3", 160)), alpha = 0.85)

plot_grid(KL1,KM1,LS1,EM1,MS1,Legend, ncol = 3, nrow = 2)
ggsave("Elasticities2_Final.tiff", width = 24, height = 12.7, dpi = 320)
#KL, EK, MK, MS