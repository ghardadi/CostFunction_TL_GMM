#Gilang Hardadi

library(ggplot2)
library(tidyr)
library(cowplot)
library(reshape)
library(RColorBrewer)

CmpIndex <- read.csv("Index_Comparison.csv")
R2_4DDynamic_copy <- R2_4DDynamic

R2_4DDynamic$Industry <- CmpIndex$Ind..Code
R2_4DDynamic$MPct <- R2_4DDynamic$Mono / (R2_4DDynamic$Mono + R2_4DDynamic$`Non-Mono`)
R2_4DDynamic$CPct <- R2_4DDynamic$Concavity / (R2_4DDynamic$Concavity + R2_4DDynamic$`Non-Concavity`)
# R2_4DDynamic[,c(6:9)] <- log10(R2_4DDynamic[,c(6:9)]) / 4 + 1

for (i in c(1:nrow(R2_4DDynamic))){
  for (j in c(6:9)){
    if (is.na(R2_4DDynamic[i,j]) == FALSE){
      if (R2_4DDynamic[i,j] < 0.005){
        R2_4DDynamic[i,j] <- 0
      } else if (R2_4DDynamic[i,j] < 0.01){
        R2_4DDynamic[i,j] <- 0.33
      } else if (R2_4DDynamic[i,j] < 0.05){
        R2_4DDynamic[i,j] <- 0.67
      } else {
        R2_4DDynamic[i,j] <- 1
      }
    }
  }
}

M1 <- melt(R2_4DDynamic[,c(1:9,14:15)], id="Industry")
M1$variable <- rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), each=160)
A <- ggplot(M1[M1$Industry %in% CmpIndex$Ind..Code[c(1:80)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
B <- ggplot(M1[M1$Industry %in% CmpIndex$Ind..Code[c(81:160)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) 
title <- ggdraw() + draw_label("Dynamic Translog (GMM)", fontfamily = "", size=18)
FigC <- plot_grid(title, plot_grid(A, B, ncol = 2, nrow = 1), ncol=1, rel_heights=c(0.05, 1))

R2_4DFixed$Industry <- CmpIndex$Ind..Code
R2_4DFixed$MPct <- R2_4DFixed$Mono / (R2_4DFixed$Mono + R2_4DFixed$`Non-Mono`)
R2_4DFixed$CPct <- R2_4DFixed$Concavity / (R2_4DFixed$Concavity + R2_4DFixed$`Non-Concavity`)
# R2_4DFixed[,c(6:9)] <- log10(R2_4DFixed[,c(6:9)]) / 4 + 1

for (i in c(1:nrow(R2_4DFixed))){
  for (j in c(6:9)){
    if (is.na(R2_4DFixed[i,j]) == FALSE){
      if (R2_4DFixed[i,j] < 0.005){
        R2_4DFixed[i,j] <- 0
      } else if (R2_4DFixed[i,j] < 0.01){
        R2_4DFixed[i,j] <- 0.33
      } else if (R2_4DFixed[i,j] < 0.05){
        R2_4DFixed[i,j] <- 0.67
      } else {
        R2_4DFixed[i,j] <- 1
      }
    }
  }
}

M2 <- melt(R2_4DFixed[,c(1:9,14:15)], id="Industry")
M2$variable <- rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), each=160)
C <- ggplot(M2[M2$Industry %in% CmpIndex$Ind..Code[c(1:80)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
D <- ggplot(M2[M2$Industry %in% CmpIndex$Ind..Code[c(81:160)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
title <- ggdraw() + draw_label("Fixed-Effect Translog", fontfamily = "", size=18)
FigB <- plot_grid(title, plot_grid(C, D, ncol = 2, nrow = 1), ncol=1, rel_heights=c(0.05, 1))

R2_4DPool$Industry <- CmpIndex$Ind..Code
R2_4DPool$MPct <- R2_4DPool$Mono / (R2_4DPool$Mono + R2_4DPool$`Non-Mono`)
R2_4DPool$CPct <- R2_4DPool$Concavity / (R2_4DPool$Concavity + R2_4DPool$`Non-Concavity`)
# R2_4DPool[,c(6:9)] <- log10(R2_4DPool[,c(6:9)]) / 4 + 1

for (i in c(1:nrow(R2_4DPool))){
  for (j in c(6:9)){
    if (is.na(R2_4DPool[i,j]) == FALSE){
      if (R2_4DPool[i,j] < 0.005){
        R2_4DPool[i,j] <- 0
      } else if (R2_4DPool[i,j] < 0.01){
        R2_4DPool[i,j] <- 0.33
      } else if (R2_4DPool[i,j] < 0.05){
        R2_4DPool[i,j] <- 0.67
      } else {
        R2_4DPool[i,j] <- 1
      }
    }
  }
}

M3 <- melt(R2_4DPool[,c(1:9,14:15)], id="Industry")
M3$variable <- rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), each=160)
E <- ggplot(M3[M3$Industry %in% CmpIndex$Ind..Code[c(1:80)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
F <- ggplot(M3[M3$Industry %in% CmpIndex$Ind..Code[c(81:160)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradientn(colours = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A")) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
title <- ggdraw() + draw_label("Pooled Translog", fontfamily = "", size=18)
FigA <- plot_grid(title, plot_grid(E, F, ncol = 2, nrow = 1), ncol=1, rel_heights=c(0.05, 1))

M4 <- M3
M4$value <- 4 * M4$value - 4 
G <- ggplot(M4[M4$Industry %in% CmpIndex$Ind..Code[c(1:80)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradient(low="#CEFEB1", high="#006400") + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
H <- ggplot(M4[M4$Industry %in% CmpIndex$Ind..Code[c(81:160)],], aes(y = Industry, x = variable, fill = value)) + geom_tile(colour = "black") +
  scale_fill_gradient(low="#CEFEB1", high="#006400") + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
title <- ggdraw() + draw_label("Pooled Translog", fontfamily = "", size=18)
FigD <- plot_grid(title, plot_grid(G, H, ncol = 2, nrow = 1), ncol=1, rel_heights=c(0.05, 1))

plot_grid(FigA, FigB, FigC, ncol=3, nrow=1)
ggsave("BG_TestFinal1.tiff", width = 15.80, height = 11.3, dpi = 320)

ggplot(M4[M4$Industry %in% CmpIndex$Ind..Code[c(81:160)],], aes(y = Industry, x = variable, fill = value)) +
  geom_tile(colour = "black") + scale_fill_gradient(low="#CEFEB1", high="#006400") +
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave("Legend.tiff", width = 11.3, height = 11.3, dpi = 320)
