#Gilang Hardadi
library(ggplot2)
library(cowplot)

Price <- data.frame(matrix(NA, ncol=13, nrow=22))
names(Price) <- c("Year", "pK", "pL", "pE", "pM", "pS", "pGO", "seK", "seL", "seE", "seM", "seS", "seGO")

KLEMS_4$pK <- KLEMS_4$K/KLEMS_4$Kc
KLEMS_4$pL <- KLEMS_4$L/KLEMS_4$Lc
KLEMS_4$pE <- KLEMS_4$E/KLEMS_4$Ec
KLEMS_4$pM <- KLEMS_4$M/KLEMS_4$Mc
KLEMS_4$pS <- KLEMS_4$S/KLEMS_4$Sc
KLEMS_4$pGO <- KLEMS_4$GO/KLEMS_4$GOc

#Uploading data from csv
for (i in 1995:2016){
  Price$Year[i-1994] <- i
  Price$pK[i-1994] <- mean(KLEMS_4$pK[KLEMS_4$Year == i])
  Price$pL[i-1994] <- mean(KLEMS_4$pL[KLEMS_4$Year == i])
  Price$pE[i-1994] <- mean(KLEMS_4$pE[KLEMS_4$Year == i])
  Price$pM[i-1994] <- mean(KLEMS_4$pM[KLEMS_4$Year == i])
  Price$pS[i-1994] <- mean(KLEMS_4$pS[KLEMS_4$Year == i])
  Price$pGO[i-1994] <- mean(KLEMS_4$pGO[KLEMS_4$Year == i])
  Price$seK[i-1994] <- sd(KLEMS_4$pK[KLEMS_4$Year == i])
  Price$seL[i-1994] <- sd(KLEMS_4$pL[KLEMS_4$Year == i])
  Price$seE[i-1994] <- sd(KLEMS_4$pE[KLEMS_4$Year == i])
  Price$seM[i-1994] <- sd(KLEMS_4$pM[KLEMS_4$Year == i])
  Price$seS[i-1994] <- sd(KLEMS_4$pS[KLEMS_4$Year == i])
  Price$seGO[i-1994] <- sd(KLEMS_4$pGO[KLEMS_4$Year == i])
}

PriceK <- ggplot(Price, aes(x=Year,y=pK)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) +
  geom_point(shape=18, colour="#E61709", size=3) + geom_line(colour="#E61709") + geom_ribbon(aes(ymin=pK-seK, ymax=pK+seK), linetype=2, alpha=0.15) +
  labs(title="Capital Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                      axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

PriceL <- ggplot(Price, aes(x=Year,y=pL)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) +
  geom_point(shape=18, colour="#FF8C00", size=3) + geom_line(colour="#FF8C00") + geom_ribbon(aes(ymin=pL-seL, ymax=pL+seL), linetype=2, alpha=0.15) +
  labs(title="Labor Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                    axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

PriceE <- ggplot(Price, aes(x=Year,y=pE)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) + geom_point(shape=18, colour="#6B8E23", size=3) +
  geom_line(colour="#6B8E23") + geom_ribbon(aes(ymin=pE-seE, ymax=pE+seE), linetype=2, alpha=0.15) +
  labs(title="Energy Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                     axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

PriceM <- ggplot(Price, aes(x=Year,y=pM)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) + geom_point(shape=18, colour="#0072CC", size=3) +
  geom_line(colour="#0072CC") + geom_ribbon(aes(ymin=pM-seM, ymax=pM+seM), linetype=2, alpha=0.15) +
  labs(title="Material Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                       axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

PriceS <- ggplot(Price, aes(x=Year,y=pS)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) + geom_point(shape=18, colour="#BA55D3", size=3) +
  geom_line(colour="#BA55D3") + geom_ribbon(aes(ymin=pS-seS, ymax=pS+seS), linetype=2, alpha=0.15) +
  labs(title="Services Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                       axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

PriceGO <- ggplot(Price, aes(x=Year,y=pGO)) + scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5,1.5,0.25)) + geom_point(shape=18, colour="#75B2DD", size=3) +
  geom_line(colour="#75B2DD") + geom_ribbon(aes(ymin=pGO-seGO, ymax=pGO+seGO), linetype=2, alpha=0.15) +
  labs(title="Output Price") + theme(plot.title = element_text(size=24, hjust = 0.5), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                                     axis.text.x = element_text(size=13), axis.text.y = element_text(size=13))

plot_grid(PriceK, PriceL, PriceE, PriceM, PriceS, PriceGO, ncol = 3, nrow = 2)
ggsave("PriceKLEMS.tiff", width = 24, height = 12.7, dpi = 320)
