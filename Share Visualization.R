#Gilang Hardadi
library(ggplot2)
library(cowplot)
library(reshape)

#Uploading data from csv
Share <- read.csv("Share.csv")

Share <- melt(Share[1:7], id=c("Aggregate", "Industry"))

Share$variable <- factor(Share$variable, levels = c("SK", "SL", "SE", "SM", "SS"))

F1 <- ggplot(Share[Share$Aggregate == 1,],
       aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Agriculture, Forestry, Fisheries", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F2 <- ggplot(Share[Share$Aggregate == 2,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Mining and Quarrying", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=7, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F3 <- ggplot(Share[Share$Aggregate == 3,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Light Manufacturing", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F4 <- ggplot(Share[Share$Aggregate == 4,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Material Manufacturing", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F5 <- ggplot(Share[Share$Aggregate == 5,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Tech Manufacturing", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F6 <- ggplot(Share[Share$Aggregate == 6,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Electricity and Utilities", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F7 <- ggplot(Share[Share$Aggregate == 7,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Waste and Water Treatment", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

F8 <- ggplot(Share[Share$Aggregate == 8,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Construction, Transportation", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12))

F9 <- ggplot(Share[Share$Aggregate == 9,],
             aes(fill=variable, y=value, x=Industry)) + scale_fill_manual(values=c("#E61709", "#FF8C00", "#6B8E23", "#0072CC", "#BA55D3")) + 
  geom_bar(position="fill", stat="identity") + labs(title="Other Services", y = "Input Share") + 
  theme(plot.title = element_text(size=18, hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_text(size=18), axis.text.x = element_text(size=9, angle = 90, hjust = 0),
        axis.text.y = element_text(size=12), legend.position = "none")

plot_grid(F1, F2, F3, F4, F5, F6, F7, F8, F9, ncol = 3, nrow = 3)
ggsave("ShareKLEMS.tiff", width = 24, height = 12.7, dpi = 320)
