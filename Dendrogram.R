install.packages("data.tree")
install.packages("networkD3")

library(readr)
library(data.tree)
library(networkD3)

Den <- read_csv("Dendrogram.csv")

#Define the hierarchy (Session/Room/Speaker)
Den$pathString <- paste(Den$National, Den$`0-D`, Den$`1-D`, Den$`2-D`,
                        Den$`3-D`, Den$`4-D`, sep = "|")
#Convert to Node
DenTree <- as.Node(Den, pathDelimiter = "|")

#Plot using networkD3
DenTreeList <- ToListExplicit(DenTree, unname = TRUE)
D3net <- radialNetwork(DenTreeList, fontSize = 12)

saveNetwork(D3net, "D3_LM.html", selfcontained = TRUE)

library(webshot)
webshot("D3_LM.html", "simpleNetwork.png", vwidth = 1027, vheight = 1027)
