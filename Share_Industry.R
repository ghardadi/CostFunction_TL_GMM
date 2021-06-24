Data4list <- split(KLEMS_4, KLEMS_4$Industry)
dlist4 <- lapply(seq_along(Data4list), function(x) as.data.frame(Data4list[[x]])) 

lapply(seq_along(dlist4), function(x) {
  assign(paste0("Data4.", x), Data4list[[x]], envir=.GlobalEnv)
}
)

Indices <- AggIndex[,c(1)]
Indices <- Indices[-c(16,17,163)]

Share_4D <- data.frame(matrix(NA, ncol=12, nrow=160))
names(Share_4D) <- c("Industry", "SK", "SL", "SE", "SM", "SS", "dSK", "dSL", "dSE", "dSM", "dSS", "Total")

for(i in c(1:160)){ #1:61
  data <- get(paste0("Data4.",i))
  
  data$TC <- data$K + data$L + data$E + data$M + data$S
  
  data$SK <- data$K / data$TC
  data$SL <- data$L / data$TC
  data$SE <- data$E / data$TC
  data$SM <- data$M / data$TC
  data$SS <- data$S / data$TC
  
  Share_4D$Industry[i] <- Indices[i]
  
  Share_4D$dSK[i] <- mean(data$SK)
  Share_4D$dSL[i] <- mean(data$SL)
  Share_4D$dSE[i] <- mean(data$SE)
  Share_4D$dSM[i] <- mean(data$SM)
  Share_4D$dSS[i] <- mean(data$SS)
  
  Share_4D$SK[i] <- mean(data$SK)
  Share_4D$SL[i] <- mean(data$SL)
  Share_4D$SE[i] <- mean(data$SE)
  Share_4D$SM[i] <- mean(data$SM)
  Share_4D$SS[i] <- mean(data$SS)
  
  Share_4D$Total[i] <- Share_4D$SK[i] + Share_4D$SL[i] + Share_4D$SE[i] + Share_4D$SM[i] + Share_4D$SS[i]
}

write.csv(Share_4D, "Share.csv")
