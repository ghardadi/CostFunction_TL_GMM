chisq <- data.frame(matrix(NA, ncol=13, nrow=160))
names(chisq) <- c("Industry", "Chi_0D", "Chi_1D", "Chi_2D", "Chi_3D", "Chi_4D", "Chi_Opt", "Pr_0D", "Pr_1D",
                  "Pr_2D", "Pr_3D", "Pr_4D", "Pr_Opt")

CmpIndex <- read.csv("Index_Comparison.csv")
extract <- ""

for (agg in c(5)){
  for (sector in c(1:160)){
    data <- get(paste0("Data4.",sector))
    
    #data <- data[data$Country %in% c("AT", "BE", "DE", "DK", "EE", "FI", "FR", "IE", "IT", "NL", "SE", "GB", "US", "JP"),]
    
    data$pK <- data$K / data$Kc
    data$pL <- data$L / data$Lc
    data$pE <- data$E / data$Ec
    data$pM <- data$M / data$Mc
    data$pS <- data$S / data$Sc
    
    data$pGO <- data$GO / data$GOc
    data$TC <- data$K + data$L + data$E + data$M + data$S
    
    data$SK <- data$K / data$TC
    data$SL <- data$L / data$TC
    data$SE <- data$E / data$TC
    data$SM <- data$M / data$TC
    data$SS <- data$S / data$TC
    
    data$lnK <- log(data$pK/data$pS)
    data$lnL <- log(data$pL/data$pS)
    data$lnE <- log(data$pE/data$pS)
    data$lnM <- log(data$pM/data$pS)
    data$lnC <- log(data$TC/data$pS)
    
    data$lnK2 <- 0.5*(data$lnK)^2
    data$lnL2 <- 0.5*(data$lnL)^2
    data$lnE2 <- 0.5*(data$lnE)^2
    data$lnM2 <- 0.5*(data$lnM)^2
    
    data$lnKL <- data$lnK*data$lnL
    data$lnKE <- data$lnK*data$lnE
    data$lnKM <- data$lnK*data$lnM
    
    data$lnLE <- data$lnL*data$lnE
    data$lnLM <- data$lnL*data$lnM
    
    data$lnEM <- data$lnE*data$lnM
    
    data$lnY <- log(data$GOc)
    data$lnY2 <- 0.5*data$lnY^2
    
    data$lnKY <- data$lnK*data$lnY
    data$lnLY <- data$lnL*data$lnY
    data$lnEY <- data$lnE*data$lnY
    data$lnMY <- data$lnM*data$lnY
    
    data$t <- data$Year - 2010
    data$t2 <- 0.5*data$t^2
    
    data$lnKt <- data$lnK*data$t
    data$lnLt <- data$lnL*data$t
    data$lnEt <- data$lnE*data$t
    data$lnMt <- data$lnM*data$t
    
    data$dK <- NA
    data$dL <- NA
    data$dE <- NA
    data$dM <- NA
    
    for(j in c(1:nrow(data))){
      ann <- data$Year[j]
      ctr <- data$Country[j]
      ind <- data$Industry[j]
      
      k_id <- data$SK[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
      l_id <- data$SL[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
      e_id <- data$SE[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
      m_id <- data$SM[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
      s_id <- data$SS[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
      
      try(c(
        data$dK[j] <- k_id,
        data$dL[j] <- l_id,
        data$dE[j] <- e_id,
        data$dM[j] <- m_id), silent = TRUE)
    }
    
    data <- na.omit(data)
    
    eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + dK #+ Country
    eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + dL #+ Country
    eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + dE #+ Country
    eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + dM #+ Country
    
    # eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + Country
    # eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + Country
    # eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + Country
    # eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + Country
    
    eqlist <- list (eq1, eq2, eq3, eq4)
    
    restrict1 <- "eq1_lnL - eq2_lnK = 0"
    restrict2 <- "eq1_lnE - eq3_lnK = 0"
    restrict3 <- "eq1_lnM - eq4_lnK = 0"
    restrict4 <- "eq2_lnE - eq3_lnL = 0"
    restrict5 <- "eq2_lnM - eq4_lnL = 0"
    restrict6 <- "eq3_lnM - eq4_lnE = 0"
    restrict7 <- "eq1_dK - eq2_dL = 0"
    restrict8 <- "eq1_dK - eq3_dE = 0"
    restrict9 <- "eq1_dK - eq4_dM = 0"
    
    restrict <- c(restrict1, restrict2, restrict3, restrict4, restrict5, restrict6,
                  restrict7, restrict8, restrict9)
    
    smallsystem_eqn <- systemfit(eqlist, data = data, method="SUR", restrict.matrix = restrict, maxiter = 100)
    
    if (paste0("DataOpt.", CmpIndex[CmpIndex$Agg4==sector, agg+3]) == extract) {
      
    } else {
      
      extract <- paste0("DataOpt.", CmpIndex[CmpIndex$Agg4==sector, agg+3])
      
      AggData <- get(extract)
      
      AggData$pK <- AggData$K / AggData$Kc
      AggData$pL <- AggData$L / AggData$Lc
      AggData$pE <- AggData$E / AggData$Ec
      AggData$pM <- AggData$M / AggData$Mc
      AggData$pS <- AggData$S / AggData$Sc
      
      AggData$pGO <- AggData$GO / AggData$GOc
      AggData$TC <- AggData$K + AggData$L + AggData$E + AggData$M + AggData$S
      
      AggData$SK <- AggData$K / AggData$TC
      AggData$SL <- AggData$L / AggData$TC
      AggData$SE <- AggData$E / AggData$TC
      AggData$SM <- AggData$M / AggData$TC
      AggData$SS <- AggData$S / AggData$TC
      
      AggData$lnK <- log(AggData$pK/AggData$pS)
      AggData$lnL <- log(AggData$pL/AggData$pS)
      AggData$lnE <- log(AggData$pE/AggData$pS)
      AggData$lnM <- log(AggData$pM/AggData$pS)
      AggData$lnC <- log(AggData$TC/AggData$pS)
      
      AggData$lnK2 <- 0.5*(AggData$lnK)^2
      AggData$lnL2 <- 0.5*(AggData$lnL)^2
      AggData$lnE2 <- 0.5*(AggData$lnE)^2
      AggData$lnM2 <- 0.5*(AggData$lnM)^2
      
      AggData$lnKL <- AggData$lnK*AggData$lnL
      AggData$lnKE <- AggData$lnK*AggData$lnE
      AggData$lnKM <- AggData$lnK*AggData$lnM
      
      AggData$lnLE <- AggData$lnL*AggData$lnE
      AggData$lnLM <- AggData$lnL*AggData$lnM
      
      AggData$lnEM <- AggData$lnE*AggData$lnM
      
      AggData$lnY <- log(AggData$GOc)
      AggData$lnY2 <- 0.5*AggData$lnY^2
      
      AggData$lnKY <- AggData$lnK*AggData$lnY
      AggData$lnLY <- AggData$lnL*AggData$lnY
      AggData$lnEY <- AggData$lnE*AggData$lnY
      AggData$lnMY <- AggData$lnM*AggData$lnY
      
      AggData$t <- AggData$Year - 2010
      AggData$t2 <- 0.5*AggData$t^2
      
      AggData$lnKt <- AggData$lnK*AggData$t
      AggData$lnLt <- AggData$lnL*AggData$t
      AggData$lnEt <- AggData$lnE*AggData$t
      AggData$lnMt <- AggData$lnM*AggData$t
      
      AggData$dK <- NA
      AggData$dL <- NA
      AggData$dE <- NA
      AggData$dM <- NA
      
      for(j in c(1:nrow(AggData))){
        ann <- AggData$Year[j]
        ctr <- AggData$Country[j]
        ind <- AggData$Industry[j]
        
        k_id <- AggData$SK[AggData$Year == (ann - 1) & AggData$Country == ctr & AggData$Industry == ind]
        l_id <- AggData$SL[AggData$Year == (ann - 1) & AggData$Country == ctr & AggData$Industry == ind]
        e_id <- AggData$SE[AggData$Year == (ann - 1) & AggData$Country == ctr & AggData$Industry == ind]
        m_id <- AggData$SM[AggData$Year == (ann - 1) & AggData$Country == ctr & AggData$Industry == ind]
        s_id <- AggData$SS[AggData$Year == (ann - 1) & AggData$Country == ctr & AggData$Industry == ind]
        
        try(c(
          AggData$dK[j] <- k_id,
          AggData$dL[j] <- l_id,
          AggData$dE[j] <- e_id,
          AggData$dM[j] <- m_id), silent = TRUE)
      }
      
      AggData <- na.omit(AggData)
      
      eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + dK #+ Country
      eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + dL #+ Country
      eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + dE #+ Country
      eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + dM #+ Country
      
      # eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + Country
      # eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + Country
      # eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + Country
      # eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + Country
      
      eqlist <- list (eq1, eq2, eq3, eq4)
      
      restrict1 <- "eq1_lnL - eq2_lnK = 0"
      restrict2 <- "eq1_lnE - eq3_lnK = 0"
      restrict3 <- "eq1_lnM - eq4_lnK = 0"
      restrict4 <- "eq2_lnE - eq3_lnL = 0"
      restrict5 <- "eq2_lnM - eq4_lnL = 0"
      restrict6 <- "eq3_lnM - eq4_lnE = 0"
      restrict7 <- "eq1_dK - eq2_dL = 0"
      restrict8 <- "eq1_dK - eq3_dE = 0"
      restrict9 <- "eq1_dK - eq4_dM = 0"
      
      restrict <- c(restrict1, restrict2, restrict3, restrict4, restrict5, restrict6,
                    restrict7, restrict8, restrict9)
      
      system_eqn <- systemfit(eqlist, data = AggData, method="SUR", restrict.matrix = restrict, maxiter = 100)
      
    }
    
    AggLength <- length(smallsystem_eqn$coefficients)
    hypomat <- matrix(0, nrow = AggLength-9, ncol = AggLength)
    coef <- rep(0, AggLength-9)
    
    loc <- c(1:9,11:15,17,20:23,25,29:31)
    
    for (i in c(1: (AggLength-9) )){
      a <- loc[i]
      hypomat[i,a] <- 1 
    }
    
    for (i in c(1:AggLength-9)){ #AggLength-1
      coef[i] <- smallsystem_eqn$coefficients[names(system_eqn$coefficients)[loc[i]]]
    }
    
    chisq[sector,agg+2] <- linearHypothesis(system_eqn, hypomat, coef, test="Chisq")$Chisq[2]
    chisq[sector,agg+8] <- linearHypothesis(system_eqn, hypomat, coef, test="Chisq")$Pr[2]
    
    print(CmpIndex$Ind..Code[sector])
  }
}

chisq[,1] <- CmpIndex$Ind..Code
write.csv(chisq, "ChiSquare.csv")
