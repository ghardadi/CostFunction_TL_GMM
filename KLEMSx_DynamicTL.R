Data4list <- split(KLEMS_4, KLEMS_4$Industry)
dlist4 <- lapply(seq_along(Data4list), function(x) as.data.frame(Data4list[[x]])) 

lapply(seq_along(dlist4), function(x) {
  assign(paste0("Data4.", x), Data4list[[x]], envir=.GlobalEnv)
}
)

#rm(list = ls()[grep("^Data2.", ls())])
#rm(list = ls()[grep("^Agg2.", ls())])

R2_4DDynamic <- data.frame(matrix(NA, ncol=13, nrow=160))
names(R2_4DDynamic) <- c("Industry", "R2_Eq1", "R2_Eq2", "R2_Eq3", "R2_Eq4", "Bg_Eq1", "Bg_Eq2",
                  "Bg_Eq3", "Bg_Eq4", "Mono", "Non-Mono", "Concavity", "Non-Concavity")

Coef_4DDynamic <- data.frame(matrix(NA, ncol=33, nrow=160))
names(Coef_4DDynamic) <- c("Industry",
                    "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                    "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                    "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                    "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

Err_4DDynamic <- data.frame(matrix(NA, ncol=33, nrow=160))
names(Err_4DDynamic) <- c("Industry",
                   "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                   "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                   "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                   "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

pval_4DDynamic <- data.frame(matrix(NA, ncol=33, nrow=160))
names(pval_4DDynamic) <- c("Industry",
                    "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                    "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                    "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                    "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

GlobalTest <- data.frame(matrix(NA, ncol=17, nrow=0))
names(GlobalTest) <- c("Industry", "Country", "Year", "ActSK", "ActSL", "ActSE", "ActSM", "FitSK", "FitSL", "FitSE", "FitSM",
                       "ResSK", "ResSL", "ResSE", "ResSM", "Monotonicity", "Concavity")

for(i in c(1:160)){ #1:61
  data <- get(paste0("Data4.",i))
  
  success <- FALSE
  
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
  
  data$lnK1 <- NA
  data$lnL1 <- NA
  data$lnE1 <- NA
  data$lnM1 <- NA
  data$lnY1 <- NA
  
  data$t1 <- NA
  
  data$dK1 <- NA
  data$dL1 <- NA
  data$dE1 <- NA
  data$dM1 <- NA
  
  for(j in c(1:nrow(data))){
    ann <- data$Year[j]
    ctr <- data$Country[j]
    ind <- data$Industry[j]
    
    k1_id <- data$SK[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    l1_id <- data$SL[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    e1_id <- data$SE[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    m1_id <- data$SM[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    s1_id <- data$SS[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    
    k2_id <- data$SK[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    l2_id <- data$SL[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    e2_id <- data$SE[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    m2_id <- data$SM[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    s2_id <- data$SS[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    
    try(c(
      data$dK[j] <- k1_id,
      data$dL[j] <- l1_id,
      data$dE[j] <- e1_id,
      data$dM[j] <- m1_id,
      
      data$dK1[j] <- k2_id,
      data$dL1[j] <- l2_id,
      data$dE1[j] <- e2_id,
      data$dM1[j] <- m2_id), silent = TRUE)#- m2_id), silent = TRUE)
  }
  
  for(j in c(1:nrow(data))){
    ann <- data$Year[j]
    ctr <- data$Country[j]
    ind <- data$Industry[j]
    
    ln_k <- data$lnK[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_l <- data$lnL[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_e <- data$lnE[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_m <- data$lnM[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_y <- data$lnY[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    t1 <- data$t[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    
    try(c(
      data$lnK1[j] <- ln_k, #- data$lnK[j],
      data$lnL1[j] <- ln_l, #- data$lnL[j],
      data$lnE1[j] <- ln_e, #- data$lnE[j],
      data$lnM1[j] <- ln_m, #- data$lnM[j],
      data$lnY1[j] <- ln_y, #- data$lnY[j],
      data$t1[j] <- t1), silent = TRUE)# - data$lnY[j]), silent = TRUE)
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
  #restrict10 <- "eq1_(Intercept) + eq2_(Intercept) + eq3_(Intercept) + eq4_(Intercept) = 1"
  #restrict11 <- "eq1_lnY + eq2_lnY + eq3_lnY + eq4_lnY = 0"
  #restrict16 <- "eq1_t + eq2_t + eq3_t + eq4_t = 0"
  
  restrict <- c(restrict1, restrict2, restrict3, restrict4, restrict5, restrict6,
                restrict7, restrict8, restrict9)#, restrict10, restrict11, restrict16)
  
  inst1 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dK1 #+ lnY
  inst2 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dL1 #+ lnY
  inst3 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dL1 #+ lnY
  inst4 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dM1 #+ lnY
  
  inst <- list(inst1, inst2, inst3, inst4)
  
  try(c(system_eqn <- systemfit(eqlist, data = data, method="3SLS", inst = inst,
                                method3sls="GMM", restrict.matrix = restrict, maxiter = 100),
        R2 <- c((summary(system_eqn)[[10]][[1]])$r.squared, (summary(system_eqn)[[10]][[2]])$r.squared, 
                (summary(system_eqn)[[10]][[3]])$r.squared, (summary(system_eqn)[[10]][[4]])$r.squared,
                pbgtest(SK ~ lnK + lnL + lnE + lnM + lnY + t + dK, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SL ~ lnK + lnL + lnE + lnM + lnY + t + dL, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SE ~ lnK + lnL + lnE + lnM + lnY + t + dE, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SM ~ lnK + lnL + lnE + lnM + lnY + t + dM, order.by = data$Year, data=data, type = "Chisq")[4]),
        success <- TRUE
  ), silent = TRUE)
  
  R2_4DDynamic[i,1] <- i
  Coef_4DDynamic[i,1] <- i
  Err_4DDynamic[i,1] <- i
  pval_4DDynamic[i,1] <- i
  
  if (success == FALSE | any(R2[1:4] < 0)){
    print(i)
  } else {
    
    for (j in 1:8){
      R2_4DDynamic[i,j+1] <- R2[j] #(summary(system_eqn)[[10]][[1]])$r.squared
    }
    
    for (j in 1:8){
      Coef_4DDynamic[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,1]
      Coef_4DDynamic[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,1]
      Coef_4DDynamic[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,1]
      Coef_4DDynamic[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,1]
      Err_4DDynamic[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,2]
      Err_4DDynamic[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,2]
      Err_4DDynamic[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,2]
      Err_4DDynamic[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,2]
      pval_4DDynamic[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,4]
      pval_4DDynamic[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,4]
      pval_4DDynamic[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,4]
      pval_4DDynamic[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,4]
    }
    
    N <- nrow(data)
    Ntrue <- sum(((fitted(system_eqn)[,1] + fitted(system_eqn)[,2] + fitted(system_eqn)[,3] + fitted(system_eqn)[,4] < 1) &
                    (fitted(system_eqn)[,1] > 0 & fitted(system_eqn)[,2] > 0)) &
                   (fitted(system_eqn)[,3] > 0 & fitted(system_eqn)[,4] > 0))
    Nfalse <- N - Ntrue
    
    R2_4DDynamic[i,10] <- Ntrue
    R2_4DDynamic[i,11] <- Nfalse
    
    #nc <- length(unique(data$Country)) - 1
    
    Lambda <- (summary(system_eqn)[[10]][[1]])$coefficients[8]
    
    # Alpha_K <- (summary(system_eqn)[[10]][[1]])$coefficients[1] / (1 - Lambda)
    # Alpha_L <- (summary(system_eqn)[[10]][[2]])$coefficients[1] / (1 - Lambda)
    # Alpha_E <- (summary(system_eqn)[[10]][[3]])$coefficients[1] / (1 - Lambda)
    # Alpha_M <- (summary(system_eqn)[[10]][[4]])$coefficients[1] / (1 - Lambda)
    # Alpha_S <- 1 - Alpha_K - Alpha_L - Alpha_E - Alpha_S
    
    Beta_KK <- (summary(system_eqn)[[10]][[1]])$coefficients[2]
    Beta_KL <- (summary(system_eqn)[[10]][[1]])$coefficients[3]
    Beta_KE <- (summary(system_eqn)[[10]][[1]])$coefficients[4]
    Beta_KM <- (summary(system_eqn)[[10]][[1]])$coefficients[5]
    Beta_KS <- - Beta_KK - Beta_KL - Beta_KE - Beta_KM
    
    Beta_LK <- Beta_KL
    Beta_LL <- (summary(system_eqn)[[10]][[2]])$coefficients[3]
    Beta_LE <- (summary(system_eqn)[[10]][[2]])$coefficients[4]
    Beta_LM <- (summary(system_eqn)[[10]][[2]])$coefficients[5]
    Beta_LS <- - Beta_LK - Beta_LL - Beta_LE - Beta_LM
    
    Beta_EK <- Beta_KE
    Beta_EL <- Beta_LE
    Beta_EE <- (summary(system_eqn)[[10]][[3]])$coefficients[4]
    Beta_EM <- (summary(system_eqn)[[10]][[3]])$coefficients[5]
    Beta_ES <- - Beta_EK - Beta_EL - Beta_EE - Beta_EM
    
    Beta_MK <- Beta_KM
    Beta_ML <- Beta_LM
    Beta_ME <- Beta_EM
    Beta_MM <- (summary(system_eqn)[[10]][[4]])$coefficients[5]
    Beta_MS <- - Beta_MK - Beta_ML - Beta_ME - Beta_MM
    
    Beta_SK <- Beta_KS
    Beta_SL <- Beta_LS
    Beta_SE <- Beta_ES
    Beta_SM <- Beta_MS
    Beta_SS <- - Beta_SK - Beta_SL - Beta_SE - Beta_SM
    
    data$kk <- (Beta_KK * Lambda + data$SK^2 - data$SK)/data$SK
    data$ll <- (Beta_LL * Lambda + data$SL^2 - data$SL)/data$SL
    data$ee <- (Beta_EE * Lambda + data$SE^2 - data$SE)/data$SE
    data$mm <- (Beta_MM * Lambda + data$SM^2 - data$SM)/data$SM
    data$ss <- (Beta_SS * Lambda + data$SS^2 - data$SS)/data$SS
    
    Ntrue <- sum((data$kk < 0 & data$ll < 0 & data$ee < 0 & data$mm < 0 & data$ss < 0))
    Nfalse <- N - Ntrue
    
    R2_4DDynamic[i,12] <- Ntrue
    R2_4DDynamic[i,13] <- Nfalse
    
    test <- data.frame(matrix(NA, ncol=17, nrow = N))
    names(test) <- c("Industry", "Country", "Year", "ActSK", "ActSL", "ActSE", "ActSM",
                     "FitSK", "FitSL", "FitSE", "FitSM", "ResSK", "ResSL",
                     "ResSE", "ResSM", "Monotonicity", "Concavity")
    
    test$Industry <- data$Industry
    test$Country <- data$Country
    test$Year <- data$Year
    test$ActSK <- data$SK
    test$ActSL <- data$SL
    test$ActSE <- data$SE
    test$ActSM <- data$SM
    
    test$FitSK <- system_eqn$eq[[1]]$fitted.values
    test$FitSL <- system_eqn$eq[[2]]$fitted.values
    test$FitSE <- system_eqn$eq[[3]]$fitted.values
    test$FitSM <- system_eqn$eq[[4]]$fitted.values
    
    test$ResSK <- system_eqn$eq[[1]]$residuals
    test$ResSL <- system_eqn$eq[[2]]$residuals
    test$ResSE <- system_eqn$eq[[3]]$residuals
    test$ResSM <- system_eqn$eq[[4]]$residuals
    
    test$Monotonicity <- ((fitted(system_eqn)[,1] + fitted(system_eqn)[,2] + fitted(system_eqn)[,3] + fitted(system_eqn)[,4] < 1) &
                            (fitted(system_eqn)[,1] > 0 & fitted(system_eqn)[,2] > 0)) &
      (fitted(system_eqn)[,3] > 0 & fitted(system_eqn)[,4] > 0)
    test$Concavity <- (data$kk < 0 & data$ll < 0 & data$ee < 0 & data$mm < 0 & data$ss < 0)
    
    GlobalTest <- rbind(GlobalTest, test)
  }
}

write.csv(R2_4DDynamic, "R2_4DDynamic.csv")

write.csv(Coef_4DDynamic, "Coef_4DDynamic.csv")
write.csv(Err_4DDynamic, "Err_4DDynamic.csv")
write.csv(pval_4DDynamic, "pval_4DDynamic.csv")

rm(list = ls()[grep("^Data4.", ls())])
rm(list = ls()[grep("^Agg4.", ls())])
rm(dlist4)
