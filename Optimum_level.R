R2_4D$MC <- NA
for(i in 1:nrow(R2_4D)){
  m <- R2_4D$Mono[i] / (R2_4D$Mono[i] + R2_4D$`Non-Mono`[i])
  c <- R2_4D$Concavity[i] / (R2_4D$Concavity[i] + R2_4D$`Non-Concavity`[i])
  R2_4D$MC[i] <- m > 0.7 & c > 0.7
}

R2_3D$MC <- NA
for(i in 1:nrow(R2_3D)){
  m <- R2_3D$Mono[i] / (R2_3D$Mono[i] + R2_3D$`Non-Mono`[i])
  c <- R2_3D$Concavity[i] / (R2_3D$Concavity[i] + R2_3D$`Non-Concavity`[i])
  R2_3D$MC[i] <- m > 0.7 & c > 0.7
}

R2_2D$MC <- NA
for(i in 1:nrow(R2_2D)){
  m <- R2_2D$Mono[i] / (R2_2D$Mono[i] + R2_2D$`Non-Mono`[i])
  c <- R2_2D$Concavity[i] / (R2_2D$Concavity[i] + R2_2D$`Non-Concavity`[i])
  R2_2D$MC[i] <- m > 0.7 & c > 0.7
}

R2_1D$MC <- NA
for(i in 1:nrow(R2_1D)){
  m <- R2_1D$Mono[i] / (R2_1D$Mono[i] + R2_1D$`Non-Mono`[i])
  c <- R2_1D$Concavity[i] / (R2_1D$Concavity[i] + R2_1D$`Non-Concavity`[i])
  R2_1D$MC[i] <- m > 0.7 & c > 0.7
}
