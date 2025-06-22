proy_est <- function(int, inf){
  v <- (1+inf)/(1+int)
  porc <- c(0.525,0.51, 0.494, 0.478, 0.462, 0.446, 0.43)
  montos <- c(2,3,4,5,6,8) * 367108.55
  res <- data.frame(Inv = 0, Pen = 0, Suc = 0, SEM = 0, 
                    InvF = rep(0, 96), PenF = 0, SucF = 0,SEMF = 0, CotAct = 0, CotPen = 0)
  anual <- (1-v)/(v^(-1/12)-1)
  anmag <- (anual + v^(11/12)) 
  min_inv <- sapply(20:115, function(x) cot_min_inv(x))
  for(id in 1:5561){
    cond_hij <- F
    cond_viu <- F
    cot_pen <- 0
    if(id <= 5196){
      cuotash <- cuotas[[id]]
      cuota <- cotizantes$cuotas[id]
      sexo <- cotizantes$sexo[id]
      x <- cotizantes$edad[id]
      salario <- cotizantes$sal_prom[id] 
      estado <- 0
      px <- 1-qx[[sexo]][[x - 19]]
      vpx <- 1-qx[[3-sexo]][[x - 19]]
      pix <- 1-all_qix[[sexo]][(x-19):96]
      cuotash <- c(cuotash, rep(0, 100))
      salariosh <- unlist(cotizaciones[id,])
      first_pen <- T
      roll_pen <- F
      porc_pen <- 1
    } else { # pensionados actuales
      if(id == 5197){
        res$InvF <- res$Inv
        res$PenF <- res$Pen
        res$SucF <- res$Suc
        res$SEMF <- res$SEM
        res$Inv <- 0
        res$Pen <- 0
        res$Suc <- 0
        res$SEM <- 0
      }
      i <- id - 5196
      x <- pensionados$edad[i]
      sexo <- pensionados$sexo[i]
      sal_pen <- pensionados$MONTO[i]*anmag
      tipo <- pensionados$COD_TIPO_PENSION[i]
      
      if(tipo == "Invalidez"){
        estado <- 2        
        px <- 1-qx[[sexo]][[x - 19]]
        vpx <- 1-qx[[3-sexo]][[x - 19]]
      } else if(tipo == "Vejez"){
        estado <- 1
        px <- 1-qx[[sexo]][[x - 19]]
        vpx <- 1-qx[[3-sexo]][[x - 19]]
      } else {
        estado <- 3
        if(pensionados$COD_PARENTESCO[i] == "H"){
          if(x >= 25){
            next
          }
          cond_hij <- T      
          opx <- 1 - oqx[[x+1]]
          c <- 0
          x <- x + 25
        } else {
          cond_viu <- T
          vpx <- 1-qx[[sexo]][[x - 19]]
        }
      }
    }
    
    for(j in 1:(115-x)){
      if(estado == 0){ # Trabaja
        if(runif(1) > px[j]){ # Muerte directa
          if(cuota >= 180 | sum(cuotash[(j+2):(j+3)])>=12){ # Solo si aplica
            estado <- 3
            cond_viu <- T
            if(x+j-25 >=0 & x+j-25 <25){
              cond_hij <- T
              c <- j-1
              opx <- 1 - oqx[[x+j-24]]
            }
          } else { # no hay sobrevivientes ni pensión alguna
            break
          }
          
        } else if(runif(1)>pix[j]){ # Invalidez
          if(cuotash[j+1] >= 180){
            estado <- 2
          } else if(x + j-1 < 48 & sum(cuotash[(j+2):(j+3)])>=12 & cuota > min_inv[x+j-20]){
            estado <- 2
          } else if(x + j-1 >= 48 & sum(cuotash[j:(j+3)])>=24 & cuota > min_inv[x+j-20]){
            estado <- 2
          } else if(cuota >= 60 & (sum(cuotash[(j+2):(j+3)])>=12|sum(cuotash[j:(j+3)])>=24)){
            estado <- 2
            porc_pen <- cuota/180
          } else { # No hay pensión ni sucesoria, gracias por aportar
            break
          }
        } else { # sobrevive, e invalidez > retiro
          prob_cot <- min(max(0, rnorm(1, 
                                       curva_densidad$media[x+j-20], curva_densidad$std[x+j-20])),1)
          cuotash[j+5] <- rbinom(1, 12, prob_cot) 
          cuota <- cuota + cuotash[j+5]
          salario <- salario*max(0,rnorm(1, curva_salarial$s_x[x+j-20],
                                         curva_salarial$std[x+j-20]))
          salariosh <- c(salariosh, rep(salario, cuotash[j+5]))
          res$CotAct[j] <- res$CotAct[j] + 0.15*cuotash[j+5]*salario # cotizaciones
          if(x+j-1 >= 65 & cuota >= 300){
            roll_pen <- T
          } else if (sexo == 2 & x+j-1 == 64 & cuota >= 357){
            roll_pen <- T
          } else if (sexo == 2 & x+j-1 == 63 & cuota >= 405){
            roll_pen <- T
          } else if (x+j-1 >= 65 & cuota >= 180 & runif(1) <= 0.9){
            estado <- 1
            porc_pen <- cuota/300
          }
          if(roll_pen){ # para ver si se retira
            roll_pen <- F
            if(first_pen){
              first_pen <- F
              ini_pen <- cuota
            }
            if(runif(1) <= 0.9){
              estado <- 1
              porc_pen <- min(1.25, 1 + (cuota-ini_pen)*2/1500 + (ini_pen-300)/1200)
            }
          }
        }
        if(estado == 1 | estado == 2 | estado == 3){
          # Se acaba justo de cambiar y hay que calcular la pensión
          cot5m <- T
          salariosh <- salariosh*(1+inf)^(1/12*c(rep(0, 360),  -6 - 12*rep(0:(j-1),cuotash[6:(j+5)])))
          if(sum(salariosh > 5e6)/length(salariosh)<0.5){ # histórico 5 millones
            cot5m <- !salariosh > 5e6
          }
          salariosh <- salariosh[salariosh > 10000 & cot5m]
          sal_pen <- mean(head(sort(salariosh),300))
          if(is.na(sal_pen)){
            break
          }
          sal_pen <-  sal_pen*porc[findInterval(sal_pen, montos) + 1]*porc_pen 
          if(sal_pen > 2e6){
            cot_pen <- sal_pen*0.05
            sal_pen <- sal_pen*0.95
          } 
          if(sal_pen > 3.5e6){
            cot_pen <- cot_pen + sal_pen - 3.5e6
            sal_pen <- 3.5e6
          }
          sal_pen <- anmag*sal_pen 
          cot_pen <- anmag*cot_pen
          if(estado == 1){
            res$Pen[j] <- res$Pen[j] + sal_pen
            res$CotPen[j] <- res$CotPen[j] + cot_pen
          } else if(estado == 2) {
            res$Inv[j] <- res$Inv[j] + sal_pen 
            res$CotPen[j] <- res$CotPen[j] + cot_pen
          }
        }
      } else if(estado == 1 | estado == 2){ # Pensionado
        if(runif(1) > px[j]){ # Muerte directa. Como ya está pensionado si recibe
          estado <- 3
          cond_viu <- T
          if(x+j-25 >=0 & x+j-25 <25){
            cond_hij <- T
            c <- j-1
            opx <- 1 - oqx[[x+j-24]]
          }
        }
        # sigue un pago más
        if(estado == 1){
          res$Pen[j] <- res$Pen[j] + sal_pen
          res$CotPen[j] <- res$CotPen[j] + cot_pen
        } else if(estado == 2) {
          res$Inv[j] <- res$Inv[j] + sal_pen 
          res$CotPen[j] <- res$CotPen[j] + cot_pen
        }
      }
      if(estado == 3){ # Muerto, considerando reciente
        if(cond_hij){ # si hay un hijo
          if(x+j-25 >= 24){ # si ya creció y pasó los 25 años
            cond_hij <- F # ya no más pensión
          }
          if(runif(1) > opx[j-c]){ # se muere el hijo
            cond_hij <- F
          } else {
            res$Suc[j] <- res$Suc[j] + sal_pen*0.3
            res$CotPen[j] <- res$CotPen[j] + cot_pen*0.3
          }
        }
        if(cond_viu){
          if(runif(1) < vpx[j]){
            if(x+j-1 < 50){
              res$Suc[j] <- res$Suc[j] + sal_pen*0.5
              res$CotPen[j] <- res$CotPen[j] + cot_pen*0.5
            } else if(x+j-1 < 60){
              res$Suc[j] <- res$Suc[j] + sal_pen*0.6
              res$CotPen[j] <- res$CotPen[j] + cot_pen*0.6
            } else {
              res$Suc[j] <- res$Suc[j] + sal_pen*0.7
              res$CotPen[j] <- res$CotPen[j] + cot_pen*0.7
            }
          } else {
            cond_viu <- F # se muere el cónyuge
          }
        }
      }
    } 
  }
  res <- res*v^(-1/2)*v^(1:96)
  res$SEM <- (res$Inv + res$Pen + res$Suc)*anual/anmag*0.085
  res$SEMF <- (res$InvF + res$PenF + res$SucF)*anual/anmag*0.085
  return(res)
}