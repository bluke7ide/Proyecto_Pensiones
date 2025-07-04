ben_dev <- function(int, inf){
  v <- (1+inf)/(1+int) 
  anual <- (1 - v)/(v^(-1/12)-1)
  anmag <- (anual + v^(11/12)) # anual más aguinaldo
  years <- 0:95
  Inv <- 0
  Pen <- 0
  Suc <- 0
  Cot <- 0
  Cuotas <- 0
  
  # Doble iteración para recorrer toda la data, individuos y años
  for(id in 1:5196){
    # Preliminares del individuo
    x <- cotizantes$edad[id]
    sexo <- cotizantes$sexo[id]
    ini_cot <- cotizantes$cuotas[id]
    n_cot <- ini_cot
    salario <- cotizantes$sal_prom[id]
    porc_viu <- porcentaje_viudez(x)
    vp_pen <- c(pensiones[[id]], rep(0, x-19))
    ind2_cot <- vp_pen > 2e6
    ind3_cot <- vp_pen > 3.5e6
    cot_pen <- rep(0, length(vp_pen))
    cot_pen[ind2_cot] <- vp_pen[ind2_cot]*0.05
    vp_pen[ind2_cot] <- vp_pen[ind2_cot]*0.95
    cot_pen[ind3_cot] <- cot_pen[ind3_cot] + vp_pen[ind3_cot] - 3.5e6
    vp_pen[ind3_cot] <- 3.5e6
    vp_pen <- vp_pen*anmag
    cot_pen <- cot_pen*anmag
    # Inicialización del dataframe del individuo
    first_pen <- T
    per <- data.frame(act = rep(0, 96), inv = 0, pen = 0, viu = 0, orp = 0,
                      cotp = 0, cotv = 0, coth = 0, cuotas = 0)
    per$act[1] <- 1
    
    for(j in years){
      if(x + j > 115){ # ya no puede vivir
        break
      }
      # Preliminares 
      qx <- all_qx[[sexo]][x+j+1,j+1]
      px <- 1-qx
      v_px <- 1 - all_qx[[3-sexo]][x+j+1,j+1]
      qix <- all_qix[[sexo]][x+j-19]
      pix <- 1 - qix
      cot_min <- cotizacion_minima(x+j, sexo)
      dens <- curv_dens[x+j-19]
      n_cot <- n_cot + dens
      ncot180 <- n_cot >= 180
      per$act[j+2] <- per$act[j+1]*px*pix
      per$cotp[j+2] <- per$cotp[j+1]*px
      per$inv[j+2] <- per$inv[j+1]*px
      salario <- salario*curv_sal[x+j-19]
      dev <- ini_cot/n_cot
      
      # Invalidez
      if(ncot180 | (dens >= 0.5 & cot_min_inv(x+j))){
        per$inv[j+2] <- per$act[j+1]*qix*vp_pen[j+1]*dev + per$inv[j+2] 
        per$cotp[j+2] <- per$act[j+1]*qix*cot_pen[j+1]*dev + per$cotp[j+2]
      } else if(n_cot>=60){
        porc_invp <- n_cot/180
        per$inv[j+2] <- per$act[j+1]*qix*porc_invp*vp_pen[j+1]*dev + per$inv[j+2] 
        per$cotp[j+2] <- per$act[j+1]*qix*porc_invp*cot_pen[j+1]*dev + per$cotp[j+2]
      }   
      per$pen[j+2] <- per$pen[j+1]*px
      
      # Vejez
      if(n_cot >= cot_min[1] & x+j >= cot_min[2]){
        if(first_pen){
          first_pen <- F
          ini_cot <- n_cot
          cot_ad <- (ini_cot-300)/1200
        }
        cot_post <- max(0, n_cot - ini_cot)*2/1500
        porc_pen <- min(1.25, 1+cot_ad +cot_post)
        
        per$pen[j+2] <- per$act[j+2]*0.9*porc_pen*vp_pen[j+1]*dev + per$pen[j+2]
        per$cotp[j+2] <- per$act[j+2]*0.9*porc_pen*cot_pen[j+1]*dev + per$cotp[j+2]
        per$act[j+2] <- per$act[j+2]*0.1
        
      } else if(ncot180 & x+j >= 65) {
        porc_penp <- n_cot/300
        per$pen[j+2] <- per$act[j+2]*0.9*porc_penp*vp_pen[j+1]*dev + per$pen[j+2]
        per$cotp[j+2] <- per$act[j+2]*0.9*porc_penp*cot_pen[j+1]*dev + per$cotp[j+2]
        per$act[j+2] <- per$act[j+2]*0.1 # puede seguir trabajando 
      } 
      per$cuotas[j+2] <- dens*salario*per$act[j+2] 
      # Sucesión
      per$cotv[j+2] <- per$cotv[j+1]*px + per$cotp[j+1]*qx
      cond_hij <- x+j-25 < 25 & x+j-25 >=0 # esta última para ver si nació
      ind_suc <- 2
      per$viu[j+2] <- per$viu[j+1]*v_px + sum(per[j+1, 2:3])*qx # ocupa repetir la pensión
      if(cond_hij){
        o_px <- 1 - 0.5*(all_qx[[1]][x-24+j,j+1] + all_qx[[2]][x-24+j,j+1])
        per$orp[j+2] <- per$orp[j+1]*o_px + sum(per[j+1, 2:3])*qx # ya hay monto pensión
        per$coth[j+2] <- per$coth[j+1]*o_px + per$cotp[j+1]*qx
      }
      if(ncot180 | dens >= 0.5){
        per$viu[j+2] <- per$viu[j+2] + per$act[j+1]*qx*vp_pen[j+1]*dev
        per$cotv[j+2] <- per$cotv[j+2] + per$act[j+1]*qx*cot_pen[j+1]*dev
        if(cond_hij){
          per$orp[j+2] <- per$orp[j+2] + per$act[j+1]*qx*vp_pen[j+1]*dev
          per$coth[j+2] <- per$coth[j+2] + per$act[j+1]*qx*cot_pen[j+1]*dev
        }
      } 
    }
    per$viu <- per$viu*porc_viu
    per$orp <- per$orp*0.3
    per$cotv <- per$cotv*porc_viu
    per$coth <- per$coth*0.3
    Inv <- Inv + per$inv
    Pen <- Pen + per$pen
    Suc <- Suc + per$orp + per$viu
    Cot <- Cot + per$cotp + per$cotv + per$coth
    Cuotas <- Cuotas + per$cuotas
    
  }
  Cuotas <- Cuotas*0.15*v^(1/2)
  SEM <- 0.085*anual*(Inv+Pen+Suc)/(anual + v^(11/12))
  return(data.frame(Inv,Pen,Suc,SEM,Cot,Cuotas)*v^years)
}