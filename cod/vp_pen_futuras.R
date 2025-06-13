vp_pen_futuras <- function(int, inf){
  int_ef <- ((1+int)/(1+inf))^(1/12) 
  anual <- (1 - int_ef^-12)/(int_ef-1)
  years <- 0:95
  Inv <- 0
  Pen <- 0
  Suc <- 0
  limit2 <- 2e6*(int_ef^-12)^(years)
  limit3.5 <- 3.5e6*(int_ef^-12)^(years)
  # Doble iteración para recorrer toda la data, individuos y años
  for(id in 1:5196){
    # Preliminares del individuo
    x <- cotizantes$edad[id]
    sexo <- cotizantes$sexo[id]
    cuota_ini <- cotizantes$cuotas[id]
    porc_viu <- porcentaje_viudez(x)
    sal_pen <- pensiones[[id]]
    vp_pen <- sal_pen/(1+int)^(0:(length(sal_pen)-1))
    ind_cot <- vp_pen > limit2[1:length(sal_pen)]
    vp_pen <- vp_pen*(anual + int_ef^(-11/12))
    temp <- vp_pen[ind_cot]
    vp_pen[ind_cot] <- temp*0.95
    cot_pen <- rep(0, length(vp_pen))
    cot_pen[ind_cot] <- temp*0.05
    
    
    # Inicialización del dataframe del individuo
    first_pen <- T
    per <- data.frame(act = rep(0, 96), inv = 0, pen = 0, viu = 0, orp = 0, cot = 0, cotv = 0, coth = 0)
    per$act[1] <- 1
    
    for(j in years){
      if(x + j > 115){ # ya no puede vivir
        break
      }
      # Preliminares 
      qx <- all_qx[[sexo]][x+j+1,j+1]
      px <- 1-qx
      v_px <- all_qx[[3-sexo]][x+j+1,j+1]
      qix <- all_qix[[sexo]][x+j-19]
      pix <- 1 - qix
      cot_min <- cotizacion_minima(x+j, sexo)
      dens <- curv_dens[x+j-19]
      n_cot <- cuota_ini + j*dens
      ncot180 <- n_cot >= 180
      per$act[j+2] <- per$act[j+1]*px*pix
      if(vp_pen[j+1]>limit3.5[j+1]){
        cot_pen[j+1] <- cot_pen[j+1] + vp_pen[j+1] - limit3.5[j+1]
        vp_pen[j+1] <- limit3.5[j+1]
      }
      per$cot[j+2] <- per$cot[j+1]*px*int_ef^-12
      # Invalidez
      if(ncot180 | (dens >= 0.5 & cot_min_inv(x+j))){
        per$inv[j+2] <- per$act[j+1]*qix*vp_pen[j+1] + per$inv[j+1]*px*int_ef^-12
      } else if(n_cot>=60 & dens >= 0.5){
        porc_invp <- n_cot/180
        per$inv[j+2] <- per$act[j+1]*qix*porc_invp*vp_pen[j+1] + per$inv[j+1]*px*int_ef^-12
        per$cot[j+2] <- per$act[j+1]*qix*porc_invp*cot_pen[j+1] + per$cot[j+2]
      } else {
        per$inv[j+2] <- per$inv[j+1]*px*int_ef^-12
      }      
      
      # Vejez
      if(n_cot >= cot_min[1] & x+j >= cot_min[2]){
        if(first_pen){
          first_pen <- F
          ini_cot <- n_cot
          cot_ad <- (ini_cot-300)/1200
        }
        cot_post <- max(0, n_cot - ini_cot)*2/1500
        porc_pen <- min(1.25, 1+cot_ad +cot_post)
        per$act[j+2] <- per$act[j+2]*0.1
        
        per$pen[j+2] <- per$act[j+1]*0.9*porc_pen*vp_pen[j+1] + per$pen[j+1]*px*int_ef^-12
        per$cot[j+2] <- per$act[j+1]*0.9*porc_pen*cot_pen[j+1] + per$cot[j+2]
        
      } else if(ncot180 & x+j >= 65) {
        porc_penp <- n_cot/300
        per$act[j+2] <- per$act[j+2]*0.1 # puede seguir trabajando 
        per$pen[j+2] <- per$act[j+1]*0.9*porc_penp*vp_pen[j+1] + per$pen[j+1]*px*int_ef^-12
        per$cot[j+2] <- per$act[j+1]*0.9*porc_penp*cot_pen[j+1] + per$cot[j+2]
      } else {
        per$pen[j+2] <- per$pen[j+1]*px*int_ef^-12
      }
      
      # Sucesión
      per$cotv[j+2] <- per$cotv[j+1]*px*int_ef^-12 + per$act[j+1]*qx
      cond_hij <- x+j-25 < 25 & x+j-25 >=0 # esta última para ver si nació
      ind_suc <- 2
      per$viu[j+2] <- per$viu[j+1]*v_px*int_ef^-12 + sum(per[j+1, 2:3])*qx # ocupa repetir la pensión
      if(cond_hij){
        o_px <- 1 - 0.5*(all_qx[[1]][x-24+j,j+1] + all_qx[[2]][x-24+j,j+1])
        per$orp[j+2] <- per$orp[j+1]*o_px*int_ef^-12 + sum(per[j+1, 2:3])*qx
        per$coth[j+2] <- per$coth[j+1]*px*int_ef^-12 + per$cot[j+1]*qx
      }
      if(ncot180 | dens >= 0.5){
        per$viu[j+2] <- per$viu[j+2] + per$act[j+1]*qx*vp_pen[j+1]
        per$cotv[j+2] <- per$cotv[j+2] + per$act[j+1]*qx*cot_pen[j+1]
        if(cond_hij){
          per$orp[j+2] <- per$orp[j+2] + per$act[j+1]*qx*vp_pen[j+1]
          per$coth[j+2] <- per$coth[j+2] + per$act[j+1]*qx*cot_pen[j+1]
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
    Cot <- per$cot + per$cotv + per$coth
  }
  SEM <- 0.085*anual*(Inv+Pen+Suc)/(anual + int_ef^(-11/12))
  return(t(data.frame(Inv,Pen,Suc,SEM,Cot)))
}

cotizacion_minima <- function(x, sexo){
  cot_min <- 300
  edad_min <- 65
  if(sexo == 2){
    if(x == 64){
      edad_min <- 64
      cot_min <- 357
    }
    if(x == 63){
      cot_min <- 405
      edad_min <- 63
    }
  }
  return(c(cot_min, edad_min))
}

porcentaje_viudez <- function(x){
  if(x < 50){
    porc_viu <- c(rep(0.5, 50-x), rep(0.6, 10), rep(0.7, 115-60))
  } else if(x < 60){
    porc_viu <- c(rep(0.6, 60-x), rep(0.7, 115-60))
  } else {
    porc_viu <- c(rep(0.7, 115-x))
  }
  return(c(0,porc_viu, rep(0, x-20)))
}

cot_min_inv <- function(x) 12 + min(max(x-24, 0), 18)*4 + min(max(0, x-42), 6)*6