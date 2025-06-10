vp_pen_futuras <- function(int, inf){
  int_ef <- ((1+int)/(1+inf))^(1/12) 
  anual <- (1 - int_ef^-12)/(int_ef-1)
  years <- 0:95
  Inv <- 0
  Pen <- 0
  Suc <- 0
  
  # Doble iteración para recorrer toda la data, individuos y años
  for(id in 1:5196){
    # Preliminares del individuo
    x <- cotizantes$edad[id]
    sexo <- cotizantes$sexo[id]
    cuota_ini <- cotizantes$cuotas[id]
    porc_viu <- porcentaje_viudez(x)
    sal_pen <- pensiones[[id]]
    vp_pen <- sal_pen*(anual + int_ef^(-11/12))
    vp_sem <- sal_pen*anual*0.085
    
    
    # Inicialización del dataframe del individuo
    first_pen <- T
    per <- data.frame(act = rep(0, 96), inv = 0, pen = 0, viu = 0, orp = 0, cot = 0)
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
      n_cot <- cuota_ini + j*dens
      ncot180 <- n_cot >= 180
      per$act[j+2] <- per$act[j+1]*px*pix
      
      # Invalidez
      if(ncot180 | (dens >= 0.5 & cot_min_inv(x+j))){
        per$inv[j+2] <- per$act[j+1]*qix*vp_pen[j+1] + per$inv[j+1]*px*int_ef^-12
      } else if(n_cot>=60 & dens >= 0.5){
        porc_invp <- n_cot/180
        per$inv[j+2] <- per$act[j+1]*qix*porc_invp*vp_pen[j+1] + per$inv[j+1]*px*int_ef^-12
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
        
      } else if(ncot180 & x+j >= 65) {
        porc_penp <- n_cot/300
        per$act[j+2] <- per$act[j+2]*0.1 # puede seguir trabajando 
        per$pen[j+2] <- per$act[j+1]*0.9*porc_penp*vp_pen[j+1] + per$pen[j+1]*px*int_ef^-12
      } else {
        per$pen[j+2] <- per$pen[j+1]*px*int_ef^-12
      }
      
      # Sucesión
      cond_hij <- x+j-25 < 25 & x+j-25 >=0 # esta última para ver si nació
      ind_suc <- 2
      per$viu[j+2] <- per$viu[j+1]*v_px*int_ef^-12 + sum(per[j+1, 2:3])*qx # ocupa repetir la pensión
      if(cond_hij){
        o_px <- 1 - 0.5*(all_qx[[1]][x-24+j,j+1] + all_qx[[2]][x-24+j,j+1])
        per$orp[j+2] <- per$orp[j+1]*o_px*int_ef^-12 + sum(per[j+1, 2:3])*qx
      }
      if(ncot180 | dens >= 0.5){
        per$viu[j+2] <- per$viu[j+2] + sum(per[j+1, 1])*qx*vp_pen[j+1]
        per$orp[j+2] <- per$orp[j+2] + sum(per[j+1, 1])*qx*vp_pen[j+1]
      } 
    }
    per$viu <- per$viu*porc_viu
    per$orp <- per$orp*0.3
    Inv <- Inv + sum(per$inv)
    Pen <- Pen + sum(per$pen)
    Suc <- Suc + sum(per$orp) + sum(per$viu)
  }
  SEM <- 0.085*anual*(Inv+Pen+Suc)/(anual + int_ef^(-11/12))
  return(t(data.frame(Inv,Pen,Suc,SEM)))
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