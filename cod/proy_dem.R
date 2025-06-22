proy_dem <- function(){
  cot_min <- sapply(0:95, function(y) cotizacion_minima(20+y, 2)[1])
  edad_min <- sapply(0:95, function(y) cotizacion_minima(20+y, 2)[2])
  res <- data.frame(Act = rep(0, 96), Inact = 0, Inv = 0, Pen = 0, Viu = 0, Orf = 0)
  # Doble iteración para recorrer toda la data, individuos y años
  for(id in 1:5196){
    # Preliminares del individuo
    x <- cotizantes$edad[id]
    sexo <- cotizantes$sexo[id]
    cuota_ini <- cotizantes$cuotas[id]
    int_years <- 0:(115-x)
    cond_act <- cotizantes$sal_prom[id] != 0

    n_cot <- cuota_ini + cumsum(curv_dens[(x-19)+int_years])
    if(sexo == 1){
      cond_pen <- n_cot >= 300 & x+int_years >= 65
    } else {
      cond_pen <- n_cot >= cot_min[int_years+1] & x+int_years >= edad_min[int_years+1]
    }
    ini_pen <- first(which(cond_pen))
    
    # Inicialización del dataframe del individuo
    per <- data.frame(act = rep(0,115-x+1), inv = 0, pen = 0, viu = 0, orf = 0)
    
    data_inv <- inv[[sexo]][[x-19]]
    cond_inv <- n_cot >= 180 | cot_min_inv(x+int_years)
    cond_invp <- n_cot>=60
    data_pen <- pen[[sexo]][[x-19]]
    cond_penp <- (n_cot >= 180 & x+int_years >= 65)
    
    # Condiciones de vejez
    qr <- c(rep(0, ini_pen-1), (0.09)^(0:(115-x+1-ini_pen))*0.9)
    temp <- data_pen*(cond_pen + (!cond_pen * cond_penp))*qr
    per$pen <- readt(temp)
    
    # Condiciones de invalidez
    qr <- c(rep(1, ini_pen-1), (0.09)^(0:(115-x+1-ini_pen))*0.9)
    
    temp <- data_inv*(cond_inv + (!cond_inv & cond_invp))*qr
    per$inv <- readt(temp)
    
    # Condiciones de sucesión 
    mux <- qx[[sexo]][[x-20]]
    cond_suc <- (n_cot >= 180)*c(1,diag(data_pen)[-ncol(data_pen)])*mux*((n_cot >= 300)+(n_cot <= 300 & n_cot >= 180))
    suc_pen <- (per$pen+per$inv)*mux +  cond_suc*qr
    # no importa invalidez y muerte al mismo tiempo, entonces por eso el 1 al inicio. 
    # Los demás si para no contabilizar doble pensión
    
    per$viu <- readt(viu[[sexo]][[x-19]]*suc_pen)
    if(x < 50){
      per$orf <- readt(orf[[x-19]]*suc_pen)
    }
    
    per$act <- diag(data_pen)*qr
    
    if(cond_act){
      res$Act <- res$Act + c(1,per$act, rep(0, 94-(115-x)))
    } else {
      res$Inact <- res$Inact + c(1,per$act, rep(0, 94-(115-x)))
    }
    res$Inv <- res$Inv + c(0,per$inv, rep(0, 94-(115-x)))
    res$Pen <- res$Pen + c(0,per$pen, rep(0, 94-(115-x)))
    res$Viu <- res$Viu + c(0,per$viu, rep(0, 94-(115-x)))
    res$Orf <- res$Orf + c(0,per$orf, rep(0, 94-(115-x)))
  }
  
  return(res)
}
# Quitando los porcentajes de pension
pen_viu <- function(){
  w <- 115
  pen_viu <- list()
  for(sex in 1:2){
    viu <- list()
    for(x in 20:w){
      res <- (w-x+1)
      vpx <- 1 - sapply(1:res, function(y) all_qx[[3-sex]][x+y, y])
      vida <- t(sapply(1:res, function(x) c(rep(0, x-1), cumprod(vpx[x:res]))))
      viu[[x-19]] <- vida
    }
    pen_viu[[sex]] <- viu
  }
  return(pen_viu)
}
viu <- pen_viu()
pen_orf <- function(){
  w <- 115
  orf <- list()
  for(x in 20:49){
    res <- (w-x+1)
    if(x < 25){
      porc_orf <- c(rep(0, 25-x), rep(1, 25), rep(0, res+x-50))
    } else if(x<50){
      porc_orf <- c(rep(1, 50-x), rep(0, res-50+x))
    } else {
      orf[[x-19]]  <- rep(0, res)
      next
    }
    opx <- 1 - (sapply(1:res, function(y) all_qx[[1]][x+y, y]) + 
                  sapply(1:res, function(y) all_qx[[2]][x+y, y]))/2
    vida <- t(sapply(1:res, function(x) c(rep(0, x-1), cumprod(opx[x:res]))*porc_orf))
    orf[[x-19]] <- vida
  }
  return(orf)
}
orf <- pen_orf()