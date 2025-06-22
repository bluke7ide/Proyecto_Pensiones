ben_dev <- function(int, inf){
  v <- (1+inf)/(1+int) 
  anual <- (1 - v)/(v^(-1/12)-1)
  anmag <- (anual + v^(11/12)) 
  cot_min <- sapply(0:95, function(y) cotizacion_minima(20+y, 2)[1])
  edad_min <- sapply(0:95, function(y) cotizacion_minima(20+y, 2)[2])
  res <- data.frame(Inv = rep(0, 96), Pen = 0, Suc = 0, SEM = 0, Cot = 0, Cuotas = 0)
  # Doble iteración para recorrer toda la data, individuos y años
  for(id in 1:5196){
    # Preliminares del individuo
    x <- cotizantes$edad[id]
    sexo <- cotizantes$sexo[id]
    cuota_ini <- cotizantes$cuotas[id]
    int_years <- 0:(115-x)
    vp_pen <- c(pensiones[[id]],0)
    ind2_cot <- vp_pen > 2e6
    ind3_cot <- vp_pen > 3.5e6
    cot_pen <- rep(0, length(vp_pen))
    cot_pen[ind2_cot] <- vp_pen[ind2_cot]*0.05
    vp_pen[ind2_cot] <- vp_pen[ind2_cot]*0.95
    cot_pen[ind3_cot] <- cot_pen[ind3_cot] + vp_pen[ind3_cot] - 3.5e6
    vp_pen[ind3_cot] <- 3.5e6
    vp_pen <- vp_pen*anmag
    cot_pen <- cot_pen*anmag
    n_cot <- cuota_ini + cumsum(curv_dens[(x-19)+int_years])
    dev <- cuota_ini/n_cot
    if(sexo == 1){
      cond_pen <- n_cot >= 300 & x+int_years >= 65
    } else {
      cond_pen <- n_cot >= cot_min[int_years+1] & x+int_years >= edad_min[int_years+1]
    }
    ini_pen <- first(which(cond_pen))
    cot_ad <- (n_cot[ini_pen]-300)/1200 + 1
    porc_pen <- sapply(int_years+1, function(y) min(1.25,max(0,n_cot[y]-n_cot[ini_pen])*2/1500+cot_ad))
    
    # Inicialización del dataframe del individuo
    per <- data.frame(inv = rep(0,115-x+1), pen = 0, viu = 0, orf = 0, cotinv = 0, cotpen = 0, cotviu = 0, cotorf = 0)
    
    data_inv <- inv[[sexo]][[x-19]]
    cond_inv <- n_cot >= 180 | cot_min_inv(x+int_years)
    cond_invp <- (n_cot>=60)*n_cot/180
    data_pen <- pen[[sexo]][[x-19]]
    cond_penp <- (n_cot >= 180 & x+int_years >= 65)*n_cot/300
    
    # Condiciones de vejez
    qr <- c(rep(0, ini_pen-1), (0.09)^(0:(115-x+1-ini_pen))*0.9)
    temp <- data_pen*(cond_pen*porc_pen + (!cond_pen * cond_penp))*qr
    per$pen <- readt(temp*vp_pen*dev)
    per$cotpen <- readt(temp*cot_pen*dev)
    
    # Condiciones de invalidez
    qr <- c(rep(1, ini_pen-1), (0.09)^(0:(115-x+1-ini_pen))*0.9)
    
    temp <- data_inv*(cond_inv + (!cond_inv & cond_invp))*qr
    per$inv <- readt(temp*vp_pen*dev)
    per$cotinv <- readt(temp*cot_pen*dev)
    
    # Condiciones de sucesión 
    mux <- qx[[sexo]][[x-20]]
    cond_suc <- (n_cot >= 180)*c(1,diag(data_pen)[-ncol(data_pen)])*mux*((n_cot >= 300)*porc_pen+(!n_cot >= 300)*n_cot/300)
    suc_pen <- (per$pen+per$inv)*mux +  cond_suc*vp_pen*qr
    suc_cot <- (per$cotpen+per$cotinv)*mux +  cond_suc*cot_pen*qr
    # no importa invalidez y muerte al mismo tiempo, entonces por eso el 1 al inicio. 
    # Los demás si para no contabilizar doble pensión
    
    per$viu <- readt(viu[[sexo]][[x-19]]*suc_pen)
    per$cotviu <- readt(viu[[sexo]][[x-19]]*suc_cot)
    if(x < 50){
      per$orf <- readt(orf[[x-19]]*suc_pen*dev)
      per$cotorf <- readt(orf[[x-19]]*suc_cot*dev)
    }
    res$Inv <- res$Inv + c(per$inv, rep(0, 95-(115-x)))
    res$Pen <- res$Pen + c(per$pen, rep(0, 95-(115-x)))
    res$Suc <- res$Suc + c(per$viu + per$orf, rep(0, 95-(115-x)))
    res$Cot <- res$Cot + c(per$cotinv+ per$cotpen + per$cotviu + per$cotorf, rep(0, 95-(115-x)))
    
    # Cotizaciones futuras
    cuotas <- diag(data_pen)*qr*cumprod(curv_sal[x-19+int_years])*cotizantes$sal_prom[id]*curv_dens[(x-19)+int_years]
    res$Cuotas <- res$Cuotas + c(cuotas, rep(0, 95-(115-x)))
  }
  res$Cuotas <- res$Cuotas*0.15*v^(-1/2)
  res$SEM <- 0.085*anual*(res$Inv+res$Pen+res$Suc)/anmag
  
  return(t(res*v^((1:96))))
}