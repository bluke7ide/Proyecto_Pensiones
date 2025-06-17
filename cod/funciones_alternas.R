sal_pen <- function(x, cuota, sal_prom, cuotas_past){
  n_cot <- c(0,cumsum(curv_dens[(x-19):96]))+cuota
  salarios <- cumprod(curv_sal[(x-19):96])*sal_prom
  enteros <- round(n_cot - n_cot[1])
  cantidades <- enteros[-1] - enteros[-length(enteros)]
  sal_pen <- rep(0, 115-x)
  sal_pen[1] <- salarios[1]
  cuotas <- unlist(cuotas_past*niveles)
  cuotas <- cuotas[cuotas>10000]
  for(i in 1:(114-x)){
    cuotas <- c(cuotas, rep(salarios[i+1], cantidades[i]))
    calc_pen <- cuotas[sum(1 - cuotas < 5e6)/enteros[i+1] > 0.5 | cuotas < 5e6]
    sal_pen[i+1] <- mean(head(sort(calc_pen, decreasing = T), 300))
  }
  porc <- c(0.525,0.51, 0.494, 0.478, 0.462, 0.446, 0.43)
  montos <- c(2,3,4,5,6,8) * 367108.55
  indices <- findInterval(sal_pen, montos) + 1
  sal_pen <- sal_pen * porc[indices]
  return(sal_pen)
}

cot_prop <- function(){
  # Tomando el valor presente
  vp_cot <- sapply(1:ncol(cotizaciones), function(x) t(cotizaciones[,x]) * niveles[[x]])
  
  # Salarios más de 10 mil cuentan como cotizaciones
  data <- vp_cot > 10000
  
  # Cantidad de cuotas
  cuotas <- rowSums(data)
  calc_pen <- sapply(1:nrow(data), function(x) vp_cot[x, 349:360])
  
  # Cálculo explícito
  Sx <- sapply(1:nrow(data), function(x) ifelse(
    is.na(mean(calc_pen[calc_pen[,x]!=0,x])), 0, 
    mean(calc_pen[calc_pen[,x]!=0,x])))
  
  return(data.frame(sal_prom = Sx, cuotas = cuotas))
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