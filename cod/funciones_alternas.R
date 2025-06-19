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