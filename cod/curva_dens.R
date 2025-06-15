curva_dens <- function(){
  vp_cot <- sapply(1:ncol(cotizaciones), function(x) t(cotizaciones[,x]) * niveles[[x]])
  ind <- (vp_cot > 10000)*1
  n_cot <- as.data.frame(t(sapply(1:5196, function(x)
    sapply(1:30, function(y) sum(ind[x,1:12+(y-1)*12])))))
  names(n_cot) <- 1:30
  n_cot$edad <- cotizantes$edad
  n_cot$inicio <- sapply(1:5196, function(x) first(which(n_cot[x,]!=0))) 
  edades <- n_cot %>% 
    pivot_longer(-c(edad,inicio)) %>% 
    mutate(edad = edad - 30 + as.numeric(name),
           considerar = inicio <= as.numeric(name))
  curva_dens <- edades[edades$considerar,] %>% 
    select(-name, - inicio, -considerar) %>% 
    group_by(edad) %>% 
    summarise(media = mean(value),
              std = var(value)^0.5)
  dens_lm <- lm(media ~ log(edad), curva_dens[10:40,]) # se fitea log
  std_lm <- lm(std ~ log(edad),  curva_dens[5:40,] )
  curva_dens[1:96,1] <- 20:115
  curva_dens[41:96,2] <- predict.lm(dens_lm, data.frame(edad = 60:115))
  curva_dens[41:96,3] <- predict.lm(std_lm, data.frame(edad = 60:115))
  return(curva_dens)
}
curva_densidad <- curva_dens()
cot_prop <- function(){
  # Tomando el valor presente
  vp_cot <- sapply(1:ncol(cotizaciones), function(x) t(cotizaciones[,x]) * niveles[[x]])
  
  # Salarios más de 10 mil cuentan como cotizaciones
  data <- vp_cot > 10000
  
  # Cantidad de cuotas
  cuotas <- rowSums(data)
  
  # Promedio de los 300 mejores salarios
  cot_pen <- vp_cot < 5e6
  
  # a menos de que históricamente haya cotizado 5m, se veta
  true_counts <- rowSums(1 - cot_pen)/cuotas > 0.5 # al menos más de la mitad 
  cot_pen[true_counts,] <- TRUE
  
  # Se fusionan las condiciones
  cot_pen <- cot_pen & data
  calc_pen <- sapply(1:nrow(data), function(x) vp_cot[x, cot_pen[x,]])
  
  # Cálculo explícito
  Sx <- sapply(1:nrow(data), function(x) 
    mean(head(sort(calc_pen[[x]], decreasing = T), 300)))
  
  return(data.frame(sal_prom = Sx, cuotas = cuotas))
}

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
