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