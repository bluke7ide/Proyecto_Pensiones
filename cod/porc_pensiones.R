porc_pensiones <- function(id){
  # Preliminares
  x <- cotizantes$edad[id]
  sexo <- cotizantes$sexo[id]
  dens <- cotizantes$densidades[id]
  rango <- 1:min(61,(115-x))
  n_cot <- unlist(cuotas[id,rango])
  qx <- all_qx(x, sexo, año)
  px <- 1 - qx
  v_px <- 1 - all_qx(x, sex_con, año)
  qix <- all_qix(x, sexo, año)
  pix <- 1 - qix
  sim <- data.frame(act = rep(0, 116-x),inv = 0,pen = 0,surv = 0,suc = 0,orph = 0)
  sim[1,1] <- 1
  
  # Porcentajes conyuge
  sex_con <- 3 - sexo
  porc_viu <- porcentaje_viudez(x)
  
  # Si aplica al hijo, o si no ha nacido
  cond_hij <- x-25 < 25 & x-25 >=0
  if(cond_hij){
    o_px <- 1 - 0.5*(all_qx(x-25, 1, año) + all_qx(x-25, 2, año))
  }
  
  # Condición y porcentajes para pensionarse
  cond_pen <- n_cot >= 300 & x+rango >= 65
  ini_cot <- which(cond_pen)[1]
  cot_ad <- (n_cot[ini_cot]-300)/1200 # 0.083%
  cot_post <- sapply(rango, function(x) max(0, n_cot[x]-n_cot[ini_cot]) *2/1500) # 0.133%
  porc_pen <- cond_pen*(1+cot_ad +cot_post)
  porc_pen <- sapply(rango, function(x) min(1.25, porc_pen[x]))
  
  if(sexo == 2){
    if(x <= 64){
      cond_pen[65-x] <- n_cot[65-x] >= 357
    }
    if(x <= 63){
      cond_pen[64-x] <- n_cot[64-x] >= 405
    }
  }
  
  # Condiciones de cuotas de pensiones
  req_invp <- sapply(x+rango, cot_min_inv)
  ncot180 <- n_cot >= 180
  cond_penp <- ncot180 & x+rango >= 65
  cond_inv <- ncot180 | (dens > 0.5 &  req_invp < n_cot) 
  cond_invp <- n_cot >= 60 & dens > 0.5
  cond_tras <- ncot180 | dens > 0.5
  
  # Se realiza una tabla acumulativa
  for(i in 1:(115-x)){
    sim[i+1,1] <- sim[i,1]*px[i]*pix[i]
    if(i < length(rango)){
      # Invalidez
      if(cond_inv[i]){
        sim[i+1,2] <- sim[i,1]*px[i]*qix[i] + sim[i,2]*px[i]
        # Si no se pudo normal, entonces pruebe proporcional
      } else if(cond_invp[i]){
        sim[i+1,2] <- sim[i,1]*px[i]*qix[i]*n_cot[i]/req_invp[i] + sim[i,2]*px[i]
      }
      
      # Vejez
      if(cond_pen[i]){
        # probabilidad de retiro, calcular % y multiplicar acá
        sim[i+1,3] <- sim[i,3]*px[i] + sim[i,1]*0.9*px[i] 
        sim[i+1,1] <- sim[i+1,1]*0.1
        
      } else if(cond_penp[i]){
        sim[i+1,3] <- sim[i,3]*px[i] + sim[i,1]*0.9*px[i]
        sim[i+1,1] <- sim[i+1,1]*0.1
        # postergación en proporcion?
      }
      
      # Sucesión
      sim[i+1,5] <- sim[i,5]*v_px[i]+sim[i,4]*qx[i]
      if(cond_tras[i]){
        sim[i+1,5] <- sim[i+1,5] + sim[i,1]*qx[i]
      }
      if(cond_hij){
        if(x-25+i > 25){
          cond_hij <- F
        } else {
          sim[i+1,6] <- sim[i,6]*o_px[i] + sim[i,4]*qx[i]
          if(cond_tras[i]){
            sim[i+1,6] <- sim[i+1,6] + sim[i,1]*qx[i]
          }
        }
      }
      
    } else {
      sim[i+1,2] <- sim[i,2]*px[i]
      sim[i+1,3] <- sim[i,3]*px[i]
      sim[i+1,5] <- sim[i,5]*v_px[i]
    }
    sim[i+1,4] <- sim[i+1,2] + sim[i+1,3]
  }
  # Porcentajes y desglose
  sim[,2] <- sim[,2]*c(0,porc_viu)
  sim[,3] <- sim[,3]*0.3
  sim[,5] <- sim[,5] + sim[,6]
  sim <- sim %>% select(2,3,5)
  return(sim+1-1) # volarse los decimales de precisión
}