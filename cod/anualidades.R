pen_inv <- function(){
  w <- 115
  pen_inv <- list()
  for(sex in 1:2){
    inv <- list()
    for(x in 20:w){
      res <- (w-x+1)
      px <- 1-sapply(1:res, function(y) all_qx[[sex]][x+y, y]) # por si acaso, 
      # la edad es una más en la tabla all_qx
      qi <- all_qix[[sex]][(x-19):96]
      pi <- 1-qi
      tpx <- cumprod(px)
      tpix <- c(1,cumprod(pi)[-res]) # prioridad de invalidez sobre retiro
      vida <- sapply(1:res, function(x) c(tpix[0:(x-1)], rep(qi[x]*tpix[x], res-x+1)))
      vida <- t(vida*tpx)
      diag(vida) <- diag(vida)
      inv[[x-19]] <- vida
    }
    pen_inv[[sex]] <- inv
  }
  return(pen_inv)
}
inv <- pen_inv()
sob_vej <- function(){
  w <- 115
  pen <- list()
  for(sex in 1:2){
    ret <- list()
    for(x in 20:w){
      res <- (w-x+1)
      px <- 1-sapply(1:res, function(y) all_qx[[sex]][x+y, y])
      pi <- 1-all_qix[[1]][(x-19):96]
      tpx <- cumprod(px)
      tpix <- cumprod(pi) # prioridad invalidez sobre retiro
      vida <- t(sapply(1:res, function(x) c(tpix[1:x], rep(tpix[x], res-x))*tpx))
      ret[[x-19]] <- vida
    }
    pen[[sex]] <- ret
  }
  return(pen)
}
pen <- sob_vej()
pen_viu <- function(){
  w <- 115
  pen_viu <- list()
  for(sex in 1:2){
    viu <- list()
    for(x in 20:w){
      res <- (w-x+1)
      if(x < 50){
        porc_viu <- c(rep(0.5, 50-x), rep(0.6, 10), rep(0.7, w+1-60))
      } else if(x < 60){
        porc_viu <- c(rep(0.6, 60-x), rep(0.7, w+1-60))
      } else {
        porc_viu <- c(rep(0.7, res))
      }
      vpx <- 1 - sapply(1:res, function(y) all_qx[[3-sex]][x+y, y])
      vida <- t(sapply(1:res, function(x) c(rep(0, x-1), cumprod(vpx[x:res]))*porc_viu))
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
      porc_orf <- c(rep(0, 25-x), rep(0.3, 25), rep(0, res+x-50))
    } else if(x<50){
      porc_orf <- c(rep(0.3, 50-x), rep(0, res-50+x))
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