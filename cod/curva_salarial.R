curva_sal <- function(){
  IPC <- read_excel("data/ipc_inec.xlsx", skip = 3)
  BD_Cotizantes <- read_excel("data/BD_Cotizantes.xlsx")
  colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)] <- as.character(
    as.Date(as.numeric(colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)]),
            origin = "1899-12-30"))
  
  # ---------------------------------
  curva_salarial <- BD_Cotizantes %>%
    select(-c(ID, Sexo)) %>%
    pivot_longer(cols = c(-Fec.Nac),
                 names_to = "Fecha",
                 values_to = "Salario") %>%
    filter(Salario > 0) %>%
    mutate(
      Fecha = ymd(Fecha),
      Fec.Nac = ymd(Fec.Nac),
      Edad = floor(as.numeric(difftime(
        Fecha, Fec.Nac, units = "days"
      )) / 365.25),
      Fecha_num = match(Fecha, sort(unique(Fecha))),
      Factor_ajuste = 110.39017 / IPC$Nivel[228 + Fecha_num],
      Salario_ajustado = Salario * Factor_ajuste
    ) %>%
    group_by(Edad) %>%
    summarise(Salario_promedio = mean(Salario_ajustado),
              std = var(Salario_ajustado)^0.5) %>%
    ungroup()  %>% 
    complete(Edad = 20:115) %>% 
    mutate(
      Salario_promedio = ifelse(is.na(Salario_promedio), 
                                Salario_promedio[Edad == 70], 
                                Salario_promedio),
      s_x = lead(Salario_promedio) / Salario_promedio,
      s_x = ifelse(is.na(s_x), 1, s_x),
      std = std/Salario_promedio
    )
    mean_lm <- lm(s_x ~ log(Edad), curva_salarial[15:40,])
    std_lm <- lm(std ~ log(Edad), curva_salarial[15:40,])
    curva_salarial[41:96,4] <- predict.lm(mean_lm, data.frame(Edad = 60:115))
    curva_salarial[41:96,3] <- predict.lm(std_lm, data.frame(Edad = 60:115))
    return(curva_salarial)
}
curva_salarial <- curva_sal()