ultimo_año <- data.frame(activo = rowSums(BD_Cotizantes %>%
                                            select(tail(names(.), 12)) > 0)>0)
cotizantes <- BD_Cotizantes %>% 
  select(2,3) %>% 
  mutate(estado = ultimo_año$activo)
pensionados <- BD_Pensionados %>% 
  select(4,5) %>% 
  mutate(estado = 2)
names(pensionados) <- names(cotizantes)
poblacion <- rbind(cotizantes, pensionados) %>% 
  mutate(edad = 2024 - year(Fec.Nac)) %>% 
  select(-1)
names(poblacion) <- tolower(names(poblacion))
poblacion[poblacion$sexo == "1",]$sexo <- "M"
poblacion[poblacion$sexo == "2",]$sexo <- "F"
cotizantes <- poblacion[1:5196,]
pensionados <- poblacion[5197:5561,]

# Crear grupos de edad
df <- poblacion %>%
  mutate(grupo_edad = cut(
    edad,
    breaks = seq(0, 100, by = 5),
    right = FALSE,
    labels = paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-")
  ))

# Agrupar por grupo de edad y sexo
df_piramide <- df %>%
  group_by(grupo_edad, sexo) %>%
  summarise(poblacion = n(), .groups = "drop") %>%
  mutate(poblacion = ifelse(sexo == "M", -poblacion, poblacion))

fig <- ggplot(df_piramide, aes(x = grupo_edad, y = poblacion, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = abs,
    limits = c(-1, 1) * max(abs(df_piramide$poblacion))
  ) +
  labs(
    x = "Grupo de Edad",
    y = "Población",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# ggsave("res/aed/piramide.pdf", fig)

df <- poblacion %>%
  mutate(
    sexo = recode(sexo, "M" = "Hombre", "F" = "Mujer"),
    estado = recode_factor(
      as.character(estado),
      "1" = "Activo",
      "0" = "No activo",
      "2" = "Pensionado"
    )
  )

# Agrupación por sexo
sexo_df <- df %>%
  count(categoria = sexo) %>%
  mutate(grupo = "Sexo")

# Agrupación por estado
estado_df <- df %>%
  count(categoria = estado) %>%
  mutate(grupo = "Estado")

resumen <- bind_rows(sexo_df, estado_df)

colores <- c(
  "Hombre" = "#377EB8",
  "Mujer" = "#984EA3",
  "Activo" = "#4DAF4A",
  "No activo" = "#FF7F00",
  "Pensionado" = "#999999"
)


fig <- ggplot(resumen, aes(x = grupo, y = n, fill = categoria)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = colores) +
  labs(
    x = NULL,
    y = "Cantidad de Personas",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "right"
  )

# ggsave("res/aed/resumen_pob.pdf", fig)

cotizantes_activos <- cotizantes[cotizantes$estado == 1, ]

bar_totals <- cotizantes_activos %>%
  count(edad) %>%
  mutate(
    mitad = n / 2,
    edad = factor(edad)  
  )


fig <- ggplot(cotizantes_activos, aes(x = factor(edad), fill = sexo)) +
  geom_bar(position = "stack") +
  # Línea horizontal en la mitad de cada barra
  geom_segment(
    data = bar_totals,
    aes(x = edad, xend = edad, y = mitad - 0.5, yend = mitad + 0.5),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 3
  ) +
  scale_x_discrete(
    breaks = as.character(seq(min(cotizantes_activos$edad), max(cotizantes_activos$edad), by = 5))
  ) +
  labs(
    x = "Edad",
    y = "Cantidad de cotizantes activos"
  ) +
  theme_minimal() 

# ggsave("res/aed/dist_activos.pdf", fig)

cotizantes <- cotizantes %>% 
  mutate(antiguedad = apply(BD_Cotizantes[, 4:ncol(BD_Cotizantes)], 1, function(x) which(x != 0)[1]))
cotizantes$antiguedad <- 2024 - year(as_datetime(names(BD_Cotizantes)[cotizantes$antiguedad+3]))

fig <- ggplot(cotizantes, aes(x = antiguedad, fill = sexo)) + 
  geom_bar(position = "stack") +
  labs(
    x = "Años de antiguedad en el régimen",
    y = "Cantidad de personas"
  ) +
  theme_minimal()

# ggsave("res/aed/activos_ant.pdf", fig)

bar_totals <- pensionados %>%
  count(edad) %>%
  mutate(
    mitad = n / 2,
    edad = factor(edad) 
  )

fig <- ggplot(pensionados, aes(x = factor(edad), fill = sexo)) +
  geom_bar(position = "stack") +
  scale_x_discrete(
    breaks = as.character(seq(min(pensionados$edad), max(pensionados$edad), by = 5))
  ) +
  labs(
    x = "Edad",
    y = "Cantidad de cotizantes activos"
  ) +
  theme_minimal() 
# ggsave("res/aed/dist_pensionados.pdf", fig)

pensionados <- pensionados %>% 
  mutate(antiguedad = 2024 - year(BD_Pensionados$`Rige de la Pensión`))
fig <- ggplot(pensionados, aes(x = antiguedad, fill = sexo)) + 
  geom_bar(position = "stack") +
  labs(
    x = "Años pensionado",
    y = "Cantidad de pensionados"
  ) +
  theme_minimal() 
# ggsave("res/aed/pensionados_ant.pdf", fig)

n_meses <- ncol(BD_Cotizantes) - 3

# cuántas ventanas de 12 meses podemos formar
n_periodos <- n_meses - 12 + 1
if(n_periodos < 1) stop("¡No hay suficientes meses para formar un año completo!")

# Preparamos un objeto para guardar los indicadores
activo <- matrix(0L, 
                 nrow = nrow(BD_Cotizantes), 
                 ncol = n_periodos)

# Recorremos cada “ventana” de 12 meses
for(k in seq_len(n_periodos)){
  cols_k <- (3 + k):(3 + k + 11)    # e.g. para k=1, cols 4:15; para k=2, 5:16; etc.
  submat  <- BD_Cotizantes[, cols_k]
  
  # fila i = 1 si en esos 12 meses hubo al menos 1 >0
  activo[, k] <- as.double(rowSums(submat > 0) > 0)
}

# Ponemos nombres a cada periodo (opcional)
colnames(activo) <- colnames(BD_Cotizantes)[15:363]

# Finalmente, volcamos en comportamientos
# (aquí asumo que quieres mantener las 3 primeras columnas y luego estos indicadores)
comportamientos <- cbind(
  BD_Cotizantes[, 1:3],
  as.data.frame(activo)
)

afiliados <- data.frame(fecha = colnames(BD_Cotizantes)[15:363], activos = colSums(activo))
fig <- ggplot(afiliados, aes(x = fecha, y = activos)) + 
  scale_x_discrete(breaks = afiliados$fecha[seq(1, nrow(afiliados), by = 60)]) +
  geom_point(color = 'purple') +
  labs(
    x = "Fecha",
    y = "Cantidad de afiliados"
  ) +
  theme_minimal() 
# ggsave("res/aed/comportamiento.pdf", fig)

diffs <- activo[, -1, drop = FALSE] - activo[, -ncol(activo), drop = FALSE]

Altas     <- colSums(diffs ==  1)  # pasó de 0 a 1
Bajas     <- colSums(diffs == -1)  # pasó de 1 a 0

resultado <- data.frame(
  fecha     = colnames(BD_Cotizantes)[16:363],
  Altas     = Altas,
  Bajas     = Bajas,
  Afiliados = colSums(activo)[2:349],
  row.names = NULL
)

