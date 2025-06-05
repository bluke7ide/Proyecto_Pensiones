source("cod/setup.R")
IPC <- read_excel("data/ipc_inec.xlsx", skip = 3)
BD_Cotizantes <- read_excel("data/BD_Cotizantes.xlsx")
colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)] <- as.character(
  as.Date(as.numeric(colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)]),
          origin = "1899-12-30"))

fecha_cols <- names(BD_Cotizantes)[!(names(BD_Cotizantes) %in% c("ID", "Fec.Nac", "Sexo"))]

base_long <- BD_Cotizantes %>%
  pivot_longer(cols = all_of(fecha_cols),
               names_to = "Fecha",
               values_to = "Salario") %>%
  mutate(
    Fecha = as.Date(Fecha),
    Fec.Nac = as.Date(Fec.Nac),
    Edad = floor(as.numeric(difftime(Fecha, Fec.Nac, units = "days")) / 365.25),
    Tiempo = 2024-year(Fecha),
    Salario_ajustado = Salario *(1+0.03)^Tiempo
  ) %>% 
  filter(Salario > 0)

curva <- base_long %>%
  group_by(Edad) %>%
  summarise(SalarioPromedio = mean(Salario_ajustado), .groups = "drop")

ggplot(curva, aes(x = Edad, y = SalarioPromedio)) +
  geom_line(size = 1) +
  labs(
    title = "Curva salarial promedio por edad y sexo",
    x = "Edad",
    y = "Salario promedio nominal",
  ) +
   theme_minimal() 

