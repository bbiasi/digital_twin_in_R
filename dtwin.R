library(readr)
library(tidyverse)
library(GGally)

# referencia

discharge <- read_csv("discharge.csv")

glimpse(discharge)
table(discharge$Battery)
table(discharge$id_cycle)

# filtrando para B0005
df <- discharge |> 
  dplyr::filter(Battery == "B0005")

# 
df |> 
  dplyr::filter(id_cycle < 8) |> 
  ggpairs(columns = c(1:5, 7),
          aes(colour = as.character(id_cycle), alpha = 0.4))

df |> 
  dplyr::filter(id_cycle < 160) |> 
  ggpairs(columns = c(1:5, 7),
          aes(colour = as.character(id_cycle), alpha = 0.4))


# maximo por ciclo
dfb <- df |> 
  dplyr::group_by(id_cycle) |> 
  dplyr::summarise(across(everything(), max, na.rm = TRUE))

# Calcular tempo cumulativo
dfb <- dfb |> 
  dplyr::mutate(Cumulated_T = cumsum(Time))



# Cálculo de L
initial_capacity <- dfb$Capacity[1]
L <- (dfb$Capacity - initial_capacity) / (-initial_capacity)

# Parâmetro K
K <- 0.13

# Cálculo de L_1
L_1 <- 1 - exp(-K * as.numeric(row.names(dfb)) * dfb$Temperature_measured / dfb$Time)

# Cálculo da Capacidade Corrigida (C. Capacity)
dfb <- dfb |> 
  dplyr::mutate(`C. Capacity` = -(L_1 * initial_capacity) + initial_capacity)


gg <- ggplot(dfb, aes(x = id_cycle)) +
  geom_line(aes(y = `C. Capacity`, color = "Physical model"), 
            linewidth = 0.8, color = "navy") +
  geom_point(aes(y = Capacity, color = "NASA dataset"), 
             size = 2, shape = 4, color = "grey50") +
  labs(
    title = "Physical model comparison",
    x = "Cycles",
    y = "C, Capacity [Ahr]",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("Physical model" = "navy", "NASA dataset" = "grey50"),
    guide = guide_legend(override.aes = list(
      linetype = c(1, 0),
      shape = c(NA, 4),
      size = c(0.8, 2)
    )))
gg
