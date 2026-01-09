#############################################################################
# Title: Preliminar Forest Change Analytics Scripts (FCAS)
# 
# Authors: 
#   Alexander Quevedo Chacón — alexander.quevedo@gmail.com
#   Luis Robles — albert.physik@gmail.com
#   Freddy Argoty — fargotty@gmail.com
#   
# Developer and Maintainer: Alexander Quevedo Chacón
# 
# Project contributors:
#   
#   Arles Taboada - arlestaboada@gmail.com
#   Jorge Nole - jorge.l.nole.m@gmail.com
#   Isabel Pino -  isapc27@gmail.com
#   Erick Principe - romelprincipea@gmail.com
#   
# Organization: PROFONANPE / MINAM
# Date: 30-12-2025
#
# License: Apache License Version 2.0
# Copyright (c) 2025 Alexander Quevedo, Luis Robles and Freddy Argotty
#############################################################################
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# See NOTICE for attribution details.

library(tidyverse)
library(glue)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("funciones.R")



data_f <- "data_2025"
areasF <- "areas"
areasM <- "AreasProb2023_mapa_riesgo_peatland"


id_parcelas <- "plotid"
id_puntos <- "orden"
col_intensidad <- "mapa_prob_jerarquia"
anios <- 2000:2023
prefijo <- "year"

ic <- 0.90


ecozona <- "SB_Nopeatland"
defo_natural <- c("TF-OT", "TF-H")

claves_filtro <- c("^TF-", "TF-TF")

alpha <- 1 - ic
z_val <- qnorm(1 - alpha / 2)

areas <- read_csv(
  paste0(data_f, "/", areasF, "/", areasM, ".csv"),
  show_col_types = FALSE
) %>%
  filter(ECOZONA == ecozona) %>%
  select(-ECOZONA)

area_intensidad <- areas %>%
  pivot_longer(
    cols = everything(),
    names_to = "estrato",
    values_to = "area"
  )

data <- read_csv(
  "data_2025/bases_para_calculo/SB_Nopeatland_completo_limpio_actualizado_01_09_LR.csv",
  show_col_types = FALSE
) |>
  refactor_plot(
    plot_idT = id_parcelas,
    pto_id = id_puntos,
    col_intensidad = col_intensidad
  ) %>%
  estandarizar_nombres_col()


crear_carpeta("Resultado_area_ecozona")
carpeta_int <- paste0("Resultado_area_ecozona/", col_intensidad)
crear_carpeta(carpeta_int)
dir_ecozona <- paste0("Resultado_area_ecozona/", col_intensidad, "/", ecozona)
crear_carpeta(dir_ecozona)
dir_defo <- file.path(dir_ecozona, "deforestacion")
dir_perdida <- file.path(dir_ecozona, "perdidaN")
dir_TF <- file.path(dir_ecozona, "TF_totales")
dir_degra <- file.path(dir_ecozona, "degradacion")
dir_recu <- file.path(dir_ecozona, "recuperacion")
dir_permanecia <- file.path(dir_ecozona, "permanencia")

crear_carpeta(dir_defo)
crear_carpeta(dir_perdida)
crear_carpeta(dir_TF)
crear_carpeta(dir_degra)
crear_carpeta(dir_recu)
crear_carpeta(dir_permanecia)


for (anio in 2000:2023) {
  col_in <- paste0("year_", anio)
  col_out <- paste0("year_", anio, "_dom")

  data <- calcular_dominancia(data, col_in, col_out) |>
    relocate(all_of(col_out), .after = all_of(col_in))
}

data_filtrado <- data |>
  filter(sample_id == 13)

df_cleaned <- data_filtrado |>
  select(plot_id, estrato, ends_with("dom")) |>
  rename_with(~ str_remove(., "_dom$"))

year_cols <- names(df_cleaned)[str_detect(names(df_cleaned), "^year_\\d{4}$")]
year_cols_typeF <- names(data_filtrado)[str_detect(
  names(data_filtrado),
  "^tipo_tf_y\\d{4}$"
)]


idplot_strata <- df_cleaned |> select(plot_id, estrato) |> unique()

n_plot <- idplot_strata |>
  group_by(estrato) |>
  tally(name = "T_muestra")


data_sub <- df_cleaned |>
  select(plot_id, all_of(year_cols))


df_muestreo <- df_cleaned

anio1 <- anios[1]

lista_transiciones <- list()
lista_plots_defo <- list()

while (anio1 < tail(anios, n = 1)) {
  anio2 <- anio1 + 1

  key <- glue("{prefijo}_{anio1}_{anio2}")
  var1 <- glue("{prefijo}_{anio1}")
  var2 <- glue("{prefijo}_{anio2}")

  message("Procesando transición: ", key)

  lista_transiciones[[key]] <- df_muestreo %>%
    group_by(
      estrato,
      .data[[var1]],
      .data[[var2]]
    ) %>%
    tally(n = "periodo") %>%
    mutate(transicion = paste0(.data[[var1]], "-", .data[[var2]])) %>%
    ungroup() %>%
    select(-all_of(c(var1, var2))) %>%
    plyr::rename(c("periodo" = key)) %>%
    select(estrato, transicion, all_of(key))

  lista_plots_defo[[key]] <- df_muestreo %>%
    select(plot_id, all_of(c(var1, var2))) |>
    mutate(transicion = paste0(.data[[var1]], "-", .data[[var2]])) %>%
    filter(
      str_detect(transicion, claves_filtro[1]),
      !(transicion %in% c(claves_filtro[2]))
    )

  anio1 <- anio2
}

c_trans <- reduce(
  lista_transiciones,
  full_join,
  by = c("estrato", "transicion")
) %>%
  group_by(estrato, transicion) %>%
  select(starts_with(prefijo)) %>%
  summarise(
    across(everything(), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum"))

reduce(lista_transiciones, full_join, by = c("estrato", "transicion")) %>%
  group_by(estrato, transicion) %>%
  select(starts_with(prefijo)) |>
  write.csv(paste0(dir_ecozona, "/", "transiciones_", ecozona, "_review.csv"))


areas_todas_t <- c_trans %>%
  left_join(n_plot, by = "estrato") %>%
  left_join(area_intensidad, by = "estrato") %>%
  mutate(across(starts_with(prefijo), ~ (.x / T_muestra) * area)) %>%
  group_by(estrato, transicion) |>
  summarise(across(starts_with(prefijo), sum, na.rm = TRUE)) %>%
  rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum"))
write.csv(
  areas_todas_t,
  paste0(dir_ecozona, "/", "areas_totales_transiciones_", ecozona, ".csv")
)

c_deforestacion <- reduce(
  lista_transiciones,
  full_join,
  by = c("estrato", "transicion")
) %>%
  filter(
    str_detect(transicion, claves_filtro[1]),
    !(transicion %in% c(claves_filtro[2], defo_natural))
  ) %>%
  group_by(estrato, transicion) %>%
  select(starts_with(prefijo)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum"))


area_def <- calcular_area_transicion(c_deforestacion, n_plot, area_intensidad)
u2_def <- calcular_U2_transicion(
  c_deforestacion,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_def <- df_anual_def <- unir_area_U2(area_def, u2_def)
write.csv(
  df_anual_def,
  paste0(dir_defo, "/", "def_antropica_", ecozona, "_review.csv")
)

df_total_def <- propagar_inc_total(df_anual_def, variable = "Area")
write.csv(
  df_total_def,
  paste0(dir_defo, "/", "def_antropica_total_", ecozona, "_.csv")
)

all_plots_defo <- lapply(lista_plots_defo, function(df) {
  df %>%
    select(plot_id)
}) %>%
  rbindlist(use.names = TRUE, fill = TRUE)

#df_defo_perdida_all <- filter(
#  df_cleaned,
#  plot_id %in% all_plots_defo$plot_id
#) |>
#  left_join(
#    select(data_filtrado, plot_id, lat, lon),
#    join_by(plot_id == plot_id)
#  )

#write.csv(
#  df_defo_perdida_all,
#  paste0(dir_defo, "/", "deforestacion_perdida_plots_", ecozona, ".csv")
#)


c_deforestacion_nat <- c_trans %>%
  filter(transicion %in% defo_natural)

area_def_nat <- calcular_area_transicion(
  c_deforestacion_nat, n_plot, area_intensidad
)
u2_def_nat <- calcular_U2_transicion(
  c_deforestacion_nat, n_plot, area_intensidad, z_val
)

df_anual_def_nat <- unir_area_U2(area_def_nat, u2_def_nat)
write.csv(
  df_anual_def_nat,
  file.path(dir_perdida, paste0("def_natural_", ecozona, "_review.csv")),
  row.names = FALSE
)

df_total_def_nat <- propagar_inc_total(df_anual_def_nat, variable = "Area")
write.csv(
  df_total_def_nat,
  file.path(dir_perdida, paste0("def_natural_total_", ecozona, "_.csv")),
  row.names = FALSE
)

all_plots_defonat <- lapply(seq_along(lista_plots_defo), function(i) {
  nm <- names(lista_plots_defo)[i]
  df <- lista_plots_defo[[i]] %>%
    mutate(Periodo = nm) %>%
    filter(transicion %in% defo_natural) %>%
    select(plot_id, Periodo)
  df
}) %>% bind_rows()

#df_perdida_nat_plots <- df_cleaned %>%
#  filter(plot_id %in% all_plots_defonat$plot_id) %>%
#  left_join(select(data_filtrado, plot_id, lat, lon), join_by(plot_id == plot_id))

#write.csv(
#  df_perdida_nat_plots,
#  file.path(dir_perdida, paste0("def_natural_plots_", ecozona, ".csv")),
#  row.names = FALSE
#)


tf_total_por_periodo <- c_trans %>%
  filter(stringr::str_ends(transicion, "TF")) %>%
  group_by(estrato) %>%
  summarise(across(starts_with(prefijo), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


area_tf_total <- calcular_area_transicion(tf_total_por_periodo, n_plot, area_intensidad)
u2_tf_total   <- calcular_U2_transicion(tf_total_por_periodo, n_plot, area_intensidad, z_val)

df_anual_tf_total <- unir_area_U2(area_tf_total, u2_tf_total)
write.csv(
  df_anual_tf_total,
  file.path(dir_TF, paste0("TF_total_por_periodo_", ecozona, "_.csv")),
  row.names = FALSE
)

df_total_tf_total <- propagar_inc_total(df_anual_tf_total, variable = "Area")
write.csv(
  df_total_tf_total,
  file.path(dir_TF, paste0("TF_total_por_periodo_total_", ecozona, "_.csv")),
  row.names = FALSE
)











Tr1 <- 2000
Tr2 <- 2009
T1 <- 2010
TF <- 2021

degra_antropica <- paste0("TF-", c("TA", "AH", "P"))
degra_natural <- paste0("TF-", c("H", "OT"))

conteo_tf_wide <- clasificar_resumen_T2(data, Tr1, Tr2)
col_anios <- paste0("year_", Tr2:TF)

tipo_degradacion_h <- tipo_degra(data, conteo_tf_wide, Tr1, Tr2)

plots_to_remove <- conteo_tf_wide |>
  filter(
    .data[[paste0("year_", Tr2, "_n_tf")]] == 8,
    .data[[paste0("resumen_", Tr2)]] == "Bosque"
  ) |>
  select(plot_id)

plots_defo <- lapply(
  lista_plots_defo[paste0("year_", Tr2:(TF - 1), "_", (Tr2 + 1):TF)],
  function(df) {
    df %>%
      select(plot_id)
  }
) %>%
  rbindlist(use.names = TRUE, fill = TRUE)

plots_forest <- filter(
  data,
  !plot_id %in% plots_defo$plot_id,
  plot_id %in% df_cleaned[df_cleaned[[paste0("year_", Tr2)]] == "TF", ]$plot_id
)


base_degra <- select(
  plots_forest,
  estrato,
  plot_id,
  sample_id,
  paste0("year_", Tr2)
) |>
  filter(if_any(everything(), ~ . == "TF")) |>
  group_by(estrato, plot_id) |>
  tally(name = "n_Tr") |>
  arrange(n_Tr)

final_degra <- select(plots_forest, estrato, plot_id, paste0("year_", TF)) |>
  filter(if_any(everything(), ~ . == "TF")) |>
  group_by(plot_id) |>
  tally(name = "n_TF") |>
  arrange(n_TF)

df_define_degra <- left_join(base_degra, final_degra, by = join_by(plot_id)) |>
  filter(!plot_id %in% plots_to_remove$plot_id)


eval_estatus <- df_define_degra |>
  mutate(
    degra = case_when(
      n_Tr == 8 & n_TF == 8 ~ 2,
      n_TF <= n_Tr - round(n_Tr * 0.10, 0) ~ 1,
      n_TF > n_Tr ~ 3,
      .default = 0
    )
  )

ptos_degra <- filter(
  data,
  plot_id %in%
    (filter(eval_estatus, degra == 1) |>
      pull(plot_id))
)

id_plots_antropicaH <- tipo_degradacion_h %>%
  inner_join(filter(eval_estatus, degra == 2), by = "plot_id") %>%
  filter(tipo_degradacion == "Antropica") %>%
  pull(plot_id)

id_plots_naturalH <- tipo_degradacion_h %>%
  inner_join(filter(eval_estatus, degra == 2), by = "plot_id") %>%
  filter(tipo_degradacion == "Natural") %>%
  pull(plot_id)

conteo_tf_por_plot <- ptos_degra |>
  select(plot_id, estrato, all_of(col_anios)) |>
  group_by(estrato, plot_id) |>
  summarise(
    across(starts_with("year_"), ~ sum(. == "TF", na.rm = TRUE)),
    .groups = "drop"
  )


conteo_dif_marcado <- conteo_tf_por_plot |>
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "anio",
    values_to = "n_tf"
  ) |>
  arrange(plot_id, anio) |>
  group_by(plot_id) |>
  mutate(
    dif = lag(n_tf) - n_tf
  ) |>
  ungroup()


marcas_mayor_summary <- conteo_dif_marcado %>%
  filter(!is.na(dif), dif > 0) %>%
  group_by(estrato, plot_id) %>%
  slice_max(dif, with_ties = FALSE)


marcas_mayor_perdida <- marcas_mayor_summary %>%
  arrange(anio) %>%
  ungroup() %>%
  mutate(marca = 1) %>%
  select(estrato, plot_id, anio, marca) %>%
  pivot_wider(
    names_from = anio,
    values_from = marca,
    values_fill = list(marca = 0)
  )


data_long <- data %>%
  select(plot_id, sample_id, estrato, all_of(year_cols)) %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "anio",
    values_to = "clase"
  ) %>%
  mutate(anio = as.integer(str_remove(anio, "year_"))) %>%
  arrange(plot_id, sample_id, anio)

dt_long <- as.data.table(data_long)
setkey(dt_long, plot_id, sample_id, anio)

dt_long[,
  `:=`(
    clase_next = shift(clase, type = "lead"),
    anio_next = shift(anio, type = "lead")
  ),
  by = .(plot_id, sample_id)
]

dt_transitions <- dt_long[!is.na(clase_next)]

dt_transitions[, transicion := paste0(clase, "-", clase_next)]
dt_transitions[, periodo := paste0("year_", anio, "_", anio_next)]

dt_transitions <- as_tibble(dt_transitions)


marcas_mayor_summary <- marcas_mayor_summary |>
  mutate(anio_n = str_remove(anio, "year_") |> as.numeric())

clasifica_degra_art <- dt_transitions |>
  semi_join(marcas_mayor_summary, by = c("plot_id", "anio_next" = "anio_n")) |>
  mutate(
    Tdregra = case_when(
      transicion %in% degra_antropica ~ "Antropica",
      transicion %in% degra_natural ~ "Natural",
      .default = "Otro"
    )
  ) |>
  filter(Tdregra != "Otro") |>
  group_by(plot_id, Tdregra) |>
  tally() |>
  slice_max(n, with_ties = FALSE)


plots_antropicaH <- df_muestreo |>
  filter(plot_id %in% id_plots_antropicaH) |>
  select(estrato, plot_id, all_of(paste0("year_", T1:TF))) |>
  mutate(across(starts_with("year_"), ~0)) |>
  mutate(!!paste0("year_", T1) := 1)

plots_naturalH <- df_muestreo |>
  filter(plot_id %in% id_plots_naturalH) |>
  select(estrato, plot_id, all_of(paste0("year_", T1:TF))) |>
  mutate(across(starts_with("year_"), ~0)) |>
  mutate(!!paste0("year_", T1) := 1)




df_sample_degraA <- safe_bind_fill(
  marcas_mayor_perdida %>%
    filter(
      plot_id %in% filter(clasifica_degra_art, Tdregra == "Antropica")$plot_id
    ),
  anti_join(
    plots_antropicaH,
    marcas_mayor_perdida %>%
      filter(
        plot_id %in% filter(clasifica_degra_art, Tdregra == "Antropica")$plot_id
      ) %>%
      select(estrato, plot_id),
    by = c("estrato", "plot_id")
  )
) %>%
  dedupe_wide_rows()

df_sample_degraN <- safe_bind_fill(
  marcas_mayor_perdida %>%
    filter(
      plot_id %in% filter(clasifica_degra_art, Tdregra == "Natural")$plot_id
    ),
  anti_join(
    plots_naturalH,
    marcas_mayor_perdida %>%
      filter(
        plot_id %in% filter(clasifica_degra_art, Tdregra == "Natural")$plot_id
      ) %>%
      select(estrato, plot_id),
    by = c("estrato", "plot_id")
  )
) %>%
  dedupe_wide_rows()

#left_join(
#  df_sample_degraA,
#  select(data_filtrado, plot_id, lat, lon),
#  join_by(plot_id == plot_id)
#) |>
#  write.csv(paste0(dir_degra, "/", "plots_degra_Antropica_", ecozona, ".csv"))


#left_join(
#  df_sample_degraN,
#  select(data_filtrado, plot_id, lat, lon),
#  join_by(plot_id == plot_id)
#) |>
#  write.csv(paste0(dir_degra, "/", "plots_degra_Natural_", ecozona, ".csv"))


resumen_por_estrato_DegraA <- df_sample_degraA %>%
  group_by(estrato) %>%
  summarise(
    across(
      starts_with("year_"),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  )

area_degraA <- calcular_area_transicion(
  resumen_por_estrato_DegraA,
  n_plot,
  area_intensidad
)
u2_degraA <- calcular_U2_transicion(
  resumen_por_estrato_DegraA,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_degraA <- unir_area_U2(area_degraA, u2_degraA)
write.csv(
  df_anual_degraA,
  paste0(dir_degra, "/", "degradacion_Antropica_", ecozona, "_.csv")
)
df_total_degraA <- propagar_inc_total(df_anual_degraA, variable = "Area")
write.csv(
  df_total_degraA,
  paste0(dir_degra, "/", "degradacion_Antropica_total_", ecozona, "_.csv")
)

resumen_por_estrato_DegraN <- df_sample_degraN %>%
  group_by(estrato) %>%
  summarise(
    across(
      starts_with("year_"),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  )
area_degraN <- calcular_area_transicion(
  resumen_por_estrato_DegraN,
  n_plot,
  area_intensidad
)
u2_degraN <- calcular_U2_transicion(
  resumen_por_estrato_DegraN,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_degraN <- unir_area_U2(area_degraN, u2_degraN)
write.csv(
  df_anual_degraN,
  paste0(dir_degra, "/", "degradacion_Natural_", ecozona, "_.csv")
)
df_total_degraN <- propagar_inc_total(df_anual_degraN, variable = "Area")
write.csv(
  df_total_degraN,
  paste0(dir_degra, "/", "degradacion_Natural_total_", ecozona, "_.csv")
)




data_long_r <- data |>
  select(plot_id, sample_id, estrato, all_of(year_cols_typeF)) %>%
  pivot_longer(
    cols = starts_with("tipo_tf_y"),
    names_to = "anio",
    values_to = "clase"
  ) %>%
  mutate(anio = as.integer(str_remove(anio, "tipo_tf_y"))) %>%
  arrange(plot_id, sample_id, anio)


dt_long_r <- as.data.table(data_long_r)
setkey(dt_long_r, plot_id, sample_id, anio)

dt_long_r[,
  `:=`(
    clase_next = shift(clase, type = "lead"),
    anio_next = shift(anio, type = "lead")
  ),
  by = .(plot_id, sample_id)
]

dt_transitions_r <- dt_long_r[!is.na(clase_next)]

dt_transitions_r[, transicion := paste0(clase, "-", clase_next)]
dt_transitions_r[, periodo := paste0("year_", anio, "_", anio_next)]

dt_transitions_r <- as_tibble(dt_transitions_r)



ptos_r_tf <- filter(
  data,
  plot_id %in%
    (filter(eval_estatus, degra == 3) |>
      pull(plot_id))
)


conteo_tf_r_plot <- ptos_r_tf |>
  select(plot_id, estrato, all_of(col_anios)) |>
  group_by(estrato, plot_id) |>
  summarise(
    across(starts_with("year_"), ~ sum(. == "TF", na.rm = TRUE)),
    .groups = "drop"
  )

conteo_dif_r_marcado <- conteo_tf_r_plot |>
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "anio",
    values_to = "n_tf"
  ) |>
  arrange(plot_id, anio) |>
  group_by(plot_id) |>
  mutate(
    dif = n_tf - lag(n_tf)
  ) |>
  ungroup()


marcas_mayor_r_summary <- conteo_dif_r_marcado %>%
  filter(!is.na(dif), dif > 0) %>%
  group_by(estrato, plot_id) %>%
  slice_max(dif, with_ties = FALSE)


marcas_mayor_ganancia <- marcas_mayor_r_summary %>%
  arrange(anio) %>%
  ungroup() %>%
  mutate(marca = 1) %>%
  select(estrato, plot_id, anio, marca) %>%
  pivot_wider(
    names_from = anio,
    values_from = marca,
    values_fill = list(marca = 0)
  )


marcas_mayor_r_summary <- marcas_mayor_r_summary |>
  mutate(anio_n = str_remove(anio, "year_") |> as.numeric())



clasifica_r_art <- dt_transitions_r |>
  semi_join(
    marcas_mayor_r_summary,
    by = c("plot_id", "anio_next" = "anio_n")
  ) |>
  mutate(
    Ttf = case_when(
      str_ends(transicion, "TF_Plantado") &
        !str_starts(transicion, "TF_Plantado") ~
        "Antropica",
      str_ends(transicion, "TF_Natural") &
        !str_starts(transicion, "TF_Natural") ~
        "Natural",
      transicion == "TF_Plantado-TF_Plantado" ~ "Permanencia",
      transicion == "TF_Natural-TF_Natural" ~ "Permanencia",
      .default = "Otro"
    )
  ) |>
  filter(!Ttf %in% c("Otro", "Permanencia")) |>
  group_by(plot_id, Ttf) |>
  tally() |>
  slice_max(n, with_ties = FALSE)


plots_antropicaR <- df_muestreo %>%
  filter(plot_id %in% filter(clasifica_r_art, Ttf == "Antropica")$plot_id) %>%
  select(estrato, plot_id, all_of(paste0("year_", T1:TF))) %>%
  mutate(across(starts_with("year_"), ~0)) %>%
  mutate(!!paste0("year_", T1) := 1)

plots_naturalR <- df_muestreo %>%
  filter(plot_id %in% filter(clasifica_r_art, Ttf == "Natural")$plot_id) %>%
  select(estrato, plot_id, all_of(paste0("year_", T1:TF))) %>%
  mutate(across(starts_with("year_"), ~0)) %>%
  mutate(!!paste0("year_", T1) := 1)



df_sample_recovA <- safe_bind_fill(
  marcas_mayor_ganancia %>%
    filter(plot_id %in% filter(clasifica_r_art, Ttf == "Antropica")$plot_id),
  anti_join(
    plots_antropicaR,
    marcas_mayor_ganancia %>%
      filter(
        plot_id %in% filter(clasifica_r_art, Ttf == "Antropica")$plot_id
      ) %>%
      select(estrato, plot_id),
    by = c("estrato", "plot_id")
  )
) %>%
  dedupe_wide_rows()

df_sample_recovN <- safe_bind_fill(
  marcas_mayor_ganancia %>%
    filter(plot_id %in% filter(clasifica_r_art, Ttf == "Natural")$plot_id),
  anti_join(
    plots_naturalR,
    marcas_mayor_ganancia %>%
      filter(plot_id %in% filter(clasifica_r_art, Ttf == "Natural")$plot_id) %>%
      select(estrato, plot_id),
    by = c("estrato", "plot_id")
  )
) %>%
  dedupe_wide_rows()

#left_join(
#  df_sample_recovA,
#  select(data_filtrado, plot_id, lat, lon),
#  by = "plot_id"
#) %>%
#  write.csv(
#    paste0(dir_recu, "/", "plots_recuperacionTF_Antropica_", ecozona, ".csv"),
#    row.names = FALSE
#  )

#left_join(
#  df_sample_recovN,
#  select(data_filtrado, plot_id, lat, lon),
#  by = "plot_id"
#) %>%
#  write.csv(
#    paste0(dir_recu, "/", "plots_recuperacionTF_Natural_", ecozona, ".csv"),
#    row.names = FALSE
#  )

resumen_por_estrato_RecovA <- df_sample_recovA %>%
  group_by(estrato) %>%
  summarise(
    across(starts_with("year_"), ~ sum(.x == 1, na.rm = TRUE)),
    .groups = "drop"
  )

area_recovA <- calcular_area_transicion(
  resumen_por_estrato_RecovA,
  n_plot,
  area_intensidad
)
u2_recovA <- calcular_U2_transicion(
  resumen_por_estrato_RecovA,
  n_plot,
  area_intensidad,
  z_val
)
df_anual_recovA <- unir_area_U2(area_recovA, u2_recovA)
write.csv(
  df_anual_recovA,
  paste0(dir_recu, "/", "recuperacionTF_Antropica_", ecozona, "_.csv")
)
df_total_recovA <- propagar_inc_total(df_anual_recovA, variable = "Area")
write.csv(
  df_total_recovA,
  paste0(dir_recu, "/", "recuperacionTF_Antropica_total_", ecozona, "_.csv")
)

resumen_por_estrato_RecovN <- df_sample_recovN %>%
  group_by(estrato) %>%
  summarise(
    across(starts_with("year_"), ~ sum(.x == 1, na.rm = TRUE)),
    .groups = "drop"
  )

area_recovN <- calcular_area_transicion(
  resumen_por_estrato_RecovN,
  n_plot,
  area_intensidad
)
u2_recovN <- calcular_U2_transicion(
  resumen_por_estrato_RecovN,
  n_plot,
  area_intensidad,
  z_val
)
df_anual_recovN <- unir_area_U2(area_recovN, u2_recovN)
write.csv(
  df_anual_recovN,
  paste0(dir_recu, "/", "recuperacionTF_Natural_", ecozona, "_.csv")
)
df_total_recovN <- propagar_inc_total(df_anual_recovN, variable = "Area")
write.csv(
  df_total_recovN,
  paste0(dir_recu, "/", "recuperacionTF_Natural_total_", ecozona, "_.csv")
)



plots_to_remove_TNF <- c(
  distinct(ptos_r_tf, plot_id) %>% pull(plot_id),
  distinct(ptos_degra, plot_id) %>% pull(plot_id),
  distinct(plots_forest, plot_id) %>% pull(plot_id)
)

data_recovery_TNF <- data %>%
  select(estrato, plot_id, sample_id, all_of(year_cols_typeF)) %>%
  filter(!plot_id %in% plots_to_remove_TNF)

names(data_recovery_TNF) <- names(data_recovery_TNF) %>%
  str_replace("^tipo_tf_y", "year_")

for (anio in 2000:2023) {
  col_in <- paste0("year_", anio)
  col_out <- paste0("year_", anio, "_dom")

  data_recovery_TNF <- calcular_dominancia_t_tf(
    data_recovery_TNF,
    col_in,
    col_out
  ) |>
    relocate(all_of(col_out), .after = all_of(col_in))
}

data_filtrado_TNF <- data_recovery_TNF |>
  filter(sample_id == 13)

df_muestreo_TNF <- data_filtrado_TNF |>
  select(plot_id, estrato, ends_with("dom")) |>
  rename_with(~ str_remove(., "_dom$"))


lista_transiciones_T_TF <- list()
lista_plots_recupera_TNF_A <- list()
lista_plots_recupera_TNF_N <- list()

for (i in seq_len(length(anios) - 1)) {
  anio1 <- anios[i]
  anio2 <- anios[i + 1]

  key <- glue("{prefijo}_{anio1}_{anio2}")
  var1 <- glue("{prefijo}_{anio1}")
  var2 <- glue("{prefijo}_{anio2}")

  message("Procesando transición: ", key)

  transiciones_df <- df_muestreo_TNF %>%
    group_by(estrato, .data[[var1]], .data[[var2]]) %>%
    tally(name = "periodo") %>%
    mutate(transicion = paste0(.data[[var1]], "-", .data[[var2]])) %>%
    ungroup()

  lista_transiciones_T_TF[[key]] <- transiciones_df %>%
    select(estrato, transicion, periodo) %>%
    rename_with(~key, "periodo")

  lista_plots_recupera_TNF_N[[key]] <- df_muestreo_TNF %>%
    select(plot_id, all_of(c(var1, var2))) |>
    mutate(transicion = paste0(.data[[var1]], "-", .data[[var2]])) |>
    filter(
      str_ends(transicion, "TF_Natural") &
        transicion != "TF_Natural-TF_Natural"
    )

  lista_plots_recupera_TNF_A[[key]] <- df_muestreo_TNF %>%
    select(plot_id, all_of(c(var1, var2))) |>
    mutate(transicion = paste0(.data[[var1]], "-", .data[[var2]])) |>
    filter(
      str_ends(transicion, "TF_Plantado") &
        transicion != "TF_Plantado-TF_Plantado"
    )
}

c_trans_T_TF <- reduce(
  lista_transiciones_T_TF,
  full_join,
  by = c("estrato", "transicion")
) %>%
  group_by(estrato, transicion) %>%
  select(starts_with(prefijo)) %>%
  summarise(
    across(everything(), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum")) |>
  select(estrato, transicion, paste0("year_", Tr2:(TF - 1), "_", (Tr2 + 1):TF))

recuperacion_natural_TNF <- c_trans_T_TF %>%
  filter(
    str_ends(transicion, "TF_Natural") & transicion != "TF_Natural-TF_Natural"
  )

recuperacion_antropica_TNF <- c_trans_T_TF %>%
  filter(
    str_ends(transicion, "TF_Plantado") &
      transicion != "TF_Plantado-TF_Plantado"
  )


area_recuN_TNF <- calcular_area_transicion(
  recuperacion_natural_TNF,
  n_plot,
  area_intensidad
)
u2_recuN_TNF <- calcular_U2_transicion(
  recuperacion_natural_TNF,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_recuN_TNF <- unir_area_U2(area_recuN_TNF, u2_recuN_TNF)
write.csv(
  df_anual_recuN_TNF,
  paste0(dir_recu, "/", "recuperacionTNF_Natural_", ecozona, "_.csv")
)

df_total_recuN_TNF <- propagar_inc_total(df_anual_recuN_TNF, variable = "Area")
write.csv(
  df_total_recuN_TNF,
  paste0(dir_recu, "/", "recuperacionTNF_Natural_total_", ecozona, "_.csv")
)


area_recuA_TNF <- calcular_area_transicion(
  recuperacion_antropica_TNF,
  n_plot,
  area_intensidad
)
u2_recuA_TNF <- calcular_U2_transicion(
  recuperacion_antropica_TNF,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_recuA_TNF <- unir_area_U2(area_recuA_TNF, u2_recuA_TNF)
write.csv(
  df_anual_recuA_TNF,
  paste0(dir_recu, "/", "recuperacionTNF_Antropica_", ecozona, "_.csv")
)

df_total_recuA_TNF <- propagar_inc_total(df_anual_recuA_TNF, variable = "Area")
write.csv(
  df_total_recuA_TNF,
  paste0(dir_recu, "/", "recuperacionTNF_Antropica_total_", ecozona, "_.csv")
)


all_plots_recupera_TNF_N <- lapply(lista_plots_recupera_TNF_N, function(df) {
  df %>%
    select(plot_id)
}) %>%
  rbindlist(use.names = TRUE, fill = TRUE)

#plots_recuperacionN_TNF <- filter(
#  df_cleaned,
#  plot_id %in% all_plots_recupera_TNF_N$plot_id
#) |>
#  left_join(
#    select(data_filtrado, plot_id, lat, lon),
#    join_by(plot_id == plot_id)
#  )

#write.csv(
#  plots_recuperacionN_TNF,
#  paste0(dir_recu, "/", "plots_recu_TNF_Natural_", ecozona, ".csv")
#)


all_plots_recupera_TNF_A <- lapply(lista_plots_recupera_TNF_A, function(df) {
  df %>%
    select(plot_id)
}) %>%
  rbindlist(use.names = TRUE, fill = TRUE)

#plots_recuperacionA_TNF <- filter(
#  df_cleaned,
#  plot_id %in% all_plots_recupera_TNF_A$plot_id
#) |>
#  left_join(
#    select(data_filtrado, plot_id, lat, lon),
#    join_by(plot_id == plot_id)
#  )


#write.csv(
#  plots_recuperacionA_TNF,
#  paste0(dir_recu, "/", "plots_recu_TNF_Antropica_", ecozona, ".csv")
#)



period_keys <- paste0("year_", Tr2:(TF - 1), "_", (Tr2 + 1):TF)
`%||%` <- function(a, b) if (is.null(a)) b else a

tf_tf_pairs <- purrr::map_dfr(period_keys, \(key) {
  y <- as.integer(stringr::str_match(key, "year_(\\d{4})_\\d{4}")[, 2])
  y2 <- y + 1
  v1 <- paste0("year_", y)
  v2 <- paste0("year_", y2)
  df_muestreo %>%
    dplyr::transmute(
      estrato,
      plot_id,
      Periodo = key,
      tf_tf = .data[[v1]] == "TF" & .data[[v2]] == "TF"
    )
}) %>%
  dplyr::filter(tf_tf) %>%
  dplyr::distinct(estrato, plot_id, Periodo)

events_from_wide <- function(df_wide, label, year_min = Tr2, year_max = TF) {
  year_cols <- intersect(paste0("year_", year_min:year_max), names(df_wide))
  if (!length(year_cols) || nrow(df_wide) == 0) {
    return(tibble::tibble(
      Periodo = character(0),
      estrato = character(0),
      plot_id = numeric(0),
      tipo = character(0)
    ))
  }
  df_wide %>%
    dplyr::select(estrato, plot_id, dplyr::all_of(year_cols)) %>%
    tidyr::pivot_longer(
      dplyr::all_of(year_cols),
      names_to = "anio",
      values_to = "marca"
    ) %>%
    dplyr::filter(!is.na(marca), marca == 1) %>%
    dplyr::mutate(
      y = as.integer(stringr::str_remove(anio, "year_")),
      Periodo = paste0("year_", y - 1, "_", y),
      tipo = label
    ) %>%
    dplyr::filter(Periodo %in% period_keys) %>%
    dplyr::distinct(estrato, plot_id, Periodo, tipo)
}

excl_tbl_list <- list()
if (exists("df_sample_degraA")) {
  excl_tbl_list[["DegraA"]] <- events_from_wide(df_sample_degraA, "DegraA")
}
if (exists("df_sample_degraN")) {
  excl_tbl_list[["DegraN"]] <- events_from_wide(df_sample_degraN, "DegraN")
}
if (exists("df_sample_recovA")) {
  excl_tbl_list[["RecovA"]] <- events_from_wide(df_sample_recovA, "RecovA")
}
if (exists("df_sample_recovN")) {
  excl_tbl_list[["RecovN"]] <- events_from_wide(df_sample_recovN, "RecovN")
}

excl_tbl_raw <- dplyr::bind_rows(excl_tbl_list)

excl_tbl <- dplyr::semi_join(
  excl_tbl_raw,
  tf_tf_pairs,
  by = c("estrato", "plot_id", "Periodo")
)

excl_by_period <- split(excl_tbl[, c("estrato", "plot_id")], excl_tbl$Periodo)

lista_permanencias_TF <- vector("list", length = length(period_keys))
names(lista_permanencias_TF) <- period_keys

incluidos_list <- vector("list", length = length(period_keys))
names(incluidos_list) <- period_keys

for (key in period_keys) {
  y <- as.integer(stringr::str_match(key, "year_(\\d{4})_\\d{4}")[, 2])
  y2 <- y + 1
  v1 <- paste0("year_", y)
  v2 <- paste0("year_", y2)

  base_key <- dplyr::filter(tf_tf_pairs, Periodo == key) %>%
    dplyr::select(estrato, plot_id, Periodo)

  excl_key <- (excl_by_period[[key]] %||%
    base_key[0, c("estrato", "plot_id")]) %>%
    dplyr::distinct()

  incluidos_key <- dplyr::anti_join(
    base_key,
    excl_key,
    by = c("estrato", "plot_id")
  )

  incluidos_list[[key]] <- incluidos_key

  lista_permanencias_TF[[key]] <- incluidos_key %>%
    dplyr::count(estrato, name = key)
}

c_permanencia <- Reduce(
  function(x, y) dplyr::full_join(x, y, by = "estrato"),
  lista_permanencias_TF
)
if (is.null(c_permanencia)) {
  c_permanencia <- tibble::tibble(estrato = unique(df_muestreo$estrato))
}
c_permanencia <- c_permanencia %>%
  dplyr::mutate(dplyr::across(
    dplyr::any_of(period_keys),
    ~ tidyr::replace_na(.x, 0L)
  ))

area_permanencia <- calcular_area_transicion(
  c_permanencia,
  n_plot,
  area_intensidad
)
u2_permanencia <- calcular_U2_transicion(
  c_permanencia,
  n_plot,
  area_intensidad,
  z_val
)

df_anual_permanencia <- unir_area_U2(area_permanencia, u2_permanencia)
write.csv(
  df_anual_permanencia,
  file.path(dir_permanecia, paste0("permanencia_", ecozona, "_review.csv")),
  row.names = FALSE
)

df_total_permanencia <- propagar_inc_total(
  df_anual_permanencia,
  variable = "Area"
)
write.csv(
  df_total_permanencia,
  file.path(dir_permanecia, paste0("permanencia_total_", ecozona, "_.csv")),
  row.names = FALSE
)

#permanencia_plots <- dplyr::bind_rows(incluidos_list) %>%
#  dplyr::arrange(Periodo, estrato, plot_id)
#if (exists("data_filtrado")) {
#  permanencia_plots <- permanencia_plots %>%
#    dplyr::left_join(
#      dplyr::select(data_filtrado, plot_id, lat, lon),
#      by = "plot_id"
#    )
#}
#write.csv(
#  permanencia_plots,
#  file.path(dir_permanecia, paste0("permanencia_plots_", ecozona, ".csv")),
#  row.names = FALSE
#)

#excl_audit_detalle <- excl_tbl %>%
#  dplyr::arrange(Periodo, estrato, plot_id, tipo)
#if (exists("data_filtrado")) {
#  excl_audit_detalle <- excl_audit_detalle %>%
#    dplyr::left_join(
#      dplyr::select(data_filtrado, plot_id, lat, lon),
#      by = "plot_id"
#    )
#}
#write.csv(
 # excl_audit_detalle,
 # file.path(
  #  dir_permanecia,
  #  paste0("permanencia_excluidos_detalle_", ecozona, ".csv")
  #),
  #row.names = FALSE
#)

#excl_audit <- excl_tbl %>% dplyr::distinct(Periodo, plot_id)
#write.csv(
#  excl_audit,
#  file.path(
#    dir_permanecia,
#    paste0("permanencia_exclusiones_SOLOperiodoTF_", ecozona, ".csv")
#  ),
#  row.names = FALSE
#)
gc()
