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

calcular_dominancia <- function(df, colname, output_colname) {
  df %>%
    group_by(plot_id) %>%
    mutate(
      !!output_colname := {
        tabla <- table(.data[[colname]])
        n_tf <- ifelse("TF" %in% names(tabla), tabla["TF"], 0)

        if (n_tf >= 8) {
          "TF"
        } else {
          max_freq <- max(tabla)
          clases_max <- names(tabla[tabla == max_freq])

          if (length(clases_max) == 1) {
            clases_max
          } else {
            jerarquia <- c("TA", "P", "AH", "OT", "H")
            clase_prioritaria <- jerarquia[jerarquia %in% clases_max][1]

            if (!is.na(clase_prioritaria)) {
              clase_prioritaria
            } else {
              clases_max[1]
            }
          }
        }
      }
    ) %>%
    ungroup()
}


calcular_dominancia_t_tf <- function(df, colname, output_colname) {
  df %>%
    group_by(plot_id) %>%
    mutate(
      !!output_colname := {
        tabla <- table(.data[[colname]])

        n_TF          <- ifelse("TF" %in% names(tabla), tabla["TF"], 0)
        n_TF_Natural  <- ifelse("TF_Natural" %in% names(tabla), tabla["TF_Natural"], 0)
        n_TF_Plantado <- ifelse("TF_Plantado" %in% names(tabla), tabla["TF_Plantado"], 0)

        if (n_TF >= 8) {
          "TF"
        } else if (n_TF_Natural >= 8) {
          "TF_Natural"
        } else if (n_TF_Plantado >= 8) {
          "TF_Plantado"
        } else {
          max_freq <- max(tabla)
          clases_max <- names(tabla[tabla == max_freq])

          if (length(clases_max) == 1) {
            clases_max
          } else {
            jerarquia <- c("TA", "P", "AH", "OT", "H")
            clase_prioritaria <- jerarquia[jerarquia %in% clases_max][1]
            if (!is.na(clase_prioritaria)) {
              clase_prioritaria
            } else {
              clases_max[1]
            }
          }
        }
      }
    ) %>%
    ungroup()
}



refactor_plot <- function(df, plot_idT, pto_id, col_intensidad) {
  stopifnot(is.data.frame(df))
  df %>%
    rename(
      plot_id   = all_of(plot_idT),
      sample_id = all_of(pto_id),
      estrato   = all_of(col_intensidad)
    )
}

crear_carpeta <- function(nombre_carpeta) {
  if (!dir.exists(nombre_carpeta)) {
    dir.create(nombre_carpeta)
    cat("Carpeta creada:", nombre_carpeta, "\n")
  } else {
    cat("La carpeta", nombre_carpeta, "ya existe \n")
  }
}

estandarizar_nombres_col <- function(df) {
  df %>%
    rename_with(
      .cols = starts_with("Y"),
      .fn = ~ ifelse(
        str_detect(., "^Y\\d{4}$"),
        str_replace(., "^Y", "year_"),
        .
      )
    ) %>%
    rename_with(tolower)
}

clasificar_resumen_T2 <- function(data, T1, T2) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(rlang)

  years_range <- T1:T2
  cols_tf <- paste0("year_", years_range)

  df_long <- data %>%
    select(plot_id, all_of(cols_tf)) %>%
    pivot_longer(
      cols = starts_with("year_"),
      names_to = "year",
      values_to = "class"
    ) %>%
    mutate(year = str_remove(year, "year_"))

  conteo_tf <- df_long %>%
    filter(class == "TF") %>%
    group_by(plot_id, year) %>%
    summarise(n_tf = n(), .groups = "drop") %>%
    mutate(year = paste0("year_", year, "_n_tf"))

  conteo_tf_wide <- conteo_tf %>%
    pivot_wider(names_from = year, values_from = n_tf, values_fill = 0)

  col_T2 <- paste0("year_", T2, "_n_tf")
  conteo_tf_wide <- conteo_tf_wide %>%
    filter(!!sym(col_T2) == 8)

  columnas_periodo <- paste0("year_", T1:T2, "_n_tf")
  resumen_col <- paste0("resumen_", T2)

  conteo_tf_wide <- conteo_tf_wide %>%
    rowwise() %>%
    mutate(!!resumen_col := {
      valores <- c_across(all_of(columnas_periodo))

      if (all(valores == 8)) {
        "Bosque"
      }
      else if (first(valores) >= 8 && last(valores) == 8 && (any(valores > 8) || any(valores < 8))) {
        "Degradacion"
      }
      else if (first(valores) < 8 && last(valores) == 8) {
        "Recuperacion"
      }
      else {
        "Otros"
      }
    }) %>%
    ungroup()

  return(conteo_tf_wide)
}

tipo_degra <- function(data, conteo_tf_wide, T1, T2) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)

  clases_antropicas <- c("TA", "AH", "P")
  clases_naturales  <- c("H", "OT")

  col_resumen <- paste0("resumen_", T2)

  parcelas_degra <- conteo_tf_wide %>%
    filter(.data[[col_resumen]] == "Degradacion") %>%
    pull(plot_id)

  cols_years <- paste0("year_", T1:T2)

  puntos_degra <- data %>%
    filter(plot_id %in% parcelas_degra) %>%
    select(plot_id, sample_id, all_of(cols_years)) %>%
    filter(.data[[paste0("year_", T1)]] == "TF") %>%
    rowwise() %>%
    mutate(
      cambio = {
        clases <- c_across(all_of(cols_years))
        primera_clase <- clases[clases != "TF"][1]
        if (is.na(primera_clase)) "Sin cambio"
        else if (primera_clase %in% clases_antropicas) "Antropica"
        else if (primera_clase %in% clases_naturales) "Natural"
        else "Otro"
      }
    ) %>%
    ungroup()

  resultado <- puntos_degra %>%
    filter(cambio != "Sin cambio") %>%
    count(plot_id, cambio) %>%
    group_by(plot_id) %>%
    mutate(n_total = sum(n)) %>%
    slice_max(n, n = 1, with_ties = TRUE) %>%
    mutate(
      tipo_degradacion = ifelse(n() > 1, "Antropica", cambio)
    ) %>%
    ungroup() %>%
    distinct(plot_id, tipo_degradacion)

  return(resultado)
}


calcular_area_transicion <- function(df_trans, n_plot, area_intensidad, prefijo = "year") {
  df_trans %>%
    left_join(n_plot, by = "estrato") %>%
    left_join(area_intensidad, by = "estrato") %>%
    mutate(across(starts_with(prefijo),
                  ~ (.x / T_muestra) * area)) %>%
    group_by(across(any_of(c("estrato", "transicion")))) %>%
    summarise(across(starts_with(prefijo), sum, na.rm = TRUE), .groups = "drop") %>%
    rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum"))
}

calcular_U2_transicion <- function(df_trans, n_plot, area_intensidad, z_val, prefijo = "year") {
  df_trans %>%
    left_join(n_plot, by = "estrato") %>%
    left_join(area_intensidad, by = "estrato") %>%
    mutate(across(starts_with(prefijo), ~ {
      p <- .x / T_muestra
      n <- T_muestra
      A <- area
      ifelse(.x > 0 & n > 1,
             (((z_val * sqrt((p * (1 - p)) / (n - 1))) / p) * 100 * (p * A))^2,
             0)
    })) %>%
    group_by(across(any_of(c("estrato", "transicion")))) %>%
    summarise(across(starts_with(prefijo), sum, na.rm = TRUE), .groups = "drop") %>%
    rename_with(~ str_replace(., "_sum$", ""), ends_with("_sum"))
}

unir_area_U2 <- function(area_df, U2_df, prefijo = "year") {
  group_cols <- intersect(c("estrato", "transicion"), names(area_df))

  area_long <- area_df %>%
    pivot_longer(
      cols = starts_with(prefijo),
      names_to = "Periodo",
      values_to = "Area"
    )

  U2_long <- U2_df %>%
    pivot_longer(
      cols = starts_with(prefijo),
      names_to = "Periodo",
      values_to = "U2"
    )

  area_long %>%
    left_join(U2_long, by = c(group_cols, "Periodo")) %>%
    mutate(Periodo = str_remove(Periodo, paste0("^", prefijo, "_"))) %>%
    group_by(across(all_of(c(group_cols, "Periodo")))) %>%
    summarise(
      Area = sum(Area, na.rm = TRUE),
      U2   = sum(U2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      UADt = sqrt(U2) / Area,
      Area = as.numeric(Area)
    )
}

propagar_inc_total <- function(df_long, variable = "Area", z_val = 1.645) {
  df_long %>%
    group_by(Periodo) %>%
    summarise(
      total_area = sum(.data[[variable]], na.rm = TRUE),
      total_U2   = sum(U2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      UADt = sqrt(total_U2) / total_area,
      EA = (total_area * UADt) / 100,
      SE = EA / z_val,
      LI = total_area - EA,
      LS = total_area + EA
    )
}

safe_bind_fill <- function(df1, df2, fill_value = 0) {
  all_cols <- union(names(df1), names(df2))

  cols_missing_df1 <- setdiff(all_cols, names(df1))
  df1 <- df1 %>%
    mutate(!!!setNames(rep(fill_value, length(cols_missing_df1)), cols_missing_df1))

  cols_missing_df2 <- setdiff(all_cols, names(df2))
  df2 <- df2 %>%
    mutate(!!!setNames(rep(fill_value, length(cols_missing_df2)), cols_missing_df2))

  df1 <- df1 %>% select(all_of(all_cols))
  df2 <- df2 %>% select(all_of(all_cols))

  rbind(df1, df2)
}



dedupe_wide_rows <- function(df_wide) {
  year_cols <- names(df_wide)[stringr::str_detect(names(df_wide), "^year_\\d{4}$")]
  if (!length(year_cols)) return(df_wide)
  df_wide %>%
    dplyr::group_by(estrato, plot_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(year_cols),
                    ~ as.integer(any(. == 1, na.rm = TRUE))),
      .groups = "drop"
    )
}
