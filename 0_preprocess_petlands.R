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

library(sf)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


sufijo <- "completo_limpio_actualizado_01_09_LR.csv"

zh <- read_csv(paste0("data_2025/bases_iniciales/ZH_", sufijo))
table(zh$turbera_central)
sb <- read_csv(paste0("data_2025/bases_iniciales/SB_", sufijo))
table(sb$turbera_central)

zh_peatland <- filter(zh, turbera_central == 401)
dim(zh_peatland)
zh_no_peatland <- filter(zh, turbera_central == 400)
dim(zh_no_peatland)
write.csv(
    zh_peatland,
    paste0("data_2025/bases_para_calculo/", "ZH_peatland_", sufijo)
)
write.csv(
    zh_no_peatland,
    paste0("data_2025/bases_para_calculo/", "ZH_Nopeatland_", sufijo)
)


sb_peatland <- filter(sb, turbera_central == 101)
dim(sb_peatland)
sb_no_peatland <- filter(sb, turbera_central == 100)
dim(sb_no_peatland)
write.csv(
    sb_peatland,
    paste0("data_2025/bases_para_calculo/", "SB_peatland_", sufijo)
)
write.csv(
    sb_no_peatland,
    paste0("data_2025/bases_para_calculo/", "SB_Nopeatland_", sufijo)
)






