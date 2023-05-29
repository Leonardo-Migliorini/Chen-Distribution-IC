# Importação dos dados


# planilha <- readr::read_csv(
#   "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
# )

planilha <- readxl::read_xlsx(
  "C:\\Users\\leona\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\Dados CO2 - original.xlsx"
)

planilha2 <- readr::read_csv(
  "C:\\Users\\leona\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\Base Complementar.csv"
)

planilha3 <- readr::read_csv(
  "C:\\Users\\leona\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\IDH.csv"
)



# Filtragem dos dados

# Modelo 2

################################################################################

# M2 <- dplyr::filter(
#   planilha, year == 2018
# ) |>
#   dplyr::select(
#     country, ghg_excluding_lucf_per_capita, co2_per_capita,
#     methane_per_capita,	energy_per_gdp
#   ) |>
#   dplyr::filter_all(
#     dplyr::all_vars(. > 0)
#   ) |>
#   dplyr::rename(
#     y = ghg_excluding_lucf_per_capita, x1 = co2_per_capita,
#     x2 = methane_per_capita, x3 = energy_per_gdp
#   )
# 
# M2 <- M2[!(row.names(M2) %in% c("159")),]

# Exportação da Tabela Filtrada

#write.csv(M2, "Dados_Filtrados_2018novo.csv", row.names = FALSE)

################################################################################

# Modelo 1

################################################################################

M1 <- dplyr::filter(
  planilha, year == 2018
) |>
  dplyr::select(
    country, co2_per_capita, ghg_excluding_lucf_per_capita,
    methane_per_capita,	energy_per_gdp
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |>
  dplyr::rename(
    y = co2_per_capita, x1 = ghg_excluding_lucf_per_capita,
    x2 = methane_per_capita, x3 = energy_per_gdp
  )

M1 <- M1[!(row.names(M1) %in% c("159")),]

# Exportação da Tabela Filtrada

#write.csv(M1, "Dados_Filtrados_2018novo.csv", row.names = FALSE)

################################################################################

# Modelo 3

pf2 <- planilha2 |> 
  dplyr::filter_all(
    dplyr::all_vars(. >= 0) 
    )|> 
  dplyr::select(
    - c(1,2,4)
    ) |> 
  dplyr::rename(country = c(1))

M3 <- dplyr::full_join(pf2, M1) |> 
  dplyr::filter_all(
    dplyr::all_vars(. >= 0)
    ) |> 
  dplyr::select(
    c(2, 9, 15, 16, 17, 18,1)
    ) |> 
  dplyr::rename(
    x1 = c(1), x2 = c(2), x3 = c(3), x4 = c(4), x5 = c(5)
    )


pf3 <- planilha3 |> 
  dplyr::select(
    c(2,34)
  ) |> 
  dplyr::rename(
   x6 = c(2)
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. >= 0)
  )

M4 <- dplyr::right_join(
  M3, pf3
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. >= 0)
  ) |> 
  dplyr::select(
    -c(7)
  ) |> 
  dplyr::select(
    6, 1:5, 7
  ) |> 
  dplyr::mutate_if(is.character, as.numeric)
  

# colSums(is.na(planilha4))

#  write.csv(M4, "Modelo 4 - 2018.csv", row.names = FALSE)

