# Importação dos dados

planilha <- readr::read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
)

# Filtragem dos dados

# Modelo 1

################################################################################

M2 <- dplyr::filter(
  planilha, year == 2018
) |> 
  dplyr::select(
    country, ghg_excluding_lucf_per_capita, co2_per_capita,	
    methane_per_capita,	energy_per_gdp
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |> 
  dplyr::rename(
    y = ghg_excluding_lucf_per_capita, x1 = co2_per_capita,
    x2 = methane_per_capita, x3 = energy_per_gdp
  )

M2 <- M2[!(row.names(M2) %in% c("159")),]

# Exportação da Tabela Filtrada

#write.csv(pf, "Dados_Filtrados_2018novo.csv", row.names = FALSE)

################################################################################

# Modelo 2

################################################################################

M1 <- dplyr::filter(
  planilha, year == 2018
) |> 
  dplyr::select(
    co2_per_capita, ghg_excluding_lucf_per_capita,	
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

#write.csv(pf, "Dados_Filtrados_2018novo.csv", row.names = FALSE)

################################################################################

