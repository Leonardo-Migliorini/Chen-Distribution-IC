# Importação dos dados

planilha <- readr::read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
  )

# Filtragem dos dados

pf <- dplyr::filter(
  planilha, year == 2018
  ) |> 
  dplyr::select(
     co2_per_capita, ghg_per_capita, ghg_excluding_lucf_per_capita,	
     methane_per_capita,	energy_per_gdp
    ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
    ) |> 
  dplyr::rename(
    Co2_pc = co2_per_capita, Ghg_pc = ghg_per_capita, Ghg_el_pc = ghg_excluding_lucf_per_capita,
    Methane_pc = methane_per_capita, Energy_pgdp = energy_per_gdp
  )
  

# Corrplot

x <- cor(pf)
corrplot::corrplot(
  x, type="upper", method = "number"
  )

# Exportação da Tabela Filtrada

#write.csv(pf, "Dados_Filtrados_2018.csv", row.names = FALSE)
 
