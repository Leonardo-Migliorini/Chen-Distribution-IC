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
    methane_per_capita,	energy_per_gdp, energy_per_capita, ghg_excluding_lucf_per_capita
   ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |> 
  dplyr::rename(
    y = co2_per_capita, x1 = ghg_per_capita, x2 = ghg_excluding_lucf_per_capita,
    x3 = methane_per_capita, x4 = energy_per_gdp, x5 = energy_per_capita
  )

pf <- pf[!(row.names(pf) %in% c("158")),]

# Corrplot

x <- cor(pf)
corrplot::corrplot(
  x, type="upper", method = "number"
  )

# Exportação da Tabela Filtrada

#write.csv(pf, "Dados_Filtrados_2018.csv", row.names = FALSE)
 
