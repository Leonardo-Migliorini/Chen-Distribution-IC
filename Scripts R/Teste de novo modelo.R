# Importação dos dados

planilha <- readr::read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
)

planilha2 <- readr::read_csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Ozone%20concentration-StateofGlobalAir/Ozone%20concentration-StateofGlobalAir.csv"
)

# Filtragem dos dados

pf <- dplyr::filter(
  planilha, year == 2015
) |> 
  dplyr::select(
    country, co2_per_capita, ghg_excluding_lucf_per_capita, methane_per_capita
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |> 
  dplyr::rename(
    x1 = co2_per_capita, x2 = ghg_excluding_lucf_per_capita,
    x3 = methane_per_capita, 
  )

pf2 <- dplyr::filter(
  planilha2, Year == 2015
) |> 
  dplyr::select(
    c(1,3)
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |> 
  dplyr::rename(
    y = c(2), country = Entity
  )

pf3 <- dplyr::full_join(
  pf, pf2 
  ) |> 
  dplyr::select(
    c(2:5)
  ) |> 
  dplyr::filter(y >0)
    
pf3 <- pf3[!(row.names(pf3) %in% c("182":"188")),]                      

# Corrplot

x <- cor(pf3)
corrplot::corrplot(
  x, type="upper", method = "number"
)

# Exportação da Tabela Filtrada

write.csv(pf3, "Dados_Filtrados_2015teste.csv", row.names = FALSE)

