# Importação dos dados Nuvem ###################################################

# planilha <- readr::read_csv(
#   "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
# )


# Importação das tabelas locais  ###############################################

planilha <- readxl::read_xlsx(
  "C:\\Users\\leona\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\Dados CO2 - original.xlsx"
)

planilha2 <- readr::read_csv(
  "C:\\Users\\leona\\\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\Base Complementar.csv"
)

planilha3 <- readr::read_csv(
  "C:\\Users\\leona\\\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\IDH.csv"
)

planilha4 <- readr::read_csv(
  "C:\\Users\\leona\\\\Dropbox\\Modelagem de dados com Distribuição Chein - Leonardo Prior\\Banco de Dados\\Dados_Agricultura.csv"
)


################################################################################

# Filtragem das Variáveis

################################################################################

# Tabela 1 #####################################################################

pf1 <- dplyr::filter(
  planilha, year == 2018
) |>
  dplyr::select(
    country, co2_per_capita
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |>
  dplyr::rename(
    y = co2_per_capita
  )

pf1 <- pf1[!(row.names(pf1) %in% c("159")),]

# Tabela 2 #####################################################################

pf2 <- planilha2 |> 
  dplyr::filter_all(
    dplyr::all_vars(. >= 0) 
    )|> 
  dplyr::select(
    c(3,12,18, 19, 20)
    ) |> 
  dplyr::rename(
    country = c(1)
    )
  pf2[,2:5] <- sapply(pf2[,2:5], as.numeric)

# Tabela 3 #####################################################################
  
  
pf3 <- planilha3 |> 
  dplyr::select(
    c(2,34)
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(. >= 0)
  )
  
# Planilha 4 ###################################################################
  
  pf4 <- planilha4

# Unificação de Tabelas ########################################################


name <- function(formatar = 0, limpar = 0, salvar = 0){
  DA <- pf1 |> 
    dplyr::full_join(pf2, by = "country") |> 
    dplyr::full_join(pf3, by = "country") |> 
    dplyr::full_join(pf4, by = "country") |> 
    dplyr::filter_all(
      dplyr::all_vars(. >= 0)
    ) |> 
    dplyr::select(
      -c(1)
    ) 
  DA <- DA[!(row.names(DA) %in% c("70")),]
  if(formatar == 1){
    DA <- DA |> 
      dplyr::rename(
        x1 = c(2), x2 = c(3), x3 = c(4), x4 = c(5), x5 = c(6), x6 = c(7), x7 = c(8),
        x8 = c(9), x9 = c(10), x10 = c(11)
      )
  }
  if(limpar == 1){
    rm(pf1, pf2, pf3, pf4, planilha, planilha2, planilha3, planilha4, envir = .GlobalEnv)
  }
  if(salvar == 1){
    write.csv(DA, "C://Users//leona//OneDrive//Área de Trabalho//Aplicação.csv", row.names = FALSE)
  }
  return(DA)
}

 # DA <- name(formatar = 0, limpar = 1, salvar = 0)

