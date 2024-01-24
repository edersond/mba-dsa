###############################################################################
##setup de pacotes
library(haven)
library(tidyverse)
library(CCP)
library(CCA)



###############################################################################
#importa dados brutos do spss
raw_data <- read_sav("Database_1.sav")



###############################################################################
#isola conjuntos de variaveis caracteristica da propriedade

vars_infra <- tibble(
    area_pasto_nativo_pct = raw_data$Conservation_area,
    area_lavoura_pct = raw_data$Agricultural_area,
    area_pasto_cultivado_pct = raw_data$Livestock_area,
    area_silagem_pct = raw_data$Capiniera_area
  )

vars_rebanho <- tibble(
    n_touros = raw_data$reprodutor,
    n_vacas_leiteiras = raw_data$vacas,
    n_vacas_lactantes = raw_data$vacas_lacta
  )

vars_manejo <- tibble(
    score_programa_saude_sanitaria = raw_data$escore_sanitária,
    programa_criterio_descarte = raw_data$ecore_mg,
    idade_desmame = raw_data$Desmame,
    intervalo_entre_parimentos = raw_data$IEP
  )

vars_produtividade <- tibble(
    prod_L_dia_propriedade = raw_data$produção,
    prod_L_dia_vaca = raw_data$produtividade,
    prod_L_ha_propriedade = raw_data$Area_produçõ
  )

vars_leite <- tibble(
    gordura_pct = raw_data$Gord,
    proteina_pct = raw_data$Pro,
    lactose_pct = raw_data$Lact,
    solidos_totais_pct = raw_data$ST,
    solidos_desengordurados_pct = raw_data$ESD,
    celulas_somaticas_cel_ml = raw_data$CCS,
    placas_cpp_ml = raw_data$cpp
  )







###############################################################################
#Faz as estatísticas descritivas

grupos_de_variaveis <- list()
grupos_de_variaveis$infra <- vars_infra
grupos_de_variaveis$rebanho <- vars_rebanho
grupos_de_variaveis$manejo <- vars_manejo
grupos_de_variaveis$produtividade <- vars_produtividade
grupos_de_variaveis$leite <- vars_leite
tabela_descritivas <- tibble()
for (grupo in names(grupos_de_variaveis)) {
  for (variavel in names(grupos_de_variaveis[[grupo]])) {
    var_n <- dim(grupos_de_variaveis[[grupo]])[1]
    var_min <- min(grupos_de_variaveis[[grupo]][[variavel]], na.rm = TRUE)
    var_mean <- mean(grupos_de_variaveis[[grupo]][[variavel]], na.rm = TRUE)
    var_sd <- sd(grupos_de_variaveis[[grupo]][[variavel]], na.rm = TRUE)
    var_iqr <- IQR(grupos_de_variaveis[[grupo]][[variavel]], na.rm = TRUE)
    var_max <- max(grupos_de_variaveis[[grupo]][[variavel]], na.rm = TRUE)
    var_descritivas <- tibble(
      grupo <- grupo,
      variavel = variavel,
      n = var_n,
      min = var_min,
      mean = var_mean,
      sd = var_sd,
      iqr = var_iqr,
      max = var_max
    )
    tabela_descritivas <- bind_rows(tabela_descritivas, var_descritivas)
  }
}
tabela_descritivas <- tabela_descritivas %>% rename(grupo = `grupo <- grupo`)
#export to csv
write.csv(tabela_descritivas, file = 'tabela_descritivas.csv')

###############################################################################
#FUNÇOES DA CORRELACAO CANONICA E TESTE DE HIPÓTESE


as_matrix <- function(vars_group) { #transforma um grupo de variaveis tibble em uma matriz
  scm <- vars_group %>% 
    as.matrix()
  return(scm)
}

perform_cca <- function(X, Y) { #reotrna objeto CCA para grupos de variáveis x e y
  X <- as_matrix(X)
  Y <- as_matrix(Y)
  cca_result <- CCA::cc(X, Y)
  return(cca_result)
}                                         

perform_sigtest <- function(cca_result, raw_data) {
  rho <- cca_result$cor #vetor de correlações entre os pares de variaveis canonicas
  n <- dim(raw_data)[1] #n observações no dado bruto
  p <- dim(cca_result$xcoef)[1] #n colunas grupo X
  q <- dim(cca_result$ycoef)[1] #n coluans grupo Y
  sigtest <- CCP::p.asym (rho, n, p, q, "Wilks")
  return(sigtest)
}

###############################################################################
#modela as correlações canônicas, imprime os resultados
#tentei fazer em loop mas estou enferrujado no R, o tempo de fazer uma a uma era o tempo de reaprender a sintaxe
#grupos_de_variaveis
# $infra <- vars_infra  = uso de area
# $rebanho <- vars_rebanho
# $manejo <- vars_manejo
# $produtividade <- vars_produtividade
# $leite <- vars_leite


analisa_cca <- function(tibble_X, nome_X, tibble_Y, nome_Y) {
  cat("\014")
  cat('Correlação canônica entre', nome_X, 'e', nome_Y, '\n \n')

  cca_result <- perform_cca(tibble_X, tibble_Y)
  cat('Coeficientes de correlação canônica: \n')
  print(cca_result$cor)
  cat('\n')
  
  r_quadrado <- cca_result$cor^2 #calcula r quadrado
  cat('R² para cada par de variáveis canônicas: \n')
  print(r_quadrado)
  cat('\n')
  
  cat('Resultado do teste de hipótese, cada linha é o resultado do teste para um par Ui Vi: \n')
  sigtest_result <- perform_sigtest(cca_result, raw_data) #testa hipótese
  cat('\n')
}

analisa_cca(grupos_de_variaveis$infra, "uso de area", grupos_de_variaveis$rebanho, "rebanho") #deu boa
analisa_cca(grupos_de_variaveis$infra, "uso de area", grupos_de_variaveis$manejo, "manejo")
analisa_cca(grupos_de_variaveis$infra, "uso de area", grupos_de_variaveis$produtividade, "produtividade") #deu boa
analisa_cca(grupos_de_variaveis$infra, "uso de area", grupos_de_variaveis$leite, "leite")
analisa_cca(grupos_de_variaveis$rebanho, "rebanho", grupos_de_variaveis$manejo, "manejo") #deu boa
analisa_cca(grupos_de_variaveis$rebanho, "rebanho", grupos_de_variaveis$produtividade, "produtividade") #deu boa
analisa_cca(grupos_de_variaveis$rebanho, "rebanho", grupos_de_variaveis$leite, "leite")
analisa_cca(grupos_de_variaveis$manejo, "manejo", grupos_de_variaveis$produtividade, "produtividade") #deu boa
analisa_cca(grupos_de_variaveis$manejo, "manejo", grupos_de_variaveis$leite, "leite")
analisa_cca(grupos_de_variaveis$produtividade, "produtividade", grupos_de_variaveis$leite, "leite")

