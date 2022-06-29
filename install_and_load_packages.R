#######################################################
######    INSTALAR E/OU CARREGAR PACOTES      #########
#######################################################

# Este script em R verifica se os pacotes necessários para executar os exemplos
# discutidos em sala de aula estão instalados e caso contrário, faz a instalação. 
# Se os pacotes já estão instalados, o script apenas carrega-os. 

message("Carregando pacotes default para a disciplina...")

# Lista de pacotes necessários
.packages = c("tidyquant", "tidyverse", "xtable", "stargazer", "highcharter", "quantmod",
              "dygraphs","tseries", "htmltools", "Quandl", "nycflights13", "magrittr",
              "discreteRV", "aTSA", "fGarch", "fUnitRoots", "vars", "MTS", "seasonal",
              "urca", "dynlm", "tbl2xts", "dlm", "backtest.R", "stats", "tcltk", "tcltk2", "rugarch")
# Verificar quais dos pacotes necessários já estão instalados. Para isso, usa a função
# installed.packages() que retorna um dataframe com a lista dos pacotes instaldos no
# R em execução. Como resultado, temos um vetor do mesmo tamanho do vetor de pacotes
# de interesse com TRUE (pacote já instalado) ou FALSE (pacote não instalado)
.inst <- .packages %in% installed.packages()


# Se o vetor de pacotes não instalados (diferença entre os pacotes de interesse e os
# pacotes instalados) tiver tamanho maior que 0, instalar apenas os pacotes não instalados
if (length(.packages[!.inst]) > 0) {
  install.packages(.packages[!.inst])
}

# Após confirmar que todos os pacotes estão instaldos, garantimos que eles estão disponíveis
# para uso fazendo a carga dos mesmos no R 
for (package in 1:length(.packages)) {
  suppressMessages(require(.packages[package], character.only = TRUE)) 
}

message("Lista de pacotes carregados....")
print(.packages)

