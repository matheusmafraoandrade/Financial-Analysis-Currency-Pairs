################################
####  MODELO ARMA-GARCH    #####
################################

#####
##   PACOTES NECESSÁRIOS
#####

#local = "C:\\Users\\matma\\OneDrive\\TCC\\R\\"
source("C:\\Users\\matma\\OneDrive\\TCC\\R\\install_and_load_packages.R")

#####
##   gbp
#####

# Dados do gbp
df <- read_csv("C:\\Users\\matma\\OneDrive\\TCC\\R\\currencies_returns.csv")
hist(df$gbp, n = 50)

##################################
####   ESTIMAÇÃO ARMA-ARCH    ####
##################################

####
##  1: Especificar uma equação para a média condicional 
####


# Teste de Estacionariedade: Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária
# na série temporal avaliada. Como observamos no gráfico da série, não há tendência
# nos dados e assim o teste verificará se a série se comporta como um passeio aleatório
# sem drift. Isto é possível por meio da opção type que tem as seguintes alternativas:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)
# Além disso, definimos que no máximo duas defasagens da série devem ser usadas como
# variáveis explicativas da regressão do teste. As hipóteses do teste são:
# - H0: raiz unitária (passeio aleatório)
# - H1: sem raiz unitária (não é um passeio aleatório)
unit_root <- fUnitRoots::adfTest(df$gbp, lags = 2, type = c("nc"))
print(unit_root)

# Como resultado do teste temos um p-valor de 0.01 indicando que rejeitamos a hipótese nula de
# presença de raiz unitária ao nível de 5% de significância. Assim, continuamos com a série de
# retornos e não com qualquer diferença dela.


# Agora, deve-se de aplicar o processo de estimação de modelos ARMA(p,q) já estudado:
# - detalhes no arquivo simulacaoarma.R disponível na pasta 4.ARMA deste projeto

# Função de Autocorrelação
acf_arma <- stats::acf(df$gbp, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da Função de Autocorrelação. 
plot(acf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) do GBP/BRL", adj = 0.5, line = 1)

# Função de Autocorrelação parcial
pacf_arma <- stats::pacf(df$gbp, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da Função de Autocorrelação parcial. 
plot(pacf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP) do GBP/BRL", adj = 0.5, line = 1)

# Teste Ljung-Box
Box.test(df$gbp,type="Ljung-Box",lag=1)

# Todas as combinações possíveis de p=0 até p=max e q=0 até q=max
pars <- expand.grid(ar = 0:3, diff = 0, ma = 0:3)

# Local onde os resultados de cada modelo serão armazenados
modelo <- list()

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars)) {
  modelo[[i]] <- arima(df$gbp, order = unlist(pars[i, 1:3]), method = "ML")
}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca <- list()
for (i in 1:length(modelo)) {
  log_verossimilhanca[[i]] <- modelo[[i]]$loglik
}

# Calcular o AIC
aicarma <- list()
for (i in 1:length(modelo)) {
  aicarma[[i]] <- stats::AIC(modelo[[i]])
}

# Calcular o BIC
bicarma <- list()
for (i in 1:length(modelo)) {
  bicarma[[i]] <- stats::BIC(modelo[[i]])
}

# Quantidade de parâmetros estimados por modelo
quant_parametros <- list()
for (i in 1:length(modelo)) {
  quant_parametros[[i]] <- length(modelo[[i]]$coef)+1 # +1 porque temos a variância do termo de erro 
}

# Montar a tabela com os resultados
especificacao <- paste0("arma",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(df$gbp), length(modelo))
resultado_arma <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca),
                             quant_parametros = unlist(quant_parametros),
                             tamanho_amostra, aic = unlist(aicarma), 
                             bic = unlist(bicarma), stringsAsFactors = FALSE)

# Mostrar a tabela de resultado
print(resultado_arma)


# Escolher o melhor modelo
which.min(resultado_arma$aic)
which.min(resultado_arma$bic)

# Planilha
write_xlsx(resultado_arma,"C:\\Users\\matma\\OneDrive\\TCC\\R\\aic_bic_gbp.xlsx")


# Como resultado temos que o modelo escolhido pelo BIC é o ARMA(1,0)
media_condicional <- arima(df$gbp, order = c(0,0,1), method = "ML")

# Verificar qual distribuição de probabilidade melhor se assemelha aos resíduos da média condicional
# Este é um passo importante para o restante da análise. Precisamos garantir que distribuição de 
# probabilidade usada no processo de estimação por meio de máxima verossimilhança faça uso da correta
# distribuição. Assim, comparamos graficamente os resíduos obtidos pela estimação da média condicional
# com duas distribuições de probabilidade (Normal e t-Student). A comparação feita aqui não considera
# assimetria e, em função disso, caso você perceba a existência de assimetria, você deve escolher a 
# distribuição que mais se assemelha aos dados, mas optar pela sua versão com assimetria no momento
# que for estimar o modelo arma-garch conjuntamente. Como resultado, temos que a distribuição t-Student
# é a melhor escolha. 

symmetric_normal = stats::density(stats::rnorm(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                               sd = sd(media_condicional$residuals)))

symmetric_student = stats::density(fGarch::rstd(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                                sd = sd(media_condicional$residuals)))

hist(media_condicional$residuals, n = 25, probability = TRUE, border = "white", col = "steelblue",
     xlab = "Resíduos estimados pela média condicional", ylab = "Densidade", ylim = c(1,60),
     main = "Comparativo da distribuição dos resíduos")
lines(symmetric_normal, lwd = 3, col = 2)
lines(symmetric_student, lwd = 2, col = 1)
legend("topleft", legend = c("Normal", "t-Student"), col = c("red", "black"), lwd = c(3,2))



#####
##   Examinar se os resíduos se comportam como ruído branco. Caso contrário, retornar ao passo 3 ou 4.
#####

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados
#  - H1: resíduos são autocorrelacionados
acf_est <- stats::acf(modelo[[which.min(resultado_arma$bic)]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_est, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Resíduos", adj = 0.5, line = 1)
Box.test(modelo[[which.min(resultado_arma$bic)]]$residuals, type="Ljung", lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados
#  - H1: quadrado dos resíduos são autocorrelacionados
acf_square <- acf(modelo[[which.min(resultado_arma$bic)]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do AR(1)", adj = 0.5, line = 1)
archTest(modelo[[which.min(resultado_arma$bic)]]$residuals)
#Box.test(modelo[[which.min(resultado_arma$bic)]]$residuals^2, type="Ljung", lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos
#  - H1: resíduos não são normalmente distribuídos
#shapiro.test(na.remove(modelo_ipca[[2]]$residuals))
#normalTest(na.remove(modelo_ipca[[2]]$residuals), method = "sw")
jarque.bera.test(na.remove(modelo[[which.min(resultado_arma$bic)]]$residuals))
normalTest(na.remove(modelo[[which.min(resultado_arma$bic)]]$residuals), method = "jb")





# Parâmetros estimados
print(media_condicional)

####
##  2: Testar existência de efeito ARCH
####

# Opção 1: visualizar a FAC dos resíduos aos quadrado (lembre-se de que eles 
# são uma proxy para o retorno ao quadrado). Como resultado temos que há defasagens
# estatisticamente significantes (acima da linha pontilhada). O gráfico da FAC com
# a linha pontilhada é apenas uma forma visual de verificar o teste Ljung-Box que
# analisa se a autocorrelação é estatisticamente diferente de zero.
acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(1,0)", adj = 0.5, line = 1)


# Opção 2: teste LM de Engle (similar ao teste F de uma regressão linear). O resultado
# mostra o p-valor do teste assumindo que a equação dele (a equação do modelo ARCH)
# pode ter tantas defasagens quantas apresentadas na coluna order. Assim, assumindo um
# modelo ARCH(4), rejeitamos a hipótese nula de que todas as defasagens são nulas, ou seja,
# há pelo menos uma diferente de zero e assim, temos heterocedasticidade condicional no erro
# da média condicional
lm_test <- aTSA::arch.test(media_condicional, output = FALSE)
lm_test

####
##  3: Especificar modelo para a volatilidade condicional 
####

# Passo 1: Identificar as ordens máximas M e N do arch e garch, respectivamente. Para tanto, 
# usamos as funções de autocorrelação parcial (FACP) e autocorrelação (FAC)

acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(0,1)", adj = 0.5, line = 1)

pacf_residuals <- stats::pacf(media_condicional$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos resíduos do ARMA(0,1)", adj = 0.5, line = 1)

# Passo 2: Modelar conjuntamente a média condicional e a variância condicional

# Todas as combinações possíveis de m=1 até m=max e n=0 até n=max
pars_arma_garch <- expand.grid(m = 1:3, n = 0:2)

# Local onde os resultados de cada modelo serão armazenados
modelo_arma_garch <- list()

# Especificação ARMA encontrada na estimação da média condicional
arma_set <- "~arma(0,1)"

# Distribuição de probabilidade assumida para o termo de erro da média condicional 
# - norm: normal, std: t-student, snorm: normal assimétrica, sstd: t-student assimétrica
arma_residuals_dist <- "std"

# Definição se o processo estimará parâmetros de assimetria e curtose para a distribuiçãoo
include.skew = FALSE
include.shape = TRUE

# Train Test Split
train = head(df$gbp, 0.50*length(df$gbp))
test = tail(df$gbp, 0.50*length(df$gbp))

dataset = df$gbp

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars_arma_garch)) {
  modelo_arma_garch[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                             data = dataset, trace = FALSE, cond.dist = arma_residuals_dist,
                                             include.skew = include.skew, include.shape = include.shape) 
}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  log_verossimilhanca_arma_garch[[i]] <- modelo_arma_garch[[i]]@fit$llh
}

# Calcular o AIC
aicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  aicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[1]
}

# Calcular o BIC
bicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  bicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[2]
}

# Quantidade de parâmetros estimados por modelo
quant_paramentros_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  quant_paramentros_arma_garch[[i]] <- length(modelo_arma_garch[[i]]@fit$coef)
}

# Montar a tabela com os resultados
especificacao <- paste0(arma_set,"-","garch",pars_arma_garch$m,pars_arma_garch$n)
tamanho_amostra <- rep(length(dataset), length(modelo_arma_garch))
resultado_arma_garch <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca_arma_garch),
                                   quant_paramentros = unlist(quant_paramentros_arma_garch),
                                   tamanho_amostra, aic = unlist(aicarma_garch), bic = unlist(bicarma_garch),
                                   stringsAsFactors = FALSE)

print(resultado_arma_garch)

# Escolher o modelo com menor AIC e/ou BIC
which.min(resultado_arma_garch$aic)
which.min(resultado_arma_garch$bic)

# Planilha
write_xlsx(resultado_arma_garch,"C:\\Users\\matma\\OneDrive\\TCC\\R\\garch_aic_bic_gbp.xlsx")

# Mostrar o resultado da tabela
#print(resultado_arma_garch)


# Estimar o modelo escolhido
media_variancia_condicional <- fGarch::garchFit(~arma(0,1)+garch(1,1), data = dataset, trace = FALSE, 
                                                cond.dist = "std", include.skew = include.skew,
                                                include.shape = include.shape)

spec = ugarchspec(mean.model=list(armaOrder=c(0,1)), distribution.model = "std",
                  variance.model=list(model= "sGARCH", garchOrder=c(1,1)), fixed.pars = list(mu=0))
gm = ugarchfit(data = df$gbp, spec = spec, out.sample = length(tail(df$gbp, 0.18*length(df$gbp))))
show(gm)

jarque.bera.test(na.remove(residuals(gm, standardize=TRUE)))

pred = ugarchforecast(gm, data = df$gbp, n.ahead = 30, n.roll = 0, out.sample = length(tail(df$gbp, 0.18*length(df$gbp))))
pred
plot(pred)

# Parâmetros estimados. Aqui, usamos a função stargazer do pacote stargazer para 
# mostrar os resultados em um formato textual mais amigável para interpretação.
# Mais detalhes? Use help("stargazer")
stargazer::stargazer(media_variancia_condicional, type = "text", title = "Resultado Estimação modelo ARMA(1,0)-GARCH(1,0)")
summary(media_variancia_condicional)

# Previsão do retorno esperado e variância esperada
(fGarch::predict(media_variancia_condicional, n.ahead = 1)$meanForecast)
(fGarch::predict(media_variancia_condicional, n.ahead = 1)$standardDeviation)^2



#### LIXO ####



###################################################
### Fit an IGARCH(1,1) model
###################################################
source ("C:\\Users\\matma\\OneDrive\\TCC\\R\\Igarch.R.txt")
m7 = Igarch (df$gbp)
names(m7)
m7$par
resi = df$gbp / m7$volatility
archTest (resi, 20)     
par (mfcol = c(1, 1))
plot.ts(m7$volatility)

# Obtain volatility
v4 = m7$volatility  							
vol = ts (v4, frequency = 12, start = c(1973, 1))
res = ts(resi, frequency = 12, start = c(1973, 1))
par (mfcol = c(1, 2))  							
plot (vol, xlab = 'year', ylab = 'volatility', type = 'l')
plot (res, xlab = 'year', ylab = 'st. resi', type = 'l') 

# Obtain ACF & PACF
par (mfcol = c(2, 2)) 							
acf (resi, lag = 24)
pacf (resi, lag = 24)
acf (resi^2, lag = 24)
pacf (resi^2, lag = 24) 

# Obtain plot of predictive intervals
par (mfcol = c(1, 1))
mu = 0
upp = mu + 2 * v4
low = mu - 2 * v4
tdx = c(1:length(df$gbp)) / 12 + 1973
plot (tdx, df$gbp, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.6, 0.6))
lines (tdx, upp, lty = 2, col = 'red')
lines (tdx, low, lty = 2, col = 'red')
abline (h = c(mu), col = 'red')

acf (resi^2, lag = 24)



####
##  4: Avaliar o modelo estimado
####

# Verificar se todos os parâmetros são estatisticamente significantes.
# Nos resultados mostrados no código da linha 231 percebemos que todos
# os parâmetros são estatisticamente significantes. Se esse não for o 
# caso, deveríamos retirar o parâmetro não significante do processo de
# estimação e verificar novamente a significância dos demais parâmetros


# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados
#  - H1: resíduos são autocorrelacionados
acf_est <- stats::acf(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_est, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Resíduos", adj = 0.5, line = 1)
Box.test(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals, type="Ljung", lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados
#  - H1: quadrado dos resíduos são autocorrelacionados
acf_square <- acf(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do AR(1)", adj = 0.5, line = 1)
archTest(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals)
Box.test(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals^2, type="Ljung", lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos
#  - H1: resíduos não são normalmente distribuídos
#shapiro.test(na.remove(modelo_ipca[[2]]$residuals))
#normalTest(na.remove(modelo_ipca[[2]]$residuals), method = "sw")
jarque.bera.test(na.remove(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals))
normalTest(na.remove(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals), method = "jb")


# Verificando se os resíduos ao quadrado ainda continuam com heterocedasticidade condicional
# O resultado mostra que a heterocedasticidade condicional foi tratada dado que não há defasagens
# estatisticamente significantes (acima da linha pontilhada) na FAC
acf_residuals_armagarch <- acf(residuals(media_variancia_condicional, standardize=TRUE)^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_armagarch, main = "", ylab = "", xlab = "Defasagem", ci=0.99)
title("FAC do quadrado dos resíduos do ARMA(1,0)-GARCH(1,1)", adj = 0.5, line = 1)


