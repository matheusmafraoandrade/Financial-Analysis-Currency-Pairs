################################
####  MODELO ARMA-GARCH    #####
################################

#####
##   PACOTES NECESS�RIOS
#####

#local = "C:\\Users\\matma\\OneDrive\\TCC\\R\\"
source("C:\\Users\\matma\\OneDrive\\TCC\\R\\install_and_load_packages.R")

#####
##   USD
#####

# Dados do USD
df <- read_csv("C:\\Users\\matma\\OneDrive\\TCC\\R\\currencies_returns.csv")
hist(df$usd, n = 50)

##################################
####   ESTIMA��O ARMA-ARCH    ####
##################################

####
##  1: Especificar uma equa��o para a m�dia condicional 
####


# Teste de Estacionariedade: Aqui, usamos a fun��o adfTest do pacote fUnitRoots para testar se h� raiz unit�ria
# na s�rie temporal avaliada. Como observamos no gr�fico da s�rie, n�o h� tend�ncia
# nos dados e assim o teste verificar� se a s�rie se comporta como um passeio aleat�rio
# sem drift. Isto � poss�vel por meio da op��o type que tem as seguintes alternativas:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleat�rio)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleat�rio com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleat�rio com constante e tend�ncia)
# Al�m disso, definimos que no m�ximo duas defasagens da s�rie devem ser usadas como
# vari�veis explicativas da regress�o do teste. As hip�teses do teste s�o:
# - H0: raiz unit�ria (passeio aleat�rio)
# - H1: sem raiz unit�ria (n�o � um passeio aleat�rio)
unit_root <- fUnitRoots::adfTest(df$usd, lags = 1, type = c("nc"))
print(unit_root)

# Como resultado do teste temos um p-valor de 0.01 indicando que rejeitamos a hip�tese nula de
# presen�a de raiz unit�ria ao n�vel de 5% de signific�ncia. Assim, continuamos com a s�rie de
# retornos e n�o com qualquer diferen�a dela.


# Agora, deve-se de aplicar o processo de estima��o de modelos ARMA(p,q) j� estudado:
# - detalhes no arquivo simulacaoarma.R dispon�vel na pasta 4.ARMA deste projeto

# Fun��o de Autocorrela��o
acf_arma <- stats::acf(df$usd, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gr�fico da Fun��o de Autocorrela��o. 
plot(acf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Fun��o de Autocorrela��o (FAC) do USD/BRL", adj = 0.5, line = 1)

# Fun��o de Autocorrela��o parcial
pacf_arma <- stats::pacf(df$usd, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gr�fico da Fun��o de Autocorrela��o parcial. 
plot(pacf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Fun��o de Autocorrela��o Parcial (FACP) do USD/BRL", adj = 0.5, line = 1)

# Teste Ljung-Box
Box.test(df$usd,type="Ljung-Box",lag=1)

# Todas as combina��es poss�veis de p=0 at� p=max e q=0 at� q=max
pars <- expand.grid(ar = 0:3, diff = 0, ma = 0:3)

# Local onde os resultados de cada modelo ser�o armazenados
modelo <- list()

# Estimar os par�metros dos modelos usando M�xima Verossimilhan�a (ML)
for (i in 1:nrow(pars)) {
  modelo[[i]] <- arima(df$usd, order = unlist(pars[i, 1:3]), method = "ML")
}

# Obter o logaritmo da verossimilhan�a (valor m�ximo da fun��o)
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

# Quantidade de par�metros estimados por modelo
quant_parametros <- list()
for (i in 1:length(modelo)) {
  quant_parametros[[i]] <- length(modelo[[i]]$coef)+1 # +1 porque temos a vari�ncia do termo de erro 
}

# Montar a tabela com os resultados
especificacao <- paste0("arma",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(df$usd), length(modelo))
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
#write_xlsx(resultado_arma,"C:\\Users\\matma\\OneDrive\\TCC\\R\\aic_bic_usd.xlsx")


# Como resultado temos que o modelo escolhido pelo BIC � o ARMA(1,0)
media_condicional <- arima(df$usd, order = c(1,0,0), method = "ML")

# Verificar qual distribui��o de probabilidade melhor se assemelha aos res�duos da m�dia condicional
# Este � um passo importante para o restante da an�lise. Precisamos garantir que distribui��o de 
# probabilidade usada no processo de estima��o por meio de m�xima verossimilhan�a fa�a uso da correta
# distribui��o. Assim, comparamos graficamente os res�duos obtidos pela estima��o da m�dia condicional
# com duas distribui��es de probabilidade (Normal e t-Student). A compara��o feita aqui n�o considera
# assimetria e, em fun��o disso, caso voc� perceba a exist�ncia de assimetria, voc� deve escolher a 
# distribui��o que mais se assemelha aos dados, mas optar pela sua vers�o com assimetria no momento
# que for estimar o modelo arma-garch conjuntamente. Como resultado, temos que a distribui��o t-Student
# � a melhor escolha. 

symmetric_normal = stats::density(stats::rnorm(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                               sd = sd(media_condicional$residuals)))

symmetric_student = stats::density(fGarch::rstd(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                                sd = sd(media_condicional$residuals)))

hist(media_condicional$residuals, n = 25, probability = TRUE, border = "white", col = "steelblue",
     xlab = "Res�duos estimados pela m�dia condicional", ylab = "Densidade", ylim = c(1,60),
     main = "Comparativo da distribui��o dos res�duos")
lines(symmetric_normal, lwd = 3, col = 2)
lines(symmetric_student, lwd = 2, col = 1)
legend("topleft", legend = c("Normal", "t-Student"), col = c("red", "black"), lwd = c(3,2))



#####
##   Examinar se os res�duos se comportam como ru�do branco. Caso contr�rio, retornar ao passo 3 ou 4.
#####

# Teste de autocorrela��o dos res�duos
#  - H0: res�duos s�o n�o autocorrelacionados
#  - H1: res�duos s�o autocorrelacionados
acf_est <- stats::acf(modelo[[which.min(resultado_arma$bic)]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_est, main = "", ylab = "", xlab = "Defasagem")
title("Fun��o de Autocorrela��o (FAC) dos Res�duos", adj = 0.5, line = 1)
Box.test(modelo[[which.min(resultado_arma$bic)]]$residuals, type="Ljung", lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos res�duos s�o n�o autocorrelacionados
#  - H1: quadrado dos res�duos s�o autocorrelacionados
acf_square <- acf(modelo[[which.min(resultado_arma$bic)]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos res�duos do AR(1)", adj = 0.5, line = 1)
archTest(modelo[[which.min(resultado_arma$bic)]]$residuals)
#Box.test(modelo[[which.min(resultado_arma$bic)]]$residuals^2, type="Ljung", lag=1)

# Teste de Normalidade dos res�duos. As hip�teses para os dois testes s�o:
#  - H0: res�duos normalmente distribu�dos
#  - H1: res�duos n�o s�o normalmente distribu�dos
#shapiro.test(na.remove(modelo_ipca[[2]]$residuals))
#normalTest(na.remove(modelo_ipca[[2]]$residuals), method = "sw")
jarque.bera.test(na.remove(modelo[[which.min(resultado_arma$bic)]]$residuals))
normalTest(na.remove(modelo[[which.min(resultado_arma$bic)]]$residuals), method = "jb")





# Par�metros estimados
print(media_condicional)

####
##  2: Testar exist�ncia de efeito ARCH
####

# Op��o 1: visualizar a FAC dos res�duos aos quadrado (lembre-se de que eles 
# s�o uma proxy para o retorno ao quadrado). Como resultado temos que h� defasagens
# estatisticamente significantes (acima da linha pontilhada). O gr�fico da FAC com
# a linha pontilhada � apenas uma forma visual de verificar o teste Ljung-Box que
# analisa se a autocorrela��o � estatisticamente diferente de zero.
acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos res�duos do ARMA(1,0)", adj = 0.5, line = 1)


# Op��o 2: teste LM de Engle (similar ao teste F de uma regress�o linear). O resultado
# mostra o p-valor do teste assumindo que a equa��o dele (a equa��o do modelo ARCH)
# pode ter tantas defasagens quantas apresentadas na coluna order. Assim, assumindo um
# modelo ARCH(4), rejeitamos a hip�tese nula de que todas as defasagens s�o nulas, ou seja,
# h� pelo menos uma diferente de zero e assim, temos heterocedasticidade condicional no erro
# da m�dia condicional
lm_test <- aTSA::arch.test(media_condicional, output = FALSE)
lm_test

####
##  3: Especificar modelo para a volatilidade condicional 
####

# Passo 1: Identificar as ordens m�ximas M e N do arch e garch, respectivamente. Para tanto, 
# usamos as fun��es de autocorrela��o parcial (FACP) e autocorrela��o (FAC)

acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos res�duos do ARMA(1,0)", adj = 0.5, line = 1)

pacf_residuals <- stats::pacf(media_condicional$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 20)
plot(pacf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos res�duos do ARMA(1,0)", adj = 0.5, line = 1)

# Passo 2: Modelar conjuntamente a m�dia condicional e a vari�ncia condicional

# Todas as combina��es poss�veis de m=1 at� m=max e n=0 at� n=max
pars_arma_garch <- expand.grid(m = 1:3, n = 0:2)

# Local onde os resultados de cada modelo ser�o armazenados
modelo_arma_garch <- list()

# Especifica��o ARMA encontrada na estima��o da m�dia condicional
arma_set <- "~arma(1,0)"

# Distribui��o de probabilidade assumida para o termo de erro da m�dia condicional 
# - norm: normal, std: t-student, snorm: normal assim�trica, sstd: t-student assim�trica
arma_residuals_dist <- "std"

# Defini��o se o processo estimar� par�metros de assimetria e curtose para a distribui��oo
include.skew = FALSE
include.shape = TRUE

# Train Test Split
train = head(df$usd, 0.50*length(df$usd))
test = tail(df$usd, 0.50*length(df$usd))

dataset = df$usd

# Estimar os par�metros dos modelos usando M�xima Verossimilhan�a (ML)
for (i in 1:nrow(pars_arma_garch)) {
  modelo_arma_garch[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                             data = dataset, trace = FALSE, cond.dist = arma_residuals_dist,
                                             include.skew = include.skew, include.shape = include.shape) 
}

# Obter o logaritmo da verossimilhan�a (valor m�ximo da fun��o)
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

# Quantidade de par�metros estimados por modelo
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
write_xlsx(resultado_arma_garch,"C:\\Users\\matma\\OneDrive\\TCC\\R\\garch_aic_bic_usd.xlsx")

# Mostrar o resultado da tabela
#print(resultado_arma_garch)


# Estimar o modelo escolhido
#media_variancia_condicional <- fGarch::garchFit(~arma(1,0)+garch(1,1), data = head(df$usd, 0.50*length(df$usd)), trace = FALSE, 
#                                                cond.dist = "std", include.skew = include.skew,
#                                                include.shape = include.shape)

spec = ugarchspec(mean.model=list(armaOrder=c(1,0)), distribution.model = "std",
                  variance.model=list(model= "sGARCH", garchOrder=c(1,2)), fixed.pars = list(mu=0))
gm = ugarchfit(data = df$usd, spec = spec, out.sample = length(tail(df$usd, 0.18*length(df$usd))))
show(gm)

jarque.bera.test(na.remove(residuals(gm, standardize=TRUE)))

pred = ugarchforecast(gm, data = df$usd, n.ahead = 30, n.roll = 0, out.sample = length(tail(df$usd, 0.18*length(df$usd))))
pred
plot(pred)

Series(pred)

dxy = my_data <- read_csv("C:\\Users\\matma\\OneDrive\\TCC\\R\\ret_dxy.csv")

symmetric_normal = stats::density(stats::rnorm(length(dxy$dxy), mean = mean(dxy$dxy), 
                                               sd = sd(dxy$dxy)))

symmetric_student = stats::density(fGarch::rstd(length(dxy$dxy), mean = mean(dxy$dxy), 
                                                sd = sd(dxy$dxy)))

hist(dxy$dxy, n = 25, probability = TRUE, border = "white", col = "steelblue",
     xlab = "Retornos dxy$dxy", ylab = "Densidade", ylim = c(1,100),
     main = "Comparativo da distribui��o dos retornos")
lines(symmetric_normal, lwd = 3, col = 2)
lines(symmetric_student, lwd = 2, col = 1)
legend("topleft", legend = c("Normal", "t-Student"), col = c("red", "black"), lwd = c(3,2))

# Par�metros estimados. Aqui, usamos a fun��o stargazer do pacote stargazer para 
# mostrar os resultados em um formato textual mais amig�vel para interpreta��o.
# Mais detalhes? Use help("stargazer")
stargazer::stargazer(media_variancia_condicional, type = "text", title = "Resultado Estima��o modelo ARMA(1,0)-GARCH(1,0)")
summary(media_variancia_condicional)

# Previs�o do retorno esperado e vari�ncia esperada
(fGarch::predict(media_variancia_condicional, n.ahead = 1)$meanForecast)
(fGarch::predict(media_variancia_condicional, n.ahead = 1)$standardDeviation)^2



#### LIXO ####

###################################################
### Fit an IGARCH(1,1) model
###################################################
source ("C:\\Users\\matma\\OneDrive\\TCC\\R\\Igarch.R.txt")
m7 = Igarch (df$usd)
names(m7)
m7$par
resi = df$usd / m7$volatility
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
tdx = c(1:length(df$usd)) / 12 + 1973
plot (tdx, df$usd, xlab = 'year', ylab = 'series', type = 'l', ylim = c(-0.6, 0.6))
lines (tdx, upp, lty = 2, col = 'red')
lines (tdx, low, lty = 2, col = 'red')
abline (h = c(mu), col = 'red')

acf (resi^2, lag = 24)



####
##  4: Avaliar o modelo estimado
####

# Verificar se todos os par�metros s�o estatisticamente significantes.
# Nos resultados mostrados no c�digo da linha 231 percebemos que todos
# os par�metros s�o estatisticamente significantes. Se esse n�o for o 
# caso, dever�amos retirar o par�metro n�o significante do processo de
# estima��o e verificar novamente a signific�ncia dos demais par�metros


# Teste de autocorrela��o dos res�duos
#  - H0: res�duos s�o n�o autocorrelacionados
#  - H1: res�duos s�o autocorrelacionados
acf_est <- stats::acf(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_est, main = "", ylab = "", xlab = "Defasagem")
title("Fun��o de Autocorrela��o (FAC) dos Res�duos", adj = 0.5, line = 1)
Box.test(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals, type="Ljung", lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos res�duos s�o n�o autocorrelacionados
#  - H1: quadrado dos res�duos s�o autocorrelacionados
acf_square <- acf(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos res�duos do AR(1)", adj = 0.5, line = 1)
archTest(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals)
Box.test(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals^2, type="Ljung", lag=1)

# Teste de Normalidade dos res�duos. As hip�teses para os dois testes s�o:
#  - H0: res�duos normalmente distribu�dos
#  - H1: res�duos n�o s�o normalmente distribu�dos
#shapiro.test(na.remove(modelo_ipca[[2]]$residuals))
#normalTest(na.remove(modelo_ipca[[2]]$residuals), method = "sw")
jarque.bera.test(na.remove(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals))
normalTest(na.remove(modelo_arma_garch[[which.min(resultado_arma_garch$bic)]]@residuals), method = "jb")


# Verificando se os res�duos ao quadrado ainda continuam com heterocedasticidade condicional
# O resultado mostra que a heterocedasticidade condicional foi tratada dado que n�o h� defasagens
# estatisticamente significantes (acima da linha pontilhada) na FAC
acf_residuals_armagarch <- acf(residuals(media_variancia_condicional, standardize=TRUE)^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_armagarch, main = "", ylab = "", xlab = "Defasagem", ci=0.99)
title("FAC do quadrado dos res�duos do ARMA(1,0)-GARCH(1,1)", adj = 0.5, line = 1)
