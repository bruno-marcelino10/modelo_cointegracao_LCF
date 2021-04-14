##### Backtest: Cointegração entre Ativos do Setor Financeiro ##############
##### 08/04/2020 ###########################################################
##### Bruno Marcelino, Miguel Manassés #####################################

# Importando pacotes...

library("tidyverse")
library("lubridate")
library("tidyquant")
library("egcm")
library("Quandl")
library("gridExtra")
library("ggthemes")

# Período de Treino: Pós Estabilização do mercado 
inicio_treino <- as.Date("2020-08-10")
fim_treino <- as.Date("2020-12-31")

# Teste de Engle-Granger
tickers <- c("BRIV4.SA", "ABCB4.SA","BEES4.SA","BMGB4.SA","RPAD3.SA","BIDI4.SA","ITUB4.SA","BBDC4.SA","BBDC3.SA","BBAS3.SA","BPAC11.SA","ITSA4.SA","ITUB4.SA","SANB11.SA")

teste_eg <- allpairs.egcm(tickers, startdate = inicio_treino, enddate = fim_treino, clear.na.inf = TRUE)

cointegrados <- teste_eg %>%
    tibble()  %>%
    filter(is.cointegrated == TRUE)

cointegrados <- cointegrados[-c(4),]

#---- ESCOLHA: NOME DOS ATIVOS (baseado nos resultados obtidos para o teste de Engle-Granger)

ativo_x <- as.character(cointegrados[3,1])
ativo_y <- as.character(cointegrados[3,2])

# ---- Importando cotações para o ADF Test  -----------------------------

df_x <- ativo_x %>% 
    tq_get(get = "stock.prices", from = inicio_treino, to = fim_treino) %>% 
    mutate(adjusted = na.locf(adjusted))

df_y <- ativo_y %>% 
    tq_get(get = "stock.prices", from = inicio_treino, to = fim_treino) %>% 
    mutate(adjusted = na.locf(adjusted))

reg_treino <- lm(df_y$adjusted ~ df_x$adjusted)
summary(reg_treino)

# ---- Teste de Dickey-Fuller Aumentado (confirmar a existência de cointegração) ----

residuos_treino <- residuals(reg_treino)
plot(residuos_treino, type = "l", col = "red", main = "Residuos", ylab = "Residuos", xlab = "Amostras")

tseries::adf.test(residuos_treino)

n_amostras <- seq(from = 1, to = length(df_y$adjusted), by = 1)
ativos <- tibble("Ativo Y" = df_y$adjusted, "Ativo X" = df_x$adjusted, "Residuos" = residuos_treino, "Amostras" = n_amostras)

# ---- Para que os ativos sejam cointegrados, a série de resíduos da regressão deve ser estacionária (Beta da regressão constante)

regplot <- ggplot(ativos, aes(y = Residuos, x = Amostras, color = "white")) + 
    geom_point() + 
    geom_smooth(method = "lm",  color = "red") + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() 

print(regplot)

# Gráfico das Cotações

cotacoes_treino <- df_x  %>%
    bind_rows(df_y)  %>%
    group_by(symbol) %>%
    select(date, adjusted)

grafico_cotacoes <- cotacoes_treino %>% ggplot(aes(x = date, y = adjusted, col = symbol)) +
    geom_line(size = 1) +
    labs(title = "", x = "", y = "Preço Ajustado", color = "", subtitle = "Cotações Diárias Importadas do Yahoo Finance") +
    facet_wrap(~ symbol, ncol = 2, scales = "free") +
    scale_y_continuous(labels = scales::dollar) +
    theme_tq() + 
    scale_color_tq()

print(grafico_cotacoes)

# -------- APLICAÇÃO DE PAIRS TRADE NO PERÍODO DE TESTE --------

#### --- Importando dados --- ####

# Período de Treino: Ano atual
inicio_teste <- as.Date("2021-01-01")
fim_teste <- as.Date("2021-03-31")

cotacoes_teste <- c(ativo_x, ativo_y) %>% 
    tq_get(get = "stock.prices", from = inicio_teste, to = fim_teste) %>% 
    mutate(adjusted = na.locf(adjusted)) %>% 
    select(symbol, date, adjusted) 

df_cotacoes <- cotacoes_teste %>% 
    group_by(symbol) %>% 
    spread(key = symbol, value = adjusted)

# Gráfico das Cotações

grafico_cotacoes <- cotacoes_teste %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "", x = "", y = "Preço Ajustado", color = "", subtitle = "Cotações Diárias Importadas do Yahoo Finance") +
    facet_wrap(~ symbol, ncol = 2, scales = "free") +
    scale_y_continuous(labels = scales::dollar) +
    theme_tq() + 
    scale_color_tq()

grafico_cotacoes

# Alfa e Beta (será utilizado o anterior no momento de aplicação da estratégia)

beta <- summary(reg_treino)$coefficients[2]
alfa <- summary(reg_treino)$coefficients[1]

# Calcularemos os resíduos com base nos coeficientes da regressão de treino
y_previsto <- alfa + (beta * df_cotacoes$BEES4.SA)
residuos_teste <- df_cotacoes$BPAC11.SA - y_previsto    

plot(residuos_teste, type = "l", col = "red", main = "Residuos", ylab = "Residuos", xlab = "Amostras")

#### --- Aplicação do modelo no período de teste --- ####

# Vetor com o Z-score para cada dia
z_score <- (residuos_teste - mean(residuos_treino)) / sd(residuos_treino)

# Criando df com os retornos
retornos <- cotacoes_teste %>% 
    group_by(symbol) %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = dailyReturn,
                 type = "log",
                 col_rename = "retornos") %>% 
    spread(key = symbol, value = retornos)

### Criando sinais

# long x, short y : 1
# short x, long y : -1
# neutro : 0

resultado <- retornos %>% 
    mutate("z_score_teste" = z_score) %>% 
    mutate("Sinal" = ifelse(z_score_teste >= 2, -1, ifelse(z_score_teste <= -2, 1, 0))) %>%  
    mutate("retorno" = ifelse(Sinal == 1, BEES4.SA + (beta * -BPAC11.SA), ifelse(Sinal == -1, -BEES4.SA + (beta * BPAC11.SA), 0)))

# Acrescentando a taxa SELIC e os custos de transação (obtidos a partir do artigo)

Quandl.api_key('TC1ow5j6G7s4SFHTzgDz') # Acrescentando a chave da API - Comando necessário pra acessar o Quandl
selic_diaria <-  as.data.frame(Quandl("BCB/11", type = "xts")/100) # Importando a serie do selic do Bacen
df_selic <- selic_diaria %>% 
    rownames_to_column() %>% 
    tibble() %>% 
    rename("date" = rowname, "selic" = V1) %>% 
    mutate(selic = na.locf(selic))

df_selic$date <- as.Date(df_selic$date)

custos <- 0.005

resultado <- resultado %>% 
    left_join(df_selic, by = "date") %>% 
    mutate(retorno = ifelse(Sinal == 0, df_selic$selic, retorno)) %>% 
    mutate(custos = ifelse(lag(Sinal) != Sinal, custos, 0)) %>% 
    mutate(retorno = retorno - custos)

resultado$retorno[1] <- 0    

# Resultados do modelo 

desempenho <- resultado %>% 
    select(date, retorno) %>% 
    mutate("retorno_acumulado" = cumprod(1 + retorno)-1) %>% 
    mutate("MM" = SMA(retorno_acumulado, 7)) %>% 
    mutate("cor" = as.factor(ifelse(retorno_acumulado >= 0, "Positivo", "Negativo")))

# Retorno acumulado por ativo
grafico_1 <- desempenho %>% 
    ggplot(aes(date, retorno_acumulado)) + 
    geom_point(aes(col = cor), size = 3) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(title = "Retorno Acumulado", x = "Data", y = "", color = "") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_discrete(name = "Desempenho") +
    theme_economist_white() 

#Drawdown por ativo
grafico_2 <- desempenho %>% 
    mutate(DD = DrawdownPeak(retorno)) %>% 
    ggplot(aes(x = date, y = DD)) +
    geom_line() +
    labs(title = "Drawdown", x = "Data", y = "", color = "") +
    theme_tq() +
    scale_y_continuous(labels = scales::percent) +
    theme_economist_white() 

grafico_3 <- desempenho %>% ggplot() + 
    geom_histogram(aes(retorno), binwidth = 0.05, fill = "3", col = "black") +
    labs(title = "Retorno Diário", x = "", y = "") +
    scale_x_continuous(labels = scales::percent) +
    theme_economist_white() 

grid.arrange(grafico_1, grafico_2, grafico_3)

# Numero de Transações
n_transacoes <- resultado %>% summarize("n" = sum(ifelse(resultado$custos[-1] != 0, 1, 0))/2) %>% .$n

# Retorno Acumulado Final
retorno_acumulado <- desempenho$retorno_acumulado[length(desempenho$retorno_acumulado)]

estatisticas <- rbind(n_transacoes, retorno_acumulado)
colnames(estatisticas) <- "Estatisticas"
rownames(estatisticas) <- c("Número de Transacoes", "Retorno Acumulado Total")

estatisticas
