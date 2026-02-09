library("readxl")
library("tidyverse")
library(dplyr)
library(tidyr)

# ----- COLETA DE DADOS

path = "data.xlsx"
data = read_excel(path)

# Dados da população
pop_data <- read_excel(path, sheet = "População-aTrabalhar")

# Dados Amostragem Aleatória Simples
sample_rank <- read_excel(path, sheet = "AmostraSorteada")
sample_rank <- round(as.numeric(as.character(sample_rank[[1]])))
#sample_rank <- sample_rank[!is.na(sample_rank)]

sample_data <- pop_data[sample_rank, ]

# ----------- AMOSTRAGEM ALEATÓRIA SIMPLES --------

# Variáveis

N <- nrow(pop_data)
n <- nrow(sample_data)
f <- n / N
z_98 <- qnorm(1 - 0.02/2)

# Cálculos

# renda

media_estimada <- mean(sample_data$renda, na.rm = TRUE)

variancia_amostral <- var(sample_data$renda, na.rm = TRUE)
variancia_estimativa <- (1 - f) * (variancia_amostral / n)

cat("=========================================================\n")
cat("      RESULTADOS: ESTIMATIVA DA MÉDIA (AAS)             \n")
cat("=========================================================\n")
cat(sprintf("Tamanho da População (N):      %d\n", N))
cat(sprintf("Tamanho da Amostra (n):        %d\n", n))
cat(sprintf("Fração de Amostragem (f):      %.4f\n", f))
cat("---------------------------------------------------------\n")
cat(sprintf("Média Estimada (renda):        %.4f\n", media_estimada))
cat(sprintf("Variância da Estimativa:       %.6f\n", variancia_estimativa))
cat(sprintf("Erro Padrão da Média:          %.4f\n", sqrt(variancia_estimativa)))
cat("=========================================================\n")

# tamanho das famílias

media_tam_estimada <- mean(sample_data$tam, na.rm = TRUE)

variancia_amostral_tam <- var(sample_data$tam, na.rm = TRUE)
variancia_estimativa_tam <- (1 - f) * (variancia_amostral_tam / n)

cat("=========================================================\n")
cat("    RESULTADOS: ESTIMATIVA DO TAMANHO DA FAMÍLIA (AAS)   \n")
cat("=========================================================\n")
cat(sprintf("Tamanho da População (N):      %d\n", N))
cat(sprintf("Tamanho da Amostra (n):        %d\n", n))
cat("---------------------------------------------------------\n")
cat(sprintf("Média Estimada (tam):          %.4f integrantes\n", media_tam_estimada))
cat(sprintf("Variância da Estimativa:       %.6f\n", variancia_estimativa_tam))
cat(sprintf("Erro Padrão da Média:          %.4f\n", sqrt(variancia_estimativa_tam)))
cat("=========================================================\n")

# total de pessoas

media_tam <- mean(sample_data$tam, na.rm = TRUE)
total_pessoas_estimado <- N * media_tam

variancia_amostral_tam <- var(sample_data$tam, na.rm = TRUE)
variancia_total_estimado <- (N^2) * (1 - f) * (variancia_amostral_tam / n)

cat("=========================================================\n")
cat("      RESULTADOS: ESTIMATIVA DO TOTAL DE PESSOAS        \n")
cat("=========================================================\n")
cat(sprintf("Tamanho da População (N famílias): %d\n", N))
cat(sprintf("Tamanho da Amostra (n famílias):   %d\n", n))
cat("---------------------------------------------------------\n")
cat(sprintf("Total Estimado de Pessoas:         %.2f\n", total_pessoas_estimado))
cat(sprintf("Variância da Estimativa do Total:  %.2f\n", variancia_total_estimado))
cat(sprintf("Erro Padrão do Total:              %.2f\n", sqrt(variancia_total_estimado)))
cat("=========================================================\n")

# proporção de fumantes - unidade amostral é o indivíduo

y <- sample_data$fumantes
x <- sample_data$tam
p_chapeu <- sum(y) / sum(x)

media_x <- mean(x)
variancia_y <- var(y)
variancia_x <- var(x)
cov_xy <- cov(x, y)

variancia_p <- ((1 - f) / (n * media_x^2)) * (variancia_y + (p_chapeu^2 * variancia_x) - (2 * p_chapeu * cov_xy))
erro_padrao_p <- sqrt(variancia_p)

limite_inf <- p_chapeu - z_98 * erro_padrao_p
limite_sup <- p_chapeu + z_98 * erro_padrao_p

cat("=========================================================\n")
cat("    ESTIMATIVA DA PROPORÇÃO DE FUMANTES (RAZÃO)         \n")
cat("=========================================================\n")
cat(sprintf("Proporção Estimada (p):        %.4f (%.2f%%)\n", p_chapeu, p_chapeu * 100))
cat(sprintf("Variância da Estimativa:       %.8f\n", variancia_p))
cat(sprintf("Erro Padrão (SE):              %.4f\n", erro_padrao_p))
cat("---------------------------------------------------------\n")
cat(sprintf("Intervalo de Confiança (98%%):"))
cat(sprintf("[%.4f ; %.4f]\n", limite_inf, limite_sup))
cat(sprintf("[%.2f%% ; %.2f%%]\n", limite_inf * 100, limite_sup * 100))
cat("=========================================================\n")

# ------- AMOSTRAGEM POR CONGLOMERADO --------

# Sorteio das localidades

conglomerados_unicos <- unique(pop_data$local)

set.seed(123)
sorteio <- sample(1:length(conglomerados_unicos), size = 3)
conglomerados_sorteados <- conglomerados_unicos[sorteio]

amostra_ac <- pop_data[pop_data$local %in% conglomerados_sorteados, ]

cat("Localidades Sorteadas:", paste(conglomerados_sorteados, collapse = ", "), "\n")
cat("Método Utilizado: Amostragem Aleatória Simples de Conglomerados via função sample().\n")

# Variáveis

M <- length(conglomerados_unicos)
m <- 3
f_ac <- m / M

# Cálculos

# --- Renda

# cálculos Agregados por Conglomerado

resumo_ac <- amostra_ac %>%
  group_by(local) %>%
  summarise(
    yi = sum(renda),
    xi = n()
  )

# variáveis

y_total_amostra <- sum(resumo_ac$yi)
x_total_amostra <- sum(resumo_ac$xi)
media_ac_renda <- y_total_amostra / x_total_amostra

# variancia da estimativa (média) - entre os conglomerados

media_x_barra <- x_total_amostra / m
s2_c <- sum((resumo_ac$yi - media_ac_renda * resumo_ac$xi)^2) / (m - 1)

variancia_media_ac <- (1 - f_ac) * (s2_c / (m * media_x_barra^2))

#resultados

cat("=========================================================\n")
cat("      RESULTADOS: AMOSTRAGEM POR CONGLOMERADO (AC-1)    \n")
cat("=========================================================\n")
cat(sprintf("Localidades Sorteadas: %s\n", paste(conglomerados_sorteados, collapse = ", ")))
cat("---------------------------------------------------------\n")
cat(sprintf("Média Estimada da Renda:       %.4f\n", media_ac_renda))
cat(sprintf("Variância da Estimativa:       %.6f\n", variancia_media_ac))
cat(sprintf("Erro Padrão da Média:          %.4f\n", sqrt(variancia_media_ac)))
cat(sprintf("Margem de Erro (95%%):          %.4f\n", 1.96 * sqrt(variancia_media_ac)))
cat("=========================================================\n")



# --- Tamanho médio das famílias

# calculo agregado por conglomerado
resumo_ac_tam <- amostra_ac %>%
  group_by(local) %>%
  summarise(
    yi = sum(tam, na.rm = TRUE),
    xi = n()
  )

# variáveis

y_total_amostra_tam <- sum(resumo_ac_tam$yi)
x_total_amostra_tam <- sum(resumo_ac_tam$xi)
media_ac_tam <- y_total_amostra_tam / x_total_amostra_tam

# variância da estimativa

media_x_barra <- x_total_amostra_tam / m
f_ac <- m / M

# variância entre conglomerados

s2_c_tam <- sum((resumo_ac_tam$yi - media_ac_tam * resumo_ac_tam$xi)^2) / (m - 1)
variancia_media_ac_tam <- (1 - f_ac) * (s2_c_tam / (m * media_x_barra^2))

#resultados

cat("=========================================================\n")
cat("    RESULTADOS: TAMANHO MÉDIO DAS FAMÍLIAS               \n")
cat("=========================================================\n")
cat(sprintf("Média Estimada (tam):          %.4f integrantes\n", media_ac_tam))
cat(sprintf("Variância da Estimativa:       %.6f\n", variancia_media_ac_tam))
cat(sprintf("Erro Padrão da Média:          %.4f\n", sqrt(variancia_media_ac_tam)))
cat("---------------------------------------------------------\n")
cat(sprintf("Intervalo de Confiança (95%%):  [%.4f ; %.4f]\n", 
            media_ac_tam - 1.96 * sqrt(variancia_media_ac_tam),
            media_ac_tam + 1.96 * sqrt(variancia_media_ac_tam)))
cat("=========================================================\n")


# --- Total de pessoas

# calculo agregado por conglomerado
resumo_ac_total <- amostra_ac %>%
  group_by(local) %>%
  summarise(
    yi = sum(tam, na.rm = TRUE)
  )

# variáveis
M <- 7
m <- 3
f_ac <- m / M

total_pessoas_estimado_ac <- (M / m) * sum(resumo_ac_total$yi)

# variância da Estimativa do Total
s2_y_totais <- var(resumo_ac_total$yi)
variancia_total_ac <- (M^2) * (1 - f_ac) * (s2_y_totais / m)

# resultados

cat("=========================================================\n")
cat("      RESULTADOS: TOTAL DE PESSOAS NA REGIÃO (AC-1)      \n")
cat("=========================================================\n")
cat(sprintf("Total Estimado de Pessoas:     %.2f\n", total_pessoas_estimado_ac))
cat(sprintf("Variância da Estimativa:       %.2f\n", variancia_total_ac))
cat(sprintf("Erro Padrão do Total:          %.2f\n", sqrt(variancia_total_ac)))
cat("---------------------------------------------------------\n")
cat(sprintf("Intervalo de Confiança (95%%):  [%.2f ; %.2f]\n", 
            total_pessoas_estimado_ac - 1.96 * sqrt(variancia_total_ac),
            total_pessoas_estimado_ac + 1.96 * sqrt(variancia_total_ac)))
cat("=========================================================\n")


# --- Proporção de fumantes

# calculo agregado por conglomerado
resumo_ac_prop <- amostra_ac %>%
  group_by(local) %>%
  summarise(
    yi = sum(fumantes, na.rm = TRUE),
    xi = sum(tam, na.rm = TRUE)
  )

# Estimativa da Proporção (Razão)
y_total_amostra_fum <- sum(resumo_ac_prop$yi)
x_total_amostra_pes <- sum(resumo_ac_prop$xi)
prop_ac_fumantes <- y_total_amostra_fum / x_total_amostra_pes

# 3. Variância da Estimativa da Proporção
m <- 3
M <- 7
f_ac <- m / M
media_x_barra_pes <- x_total_amostra_pes / m

# Variância residual entre os totais observados e os esperados pela razão
s2_c_prop <- sum((resumo_ac_prop$yi - prop_ac_fumantes * resumo_ac_prop$xi)^2) / (m - 1)
variancia_prop_ac <- (1 - f_ac) * (s2_c_prop / (m * media_x_barra_pes^2))

# Intervalo de Confiança (98%)
z_98 <- qnorm(1 - 0.02/2) # Valor crítico ~2.326
erro_padrao_prop <- sqrt(variancia_prop_ac)
limite_inf <- prop_ac_fumantes - z_98 * erro_padrao_prop
limite_sup <- prop_ac_fumantes + z_98 * erro_padrao_prop

# Resultados
cat("=========================================================\n")
cat("    RESULTADOS: PROPORÇÃO DE FUMANTES                    \n")
cat("=========================================================\n")
cat(sprintf("Proporção Estimada:            %.4f ou %.2f%%\n", prop_ac_fumantes, prop_ac_fumantes * 100))
cat(sprintf("Variância da Estimativa:       %.8f\n", variancia_prop_ac))
cat(sprintf("Erro Padrão da Proporção:      %.4f\n", erro_padrao_prop))
cat("---------------------------------------------------------\n")
cat("INTERVALO DE CONFIANÇA (98%):\n")
cat(sprintf("Formato Decimal:               [%.4f ; %.4f]\n", limite_inf, limite_sup))
cat(sprintf("Formato Porcentagem:           [%.2f%% ; %.2f%%]\n", limite_inf * 100, limite_sup * 100))
cat("=========================================================\n")





# AMOSTRAGEM ESTRATIFICADA

avaliar_potencial <- function(df, var_alvo, var_estrato) {
  df %>%
    group_by(.data[[var_estrato]]) %>%
    summarise(
      n = n(),
      media = mean(.data[[var_alvo]], na.rm = TRUE),
      sd_interna = sd(.data[[var_alvo]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    summarise(
      Variavel = var_estrato,
      # Desvio padrão das médias -> Quanto maior o std, melhor a separação entre grupos
      Separacao_Medias = sd(media, na.rm = TRUE),
      # Média ponderada dos desvios padrões (Quanto menor, mais homogêneo o estrato)
      Homogeneidade_Interna = weighted.mean(sd_interna, n, na.rm = TRUE)
    )
}

# Lista de variáveis para teste
candidatas <- c("local", "sexo", "ensino", "tam", "fumantes")

# Execução do Ranking
ranking <- lapply(candidatas, function(v) avaliar_potencial(pop_data, "renda", v)) %>%
  bind_rows() %>%
  # Criamos um Score: Prioriza alta separação de médias e baixa variância interna
  mutate(Score_Recomendacao = Separacao_Medias / Homogeneidade_Interna) %>%
  arrange(desc(Score_Recomendacao))

# Resultados

cat("=========================================================\n")
cat("      RANKING DE VARIÁVEIS PARA ESTRATIFICAÇÃO          \n")
cat("          (Objetivo: Estimar a Renda)                   \n")
cat("=========================================================\n")
print(as.data.frame(ranking), digits = 4)
cat("---------------------------------------------------------\n")

melhor_var <- ranking$Variavel[1]
cat(sprintf("RECOMENDAÇÃO: A variável '%s' é a mais adequada.\n", melhor_var))
cat(sprintf("JUSTIFICATIVA: Ela apresenta o melhor equilíbrio entre\n"))
cat(sprintf("diferenciação de rendas e grupos internos homogêneos.\n"))
cat("=========================================================\n")
