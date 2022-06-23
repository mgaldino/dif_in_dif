library(arm)
library(fixest)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(bacondecomp)

## Um tratamento apenas, efeito homogêneo no tempo
# experimento:
# duas rodas
# na primeira rodada, todos no controle.
# na segunda rodada, pode ser controle ou tratamento

n <- 1000

id_aux <- rep(1:n, each=4)

df <- expand.grid(
  periodos = c(0,1),
  grupos = c("A", "B"),
  y0 = rep(2, n),
  stringsAsFactors = F
) %>%
  as_tibble() %>%
  mutate(id_unit = id_aux) %>%
  group_by(periodos, id_unit) %>%
  slice_sample(n=1) %>%
  ungroup() %>%
  arrange(id_unit) %>%
  mutate(delta_periodo = 12,
         delta_grupo = 15,
         erro = rnorm(n(), 3),
         grupo_numeric = as.numeric(as.factor(grupos)) - 1,
         alpha_time = delta_periodo*periodos,
         alpha_group = delta_grupo*grupo_numeric,
         delta_treatment = 10,
         y = y0 + alpha_time + alpha_group + periodos*grupo_numeric*delta_treatment +  erro)

#diff in diff estimator
      
df_periodo0 <- df %>%
  filter(periodos == 0) %>%
  group_by(periodos, grupos) %>%
  summarise(media = mean(y)) %>%
  mutate(dif = media - lag(media)) 

df_aux0 <- df_periodo0 %>%
  filter(!is.na(dif)) %>%
  mutate(tipo_dif = "dif grupos")



df_periodo1 <- df %>%
  filter(periodos == 1) %>%
  group_by(periodos, grupos) %>%
  summarise(media = mean(y)) %>%
  mutate(dif = media - lag(media)) 

df_aux1 <- df_periodo1 %>%
  filter(!is.na(dif)) %>%
  mutate(tipo_dif = "dif time")

df_did <- bind_rows(df_aux0, df_aux1) %>%
  ungroup() %>%
  mutate(did_estimator = dif - lag(dif))

df_did %>%
  filter(!is.na(did_estimator)) %>%
  select(did_estimator)

## funciona!

#
#Agora com regressão

df1 <- df %>%
  mutate(x = periodos*grupo_numeric)


reg <- lm(y ~ as.factor(periodos) + as.factor(grupos) + x, data=df1)
summary(reg)


reg2 <- feols(y ~ x | grupos + periodos, data=df1)
summary(reg2)

# agora, efeito de tratamento heterogêneo, três tratamentos, n períodos = 30


n <- 900
t <- 30
grupos <- rep(c("A", "B", "C"), n/3)
k <- t/3
l <- t - k/2

df <- expand.grid(
  # periodos = c(0,1,2),
  id = 1:n,
  time = 1:t,
  # grupos = c("A", "B", "C"),
  # intercept = rep(2,n),
  stringsAsFactors = F
) %>%
  as_tibble() %>%
  mutate(intercept = 2,
         grupos = rep(grupos, t),
         treatment_0 = as.numeric(grupos == "A"),
         treatment_1 = as.numeric(grupos == "B"),
         treatment_2 = as.numeric(grupos == "C"),
         timing = ifelse(grupos == "B" & time > k, 1,
                                ifelse(grupos == "C" & time > l, 1, 0)),
         erro = rnorm(n(), 3),
         grupo_b = as.numeric(grupos == "B"),
         grupo_c = as.numeric(grupos == "C"),
         delta_b = 15,
         delta_c = 5,
         delta_time = .5, # efeito do tempo
         alpha_group_B = delta_b*grupo_b,
         alpha_group_C = delta_c*grupo_c,
         delta_treatment1 = 10,
         delta_treatment2 = 15,
         efeito_tempo = delta_time*time,
         y = intercept + delta_time*time + alpha_group_B*treatment_1 + alpha_group_C*treatment_2 + 
           delta_treatment1*timing*treatment_1 +  delta_treatment2*timing*treatment_2 + erro)


df %>%
  ggplot(aes(y=y, x=time, colour=grupos)) +geom_point() #+geom_smooth(method = "lm")

reg <- lm(y ~ as.factor(time) + as.factor(grupos) + timing, data=df)
summary(reg)

reg2 <- feols(y ~ timing | grupos + time , data=df)
summary(reg2)

df_bacon <- bacon(y ~ timing,
                  data = df,
                  id_var = "id",
                  time_var = "time")

#>                       type  weight  avg_est
#> 1 Earlier vs Later Treated 0.05976 -0.00554
#> 2 Later vs Earlier Treated 0.03190  0.07032
#> 3     Treated vs Untreated 0.90834  0.08796
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))
#> [1] "Weighted sum of decomposition = 0.0818"
#> 
#> 
## tratamento variando no tempo



n <- 900
t <- 30
grupos <- rep(c("A", "B", "C"), n/3)
k <- t/3
l <- t - k

df <- expand.grid(
  # periodos = c(0,1,2),
  id = 1:n,
  time = 1:t,
  # grupos = c("A", "B", "C"),
  # intercept = rep(2,n),
  stringsAsFactors = F
) %>%
  as_tibble() %>%
  mutate(intercept = 2,
         grupos = rep(grupos, t),
         treatment_0 = as.numeric(grupos == "A"),
         treatment_1 = as.numeric(grupos == "B"),
         treatment_2 = as.numeric(grupos == "C"),
         timing = ifelse(grupos == "B" & time > k, 1,
                         ifelse(grupos == "C" & time > l, 1, 0)),
         erro = rnorm(n(), 3),
         grupo_b = as.numeric(grupos == "B"),
         grupo_c = as.numeric(grupos == "C"),
         delta_b = 15,
         delta_c = 5,
         delta_time = .5, # efeito do tempo
         alpha_group_B = delta_b*grupo_b,
         alpha_group_C = delta_c*grupo_c,
         delta_treatment = 2*time,
         efeito_tempo = delta_time*time,
         y = intercept + delta_time*time + alpha_group_B*treatment_1 + alpha_group_C*treatment_2 + 
           delta_treatment*timing*treatment_1 +  delta_treatment*timing*treatment_2 + erro)

df %>%
  ggplot(aes(y=y, x=time, colour=grupos)) +geom_point() #+geom_smooth(method = "lm")

reg <- lm(y ~ as.factor(time) + as.factor(grupos) + timing, data=df)
summary(reg)

reg2 <- feols(y ~ timing | grupos + time , data=df)
summary(reg2)

df_bacon <- bacon(y ~ timing,
                  data = df,
                  id_var = "id",
                  time_var = "time")

#>                       type  weight  avg_est
#> 1 Earlier vs Later Treated 0.05976 -0.00554
#> 2 Later vs Earlier Treated 0.03190  0.07032
#> 3     Treated vs Untreated 0.90834  0.08796
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

