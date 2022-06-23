library(arm)
library(fixest)
library(tidyverse)
library(ggplot2)
library(tidyr)

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

### 
## três rodadas
## a cada rodada, indivíduo pode estar em um dos trÊs grupos
## A é controle, B é tratamento 1, C é tratamento 2
## efeito homogêneo no tempo de cada tratamento
n <- 1000

df <- expand.grid(
  # periodos = c(0,1,2),
  id = 1:n,
  grupos = c("A", "B", "C"),
  intercept = rep(2,n),
  stringsAsFactors = F
) %>%
  group_by(id) %>%
  slice_sample(n=1) %>%
  as_tibble() %>%
  mutate(treatment_0 = as.numeric(grupos == "A"),
         treatment_1 = as.numeric(grupos == "B"),
         treatment_2 = as.numeric(grupos %in% c("B", "C"))) %>%
  ungroup() %>%
  # group_by(id_unit, periodos) %>%
  # mutate(grupos = max(grupos))
  mutate(erro0 = rnorm(n(), 3),
         erro1 = rnorm(n(), 3),
         erro2 = rnorm(n(), 3),
         grupo_b = as.numeric(grupos == "B"),
         grupo_c = as.numeric(grupos == "C"),
         delta_b = 8,
         delta_c = 10,
         delta_1 = 12,
         delta_2 = 15,
         alpha_group_B = delta_b*grupo_b,
         alpha_group_C = delta_c*grupo_c,
         delta_treatment = 20,
         y0 = intercept + alpha_group_B + alpha_group_C + erro0,
         y1 = intercept + delta_1 + alpha_group_B + alpha_group_C + delta_treatment*treatment_1 + erro1,
         y2 = intercept + delta_1 + delta_2 + alpha_group_B + alpha_group_C + 
           delta_treatment*treatment_1 +  delta_treatment*treatment_2 + erro2)



df1 <- df %>%
  select(id, grupos, y0, y1, y2, treatment_0, treatment_1, treatment_2) %>%
  pivot_longer(cols = c(treatment_0, treatment_1, treatment_2),
               names_to = "periodo",
               values_to = "tratamento",
               names_prefix = "treatment_") %>%
  pivot_longer(cols = c(y0, y1, y2),
               names_to = "periodo1",
               values_to = "y",
               names_prefix = "y") %>%
  group_by(id, grupos, periodo1 ) %>%
  summarise(y = max(y)) %>%
  mutate(periodo1 = as.numeric(periodo1),
         periodo2 = as.numeric(periodo1 > 0), 
         periodo3 = periodo1 - 1,
         periodo3 = ifelse(periodo3 < 0, 0, periodo3),
         x = case_when(grupos == "B" ~ periodo2,
                       grupos == "C" ~ periodo2*periodo3,
                       TRUE ~ 0)) %>%
  select(-c(periodo2, periodo3)) %>%
  rename(periodos =periodo1)


reg <- lm(y ~ as.factor(periodos) + as.factor(grupos) + x, data=df1)
summary(reg)

reg2 <- feols(y ~ x | grupos + periodos, data=df1)
summary(reg2)


# agora, efeito de tratamento heterogêneo


n <- 1000

df <- expand.grid(
  # periodos = c(0,1,2),
  id = 1:n,
  grupos = c("A", "B", "C"),
  intercept = rep(2,n),
  stringsAsFactors = F
) %>%
  group_by(id) %>%
  slice_sample(n=1) %>%
  as_tibble() %>%
  mutate(treatment_0 = as.numeric(grupos == "A"),
         treatment_1 = as.numeric(grupos == "B"),
         treatment_2 = as.numeric(grupos %in% c("B", "C"))) %>%
  ungroup() %>%
  # group_by(id_unit, periodos) %>%
  # mutate(grupos = max(grupos))
  mutate(erro0 = rnorm(n(), 3),
         erro1 = rnorm(n(), 3),
         erro2 = rnorm(n(), 3),
         grupo_b = as.numeric(grupos == "B"),
         grupo_c = as.numeric(grupos == "C"),
         delta_b = 8,
         delta_c = 10,
         delta_1 = 12,
         delta_2 = 15,
         alpha_group_B = delta_b*grupo_b,
         alpha_group_C = delta_c*grupo_c,
         delta_treatment1 = 30,
         delta_treatment2 = 10,
         y0 = intercept + alpha_group_B + alpha_group_C + erro0,
         y1 = intercept + delta_1 + alpha_group_B + alpha_group_C + delta_treatment1*treatment_1 + erro1,
         y2 = intercept + delta_1 + delta_2 + alpha_group_B + alpha_group_C + 
           delta_treatment1*treatment_1 +  delta_treatment2*treatment_2 + erro2)



df1 <- df %>%
  select(id, grupos, y0, y1, y2, treatment_0, treatment_1, treatment_2) %>%
  pivot_longer(cols = c(treatment_0, treatment_1, treatment_2),
               names_to = "periodo",
               values_to = "tratamento",
               names_prefix = "treatment_") %>%
  pivot_longer(cols = c(y0, y1, y2),
               names_to = "periodo1",
               values_to = "y",
               names_prefix = "y") %>%
  group_by(id, grupos, periodo1 ) %>%
  summarise(y = max(y)) %>%
  mutate(periodo1 = as.numeric(periodo1),
         periodo2 = as.numeric(periodo1 > 0), 
         periodo3 = periodo1 - 1,
         periodo3 = ifelse(periodo3 < 0, 0, periodo3),
         x = case_when(grupos == "B" ~ periodo2,
                       grupos == "C" ~ periodo2*periodo3,
                       TRUE ~ 0)) %>%
  select(-c(periodo2, periodo3)) %>%
  rename(periodos =periodo1)

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
