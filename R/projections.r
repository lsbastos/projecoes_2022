# Projeção 

library(tidyverse)
library(lubridate)

raw.csv <- "https://raw.githubusercontent.com/Nexo-Dados/pesquisas-presidenciais-2022/main/pesquisas_1t.csv"

dados <- read_csv(file = raw.csv)

library(INLA)

data.ini <- min(dados$Data, na.rm = T)
data.last <- max(dados$Data, na.rm = T)+1 
data.fim <- ymd("2022-10-02")

# Adicionando linhas ao banco até a data da eleicao
# com NA no valor das proporcoes de cada candidato
# pois isso será usado na revisao
dados.div <- dados %>% 
  # Usando apenas as pesquisas a partir de 1/8/2022
  # # filter(!is.na(`Data divulgação`)) %>% 
  # filter(`Data divulgação`>= "2022-08-01") %>% 
  bind_rows(
    tibble(Data = seq(from = data.last, to = data.fim, by = "day"))
  ) %>% 
  # Passo para o inla
  left_join(
    y = tibble(
      Data = seq(from = data.ini, to = data.fim, by = "day")
    ) %>% 
      rowid_to_column(var = "Time"), 
    by = c("Data")
  )


gg.0 <- dados.div %>% 
  dplyr::select(Data, Lula:BNI) %>% 
  gather(key = "Candidato", 
         value = "Prop", -Data) %>% 
  mutate(
    Candidato = factor(Candidato, 
                       levels = c("Lula", "Bolsonaro",
                                  "Ciro", "Tebet", 
                                  "Outros", "BNI"),
                       ordered = T)
  ) %>% 
  ggplot(aes(x = Data,
             y = Prop,
             color = Candidato,
             fill = Candidato)) +
  geom_point() +
  geom_smooth() + 
  theme_bw()

gg.0

library(arm)

# Lula
dados.inla <- dados.div %>% 
  # select(Time, Data, Lula:BNI) %>% 
  mutate_at(.vars = c("Lula", "Bolsonaro",
                      "Ciro", "Tebet", 
                      "Outros", "BNI"), .funs = function(x) logit(x/100))

pred.id <- which(is.na(dados.div$Lula))



M = 1000
formula <- Y ~ 1 + f(Time, model = "rw2")

# Lula

m.Lula <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Lula
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Lula <- m.Lula$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "Lula" )

teste <- inla.posterior.sample(m.Lula, n = M)

Election.Lula <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "Lula" )

# Bolsonaro

m.Bolsonaro <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Bolsonaro
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Bolsonaro <- m.Bolsonaro$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "Bolsonaro" )

teste <- inla.posterior.sample(m.Bolsonaro, n = M)

Election.Bolsonaro <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "Bolsonaro" )


# Ciro

m.Ciro <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Ciro
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Ciro <- m.Ciro$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "Ciro" )

teste <- inla.posterior.sample(m.Ciro, n = M)

Election.Ciro <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "Ciro" )


# Tebet

m.Tebet <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Tebet
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Tebet <- m.Tebet$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "Tebet" )

teste <- inla.posterior.sample(m.Tebet, n = M)

Election.Tebet <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "Tebet" )


# Outros

m.Outros <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Outros
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Outros <- m.Outros$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "Outros" )

teste <- inla.posterior.sample(m.Outros, n = M)

Election.Outros <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "Outros" )


# BNI

m.BNI <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = BNI
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.BNI <- m.BNI$summary.linear.predictor[pred.id, c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data[pred.id], 
    Candidato = "BNI" )

teste <- inla.posterior.sample(m.BNI, n = M)

Election.BNI <- map(.x = teste, .f = function(x) x$latent[pred.id,][10] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "BNI" )


########################

pred.cand <- bind_rows(pred.Lula, pred.Bolsonaro, pred.Ciro, pred.Tebet, pred.Outros, pred.BNI)

gg.0 +
  geom_line(data = pred.cand %>% 
              mutate(
                Data = ymd(Data),
                `0.5quant` = `0.5quant` * 100
              ), 
            mapping = aes(x = Data, y = `0.5quant`, color = Candidato)) +
  geom_ribbon(data = pred.cand %>% 
              mutate(
                Data = ymd(Data),
                `0.5quant` = `0.5quant` * 100,
                `0.025quant` = `0.025quant` * 100,
                `0.975quant` = `0.975quant` * 100
              ), 
            mapping = aes(x = Data, y = `0.5quant`, 
                          ymin = `0.025quant`,
                          ymax = `0.975quant`,
                          fill = Candidato), 
            color = NA, alpha = .25)


Election.cand <- bind_rows(Election.Lula, Election.Bolsonaro, Election.Ciro, Election.Tebet, Election.Outros) %>% 
  add_column(Seq = rep(1:M,5))


Election.cand %>% 
  group_by(Seq) %>% 
  mutate(
    `Predictor:136` = `Predictor:136` / sum(`Predictor:136`)
  ) %>% ungroup() %>%
  group_by(Candidato) %>% 
  summarise(
    Prop = mean(`Predictor:136`),
    LI = quantile(`Predictor:136`, probs = 0.025),
    LS = quantile(`Predictor:136`, probs = 0.975),
    Prob_win = mean(`Predictor:136` > 0.5)
  )
