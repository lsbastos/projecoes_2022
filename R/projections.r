# Projeção 

library(tidyverse)
library(lubridate)
library(arm)
# Para instalar o INLA caso nao tenha
# Ver https://www.r-inla.org/download-install
library(INLA)

raw_nexo <- "https://raw.githubusercontent.com/Nexo-Dados/pesquisas-presidenciais-2022/main/pesquisas_1t.csv"

dados <- read_csv(file = raw_nexo)


data.ini <- min(dados$Data, na.rm = T)
data.last <- max(dados$Data, na.rm = T) 
data.fim <- ymd("2022-10-02")

save_plots <- FALSE

# Adicionando linhas ao banco até a data da eleicao
# com NA no valor das proporcoes de cada candidato
# pois isso será usado na revisao
dados.div <- dados %>% 
  # Usando apenas as pesquisas a partir de 1/8/2022
  # # filter(!is.na(`Data divulgação`)) %>% 
  
  # Usando apenas as pesquisas a partir de 1/6/2022
  # filter(`Data divulgação`>= "2022-06-01") %>%
  filter(Instituto %in% c("Ipec", "Datafolha",
                        "Real Time Big Data", 
                        "Sensus", "FSB", 
                        "Quaest", 
                        "Ideia Big Data" )) %>%
  bind_rows(
    tibble(Data = seq(from = data.last+1, to = data.fim, by = "day"))
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
  theme_bw()

# gg.0



dados.inla <- dados.div %>% 
  # select(Time, Data, Lula:BNI) %>% 
  mutate_at(.vars = c("Lula", "Bolsonaro",
                      "Ciro", "Tebet", 
                      "Outros", "BNI"), .funs = function(x) logit(x/100))


pred.id <- which(is.na(dados.div$Lula))
pred.id <- c(58,59)
MM <- length(pred.id)

M = 1000

formula <- Y ~ 1 + f(Time, model = "rw2") + 
  f(Instituto, model = "iid")


# Lula

m.Lula <- inla(formula, family = "gaussian", 
               data = dados.inla %>% 
                 mutate(
                   Y = Lula
                 ), 
               control.predictor = list( compute = T),
               control.compute = list(config = TRUE))


pred.Lula <- m.Lula$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "Lula" )

teste <- inla.posterior.sample(m.Lula, n = M)

Election.Lula <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
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


pred.Bolsonaro <- m.Bolsonaro$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "Bolsonaro" )

teste <- inla.posterior.sample(m.Bolsonaro, n = M)

Election.Bolsonaro <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
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


pred.Ciro <- m.Ciro$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "Ciro" )

teste <- inla.posterior.sample(m.Ciro, n = M)

Election.Ciro <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
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


pred.Tebet <- m.Tebet$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "Tebet" )

teste <- inla.posterior.sample(m.Tebet, n = M)

Election.Tebet <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
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


pred.Outros <- m.Outros$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "Outros" )

teste <- inla.posterior.sample(m.Outros, n = M)

Election.Outros <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
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


pred.BNI <- m.BNI$summary.linear.predictor[ , c(4,3,5)] %>% 
  invlogit() %>% 
  bind_cols(
    Data = dados.div$Data, 
    Candidato = "BNI" )

teste <- inla.posterior.sample(m.BNI, n = M)

Election.BNI <- map(.x = teste, .f = function(x) x$latent[pred.id,][MM] %>% invlogit()) %>% 
  bind_rows() %>%  
  bind_cols(
    Candidato = "BNI" )


########################

pred.cand <- bind_rows(pred.Lula, pred.Bolsonaro, pred.Ciro, 
                       pred.Tebet, pred.Outros, pred.BNI)


pred.cand.1 <-pred.cand %>% 
  mutate(
    Data = ymd(Data),
    Mediana = `0.5quant` * 100,
    LI = `0.025quant` * 100,
    LS = `0.975quant` * 100
  ) %>% 
  # Removendo duplicatas nas datas
  group_by(Data, Candidato) %>% 
  summarise(
    Mediana = Mediana[1],
    LI = LI[1],
    LS = LS[1]
  ) %>% ungroup()



gg.1 <- gg.0 +
  geom_line(data = pred.cand.1, 
            mapping = aes(x = Data, y = Mediana, color = Candidato)) +
  geom_ribbon(data = pred.cand.1, 
            mapping = aes(x = Data, y = Mediana, 
                          ymin = LI,
                          ymax = LS,
                          fill = Candidato), 
            color = NA, alpha = .25) +
  geom_hline( yintercept = 50, linetype = "dashed") +
  scale_y_continuous( breaks = (0:5) * 10 ) +
  scale_x_date( date_breaks = "2 months",  ) +
  scale_fill_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  scale_color_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  theme_bw(base_size = 16) +
  labs(
    x = "Data (yyyy-mm-dd)",
    y = "Proporção de votos (%)",
    title = "" , # (2/Out/2022)",
    caption = "Modelo proposto por @leosbastos"
  ) 

gg.1
if(save_plots)
  ggsave(plot = gg.1, filename = "figs/projecoes.png", device = "png")




Election.cand <- bind_rows(Election.Lula, Election.Bolsonaro, Election.Ciro, 
                           Election.Tebet, Election.Outros) %>% #, Election.BNI) %>% 
  add_column(Seq = rep(1:M,5)) %>% 
  # add_column(Seq = rep(1:M,6)) %>% 
  mutate(
    Candidato = factor(Candidato, 
                       levels = c("Lula", "Bolsonaro",
                                  "Ciro", "Tebet", 
                                  "Outros"), #, "BNI"),
                       ordered = T)
  ) 

names(Election.cand)[1] <- "Predictor"

Election.cand.validos <- Election.cand %>% 
  group_by(Seq) %>% 
  mutate(
    Predictor = Predictor / sum(Predictor)
  ) %>% 
  # ungroup() %>%
  # filter(Candidato != "BNI") %>% 
  # group_by(Seq) %>% 
  # mutate(
  #   Predictor2 = Predictor / sum(Predictor)
  # ) %>% 
  ungroup() 

gg.dens <- Election.cand.validos %>%
  ggplot(aes(x = Predictor*100, fill = Candidato, color=Candidato) ) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 50, linetype = "dashed") + 
  scale_fill_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  scale_color_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  # scale_fill_manual(values = c("red", "#7B5804", "blue", "green", "lightgrey", "darkgrey")) +
  # scale_color_manual(values = c("red", "#7B5804", "blue", "green", "lightgrey", "darkgrey")) +
  theme_bw(base_size = 16) +
  labs(
    y = "Densidade",
    x = "Proporção de votos válidos (%)",
    title = "Densidade da proporção de votos válidos no dia da eleição" , 
    subtitle = paste("Última pesquisa:", data.last),# (2/Out/2022)",
    caption = "Modelo proposto por @leosbastos"
  ) 


if(save_plots)
  ggsave(plot = gg.dens, filename = "figs/density.png", device = "png")

gg.violin <- Election.cand.validos %>%
  ggplot(aes(y = Predictor*100, x = Candidato, fill=Candidato) ) +
  geom_violin(alpha = 0.8, show.legend = F) + 
  geom_hline(yintercept = 50, linetype = "dashed") + 
  theme_bw(base_size = 16) +
  scale_fill_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  scale_color_manual(values = c("red", "gold", "blue", "green", "lightgrey", "darkgrey")) +
  labs(
    x = "Candidatos",
    y = "Proporção de votos válidos (%)",
    title = "Projeção de votos válidos para o 1o turno no dia da eleição" , # (2/Out/2022)",
    caption = "Modelo proposto por @leosbastos"
  ) 

gg.violin

if(save_plots)
  ggsave(plot = gg.violin, filename = "figs/violin.png", device = "png")

  
Election.cand.validos %>%
  group_by(Candidato) %>% 
  summarise(
    Prop = median(Predictor),
    LI = quantile(Predictor, probs = 0.025),
    LS = quantile(Predictor, probs = 0.975),
    Prob_vitoria_1o_turno = mean(Predictor > 0.5)
  )
