# Projeção 

library(tidyverse)
library(lubridate)

raw.csv <- "https://raw.githubusercontent.com/Nexo-Dados/pesquisas-presidenciais-2022/main/pesquisas_1t.csv"

dados <- read_csv(file = raw.csv)

library(INLA)

data.ini <- min(dados$Data, na.rm = T)
data.last <- max(dados$Data, na.rm = T) 
data.fim <- ymd("2022-10-02")

# Adicionando linhas ao banco até a data da eleicao
# com NA no valor das proporcoes de cada candidato
# pois isso será usado na revisao
dados.div <- dados %>% 
  # Usando apenas as pesquisas a partir de 1/8/2022
  # filter(!is.na(`Data divulgação`)) %>% 
  filter(`Data divulgação`>= "2022-08-01") %>% 
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


dados.div %>% 
  select(`Data divulgação`, Lula:BNI) %>% 
  gather(key = "Candidato", 
         value = "Prop", -`Data divulgação`) %>% 
  mutate(
    Candidato = factor(Candidato, 
                       levels = c("Lula", "Bolsonaro",
                                  "Ciro", "Tebet", 
                                  "Outros", "BNI"),
                       ordered = T)
  ) %>% 
  ggplot(aes(x = `Data divulgação`,
             y = Prop,
             color = Candidato,
             fill = Candidato)) +
  geom_point() +
  geom_smooth() + 
  theme_bw()



# Lula
dados.inla <- dados %>% 
  select(Time, `Data divulgação`, Lula)


dados.m <- dados %>% 
  select(`Data divulgação`, Lula:BNI) %>% 
  gather(key = "Candidato", 
         value = "Prop", -`Data divulgação`) %>% 
  mutate(
    Prop = Prop / 100,
    Propt = arm::invlogit(Prop)
  ) 

dados.m %>% ggplot(aes(x = `Data divulgação`,
                       y = Prop * 100,
                       color = Candidato,
                       fill = Candidato)) +
  geom_point() +
  geom_smooth() + 
  theme_bw()


dados.m2 <- dados.m %>% filter(Candidato != "BNI") 

