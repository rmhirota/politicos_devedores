library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

politicos_total <- readRDS("data/politicos_total.rds")

# 2018 --------------------------------------------------------------------

# pol_dev_2018 <- data.table::fread("cruzamento_2018.csv")
#
#
# # Número de políticos
# n_distinct(pol_dev_2018$cpf_candidato)
#
# # Valor por pessoa
# pol_dev_2018 %>% group_by(cpf_candidato, nome_candidato, sigla_partido_novo, descricao_cargo) %>%
#   summarise(n = sum(valor_consolidado)) %>%
#   arrange(desc(n)) %>%
#   View()
#
# # Total da dívida de políticos
# pol_dev_2018 %>% summarise(sum(valor_consolidado))
#
# # Por partido
# pol_dev_2018 %>% group_by(sigla_partido_novo) %>%
#   summarise(valor = sum(valor_consolidado)) %>%
#   arrange(desc(valor))
# # Políticos do partido NOVO não constam entre devedores
# setdiff(unique(politicos_2018$sigla_partido_novo), unique(pol_dev_2018$sigla_partido_novo))



# Total = 2018 + 2016 + 2014 (senadores) ----------------------------------------------

pol_dev_total <- readRDS("data/cruzamento_pf.rds")
glimpse(pol_dev_total)

# Número de políticos
n_distinct(pol_dev_total$cpf_candidato)
n_distinct(pol_dev_total$cpf_candidato)/n_distinct(politicos_total$cpf_candidato)

# Valor por pessoa agregado
pol_dev_total %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo,
           descricao_cargo, sigla_uf, descricao_ue, situacao_inscricao) %>%
  summarise(valor = sum(valor_consolidado)) %>%
  arrange(desc(valor)) %>% readr::write_csv("cruzamento_total_agregado.csv")

# Total da dívida de políticos
pol_dev_total %>% summarise(sum(valor_consolidado))

# Por partido
pol_dev_total %>% group_by(sigla_partido_novo) %>%
  summarise(valor = sum(valor_consolidado)) %>%
  arrange(desc(valor))
# Políticos do PCB não constam entre devedores
setdiff(unique(politicos_total$sigla_partido_novo), unique(pol_dev_total$sigla_partido_novo))



# Políticos por cargo
pol_dev_total %>% group_by(descricao_cargo) %>%
  summarise(n_politicos = n_distinct(cpf_candidato),
            valor = sum(valor_consolidado)) %>%
  arrange(desc(valor)) %>%
  left_join(politicos_total %>%
              group_by(descricao_cargo) %>%
              summarise(n=n_distinct(cpf_candidato))) %>%
  mutate(pct_devedores = n_politicos/n) %>% arrange(desc(pct_devedores))


# Dívida mais antiga
pol_dev_total %>% mutate(data_inscricao = lubridate::dmy(data_inscricao)) %>%
  filter(data_inscricao == min(data_inscricao)) %>% t()

# Total de dívidas
pol_dev_total %>% summarise(mean(valor_consolidado))


# Senadores e deputados federais
pol_dev_total %>% filter(descricao_cargo == "SENADOR" | descricao_cargo == "DEPUTADO FEDERAL") %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo, sigla_uf,  descricao_cargo, ) %>%
  summarise(n = sum(valor_consolidado)) %>%
  arrange(desc(n)) %>% head(10) %>% clipr::write_clip()

pol_dev_total %>% filter(descricao_cargo == "GOVERNADOR") %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo, sigla_uf) %>%
  summarise(n = sum(valor_consolidado)) %>%
  arrange(desc(n)) %>% head(10)



pol_dev_total %>% filter(valor_consolidado < 100000) %>%
  mutate(descricao_cargo = ifelse(descricao_cargo=="DEPUTADO DISTRITAL",
                                  "DEPUTADO ESTADUAL", descricao_cargo)) %>%
  ggplot() +
  geom_histogram(aes(x = valor_consolidado,
                     y = stat(count / sum(count)),
                     group = descricao_cargo,
                     fll=descricao_cargo),
                 position = "identity") +
  geom_density(aes(x=valor_consolidado,y = stat(count / sum(count)), group = descricao_cargo)) +
  facet_wrap(~descricao_cargo)


pol_dev_total %>% mutate(data = lubridate::dmy(data_inscricao)) %>%
  group_by(data) %>%
  summarise(total = sum(valor_consolidado)) %>%
  arrange(data) %>%
  mutate(total_acum = cumsum(total)) %>%
  ggplot() +
  geom_line(aes(x = data, y = total_acum))



pol_dev_total %>% group_by(descricao_cargo, sigla_uf) %>%
  summarise(n_politicos_devedores = n_distinct(cpf_candidato),
            valor = sum(valor_consolidado)) %>%
  arrange(desc(valor)) %>%
  left_join(politicos_total %>%
              group_by(descricao_cargo, sigla_uf) %>%
              summarise(n_politicos=n_distinct(cpf_candidato))) %>%
  mutate(pct_devedores = n_politicos_devedores/n_politicos) %>% arrange(desc(pct_devedores)) %>%
  readr::write_csv("pct_devedores_uf.csv")






pol_dev_total %>% filter(valor_consolidado < 100000) %>%
  mutate(descricao_cargo = ifelse(descricao_cargo=="DEPUTADO DISTRITAL",
                                  "DEPUTADO ESTADUAL", descricao_cargo)) %>%
  ggplot() +
  geom_histogram(aes(x = valor_consolidado,
                     y = stat(width*density),
                     group = descricao_cargo,
                     fill= descricao_cargo),
                 position = "identity") +
  geom_density(aes(x=valor_consolidado,y = ..density.., group = descricao_cargo)) +
  facet_wrap(~descricao_cargo) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()


# Situação de inscrição de políticos específicos
pol_dev_total %>% filter(str_detect(nome_candidato, "CARLOS GOMES BEZERRA")) %>% pull(situacao_inscricao) %>% unique()

pol_dev_total %>% filter(str_detect(nome_candidato, "CARLOS GOMES BEZERRA")) %>% View()

