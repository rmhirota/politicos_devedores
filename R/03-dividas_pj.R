library(janitor)
library(dplyr)
library(stringr)
library(RPostgreSQL)
library(lubridate)


# Leitura bases tratadas
politicos_total <- readRDS("data/politicos_total.rds")
politicos_socios_red <- readRDS("data/politicos_socios_red.rds")
devedores_pj <- readRDS("data/devedores_pj.rds")
cruzamento_pj <- readRDS("data/cruzamento_pj.rds")



# Visão cnpj (empresas)
devedores_pj_agregado <- devedores_pj %>% group_by(cpf_cnpj) %>% summarise(valor_agregado = sum(valor_consolidado))
# Cria flag para empresas de políticos
devedores_pj_agregado <- devedores_pj_agregado %>%
  mutate(socio_politico = ifelse(cpf_cnpj %in% politicos_socios_red$cnpj_str, TRUE, FALSE))


# 0. Quantos políticos foram eleitos em 2014 (senadores) + 2016 + 2018?  64938 ----
politicos_total$cpf_candidato %>% n_distinct()

# 1. Quantos políticos possuem empresas?  14405 ----
politicos_socios_red$cpf_candidato %>% n_distinct()

# 2. Quantos políticos possuem empresas com dívida ativa na união?  2452 ----
cruzamento_pj$cpf_candidato %>% n_distinct()
# ou: devedores_pj_agregado %>% filter(socio_politico == TRUE) %>% nrow()

# 3. Número de empresas de políticos com dívida ativa na união?  2885 ----
cruzamento_pj$cnpj_str %>% n_distinct()
# ou: devedores_pj_agregado %>% filter(socio_politico == TRUE) %>% nrow()


# 4. Lista de políticos com mais de uma empresa com dívida ativa na união ----
cruzamento_pj %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo, descricao_cargo) %>%
  summarise(n_empresas = n_distinct(cnpj_str)) %>%
  filter(n_empresas > 1) %>%
  arrange(desc(n_empresas))


# 5. Total, em R$, que empresas de políticos devem para a união: R$ 2.785.815.367 ----
devedores_pj_agregado %>% filter(socio_politico == TRUE) %>%
  summarise(total = sum(valor_agregado)) %>%
  mutate(pct_da_divida_pj = total/sum(devedores_pj$valor_consolidado))

cruzamento_pj %>%
  summarise(total = sum(valor_consolidado)) %>%
  mutate(pct_da_divida_pj = total/sum(devedores_pj$valor_consolidado))


# 6. Ranking de políticos com maior dívida ----
# Políticos devedores (valor total e último cargo)
cruzamento_pj %>% arrange(ano_eleicao) %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo) %>%
  summarise(valor = sum(valor_consolidado),
            descricao_cargo = last(descricao_cargo),
            descricao_ue = last(descricao_ue),
            sigla_uf = last(sigla_uf),
            empresa = list(unique(razao_social)),
            cnpj = list(unique(cnpj))) %>%
  arrange(desc(valor)) %>% head(10) %>% View()



# Medidas resumo (políticos e base geral) ----------------------------------------
# 7. Valor médio e mediano da dívida (por empresa)
# Geral (por empresa)
mean(devedores_pj_agregado$valor_agregado)
median(devedores_pj_agregado$valor_agregado)

# Empresas de políticos
cruzamento_pj %>%
  group_by(cnpj_str) %>%
  summarise(valor_agregado = sum(valor_consolidado)) %>%
  pull(valor_agregado) %>% mean()
cruzamento_pj %>%
  group_by(cnpj_str) %>%
  summarise(valor_agregado = sum(valor_consolidado)) %>%
  pull(valor_agregado) %>% median()




# Tempo de dívida (políticos e base geral) ---------------------------------------------------------
# 8. Idade média da dívida
# Políticos
cruzamento_pj %>%
  mutate(data_inscricao = dmy(data_inscricao),
         duracao_dias = today() - data_inscricao,
         duracao_anos = duracao_dias/365.25) %>%
  pull(duracao_anos) %>% mean()

# Geral
devedores_pj %>%
  mutate(data_inscricao = dmy(data_inscricao),
         duracao_dias = today() - data_inscricao,
         duracao_anos = duracao_dias/365.25) %>%
  pull(duracao_anos) %>% mean()


# 9. Políticos que devem há mais tempo (visão doc - dívida)
cruzamento_pj %>%
  mutate(data_inscricao = dmy(data_inscricao)) %>%
  arrange(data_inscricao) %>%
  select(nome_candidato, sigla_partido_novo, descricao_cargo, data_inscricao, nome_devedor) %>%
  head(12)




# Base geral --------------------------------------------------------------


# 1-Geral. Quantas empresas estão inscritas na dívida ativa da união?  2.627.524
devedores_pj$cpf_cnpj %>% n_distinct()

# 2-Geral. Quanto, em R$, elas devem no total (% da dívida inteira, considerando PF)
# R$ 2,48 trilhões (68,5% do total de dívida ativa)
sum(devedores_pj$valor_consolidado) / (sum(devedores_pj$valor_consolidado) + sum(devedores_pf$valor_consolidado))

# 3-Geral. Ranking das empresas com maior dívida
devedores_pj %>%
  group_by(cpf_cnpj, nome_devedor) %>%
  summarise(valor_agregado = sum(valor_consolidado)) %>%
  arrange(desc(valor_agregado)) %>% head(10)

# 9-Geral. Empresas que devem há mais tempo
devedores_pj %>%
  mutate(data_inscricao = dmy(data_inscricao)) %>%
  arrange(data_inscricao) %>%
  select(cpf_cnpj, nome_devedor, data_inscricao) %>%
  head(10)




# Outros questionamentos --------------------------------------------------


# Empresas devedoras com maior número de políticos
cruzamento_pj %>% group_by(cnpj2, razao_social, ) %>%
  summarise(n_politicos = n_distinct(cpf_candidato)) %>%
  arrange(desc(n_politicos)) %>% head(10)

# Por partido
n_politicos_total <- politicos_total %>% group_by(sigla_partido_novo) %>%
  summarise(n_politicos_total = n_distinct(cpf_candidato))

cruzamento_pj %>% group_by(sigla_partido_novo, cnpj_str) %>%
  summarise(candidatos = n_distinct(cpf_candidato), valor = first(valor_consolidado)) %>%
  group_by(sigla_partido_novo) %>%
  summarise(total = sum(valor), n_politicos = sum(candidatos)) %>%
  left_join(n_politicos_total, by = 'sigla_partido_novo') %>%
  mutate(pct_politicos = n_politicos/n_politicos_total) %>%
  arrange(desc(pct_politicos))

# políticos endividados duplamente
pol_dev_total <- readRDS("data/cruzamento_pf.rds")

pol_dev_total %>% mutate(cpf_candidato = as.numeric(cpf_candidato)) %>%
  left_join(cruzamento_pj %>% mutate(cpf_candidato = as.numeric(cpf_candidato)),
            by = c("cpf_candidato", "nome_candidato", "sigla_partido_novo", "descricao_cargo",
                   "descricao_ue", "numero_candidato", "ano_eleicao", "num_turno",
                   "des_situacao_candidatura", "codigo_legenda","composicao_legenda",
                   "nome_coligacao")) %>%
  filter(!is.na(valor_consolidado.y)) %>%
  summarise(n_distinct(cpf_candidato))

# CSV para subir na matéria ----------



cruzamento_pj %>%
  mutate(qualificacao_do_responsavel = as.numeric(qualificacao_do_responsavel)) %>%
  left_join(depara_qualificacao,
                            by = c("qualificacao_do_responsavel" = "codigo")) %>%
  select(cpf_candidato, nome_candidato, sigla_partido,
         descricao_cargo, sigla_uf, descricao_ue,
         razao_social, cnpj, qualificacao_do_responsavel, descricao,
         situacao_inscricao, valor_consolidado, data_inscricao) %>%
  readr::write_csv("cruzamento_pj_tabela.csv")


depara_qualificacao$descricao %>% unique()

