library(janitor)
library(dplyr)
library(stringr)
library(RPostgreSQL)
library(lubridate)


politicos_total <- readRDS("data/politicos_total.rds")
politicos_socios_red <- readRDS("data/politicos_socios_red.rds")
devedores_pj <- readRDS("data/devedores_pj.rds")
cruzamento_pj <- readRDS("data/cruzamento_pj.rds")



# Visão cnpj (empresas)
devedores_pj_agregado <- devedores_pj %>% group_by(cpf_cnpj) %>% summarise(valor_agregado = sum(valor_consolidado))

# Empresas de políticos
devedores_pj_agregado <- devedores_pj_agregado %>%
  mutate(socio_politico = ifelse(cpf_cnpj %in% politicos_socios_red$cnpj_str, TRUE, FALSE))

# Total de políticos: 64938
politicos_total$cpf_candidato %>% n_distinct()

# Total de políticos que possuem empresas: 14405
politicos_socios_red$cpf_candidato %>% n_distinct()

# Total de empresas de políticos: 2885
cruzamento_pj$cnpj_str %>% n_distinct()
# ou: devedores_pj_agregado %>% filter(socio_politico == TRUE) %>% nrow()

# Total de políticos sócios de empresas na dívida ativa
cruzamento_pj$cpf_candidato %>% n_distinct()

# Políticos com mais de uma empresa inscrita na dívida ativa
cruzamento_pj %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo, descricao_cargo) %>%
  summarise(n_empresas = n_distinct(cnpj_str)) %>%
  filter(n_empresas > 1) %>%
  arrange(desc(n_empresas))


# Valor total devido por empresas de políticos: R$ 2.785.815.367
devedores_pj_agregado %>% filter(socio_politico == TRUE) %>%
  summarise(total = sum(valor_agregado)) %>%
  mutate(pct_da_divida_pj = total/sum(devedores_pj$valor_consolidado))


# Políticos devedores (valor total e último cargo)
cruzamento_pj %>% arrange(ano_eleicao) %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo) %>%
  summarise(valor = sum(valor_consolidado),
            descricao_cargo = last(descricao_cargo),
            descricao_ue = last(descricao_ue),
            empresa = list(unique(razao_social)),
            cnpj = list(unique(cnpj))) %>%
  arrange(desc(valor)) %>% head(10) %>% View()

# Empresas devedoras com maior número de políticos
cruzamento_pj %>% group_by(cnpj2, razao_social, ) %>%
  summarise(n_politicos = n_distinct(cpf_candidato)) %>%
  arrange(desc(n_politicos)) %>% head(10)

# Total de políticos com empresas endividadas: 2452
n_distinct(cruzamento_pj$cpf_candidato)

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


# Medidas resumo ----------------------------------------
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





# Tempo de dívida ---------------------------------------------------------
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

# Políticos que devem há mais tempo (visão doc - dívida)
cruzamento_pj %>%
  mutate(data_inscricao = dmy(data_inscricao)) %>%
  arrange(data_inscricao) %>%
  select(nome_candidato, sigla_partido_novo, descricao_cargo, data_inscricao, nome_devedor) %>%
  head(12)



# políticos endividados duplamente ----------------------------------------

pol_dev_total <- readRDS("data/cruzamento_pf.rds")

pol_dev_total %>% mutate(cpf_candidato = as.numeric(cpf_candidato)) %>%
  left_join(cruzamento_pj %>% mutate(cpf_candidato = as.numeric(cpf_candidato)),
            by = c("cpf_candidato", "nome_candidato", "sigla_partido_novo", "descricao_cargo",
                   "descricao_ue", "numero_candidato", "ano_eleicao", "num_turno",
                   "des_situacao_candidatura", "codigo_legenda","composicao_legenda",
                   "nome_coligacao")) %>%
  filter(!is.na(valor_consolidado.y)) %>%
  summarise(n_distinct(cpf_candidato))


pol_dev_total %>% select(cpf_candidato) %>% glimpse()
cruzamento_pj %>% select(cpf_candidato) %>% glimpse()



# Base geral --------------------------------------------------------------

devedores_pj %>%
  group_by(cpf_cnpj, nome_devedor) %>%
  summarise(valor_agregado = sum(valor_consolidado)) %>%
  arrange(desc(valor_agregado)) %>% head(10)

# Empresas que devem há mais tempo
devedores_pj %>%
  mutate(data_inscricao = dmy(data_inscricao)) %>%
  arrange(data_inscricao) %>%
  select(cpf_cnpj, nome_devedor, data_inscricao) %>%
  head(10)


# CSV para subir na matéria ----------



cruzamento_pj %>%
  mutate(qualificacao_do_responsavel = as.numeric(qualificacao_do_responsavel)) %>%
  left_join(depara_qualificacao,
                            by = c("qualificacao_do_responsavel" = "codigo")) %>%
  select(cpf_candidato, nome_candidato, sigla_partido,
         descricao_cargo, sigla_uf, descricao_ue,
         razao_social, cnpj, descricao,
         situacao_inscricao, valor_consolidado, data_inscricao) %>%
  readr::write_csv("cruzamento_pj_tabela.csv")



