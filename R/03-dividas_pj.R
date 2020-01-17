library(janitor)
library(dplyr)
library(stringr)
library(RPostgreSQL)


politicos_total <- readRDS("data/politicos_total.rds")
devedores_pj <- readRDS("data/devedores_pj.rds")
cruzamento_pj <- readRDS("data/cruzamento_pj.rds")


devedores_pj <- devedores_pj %>% mutate(cnpj = str_replace_all(cpf_cnpj, "[\\./-]", ""))

# Valor devido por empresas
devedores_pj_agregado <- devedores_pj %>% group_by(cnpj) %>% summarise(valor_agregado = sum(valor_consolidado))

# Empresas de políticos
devedores_pj_agregado <- devedores_pj_agregado %>%
  mutate(socio_politico = ifelse(cnpj %in% politicos_socios_red$cnpj_str, TRUE, FALSE))

# Total de empresas de políticos: 2885
devedores_pj_agregado %>% filter(socio_politico == TRUE) %>% nrow()

# Valor total devido por empresas de políticos: R$ 2.785.815.367
devedores_pj_agregado %>% filter(socio_politico == TRUE) %>%
  summarise(sum(valor_agregado))


# Políticos devedores (valor total e último cargo)
cruzamento_pj %>% arrange(ano_eleicao) %>%
  group_by(cpf_candidato, nome_candidato, sigla_partido_novo) %>%
  summarise(valor = sum(valor_consolidado),
            descricao_cargo = last(descricao_cargo),
            descricao_ue = last(descricao_ue),
            empresa = list(unique(razao_social))) %>%
  arrange(desc(valor)) %>% head(10) %>% View()

# Empresas devedoras com maior número de políticos
cruzamento_pj %>% group_by(cnpj, razao_social, ) %>%
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



