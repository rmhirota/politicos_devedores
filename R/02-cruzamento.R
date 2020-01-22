library(dplyr)
library(janitor)
library(stringr)
library(RPostgreSQL)



# Cruzamento PF -----------------------------------------------------------

politicos_total <- readRDS("data/politicos_total.rds")
devedores_pf <- readRDS("data/devedores_pf.rds")

cruzamento <- politicos_total %>% left_join(devedores_pf, by = c('subset_cpf' = 'cpf_cnpj'))
cruzamento_pf <- cruzamento %>% filter(nome_candidato == nome_devedor)
cruzamento_pf %>% saveRDS("data/cruzamento_pf.rds")



# Cruzamento PJ -----------------------------------------------------------

devedores_pj <- readRDS("data/devedores_pj.rds")

db <- dbConnect(PostgreSQL(),
                user = "rows_user",
                password = "oTSZ5ND4vTSxtdrkSyUz",
                dbname = "postgres",
                host = "vortex-metabase.ceojcho0vrjr.us-east-2.rds.amazonaws.com")

# lista as tabelas do banco de dados
dbListTables(db)
dbGetQuery(db, "select * from politicos_total limit 10") %>% glimpse()

politicos_socios <- (dbGetQuery(db,
                                "select *
                     from politicos_total left join socios
                     on politicos_total.subset_cpf = socios.cnpj_cpf_do_socio and
                        politicos_total.nome_candidato = socios.nome_socio
                     left join empresas on socios.cnpj = empresas.cnpj"))


# Base politicos_socios extraída do postgres
# politicos_socios <- data.table::fread("data/data-1579618845082.csv", colClasses = c(cpf_candidato = "character"))
# glimpse(politicos_socios)

politicos_socios <- politicos_socios %>% filter(cnpj != "NULL")

politicos_socios_red <- politicos_socios %>%
  select(ano_eleicao, num_turno, descricao_eleicao, sigla_uf, sigla_ue,
         descricao_ue, codigo_cargo, descricao_cargo, nome_candidato,
         numero_candidato, cpf_candidato, des_situacao_candidatura,
         sigla_partido, nome_partido, codigo_legenda, sigla_legenda,
         composicao_legenda, nome_coligacao, num_titulo_eleitoral_candidato,
         desc_sit_tot_turno, cnpj, data_entrada_sociedade, razao_social, nome_fantasia,
         data_inicio_atividade, qualificacao_do_responsavel, cnae_fiscal)


politicos_socios_red <- politicos_socios_red %>%
  mutate(cnpj_str = str_pad(cnpj, 14, "left", "0"))


devedores_pj <- devedores_pj %>%
  mutate(cpf_cnpj = str_remove_all(cpf_cnpj, "[\\./-]"))

cruzamento_pj <- politicos_socios_red %>% left_join(devedores_pj, by = c("cnpj_str"="cpf_cnpj"))
cruzamento_pj <- cruzamento_pj %>% filter(!is.na(valor_consolidado))
# Atualiza partidos
cruzamento_pj <- cruzamento_pj %>% mutate(sigla_partido_novo = ifelse(
  sigla_partido == "PMDB", "MDB", ifelse(
    sigla_partido == "PT do B", "AVANTE", ifelse(
      sigla_partido == "PTN", "PODE", ifelse(
        sigla_partido == "PEN", "PATRI", ifelse(
          sigla_partido == "PSDC", "DC", sigla_partido))))))

cruzamento_pj %>% saveRDS("data/cruzamento_pj.rds")
politicos_socios_red %>% saveRDS("data/politicos_socios_red.rds")
