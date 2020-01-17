library(janitor)
library(dplyr)
library(stringr)
library(cepespR)

# Coleta dados de políticos eleitos em 2018
depfederal_2018 <- get_candidates(year = 2018, position = "Deputado Federal", only_elected = TRUE) %>% clean_names()
depestadual_2018 <- get_candidates(year = 2018, position = "Deputado Estadual", only_elected = TRUE) %>% clean_names()
senador_2018 <- get_candidates(year = 2018, position = "Senador", only_elected = TRUE) %>% clean_names()
governador_2018 <- get_candidates(year = 2018, position = "Governador", only_elected = TRUE) %>% clean_names()
presid_2018 <- get_candidates(year = 2018, position = "Presidente", only_elected = TRUE) %>% clean_names()
presid_2018$cpf_candidato <- as.character(presid_2018$cpf_candidato)
politicos_2018 <- bind_rows(presid_2018, depfederal_2018, depestadual_2018, senador_2018, governador_2018)

# Coleta dados de políticos eleitos em 2016
vereador_2016 <- get_candidates(year = 2016, position = "Vereador", only_elected = TRUE) %>% clean_names()
prefeito_2016 <- get_candidates(year = 2016, position = "Prefeito", only_elected = TRUE) %>% clean_names()
politicos_2016 <- bind_rows(vereador_2016, prefeito_2016) %>% clean_names()

# Coleta dados de políticos eleitos em 2018
depfederal_2014 <- get_candidates(year = 2014, position = "Deputado Federal", only_elected = TRUE) %>% clean_names()
depestadual_2014 <- get_candidates(year = 2014, position = "Deputado Estadual", only_elected = TRUE) %>% clean_names()
senador_2014 <- get_candidates(year = 2014, position = "Senador", only_elected = TRUE) %>% clean_names()
governador_2014 <- get_candidates(year = 2014, position = "Governador", only_elected = TRUE) %>% clean_names()
presid_2014 <- get_candidates(year = 2014, position = "Presidente", only_elected = TRUE) %>% clean_names()
presid_2014$cpf_candidato <- as.character(presid_2014$cpf_candidato)
politicos_2014 <- bind_rows(presid_2014, depfederal_2014, depestadual_2014, senador_2014, governador_2014)


politicos_2018 <- politicos_2018 %>% mutate(subset_cpf = as.integer(str_sub(cpf_candidato, 4,9)))
politicos_2016 <- politicos_2016 %>% mutate(subset_cpf = as.integer(str_sub(cpf_candidato, 4,9)))
politicos_2014 <- politicos_2014 %>% mutate(subset_cpf = as.integer(str_sub(cpf_candidato, 4,9)))

# Total de políticos eleitos
politicos_total <- bind_rows(politicos_2018, politicos_2016)
politicos_total <- politicos_2014 %>% filter(descricao_cargo == "SENADOR") %>%
  bind_rows(politicos_total)


# Siglas dos PARTIDOS ATUAIS
politicos_total <- politicos_total %>%
  mutate(sigla_partido_novo = ifelse(
    sigla_partido == "PMDB", "MDB", ifelse(
      sigla_partido == "PT do B", "AVANTE", ifelse(
        sigla_partido == "PTN", "PODE", ifelse(
          sigla_partido == "PEN", "PATRI", ifelse(
            sigla_partido == "PSDC", "DC", sigla_partido))))))


politicos_total %>% readr::write_csv("data/politicos_total.csv")
politicos_total %>% saveRDS("data/politicos_total.rds")

