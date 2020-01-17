library(dplyr)
library(stringr)

base <- data.table::fread("data_raw/devedores_uniao.csv")
base <- base %>% select(-V1) %>% janitor::clean_names()


# PF ----------------------------------------------------------------------

# Filtra devedores que são pessoa física
devedores_pf <- base %>% filter(tipo_pessoa == "Pessoa física")
# Pega do quarto ao nono algarismo do CPF de devedores
devedores_pf <- devedores_pf %>% mutate(cpf_cnpj = as.integer(str_replace_all(cpf_cnpj, "[X\\.]", "")))

devedores_pf %>% saveRDS("data/devedores_pf.rds")





# PJ ----------------------------------------------------------------------

# Filtra devedores que são pessoa jurídica
devedores_pj <- base %>% filter(tipo_pessoa == "Pessoa jurídica")

devedores_pj %>% saveRDS("data/devedores_pj.rds")


