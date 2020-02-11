# Conexão metabase
library(RPostgreSQL)

db <- dbConnect(PostgreSQL(),
                user = "rows_user",
                password = "oTSZ5ND4vTSxtdrkSyUz",
                dbname = "postgres",
                host = "vortex-metabase.ceojcho0vrjr.us-east-2.rds.amazonaws.com")
