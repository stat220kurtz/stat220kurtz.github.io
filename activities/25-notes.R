library(tidyverse)
library(mdsr)

db <- dbConnect_scidb("airlines")
flights <- tbl(db, "flights")
carriers <- tbl(db, "carriers")

flights %>% 
  group_by(year) %>%
  summarize(
    n = n()
  )

nycflights23::flights %>%
  group_by(year) %>%
  summarize(
    n = n()
  )


nycflights23::flights %>%
  inner_join(nycflights23::airlines) %>%
  filter(dest == "MSP") %>%
  group_by(name) %>%
  summarize(
    n = n(),
    pct_ontime = sum(arr_delay <= 15, na.rm = TRUE) / n()
  )

con <- DBI::dbConnect(duckdb::duckdb())

dbplyr::copy_nycflights13(con)
#> Creating table: airlines
#> Creating table: airports
#> Creating table: flights
#> Creating table: planes
#> Creating table: weather
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")
