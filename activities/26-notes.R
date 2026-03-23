library(DBI)

con <- mdsr::dbConnect_scidb(dbname = "imdb")

table_names <- dbListTables(con)

map(table_names, \(x) dbListFields(con, x))

dbListFields(con, "title")
dbListFields(con, "complete_cast")

dbGetQuery(con, "SELECT * FROM complete_cast LIMIT 5")
dbGetQuery(con, "SELECT * FROM aka_name LIMIT 5")
dbGetQuery(con, "SELECT * FROM char_name LIMIT 5")

print_n = function(table_name, n=5){
  str_c("SELECT * FROM ", table_name, " LIMIT ", n)
}

map(table_names, \(x) dbGetQuery(con, print_n(x)))
