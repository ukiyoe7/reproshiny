## CODE TO MEASURE RUN TIME

test_func <- function() {  
  
  sql <- readLines("C:\\Users\\Repro\\Documents\\R\\ADM\\SHINY\\app\\SQL\\result.sql")
  sql <- paste(sql, collapse = " ")
  
  
  result <- dbGetQuery(con2,sql)

}


startTime <- Sys.time()

test_func()

endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)
