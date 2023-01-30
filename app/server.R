#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

sql <- readLines("C:\\Users\\Repro\\Documents\\R\\ADM\\SHINY\\app\\SQL\\result.sql")
sql <- paste(sql, collapse = " ")


result <- dbGetQuery(con2,sql)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    output$sales <- renderPlot({

      result %>% 
        ggplot(.,aes(x=PEDDTEMIS,y=VRVENDA)) + 
         geom_line(color="white") +
          geom_text(aes(label=format(VRVENDA,big.mark=",")),color="white") +
           scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
            theme(panel.background = element_rect(fill = "#0c1839"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_line(colour = "#15295f"),
              legend.position = "top")
      

    })

})
