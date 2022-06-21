library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(zoo)
library(reshape2)
library(scales)
library(ggrepel)
library(stringr)
load("sales.RData")
metas<- read.csv2("metas_setores.csv", header = TRUE, sep = ";", dec = ",")

metas_setores<- metas %>% as.data.frame()%>% mutate(month=as.Date(month, format = "%Y-%m-%d"))


sidebar <- dashboardSidebar(width = 130,
                            sidebarMenu(
                              menuItem("RESULTADO", tabName = "geral", icon = icon("dashboard")),
                              menuItem("VENDAS", icon = icon("chart-bar"), tabName = "vendas", badgeColor = "green"),
                              menuItem("PILARES", icon = icon("chart-pie"), tabName = "pilares", badgeColor = "green"),
                              menuItem("CLIENTES", icon = icon("store"), tabName = "clientes", badgeColor = "green"),
                              menuItem("PRODUTOS", icon = icon("glasses"), tabName = "produtos", badgeColor = "green"),
                              menuItem("CIDADES", icon = icon("map"), tabName = "cidades", badgeColor = "green")
                              
                            )
)

body <- dashboardBody(tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
),
tabItems(
  tabItem(tabName = "geral",
          
          fluidRow(
            valueBoxOutput("emis", width = 2),
            valueBoxOutput("alcanceemis",width = 2),
            valueBoxOutput("esperadoemis",width = 2),
            valueBoxOutput("fat",width = 2),
            valueBoxOutput("alcancefat",width = 2),
            valueBoxOutput("esperadofat",width = 2)
          ),
          
          
          fluidRow(
            
            
            tabBox( side = "left", height = "500px",
                    
                    tabPanel("RESULTADO", 
                             plotOutput(outputId = "plotemis")
                             
                    ),
                    
                    tabPanel("METAS", 
                             plotOutput(outputId = "plotmetasemis")
                    ),
                    tabPanel("ALCANCE", 
                             plotOutput(outputId = "plotalcanceemis")
                    )
                    
            ),
            
            
            tabBox( side = "left", height = "500px",
                    
                    tabPanel("RESULTADO", 
                             plotOutput(outputId = "plotfat")
                             
                    ),
                    
                    tabPanel("METAS", 
                             plotOutput(outputId = "plotmetasfat")
                    ),
                    tabPanel("ALCANCE", 
                             plotOutput(outputId = "plotalcancefat")
                    )
                    
            )
            
          )
          
  ),
  tabItem(tabName = "vendas",
          fluidRow(
            
            tags$div(class="boxes",box(title = "VENDAS DIARIAS - DIAS UTEIS",width = 500,status = "primary", solidHeader = TRUE,
                                       tags$div(class="flex-container",
                                                tags$div(class="dateselector",dateRangeInput('daterange',
                                                                                             label = paste('Selecione o periodo 1'),
                                                                                             start = floor_date(Sys.Date(),"month"), end = Sys.Date()-1,
                                                                                             min = Sys.Date()-400, max = Sys.Date() -1,
                                                                                             separator = " - ", format = "dd/mm/yy",
                                                                                             startview = 'year', language = 'pt-br', weekstart = 1,
                                                                                             width ='350px')), 
                                                
                                                tags$div(class="dateselector",dateRangeInput('daterange2',
                                                                                             label = paste('Selecione o periodo 2'),
                                                                                             start = floor_date(Sys.Date(),"month"), end = Sys.Date()-1,
                                                                                             min = Sys.Date()-400, max = Sys.Date() -1,
                                                                                             separator = " - ", format = "dd/mm/yy",
                                                                                             startview = 'year', language = 'pt-br', weekstart = 1,
                                                                                             width ='350px'))
                                       ),
                                       tags$div(class = "dailysales", h3("Periodo 1")),
                                       plotOutput(outputId = "plotsalesdaily",height = "350px"),
                                       tags$div(class = "dailysales", h3("Periodo 2")),
                                       plotOutput(outputId = "plotsalesdaily2",height = "350px"))
            )
            
            
            
          ),
          fluidRow(
            status = "info", solidHeader = TRUE,title = "SETOR",width = 3,
            
            tags$div(class="boxes",box(title = "VENDAS DIARIAS - DIAS UTEIS POR SETOR",width = 500,status = "primary", 
                                       solidHeader = TRUE,
                                       tags$div(class="flex-container",
                                                tags$div(class="setorselector",selectInput(inputId="setor",label=paste('Selecione o setor'),choices = c("SETOR 1","SETOR 2","SETOR 3","SETOR 4","SETOR 5","SETOR 6"),width = '200px')),
                                                tags$div(tags$div(class="dateselector",dateRangeInput('daterange3',
                                                                                                      label = paste('Selecione o periodo'),
                                                                                                      start = floor_date(Sys.Date(),"month"), end = Sys.Date()-1,
                                                                                                      min = Sys.Date()-400, max = Sys.Date() -1,
                                                                                                      separator = " - ", format = "dd/mm/yy",
                                                                                                      startview = 'year', language = 'pt-br', weekstart = 1,
                                                                                                      width ='400px'))),
                                                
                                                tags$div(class="setorselector2",selectInput(inputId="setor2",label=paste('Selecione o setor'),choices = c("SETOR 1","SETOR 2","SETOR 3","SETOR 4","SETOR 5","SETOR 6"),width = '200px')),
                                                tags$div(tags$div(class="dateselector2",dateRangeInput('daterange4',
                                                                                                       label = paste('Selecione o periodo'),
                                                                                                       start = floor_date(Sys.Date(),"month"), end = Sys.Date()-1,
                                                                                                       min = Sys.Date()-400, max = Sys.Date() -1,
                                                                                                       separator = " - ", format = "dd/mm/yy",
                                                                                                       startview = 'year', language = 'pt-br', weekstart = 1,
                                                                                                       width ='400px')))
                                       )
                                       ,
                                       
                                       plotOutput(outputId = "plotsalesdailysectors",height = "370px"),
                                       plotOutput(outputId = "plotsalesdailysectors2",height = "370px")))
          )
          
          
  ),
  tabItem(tabName = "pilares",
          
          fluidRow(
            tags$div(class="boxes",tabBox( width = "600px",
                                           tabPanel(
                                             "VALORES R$",plotOutput(outputId = "plotpilargeralvalor",height = "500px"),
                                             "VALORES R$",plotOutput(outputId = "plotpilarsetoresvalor",height = "2000px")
                                           ),
                                           tabPanel(
                                             "VALORES QTD",plotOutput(outputId = "plotpilargeralqtd",height = "500px"),
                                             "VALORES QTD",plotOutput(outputId = "plotpilarsetoresqtd",height = "2000px")
                                           )
                                           
                                           
            )))
  ),
  tabItem(tabName = "clientes",
          fluidRow(
            
            tags$div(class="boxes",tabBox(width = "600px",
                                          tabPanel("RANKING VALORES",plotOutput(outputId = "plotclientesvalue",height = "800px"),
                                                   tags$div(class="setorselector2",selectInput(inputId="setor3",label=paste('Selecione o setor'),choices = c("SETOR 1","SETOR 2","SETOR 3","SETOR 4","SETOR 5","SETOR 6"),width = '200px')),
                                                   plotOutput(outputId ="plotclientessetoresvalue",height = "800px")),
                                          
                                          tabPanel("RANKING QTD",plotOutput(outputId = "plotclientesqtd",height = "800px")),
                                          
                                          tabPanel(
                                            
                                            "ANALISE MENSAL",
                                            
                                            fluidRow(
                                              
                                              
                                              
                                              tags$div(class="clientesselector",selectInput(inputId="cliente",label="",
                                                                                            sales %>% select(CLIENTE) %>% distinct() %>% arrange(.,CLIENTE),width="400px")))
                                            ,plotOutput(outputId = "plotcliente",height = "470px"))
                                          
            )))
          
  ),
  tabItem(tabName = "produtos",
          
          fluidRow(
            
            tags$div(class="boxes", tabBox(width = "600px",
                                           tabPanel("VALORES",plotOutput(outputId = "plotprodvalue",height = "800px")),
                                           
                                           tabPanel("QTD",plotOutput(outputId = "plotprodqtd",height = "800px")),
                                           
                                           tabPanel("MIX",
                                                    fluidRow(
                                                      
                                                      tags$div(class="clientesselector",selectInput(inputId="clientmix",label="",
                                                                                                    sales %>% select(CLIENTE) %>% distinct() %>% arrange(.,CLIENTE),width="400px"))),
                                                    
                                                    plotOutput(outputId = "plotprodmix",height = "600px"))
                                           
            ))
            
          )
          
  ),
  
  tabItem(tabName = "cidades",
          
          fluidRow(
            
            status = "info", solidHeader = TRUE,title = "CIDADES",width = 3,
            
            
            tags$div(class="boxes",tabBox(width = "500px",
                                          
                                          tabPanel("RANKING TOP 50 CIDADES",
                                                   
                                                   plotOutput(outputId = "plotcidade",height = "1000px")),
                                          
                                          tabPanel("CLIENTES POR CIDADE",
                                                   
                                                   tags$div(class="setorselector",selectInput(inputId="cidades",label="Selecione a cidade",
                                                                                              sales %>% select(CIDADE) %>% distinct() %>% arrange(.,CIDADE))),
                                                   
                                                   plotOutput(outputId = "plotcidadevalor"))
                                          
                                          
                                          
            ))
          )
          
          
  )
  
  
)
)


ui <- fluidPage(dashboardPage(
  dashboardHeader(title =" REPRO",titleWidth = 250),
  sidebar,
  body
)
)


server <- function(input, output) {
  
  output$plotemis <- renderPlot({
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") & PEDDTEMIS<Sys.Date()) %>%  
      group_by(PEDDTEMIS,ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) %>% 
      group_by(ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) %>% filter(ZODESCRICAO!='NA') %>% 
      mutate(ZODESCRICAO=factor(ZODESCRICAO,levels=rev(ZODESCRICAO),ordered = TRUE))
    
    p <- ggplot(d,aes(x=ZODESCRICAO,y=VRVENDA,fill=-VRVENDA)) +geom_bar(stat = "identity") +
      geom_text(aes(label=format(VRVENDA,big.mark = ",")),hjust=-0.1,size=5) + coord_flip() + 
      scale_fill_gradient(high = "#f39f63", low = "#d66a1d", na.value = NA)+
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.y = element_text(size = 11,face = "bold"),
            axis.text.x = element_blank(),axis.ticks.x = element_blank()) 
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) 
  })  
  
  output$plotmetasemis <- renderPlot({
    data_metas_emis <- metas_setores %>% filter(month(floor_date(month,"month"))==month(Sys.Date())) %>% 
      filter(SETOR!='NA')  %>% mutate(SETOR=factor(SETOR,levels=rev(SETOR),ordered = TRUE))
    
    plot_metas_emis <- ggplot(data_metas_emis,aes(x=SETOR,y=value,fill=-value)) + geom_bar(stat ="identity") + 
      geom_text(aes(label=format(value,big.mark = ",")),hjust=-0.1,size=5) + 
      scale_y_continuous(expand = c(0,0),limits=c(0,2500000)) + coord_flip() + 
      scale_fill_gradient(high = "#f39f63", low = "#d66a1d", na.value = NA) + 
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.x = element_blank(),axis.text.y = element_text(size = 11,face = "bold"),
            axis.ticks.x = element_blank())
    plot_metas_emis + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(plot_metas_emis)$y)*1.5)) 
  })
  
  output$plotalcanceemis <- renderPlot({
    data_alcance_emis <- inner_join(sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") & PEDDTEMIS<=Sys.Date()-1 & PEDDTEMIS!=as.Date("2020-12-25")) %>% 
                                      group_by(PEDDTEMIS,ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) 
                                    %>% group_by(ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) 
                                    %>% filter(ZODESCRICAO!='NA') %>% rename(.,SETOR=ZODESCRICAO),metas_setores 
                                    %>% filter(month(floor_date(month,"month"))==month(Sys.Date())) 
                                    %>% filter(SETOR!='NA'),by="SETOR") %>% rename(.,VMETAS=value) %>%.[,c(-3,-4)] %>% 
      mutate(ALCANCE=VRVENDA/VMETAS) %>% mutate(SETOR=factor(SETOR,levels=rev(SETOR),ordered = TRUE))
    
    plot_alcance_emis <- ggplot(data_alcance_emis,aes(x=SETOR,y=ALCANCE,fill=-ALCANCE)) + geom_bar(stat = "identity") +
      coord_flip() + scale_fill_gradient(high = "#f39f63", low = "#d66a1d", na.value = NA) + 
      geom_text(aes(label=percent(ALCANCE)),hjust=-0.1,size=5) +
      scale_y_continuous(expand = c(0,0),limits=c(0,1.5)) +
      theme(legend.position = "none", axis.title = element_blank(),axis.text.x = element_blank(),
            axis.text.y = element_text(size = 11,face = "bold"),axis.ticks.x = element_blank())
    plot_alcance_emis + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(plot_alcance_emis)$y)*1.5)) 
  })
  
  
  output$plotfat <- renderPlot({
    data_baixa <- sales %>% filter(PEDDTBAIXA>=floor_date(Sys.Date(),"month") & PEDDTBAIXA<Sys.Date()) %>%  
      group_by(PEDDTBAIXA,ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) %>% 
      group_by(ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) %>% filter(ZODESCRICAO!='NA') %>% 
      mutate(ZODESCRICAO=factor(ZODESCRICAO,levels=rev(ZODESCRICAO),ordered = TRUE))
    
    plot_baixa <- ggplot(data_baixa,aes(x=ZODESCRICAO,y=VRVENDA,fill=-VRVENDA)) +geom_bar(stat = "identity") +
      geom_text(aes(label=format(VRVENDA,big.mark = ",")),hjust=-0.1,size=5) + coord_flip() + 
      scale_fill_gradient(high = "#6f94b9", low = "#0e467d", na.value = NA)+
      theme(legend.position = "none", axis.title = element_blank(),
            axis.text.y = element_text(size = 11,face = "bold"),
            axis.text.x = element_blank(),axis.ticks.x = element_blank())  
    plot_baixa + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(plot_baixa)$y)*1.5)) 
  })  
  
  
  
  output$plotmetasfat <- renderPlot({
    data_metas_fat <- metas_setores %>% filter(month(floor_date(month,"month"))==month(Sys.Date())) %>% 
      filter(SETOR!='NA')  %>% mutate(SETOR=factor(SETOR,levels=rev(SETOR),ordered = TRUE)) 
    
    plot_metas_fat <- ggplot(data_metas_fat,aes(x=SETOR,y=value,fill=-value)) + geom_bar(stat ="identity") + 
      geom_text(aes(label=format(value,big.mark = ",")),hjust=-0.1,size=5) + 
      scale_y_continuous(expand = c(0,0),limits=c(0,2500000)) + coord_flip() + 
      scale_fill_gradient(high = "#6f94b9", low = "#0e467d", na.value = NA) + 
      theme(legend.position = "none", axis.title = element_blank(),axis.text.x = element_blank(),
            axis.text.y = element_text(size = 11,face = "bold"),axis.ticks.x = element_blank())
    
    plot_metas_fat + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(plot_metas_fat)$y)*1.5))
    
  })
  
  output$plotalcancefat <- renderPlot({
    data_alcance_fat <- inner_join(sales %>% filter(PEDDTBAIXA>=floor_date(Sys.Date(),"month") & PEDDTBAIXA<=Sys.Date()-1) 
                                   %>%  group_by(PEDDTBAIXA,ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) 
                                   %>% group_by(ZODESCRICAO) %>% summarize(VRVENDA=sum(VRVENDA)) 
                                   %>% filter(ZODESCRICAO!='NA') %>% rename(.,SETOR=ZODESCRICAO),metas_setores 
                                   %>% filter(month(floor_date(month,"month"))==month(Sys.Date())) 
                                   %>% filter(SETOR!='NA'),by="SETOR") %>% rename(.,VMETAS=value) %>%.[,c(-3,-4)] %>% 
      mutate(ALCANCE=VRVENDA/VMETAS) %>% mutate(SETOR=factor(SETOR,levels=rev(SETOR),ordered = TRUE))
    
    plot_alcance_fat <- ggplot(data_alcance_fat,aes(x=SETOR,y=ALCANCE,fill=-ALCANCE)) + geom_bar(stat = "identity") +
      coord_flip() + scale_fill_gradient(high = "#6f94b9", low = "#0e467d", na.value = NA) + 
      geom_text(aes(label=percent(ALCANCE)),hjust=-0.1,size=5) +
      scale_y_continuous(expand = c(0,0),limits=c(0,1.5)) +
      theme(legend.position = "none", axis.title = element_blank(),axis.text.x = element_blank(),
            axis.text.y = element_text(size = 11,face = "bold"),axis.ticks.x = element_blank())
    plot_alcance_fat + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(plot_alcance_fat)$y)*1.5)) 
  })
  
  
  emis <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") & PEDDTEMIS<Sys.Date()) %>% group_by(PEDDTEMIS) %>% summarize(v=sum(VRVENDA)) %>% 
    summarize(format(round(sum(v),1),big.mark = ","))
  
  fat <- sales %>% filter(PEDDTBAIXA>=floor_date(Sys.Date(),"month") & PEDDTBAIXA<Sys.Date()) %>%  group_by(PEDDTBAIXA) %>% summarize(v=sum(VRVENDA)) %>% 
    summarize(format(round(sum(v),1),big.mark = ","))
  
  output$emis <- renderValueBox({
    valueBox(
      value = tags$p(emis, style = "font-size: 90%;"),
      "EMISSAO",
      color = "orange"
    )
  })
  
  output$fat <- renderValueBox({
    valueBox(
      value = tags$p(fat, style = "font-size: 90%;"),
      "FATURAMENTO",
      color = "blue"
    )
  })
  
  
  alcanceemis <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") & PEDDTEMIS<=Sys.Date()-1) %>% group_by(PEDDTEMIS) %>%  
    summarize(v=sum(VRVENDA)) %>% summarize(v=percent(round(sum(v),2)/(metas_setores %>% as.data.frame()%>% 
                                                                         mutate(month=as.Date(month, format = "%Y-%m-%d")) %>% filter(month(floor_date(month,"month"))==month(Sys.Date()))%>% 
                                                                         summarize(v=sum(value)) %>% as.numeric()),.1))
  
  
  alcancefat <- sales %>% filter(PEDDTBAIXA>=floor_date(Sys.Date(),"month") & PEDDTBAIXA<=Sys.Date()-1) %>% group_by(PEDDTEMIS) %>%  
    summarize(v=sum(VRVENDA)) %>% summarize(v=percent(round(sum(v),2)/(metas_setores %>% as.data.frame()%>% 
                                                                         mutate(month=as.Date(month, format = "%Y-%m-%d")) %>%
                                                                         filter(month(floor_date(month,"month"))==month(Sys.Date()))  %>% summarize(v=sum(value)) %>% as.numeric()),.1))
  
  output$alcanceemis <- renderValueBox({
    valueBox(
      alcanceemis,
      tags$p("ALCANCE EMISSAO",style = "font-size: 97%;"),
      color = "orange"
    )
  })
  
  output$alcancefat <- renderValueBox({
    valueBox(
      alcancefat,
      tags$p("ALCANCE FATURAMENTO",style = "font-size: 97%;"),
      color = "blue"
    )
  })
  
  # esperado ---
  
  esperadoemis <- percent(metas_setores %>% as.data.frame()%>% 
                            mutate(month=as.Date(month, format = "%Y-%m-%d")) %>% 
                            filter(month(floor_date(month,"month"))==month(Sys.Date()) )%>% 
                            summarize(v=sum(value)) %>% 
                            as.numeric()/wday(seq.Date(floor_date(Sys.Date(),"month"),ceiling_date(Sys.Date(),"month")-1,by="day") %>% .[!grepl("2020-12-25",.)])%>%  .[!.%in% c(1,7)] %>% length() 
                          * wday(seq.Date(floor_date(Sys.Date(),"month"),Sys.Date()-1,by="day") %>% .[!grepl("2020-12-25",.)])%>% 
                            .[!.%in% c(1,7)] %>% length()/metas_setores %>% as.data.frame()%>% 
                            mutate(month=as.Date(month, format = "%Y-%m-%d")) %>% 
                            filter(month(floor_date(month,"month"))==month(Sys.Date()))%>% summarize(v=sum(value)) %>% 
                            as.numeric(),.1) 
  
  esperadofat <- percent(metas_setores %>% as.data.frame()%>% 
                           mutate(month=as.Date(month, format = "%Y-%m-%d")) %>% 
                           filter(month(floor_date(month,"month"))==month(Sys.Date()))%>% summarize(v=sum(value)) %>% 
                           as.numeric()/wday(seq.Date(floor_date(Sys.Date(),"month"),ceiling_date(Sys.Date(),"month")-1,by="day") %>% .[!grepl("2020-12-25",.)])%>%  .[!.%in% c(1,7)] %>% length() 
                         * wday(seq.Date(floor_date(Sys.Date(),"month"),Sys.Date()-1,by="day") %>% .[!grepl("2020-12-25",.)])%>% 
                           .[!.%in% c(1,7)] %>% length()/metas_setores %>% as.data.frame()%>% 
                           mutate(month=as.Date(month, format = "%Y-%m-%d")) %>% 
                           filter(month(floor_date(month,"month"))==month(Sys.Date()))%>% summarize(v=sum(value)) %>% 
                           as.numeric(),.1)
  
  output$esperadoemis <- renderValueBox({
    valueBox(
      esperadoemis,
      tags$p("ESPERADO EMISSAO",style = "font-size: 97%;"),
      color = "orange"
    )
  })
  
  output$esperadofat <- renderValueBox({
    valueBox(
      esperadofat,
      tags$p("ESPERADO FATURAMENTO",style = "font-size: 97%;"),
      color = "blue"
    )
  })
  
  
  
  # daily ---
  
  
  
  output$plotsalesdaily<- renderPlot({ 
    d <- sales %>% filter(!wday(PEDDTEMIS) %in% c(1,7) & PEDDTEMIS>=as.Date(input$daterange[1]) & PEDDTEMIS<=as.Date(input$daterange[2]) & PEDDTEMIS !=as.Date('2020-10-12') & PEDDTEMIS !=as.Date('2020-11-02')) %>% group_by(PEDDTEMIS) %>% summarize(VRVENDA=sum(VRVENDA)) %>% mutate(MEDIA=rollmeanr(VRVENDA,k=5,fill=NA)) %>% as.data.frame() %>% melt(.,id.vars="PEDDTEMIS")
    p <- ggplot(d,aes(x=PEDDTEMIS,y=value,color=factor(variable))) + geom_line() + geom_point() +geom_text_repel(aes(label=format(round(value,0),big.mark = ",")),vjust=-0.5,size=5,segment.color="black") 
    p +scale_y_continuous(expand = c(0,0),limits=c(100000,500000),breaks=seq(0,800000,by=50000),labels=comma)+scale_x_datetime(date_breaks = "day",date_labels = "%d/%m",expand = c(0.01, 0))  + scale_color_manual(name="INDICADOR",values = c("#6ad600", "#395f82"),labels = c("VENDAS POR EMISSAO", "MEDIA MOVEL DIARIA - 5 DIAS")) +theme(panel.background = element_rect(fill = "#041729"),panel.grid.minor = element_blank(),panel.grid.major.y = element_line(color="#1d2e3e"),legend.position="top",legend.key = element_rect(fill = "black"),legend.background = element_rect(fill="#041729",size=20),legend.text = element_text(colour="#cfdbe7", size=13),legend.title = element_text(colour = "#cfdbe7",size=14),legend.justification = c(0,1),legend.margin=margin(10,15,10,10),legend.box.margin=margin(-10,-10,-10,-10),axis.text.x = element_text(size = 14,face="bold",margin = margin(t = 0),color="#082745",angle=25),axis.text.y = element_text(color="#082745",size = 10,face="bold",margin = margin(t = 0)),axis.title=element_blank(),axis.ticks.x = element_line(colour = "#cfdbe7",size=1),axis.ticks.y = element_line(colour = "#cfdbe7",size=1),axis.line = element_line(colour = "#041729",size=3),axis.ticks.length.x = unit(0.5, "cm"),panel.grid.major.x = element_line(colour = "#061f37"),axis.ticks.length.y = unit(0.5, "cm"))
  })
  
  
  output$plotsalesdaily2<- renderPlot({ 
    d <- sales %>% filter(!wday(PEDDTEMIS) %in% c(1,7) & PEDDTEMIS>=as.Date(input$daterange2[1]) & PEDDTEMIS<=as.Date(input$daterange2[2]) & PEDDTEMIS !=as.Date('2020-10-12') & PEDDTEMIS !=as.Date('2020-11-02')) %>% group_by(PEDDTEMIS) %>% summarize(VRVENDA=sum(VRVENDA)) %>% mutate(MEDIA=rollmeanr(VRVENDA,k=5,fill=NA)) %>% as.data.frame() %>% melt(.,id.vars="PEDDTEMIS")
    p <- ggplot(d,aes(x=PEDDTEMIS,y=value,color=factor(variable))) + geom_line() + geom_point() +geom_text_repel(aes(label=format(round(value,0),big.mark = ",")),vjust=-0.5,size=5,segment.color="black") 
    p +scale_y_continuous(expand = c(0,0),limits=c(100000,500000),breaks=seq(0,800000,by=50000),labels=comma)+scale_x_datetime(date_breaks = "day",date_labels = "%d/%m",expand = c(0.01, 0))  + scale_color_manual(name="INDICADOR",values = c("#6ad600", "#395f82"),labels = c("VENDAS POR EMISSAO", "MEDIA MOVEL DIARIA - 5 DIAS")) +theme(panel.background = element_rect(fill = "#041729"),panel.grid.minor = element_blank(),panel.grid.major.y = element_line(color="#1d2e3e"),legend.position="none",axis.text.x = element_text(size = 14,face="bold",margin = margin(t = 0),color="#082745",angle=25),axis.text.y = element_text(color="#082745",size = 10,face="bold",margin = margin(t = 0)),axis.title=element_blank(),axis.ticks.x = element_line(colour = "#cfdbe7",size=1),axis.ticks.y = element_line(colour = "#cfdbe7",size=1),axis.line = element_line(colour = "#041729",size=3),axis.ticks.length.x = unit(0.5, "cm"),panel.grid.major.x = element_line(colour = "#061f37"),axis.ticks.length.y = unit(0.5, "cm"))
  })
  
  output$plotsalesdailysectors<- renderPlot({
    
    setorInput <- reactive({
      switch(input$setor,"SETOR 1"=20,"SETOR 2"=21,"SETOR 3"=22,"SETOR 4"=23,"SETOR 5"=24,"SETOR 6"=28)
    })
    d <- sales %>% filter(!wday(PEDDTEMIS) %in% c(1,7) & PEDDTEMIS>=as.Date(input$daterange3[1]) & PEDDTEMIS<=as.Date(input$daterange3[2]) & PEDDTEMIS !=as.Date('2020-10-12') & PEDDTEMIS !=as.Date('2020-11-02') & ZOCODIGO==setorInput()) %>% group_by(PEDDTEMIS) %>% summarize(VRVENDA=sum(VRVENDA)) %>% mutate(MEDIA=rollmeanr(VRVENDA,k=5,fill=NA)) %>% as.data.frame() %>% melt(.,id.vars=c("PEDDTEMIS"))
    p <- ggplot(d,aes(x=PEDDTEMIS,y=value,color=factor(variable))) + geom_line() + geom_point() + geom_text_repel(aes(label=format(round(value,0),big.mark = ",")),vjust=-0.5,size=5,segment.color="black")
    p +scale_y_continuous(expand = c(0,0),limits=c(0,120000),breaks=seq(20000,200000,by=20000),labels=comma)+scale_x_datetime(date_breaks = "day",date_labels = "%d/%m",expand = c(0.01, 0))  + scale_color_manual(name="INDICADOR",values = c("#6ad600", "#395f82"),labels = c("VENDAS POR EMISSAO", "MEDIA MOVEL DIARIA - 5 DIAS")) +theme(panel.background = element_rect(fill = "#041729"),panel.grid.minor = element_blank(),panel.grid.major.y = element_line(color="#1d2e3e"),legend.position="top",legend.key = element_rect(fill = "black"),legend.background = element_rect(fill="#041729",size=20),legend.text = element_text(colour="#cfdbe7", size=13),legend.title = element_text(colour = "#cfdbe7",size=14),legend.justification = c(0,1),legend.margin=margin(10,15,10,10),legend.box.margin=margin(-10,-10,-10,-10),axis.text.x = element_text(size = 14,face="bold",margin = margin(t = 0),color="#082745",angle=25),axis.text.y = element_text(color="#082745",size = 10,face="bold",margin = margin(t = 0)),axis.title=element_blank(),axis.ticks.x = element_line(colour = "#cfdbe7",size=1),axis.ticks.y = element_line(colour = "#cfdbe7",size=1),axis.line = element_line(colour = "#041729",size=3),axis.ticks.length.x = unit(0.5, "cm"),panel.grid.major.x = element_line(colour = "#061f37"),axis.ticks.length.y = unit(0.5, "cm"))
    
  }) 
  
  
  output$plotsalesdailysectors2<- renderPlot({
    
    setorInput <- reactive({
      switch(input$setor2,"SETOR 1"=20,"SETOR 2"=21,"SETOR 3"=22,"SETOR 4"=23,"SETOR 5"=24,"SETOR 6"=28)
    })
    d <- sales %>% filter(!wday(PEDDTEMIS) %in% c(1,7) & PEDDTEMIS>=as.Date(input$daterange4[1]) & PEDDTEMIS<=as.Date(input$daterange4[2]) & PEDDTEMIS !=as.Date('2020-10-12') & PEDDTEMIS !=as.Date('2020-11-02') & ZOCODIGO==setorInput()) %>% group_by(PEDDTEMIS) %>% summarize(VRVENDA=sum(VRVENDA)) %>% mutate(MEDIA=rollmeanr(VRVENDA,k=5,fill=NA)) %>% as.data.frame() %>% melt(.,id.vars=c("PEDDTEMIS"))
    p <- ggplot(d,aes(x=PEDDTEMIS,y=value,color=factor(variable))) + geom_line() + geom_point() + geom_text_repel(aes(label=format(round(value,0),big.mark = ",")),vjust=-0.5,size=5,segment.color="black")
    p +scale_y_continuous(expand = c(0,0),limits=c(0,120000),breaks=seq(20000,200000,by=20000),labels=comma)+scale_x_datetime(date_breaks = "day",date_labels = "%d/%m",expand = c(0.01, 0))  + scale_color_manual(name="INDICADOR",values = c("#6ad600", "#395f82"),labels = c("VENDAS POR EMISSAO", "MEDIA MOVEL DIARIA - 5 DIAS")) +theme(panel.background = element_rect(fill = "#041729"),panel.grid.minor = element_blank(),panel.grid.major.y = element_line(color="#1d2e3e"),legend.position="none",axis.text.x = element_text(size = 14,face="bold",margin = margin(t = 0),color="#082745",angle=25),axis.text.y = element_text(color="#082745",size = 10,face="bold",margin = margin(t = 0)),axis.title=element_blank(),axis.ticks.x = element_line(colour = "#cfdbe7",size=1),axis.ticks.y = element_line(colour = "#cfdbe7",size=1),axis.line = element_line(colour = "#041729",size=3),axis.ticks.length.x = unit(0.5, "cm"),panel.grid.major.x = element_line(colour = "#061f37"),axis.ticks.length.y = unit(0.5, "cm"))
    
  }) 
  
  # Pilares ----
  
  output$plotpilargeralvalor <-renderPlot({
    
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
                            PEDDTEMIS<floor_date(Sys.Date(),"month")) %>%  filter(!is.na(PILAR))  %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(v=sum(VRVENDA)) %>%
      group_by(PILAR) %>% summarize(m=sum(v)/6) %>% arrange(desc(m)) %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% 
                  summarize(v=sum(VRVENDA)),by= "PILAR") %>% left_join(sales %>% 
                                                                         filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% 
                                                                         filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(v=sum(VRVENDA)),by= "PILAR") %>% .[,c(-3,-5)] %>% `colnames<-`(c("PILAR","MEDIA 6 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(v=sum(VRVENDA)) %>% group_by(PILAR) %>% summarize(m=sum(v)/6) %>% arrange(desc(m)) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PILAR=factor(PILAR,levels=rev(unique(o$PILAR))))
    
    p<-ggplot(d2,aes(x=PILAR,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57"),axis.ticks.x = element_blank()) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +scale_fill_gradient(low = "#6f94b9", high = "#092e53")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
    
  })
  
  
  
  
  output$plotpilargeralqtd <-renderPlot({
    
    
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
                            PEDDTEMIS<floor_date(Sys.Date(),"month")) %>%  filter(!is.na(PILAR))  %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(qtd=sum(QTD)) %>%
      group_by(PILAR) %>% summarize(m=round(sum(qtd)/6,0)) %>% arrange(desc(m))  %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% 
                  summarize(qtd=sum(QTD)),by= "PILAR") %>% left_join(sales %>% 
                                                                       filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% 
                                                                       filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(qtd=sum(QTD)),by= "PILAR") %>% .[,c(-3,-5)] %>% `colnames<-`(c("PILAR","MEDIA 6 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR) %>% summarize(qtd=sum(QTD)) %>% group_by(PILAR) %>% summarize(m=sum(qtd)/6) %>% arrange(desc(m)) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PILAR=factor(PILAR,levels=rev(unique(o$PILAR))))
    
    p<-ggplot(d2,aes(x=PILAR,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57"),axis.ticks.x = element_blank()) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +scale_fill_gradient(low = "#6f94b9", high = "#092e53")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
    
    
    
  })
  
  
  output$plotpilarsetoresvalor <-renderPlot({
    d <- sales %>% 
      filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
               PEDDTEMIS<floor_date(Sys.Date(),"month")) %>%  filter(!is.na(PILAR))  %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% summarize(v=sum(VRVENDA)) %>%
      group_by(PILAR,ZODESCRICAO) %>% summarize(m=round(sum(v)/6,2)) %>% arrange(desc(m)) %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
                  summarize(v=round(sum(VRVENDA),2)),by=c("ZODESCRICAO","PILAR")) %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & 
                                   floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & 
                                   floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
                  summarize(v=round(sum(VRVENDA),2)),by=c("ZODESCRICAO","PILAR")) %>% .[,c(-4,-6)] %>%
      `colnames<-`(c("PILAR","SETOR","MEDIA 6 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% 
      melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
                            PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(!is.na(PILAR)) %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
      summarize(v=sum(VRVENDA)) %>% group_by(PILAR,ZODESCRICAO) %>% 
      summarize(m=sum(v)/6) %>% arrange(desc(m)) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PILAR=factor(PILAR,levels=rev(unique(o$PILAR))))
    
    p <- ggplot(d2,aes(x=PILAR,y=VENDAS,fill=VENDAS))  + 
      geom_bar(stat = "identity",position = position_dodge2(preserve = "total")) + 
      geom_text(aes(label=format(VENDAS,big.mark = ",")),
                position = position_dodge(width = 1),size=5,hjust=-0.1,color="white") + 
      scale_fill_gradient(low = "#9cacb4", high = "#072636") +
      coord_flip() +
      theme(panel.background = element_rect(fill = "#061f37"),
            panel.grid = element_blank(),legend.position = "nove",legend.justification = c(0, 0),
            legend.text = element_text(size = 12,face = "bold"),axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),axis.text.y = element_text(face = "bold",
                                                                    size = 11,color="black"),axis.ticks.y = element_line(size = 1,colour = "#EE1289"),
            axis.ticks.length=unit(.3, "cm"),axis.title = element_blank()) + facet_grid(SETOR ~ INDICADOR) + 
      theme(strip.background = element_rect(fill = "#01070d"),
            strip.text=element_text(color="white",size=14,face="bold",margin = margin(0,1,0,1,"cm")))
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.7))
  })
  
  
  
  output$plotpilarsetoresqtd <-renderPlot({
    
    d <- sales %>% 
      filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
               PEDDTEMIS<floor_date(Sys.Date(),"month")) %>%  filter(!is.na(PILAR))  %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% summarize(qtd=sum(QTD)) %>%
      group_by(PILAR,ZODESCRICAO) %>% summarize(m=round(sum(qtd)/6,2)) %>% arrange(desc(m)) %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
                  summarize(qtd=round(sum(QTD),2)),by=c("ZODESCRICAO","PILAR")) %>% 
      left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & 
                                   floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & 
                                   floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% 
                  filter(!is.na(PILAR)) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
                  summarize(qtd=round(sum(QTD),2)),by=c("ZODESCRICAO","PILAR")) %>% .[,c(-4,-6)] %>%
      `colnames<-`(c("PILAR","SETOR","MEDIA 6 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% 
      melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(6) & 
                            PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(!is.na(PILAR)) %>% 
      group_by(MES=floor_date(PEDDTEMIS,"month"),PILAR,ZODESCRICAO) %>% 
      summarize(qtd=sum(QTD)) %>% group_by(PILAR,ZODESCRICAO) %>% 
      summarize(m=sum(qtd)/6) %>% arrange(desc(m)) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PILAR=factor(PILAR,levels=rev(unique(o$PILAR))))
    
    p <- ggplot(d2,aes(x=PILAR,y=VENDAS,fill=VENDAS))  + 
      geom_bar(stat = "identity",position = position_dodge2(preserve = "total")) + 
      geom_text(aes(label=format(VENDAS,big.mark = ",")),
                position = position_dodge(width = 1),size=5,hjust=-0.1,color="white") + 
      scale_fill_gradient(low = "#9cacb4", high = "#072636") +
      coord_flip() +
      theme(panel.background = element_rect(fill = "#061f37"),
            panel.grid = element_blank(),legend.position = "nove",legend.justification = c(0, 0),
            legend.text = element_text(size = 12,face = "bold"),axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),axis.text.y = element_text(face = "bold",
                                                                    size = 11,color="black"),axis.ticks.y = element_line(size = 1,colour = "#EE1289"),
            axis.ticks.length=unit(.3, "cm"),axis.title = element_blank()) + facet_grid(SETOR ~ INDICADOR) + 
      theme(strip.background = element_rect(fill = "#01070d"),
            strip.text=element_text(color="white",size=14,face="bold",margin = margin(0,1,0,1,"cm")))
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.2))
    
  })
  
  
  # Clients ----
  
  output$plotclientesvalue <-renderPlot({
    
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30)%>% na.omit() %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE") %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE") %>% .[,c(-3,-5)] %>% `colnames<-`(c("CLIENTE","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(CLIENTE=factor(CLIENTE,levels=rev(unique(o$CLIENTE))))
    
    
    p<-ggplot(d2,aes(x=CLIENTE,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57")) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +scale_fill_gradient(low = "#fdbb84", high = "#e34a33")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))  
  })
  
  
  
  
  
  output$plotclientessetoresvalue <-renderPlot({
    
    setorInput <- reactive({
      switch(input$setor3,"SETOR 1"=20,"SETOR 2"=21,"SETOR 3"=22,"SETOR 4"=23,"SETOR 5"=24,"SETOR 6"=28)
    })
    
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(ZOCODIGO==setorInput()) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30)%>% na.omit() %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% filter(ZOCODIGO==setorInput()) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE") %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>%filter(ZOCODIGO==setorInput()) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE") %>% .[,c(-3,-5)] %>% `colnames<-`(c("CLIENTE","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% filter(ZOCODIGO==setorInput()) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(CLIENTE=factor(CLIENTE,levels=rev(unique(o$CLIENTE))))
    
    
    p<-ggplot(d2,aes(x=CLIENTE,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57")) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +scale_fill_gradient(low = "#fdbb84", high = "#e34a33")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))  
  })
  
  
  
  
  
  
  output$plotclientesqtd <-renderPlot({
    
    d <- sales %>% filter(TIPO %in% c("P","F")) %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(qtd=sum(QTD)) %>% group_by(CLIENTE) %>% summarize(m=round(sum(qtd)/3,0)) %>% na.omit(.) %>% arrange(desc(m)) %>% top_n(30)%>% left_join(sales %>%  filter(TIPO %in% c("P","F")) %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(qtd=sum(QTD)),by= "CLIENTE") %>% left_join(sales %>%  filter(TIPO %in% c("P","F")) %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% select(CLIENTE,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(qtd=sum(QTD)),by= "CLIENTE") %>% .[,c(-3,-5)] %>% `colnames<-`(c("CLIENTE","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>%  filter(TIPO %in% c("P","F"))  %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(qtd=sum(QTD)) %>% group_by(CLIENTE) %>% summarize(m=sum(qtd)/3) %>% na.omit(.) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(CLIENTE=factor(CLIENTE,levels=rev(unique(o$CLIENTE))))
    
    
    p<-ggplot(d2,aes(x=CLIENTE,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57")) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) + scale_fill_gradient(low = "#fdbb84", high = "#e34a33") + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))   })
  
  
  output$plotcliente <-renderPlot({
    cli_data_sales <- sales %>% filter(CLIENTE==input$cliente) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),ANO=floor_date(PEDDTEMIS,"year")) %>% summarize(v=sum(VRVENDA)) %>% mutate(ANO=as.factor(format(ANO,format="%Y")))
    
    cli_data_sales_plot <-  ggplot(cli_data_sales,aes(x=MES,y=v,fill=as.factor(ANO))) + geom_bar(stat="identity",position =position_dodge2(preserve = "total")) + geom_text(aes(label=format(round(v,0),big.mark = ",")),position = position_dodge(width = 1),size=4.5,vjust=-0.2,color="white") +scale_fill_manual(values = c("#fdbb84","#e34a33")) + theme(panel.background = element_rect(fill = "#061f37"),panel.grid = element_blank(),legend.position = "top",legend.justification = c(0, 0),legend.text = element_text(size = 12,face = "bold"),axis.ticks.y=element_blank(),axis.ticks.x=element_line(color="red"),axis.text.x = element_text(size=13,face="bold"),axis.text.y = element_blank(),axis.title = element_blank()) + guides(fill=guide_legend(title="ANO"))
    cli_data_sales_plot + scale_x_datetime(date_breaks="month", date_labels="%b",expand = c(0, 0)) +scale_y_continuous(expand = c(0,0),limits = c(0,max(layer_data(cli_data_sales_plot)$y)*1.2)) + guides(fill=guide_legend(title="ANO"))     
  })
  
  # products ----
  
  output$plotprodvalue  <-renderPlot({
    
    d <- sales %>% filter(TIPO %in% c("F","P")) %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(VRVENDA)) %>% group_by(PRODUTO) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30)%>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(VRVENDA)),by= "PRODUTO") %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% select(PRODUTO,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(VRVENDA)),by= "PRODUTO") %>% .[,c(-3,-5)] %>% `colnames<-`(c("PRODUTO","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(TIPO %in% c("F","P")) %>%  filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(VRVENDA)) %>% group_by(PRODUTO) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PRODUTO=factor(PRODUTO,levels=rev(unique(o$PRODUTO))))
    
    
    p<-ggplot(d2,aes(x=PRODUTO,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57")) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) + scale_fill_gradient(low = "#28678F", high = "#DBA962")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
  })
  
  output$plotprodqtd  <-renderPlot({
    
    d <- sales %>% filter(TIPO %in% c("F","P")) %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(qtd=sum(QTD)) %>% group_by(PRODUTO) %>% summarize(m=round(sum(qtd)/3,0)) %>% arrange(desc(m)) %>% top_n(30)%>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(QTD)),by= "PRODUTO") %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% select(PRODUTO,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(v=sum(QTD)),by= "PRODUTO") %>% .[,c(-3,-5)] %>% `colnames<-`(c("PRODUTO","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(TIPO %in% c("F","P")) %>%  filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(PRODUTO,PEDDTEMIS,QTD) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),PRODUTO) %>% summarize(qtd=sum(QTD)) %>% group_by(PRODUTO) %>% summarize(m=sum(qtd)/3) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(PRODUTO=factor(PRODUTO,levels=rev(unique(o$PRODUTO))))
    
    
    p<-ggplot(d2,aes(x=PRODUTO,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57")) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + scale_fill_gradient(low = "#28678F", high = "#DBA962")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
    
  })
  
  output$plotprodmix  <-renderPlot({
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(PEDDTEMIS,CLIENTE,MIX,QTD,TIPO) %>% filter(!TIPO %in% c('M','T') & grepl(input$clientmix,CLIENTE)) %>% group_by(MIX) %>% summarize(qtd=sum(QTD)) %>% mutate(p=qtd/sum(qtd)) %>% arrange(desc(qtd))
    
    p <- ggplot(d,aes(x=reorder(MIX,p),y=p, fill=p)) + geom_bar(stat="identity") + geom_text(aes(label=percent(p)),hjust=-0.1,size=5) + coord_flip() + theme(legend.position = "none",axis.title = element_blank(),panel.grid = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_text(size = 10,hjust=0.95,vjust=0.2))
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) + scale_fill_gradient(low="#587272",high="#121f1f")
    
    
  })
  
  
  #cidades
  
  plotheight <- reactive({
    sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month") & CIDADE==input$cidades) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=round(sum(v)/3,0)) %>% arrange(desc(m))%>% select(CLIENTE) %>%  na.omit(.) %>% distinct()%>% split(.,.$CLIENTE) %>% length() %>% ifelse(.==0,1,.)
  })
  
  observe({output$plotcidadevalor <-renderPlot({
    
    d <- sales %>% filter(CIDADE==input$cidades) %>%  filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=round(sum(v)/3,0)) %>% arrange(desc(m))%>%  na.omit(.) %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% filter(CIDADE==input$cidades) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE")  %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% filter(CIDADE==input$cidades) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)),by= "CLIENTE") %>% .[,c(-3,-5)] %>% `colnames<-`(c("CLIENTE","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(CIDADE==input$cidades) %>%  filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CLIENTE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CLIENTE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CLIENTE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% as.data.frame() %>% na.omit(.)
    
    d2 <- d %>% mutate(CLIENTE=factor(CLIENTE,levels=rev(unique(o$CLIENTE))))
    
    
    p<-ggplot(d2,aes(x=CLIENTE,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS)),width =0.5) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57"),axis.ticks.x = element_blank()) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+scale_fill_gradient(low = "#288F55", high = "#DB5E53")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
    
  },height = plotheight()*70)
  
  })
  
  output$plotcidade <-renderPlot({
    d <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CIDADE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CIDADE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CIDADE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30)%>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date(),"month")) %>% select(CIDADE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CIDADE) %>% summarize(v=sum(VRVENDA)),by= "CIDADE") %>% left_join(sales %>% filter(floor_date(PEDDTEMIS,"year")==floor_date(Sys.Date()-years(1),"years") & floor_date(PEDDTEMIS,"month")==floor_date(Sys.Date()-years(1),"months") & floor_date(PEDDTEMIS,"day")<floor_date(Sys.Date()-years(1),"days")) %>% select(CIDADE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CIDADE) %>% summarize(v=sum(VRVENDA)),by= "CIDADE") %>% .[,c(-3,-5)] %>% `colnames<-`(c("CIDADE","MEDIA 3 MESES","MES ATUAL","MES ANO ANTERIOR")) %>% melt(.,variable.name="INDICADOR",value.name="VENDAS")
    
    o <- sales %>% filter(PEDDTEMIS>=floor_date(Sys.Date(),"month") %m-% months(3) & PEDDTEMIS<floor_date(Sys.Date(),"month")) %>% select(CIDADE,PEDDTEMIS,VRVENDA) %>% group_by(MES=floor_date(PEDDTEMIS,"month"),CIDADE) %>% summarize(v=sum(VRVENDA)) %>% group_by(CIDADE) %>% summarize(m=sum(v)/3) %>% arrange(desc(m)) %>% top_n(30) %>% as.data.frame() 
    
    d2 <- d %>% mutate(CIDADE=factor(CIDADE,levels=rev(unique(o$CIDADE))))
    
    
    p<-ggplot(d2,aes(x=CIDADE,y=VENDAS)) + geom_col(aes(fill = as.numeric(VENDAS))) + geom_text(aes(label=format(VENDAS,big.mark = ",")),size=5,hjust=-0.1,color="white") + coord_flip() + theme(axis.title = element_blank(),legend.position = "none",axis.text.x = element_blank(),axis.text = element_text(size = 11,face = "bold"),panel.background = element_rect(fill = "#061f37"),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(color = "#203c57"),axis.ticks.x = element_blank()) +facet_wrap(~INDICADOR)
    
    p + scale_y_continuous(expand = c(0,0),limits=c(0,max(layer_data(p)$y)*1.5)) +  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +scale_fill_gradient(low = "#288F55", high = "#DB5E53")  + theme(strip.background = element_rect(fill="#041729",color="white",  size=1, linetype="solid"),strip.text.x = element_text(size = 12, color = "white"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
