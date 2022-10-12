

df <- dbGetQuery(con2, statement = read_file('SALES.sql'))

View(df)

ggplot(df,aes(x=SETOR,y=VRVENDA,fill=Segment)) + 
  geom_bar(stat = "identity") + coord_flip() +  
   geom_text(aes(label=percent(round(value,4))),position=position_stack(vjust = 0.5)) + 
    scale_y_continuous(expand = c(0,0),labels = scales::percent) + 
     scale_x_discrete() + theme(legend.position = "top",
                                axis.title.x = element_blank(),
                                 axis.text.y = element_text(size = 12,face = "bold"),
                                  axis.text.x = element_text(size = 10),
                                   panel.grid = element_blank(),
                                    panel.background = element_rect(fill= "white"),
                                     axis.title.y = element_blank(),
                                      axis.line.x = element_line(colour = "#969393")) + 
                                       scale_fill_manual(values = c("#b3cde0","#fce9db","#b2d8d8")) + 
                                         guides(fill = guide_legend(reverse = TRUE))