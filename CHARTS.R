library(tidyverse)


sql <- readLines("C:\\Users\\Repro\\Documents\\R\\ADM\\SHINY\\app\\SQL\\result.sql")
sql <- paste(sql, collapse = " ")


result <- dbGetQuery(con2,sql)


result <- result %>% group_by(SETOR) %>% 
            summarize(VRVENDA=sum(VRVENDA))

metas <- read.table(text = read_clip(), header = TRUE, sep = "\t") 


portuguese_month_names <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho",
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")



metas2 <-  metas %>%  melt(.,id.vars="SETORES") %>% 
    rename(MES=variable, VALOR=value) %>% 
  mutate(VALOR = gsub("[.]", "", VALOR)) %>%
    mutate(VALOR = as.numeric(VALOR)) %>% 
  mutate(MES = match(MES, portuguese_month_names)) %>% 
  mutate(MES =ymd(paste0("2023-", MES, "-01"))) %>% filter(month(MES) == month(Sys.Date()))


result2 <- cbind(result,metas2) %>% mutate(ALCANCE=(VRVENDA/VALOR)*100) %>% 
            mutate(SETOR=substr(SETOR,1, 8)
)

View(result2)

result %>% 
  ggplot(.,aes(x=PEDDTEMIS,y=VRVENDA,fill=SETOR)) + 
  geom_line(color="white") +
  geom_text(aes(label=format(VRVENDA,big.mark=",")),color="white") +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")



## BULLET CHART

name = c("Alex","Bob","David","Mike","Jess","Steve","Elina","Ethan","Jordan","Jim")
count= c(89,85,76,64,50,45,29,20,10,5)
data = data.frame(name, count, stringsAsFactors = TRUE)

data <- result2 %>% mutate(width = seq(.8, .1, length.out = nrow(result)))

bullet_base <- data.frame(rank = c("Poor", "Ok", "Good", "Excellent"),
                          value = c(20, 20, 20, 40))
bullet_base_rep <- 
  do.call("rbind", replicate(nrow(result2), bullet_base, simplify = FALSE)) %>%
  mutate(SETOR = sort(rep(result2$SETOR, 4) ))


bullet_colors <- c("#E9FFE3", "#A3D694", "#61AB40", "#318100")
names(bullet_colors) <- c("Poor", "Ok", "Good", "Excellent")

ggplot() +
  geom_bar(data = bullet_base_rep, 
           aes(x = SETOR, y = value, fill = rank), stat = "identity",
           position = "stack") +
  geom_bar(data = result2, 
           aes(x = SETOR, y = ALCANCE ), fill = "black", width = .2,
           stat = "identity") +
  scale_fill_manual(values = bullet_colors) +
  coord_flip(expand = FALSE)

