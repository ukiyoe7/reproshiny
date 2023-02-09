library(tidyverse)
library(lubridate)
library(DBI)
library(clipr)
library(readxl)
library(reshape2)
library(scales)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


## CONNECTION

sql <- readLines("C:\\Users\\Repro\\Documents\\R\\ADM\\SHINY\\app\\SQL\\result.sql")
sql <- paste(sql, collapse = " ")
result <- dbGetQuery(con2,sql)

## RESULT

result2 <-
result %>% group_by(SETOR) %>% summarize(VRVENDA=sum(VRVENDA))

## METAS

metas <- read.table(text = read_clip(), header = TRUE, sep = "\t") 


portuguese_month_names <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho",
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")


metas2 <-  metas %>%  melt(.,id.vars="SETOR") %>% 
    rename(MES=variable, META=value) %>% 
  
  mutate(MES = match(MES, portuguese_month_names)) %>% 
  mutate(MES =ymd(paste0("2023-", MES, "-01"))) %>% filter(month(MES) == month(Sys.Date())) %>% 

mutate(META = gsub("[.]", "", META)) %>%
  mutate(META = as.numeric(META)) %>%


## ALCANCE

result3 <- left_join(result2,metas2,by="SETOR") %>%
              mutate(ALCANCE=(VRVENDA/META)*100) %>% 
                mutate(SETOR=substr(SETOR,1, 8))
                  
View(result3)

# CHART

result3 %>% 
  ggplot(.,aes(x=SETOR,y=VRVENDA)) + 
  geom_line(color="#ffc3a0") +
  geom_text(aes(label=format(VRVENDA,big.mark=",")),color="white") +
  scale_x_datetime(date_breaks = "day",date_labels = "%d/%m") +
  theme(panel.background = element_rect(fill = "#0c1839"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#15295f"),
        legend.position = "top")

## BULLET CHART


data <- result3 %>% mutate(width = seq(.8, .1, length.out = nrow(result)))

bullet_base <- data.frame(rank = c("Ruim", "Regular", "Bom", "Otimo"),
                          value = c(50, 30, 0.5, 18.5))
bullet_base_rep <- 
  do.call("rbind", replicate(nrow(result3), bullet_base, simplify = FALSE)) %>%
  mutate(SETOR = sort(rep(result3$SETOR, 4) ))


bullet_colors <- c("#ebe7e7", "#e5dfe0", "#000000","#dfd7d8")
names(bullet_colors) <- c("Ruim", "Regular", "Bom", "Otimo")

ggplot() +
  geom_bar(data = bullet_base_rep, 
           aes(x =fct_rev(SETOR), y = value,fill = rank,width = .7), stat = "identity",
           position = "stack") +
  geom_bar(data = result3, 
           aes(x = fct_rev(SETOR), y = ALCANCE ), fill = "#1974d2", width = .5,
           stat = "identity") + 
  geom_text(data = result3, aes(x = fct_rev(SETOR), y = ALCANCE, label = percent(round(ALCANCE,1)/100)), 
            position = position_dodge(width = .7), vjust = 0.5, hjust = -0.1,size=5) +
  scale_fill_manual(values = bullet_colors) +
  
  coord_flip(expand = FALSE) + theme(axis.title.y=element_blank(),
                                     axis.text.y = element_text(size = 13),legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



