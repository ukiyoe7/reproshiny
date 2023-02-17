library(tidyverse)
library(lubridate)
library(DBI)
library(clipr)
library(readxl)
library(reshape2)
library(scales)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## METAS

## metas <- read.table(text = read_clip(), header = TRUE, sep = "\t") 
## save(metas,file = "app/metas2023.RData")

metas2023 <- get(load(file = "app/metas2023.RData"))


## CONNECTION

sql <- readLines("C:\\Users\\Repro\\Documents\\R\\ADM\\SHINY\\app\\SQL\\result.sql")
sql <- paste(sql, collapse = " ")
result <- dbGetQuery(con2,sql)

## RESULT

result2 <-
result %>% group_by(SETOR) %>% summarize(VRVENDA=sum(VRVENDA))


portuguese_month_names <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho",
                            "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")

## METAS
metas2 <-  metas2023 %>%  melt(.,id.vars="SETOR") %>% 
    rename(MES=variable, META=value) %>% 
  
  mutate(MES = match(MES, portuguese_month_names)) %>% 
  mutate(MES =ymd(paste0("2023-", MES, "-01"))) %>% filter(month(MES) == month(Sys.Date())) %>% 
mutate(META = gsub("[.]", "", META)) %>%
  mutate(META = as.numeric(META)) 

## ESPERADO

first_day <- floor_date(Sys.Date(), unit = "month")
num_days <- as.numeric(difftime(Sys.Date(), first_day, units = "days"))


num_days <- as.numeric(days_in_month(first_day))



num_weekends <- sum(wday(seq(first_day, ceiling_date(Sys.Date(),unit = "month")-1, by = "day")) %in% c(1,7))
num_days = num_days - num_weekends -2

num_days_2 <- as.numeric(difftime(Sys.Date(), first_day, units = "days"))
num_weekends_2 <- sum(wday(seq(first_day, Sys.Date(), by = "day")) %in% c(1,7))
num_days_2 = num_days_2 - num_weekends_2

## ALCANCE

result3 <- left_join(result2,metas2,by="SETOR") %>%
              mutate(ALCANCE=(VRVENDA/META)*100) %>% 
                mutate(SETOR=substr(SETOR,1, 8)) %>% 
                  mutate(ESPERADO=((META/num_days)*num_days_2/META)*100)
                  
View(result3)



## BULLET CHART



bullet_base <- data.frame(rank = c("Ruim", "Regular","ok" ,"Bom", "Otimo"),
                          value = c(50,20,10,0.5,20))
bullet_base_rep <- 
  do.call("rbind", replicate(nrow(result3), bullet_base, simplify = FALSE)) %>%
  mutate(SETOR = sort(rep(result3$SETOR, 5) ))


bullet_colors <- c("#c3c3c3", "#b4b4b4","#8995a2","#7c0a02","#b4b4b4")
names(bullet_colors) <- c("Ruim", "Regular","ok" ,"Bom", "Otimo")

colors <- c("#560701", "#7c0a02")

ggplot() +
  geom_bar(data = bullet_base_rep, 
           aes(x =fct_rev(SETOR), y = value,fill = rank,width = .7), stat = "identity",
           position = "stack") +
  
  geom_bar(data = result3, 
           aes(x = fct_rev(SETOR), y = ESPERADO ,  fill = "ESPERADO"), width = .7,
           stat = "identity") + 
  
  geom_bar(data = result3, 
           aes(x = fct_rev(SETOR), y = ALCANCE , fill = "ALCANCE"), width = .55,
           stat = "identity") + 
  

  
  geom_text(data = result3, aes(x = fct_rev(SETOR), y = ALCANCE, label = percent(round(ALCANCE,1)/100)), 
            position = position_dodge(width = .7), vjust = 0.5, hjust = -0.1,size=4.5) +
  scale_fill_manual(values = c(bullet_colors, ESPERADO = "#f4d27a", ALCANCE = "#1974d2"), 
                    breaks = c("ESPERADO", "ALCANCE")) +
  coord_flip(expand = FALSE) + theme(axis.title.y=element_blank(),
                                     axis.text.y = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 13)) +
  
  theme(legend.position = "top", # adjust the legend position
        legend.text = element_text(size = 14), # increase the font size of legend text
        legend.title = element_blank(), # remove the legend title
        axis.title.y = element_blank(), axis.text.y = element_text(size = 15))+ 
  theme(
        axis.text.y = element_text(size = 13))  +
  
  scale_y_continuous(breaks=c(0,50,90,100))
  
## FATURADO





