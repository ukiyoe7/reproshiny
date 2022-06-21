library(dplyr)
library(DBI)


con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)


#CURRENT MONTH

dbGetQuery(con2,"

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),


     CLI AS (SELECT C.CLICODIGO, 
                      CLINOMEFANT,
                       GCLCODIGO GRUPO, 
                        SETOR 
                         FROM CLIEN C
                          INNER JOIN (SELECT CLICODIGO, ZODESCRICAO SETOR FROM ENDCLI E
                           INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA)Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') ED ON C.CLICODIGO=ED.CLICODIGO
                            WHERE CLICLIENTE='S'),

     PED AS (SELECT ID_PEDIDO,
                     PEDDTBAIXA,
                       P.CLICODIGO,
                        PEDORIGEM
                        FROM PEDID P
                         INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                          LEFT JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
                           WHERE PEDDTBAIXA BETWEEN  '01.06.2022' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))
                                   
     SELECT PEDDTBAIXA,
             CLICODIGO,
              PEDORIGEM,
               SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
               FROM PDPRD PD
                INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                 GROUP BY 1,2,3") %>% View()






