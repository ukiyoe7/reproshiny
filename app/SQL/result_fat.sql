

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),

       CLI AS (SELECT DISTINCT C.CLICODIGO,
                       CLINOMEFANT,
                        ENDCODIGO,
                         SETOR
                          FROM CLIEN C
                           LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                            INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO 
                                                              FROM ZONA 
                                                               WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                                                E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
                                                                 C.CLICODIGO=A.CLICODIGO
                                                                  WHERE CLICLIENTE='S'),

         PED AS (SELECT ID_PEDIDO,
                         P.CLICODIGO,
                          SETOR,
                           PEDDTBAIXA
                            FROM PEDID P
                             INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
                              INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                               WHERE
                                PEDDTBAIXA BETWEEN (CURRENT_DATE) - EXTRACT(DAY FROM (CURRENT_DATE)) + 1 AND 'YESTERDAY'
                                    AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))

        SELECT PEDDTBAIXA,
                SETOR,
                  SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                   FROM PDPRD PD
                    INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                     GROUP BY 1,2