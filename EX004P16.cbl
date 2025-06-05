      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         EX004P16.
       AUTHOR.                             GABRIEL.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
           SELECT MOVTOEST ASSIGN          TO MOVTOEST
           FILE STATUS IS FS-MOVTOEST
           .
           SELECT RELMOV01 ASSIGN          TO RELMOV01
           FILE STATUS IS FS-RELMOV01
           .
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *----------------------------------------------------------------*
       FD  MOVTOEST
           RECORDING MODE IS F
           .
       01  REG-MOVTOEST                    PIC X(33).

       FD  RELMOV01
           RECORDING MODE IS F
           .
       01  REG-RELMOV01                      PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *    CABECALHO DO RELATORIO
      *----------------------------------------------------------------*
       01  WS-CABEC1.
           05 WS-DATA-CABEC1               PIC X(10).
           05 FILLER                       PIC X(17)
                                                   VALUE SPACES.

           05 FILLER                       PIC X(27)
                                                    VALUE
      *        123456789012345678901234567
              "** BOX COMPANY DO BRASIL **".
           05 FILLER                       PIC X(18)
                                                   VALUE SPACES.
           05 WS-HORA-CABEC1               PIC X(08).                         
           
       01  WS-CABEC2.                       
           05 FILLER                       PIC X(40)
                                                   VALUE
      *        1234567890123456789012345678901234567890                       
              "RELATORIO DE MOVIMENTACAO DE ESTOQUE (RE".
           05 FILLER                       PIC X(09)
                                                   VALUE
      *        1234567890123456789012345678901234567890                     
              "FERENCIA ". 
           05 WS-REF1-CABEC2               PIC 9(04).
           05 FILLER                       PIC X(01)
                                                   VALUE "/".
           05 WS-REF2-CABEC2               PIC 99.
           05 FILLER                       PIC X(01)
                                                   VALUE ")".
           05 FILLER                       PIC X(13)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(05)
                                                   VALUE 
      *        1234567890123456789012345678901234567890
               "PAG. ".
           05 WS-PAG-CABEC2                PIC Z.ZZ9.  
       01  WS-CABEC3.
           05 FILLER                       PIC X(80)
                                                   VALUE ALL "-".
       01  WS-CABEC4.
           05 FILLER                       PIC X(40)
                                                   VALUE
      *        1234567890123456789012345678901234567890                        
              "         NUMERO        DATA             ".
           05 FILLER                       PIC X(40)
                                                   VALUE
              "HORA       PRODUTO     QUANTIDADE       ".
       01  WS-CABEC5.
           05 FILLER                       PIC X(40)
                                                   VALUE
      *        1234567890123456789012345678901234567890 
              "         ----------    -----------      ".
           05 FILLER                       PIC X(40)
                                                   VALUE
      *        1234567890123456789012345678901234567890 
              "--------   -------     ------------     ".
      *----------------------------------------------------------------*
      *                          DETALHE                               *     
      *----------------------------------------------------------------*
       01  WS-LINDET.
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
           05 LD-NUMMOVTO                  PIC 99.999.999.
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 LD-DATAMOVTO                 PIC X(10).
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 LD-HORAMOVTO                 PIC X(08).
           05 FILLER                       PIC X(03)
                                                   VALUE SPACES.
           05 LD-CODPRODMOVTO              PIC 9999.
           05 FILLER                       PIC X(03)
                                                   VALUE SPACES.
           05 LD-QTDMOVTO                  PIC ZZ.ZZZ.ZZ9+.
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.   
      *----------------------------------------------------------------*
      *                          RODAPÉ                                *     
      *----------------------------------------------------------------*
       01  WS-RODAPE1                      PIC X(80)
                                                   VALUE ALL "-".             
       01  WS-RODAPE2.
           05 FILLER                       PIC X(40)
                                                   VALUE
      *        1234567890123456789012345678901234567890
              "APOS O USO UTILIZE ESTE PAPEL COMO RASCU".
           05 FILLER                       PIC X(40)
                                                   VALUE
              "NHO                  RECICLE SUAS IDEIAS".
      *----------------------------------------------------------------*
      *                 VARIAVEIS ESPELHO                              *     
      *----------------------------------------------------------------*
           COPY BMOVTOES.
           COPY VARDATA.
           COPY VARPROSS.
           COPY CPMOV.
      *----------------------------------------------------------------*
      *                 VARIAVEIS DE TRABALHO                          *     
      *----------------------------------------------------------------*
       01  WS-DATA-SYS.
           05 WS-ANO-SYS                   PIC 99.
           05 WS-MES-SYS                   PIC 99.
           05 WS-DIA-SYS                   PIC 99.
       
       01  WS-HORA-SYS.
           05 WS-HORA                      PIC 99.
           05 WS-MIN                       PIC 99.
           05 WS-SEG                       PIC 99.
       
       01  WS-DATA-FORMATADA.
           05 WS-DIA-FT                    PIC 9(02).
           05 FILLER                       PIC X(01)
                                                   VALUE "/".
           05 WS-MES-FT                    PIC 9(02).
           05 FILLER                       PIC X(01)
                                                   VALUE "/".
           05 WS-ANO-FT                    PIC 99.

       01  WS-HORA-FORMATADA.
           05 WS-HORA-FT                   PIC 9(02).
           05 FILLER                       PIC X(01)
                                                   VALUE ":".
           05 WS-MIN-FT                    PIC 9(02).
           05 FILLER                       PIC X(01)
                                                   VALUE ":".
           05 WS-SEG-FT                    PIC 9(02).

      *VARIAVEIS DE FILE STATUS
       01  FS-MOVTOEST                     PIC X(02).
           88 SUCESSO-M                    VALUE '00'.
           88 FIM-ARQUIVO-M                VALUE '10'.
       01  FS-RELMOV01                     PIC X(02).
           88 SUCESSO-R                    VALUE '00'.
           88 FIM-ARQUIVO-R                VALUE '10'.
       77  WS-MSG                          PIC X(60).
       77  WS-FS                           PIC X(02).
      *VARIAVEIS DE CONTAGEM
       77  WS-CTLIDO                       PIC 9(05).
       77  WS-CTIMPRESSO                   PIC 9(05).
      *VARIAVEIS DE CONTROLE DO RELATÓRIO
       77  WS-CTLIN                        PIC 9(02).
       77  WS-CTPAG                        PIC 9(02).
       77  WS-PULA                         PIC 9(02).

      *VARIAVEL AUXILIAR DE FORMATACAO DATA
       01  WS-DATA-FORMATADA-AUX.
           05 WS-ANO-REF-CAD               PIC 9(04).
           05 WS-MES-REF-CAD               PIC 9(02).

      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-EX004P16.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
               UNTIL WS-TIPO-REG-MOVTO = 'T'
           PERFORM 3000-TERMINO
           GOBACK
           .
      *
       1000-INICIALIZAR.
           ACCEPT WS-HORA-SYS              FROM TIME
           ACCEPT WS-HORARIO-INICIAL       FROM TIME
           ACCEPT WS-DATA-SYS              FROM DATE

           MOVE ZEROS                      TO WS-CTLIDO
                                              WS-CTPAG
                                              WS-CTIMPRESSO
           MOVE 99                         TO WS-CTLIN    

           OPEN INPUT MOVTOEST
           IF NOT SUCESSO-M
              MOVE "ERRO NA ABERTURA DE MOVTOEST" TO WS-MSG
              MOVE FS-MOVTOEST             TO WS-FS
              GO TO 9000-ERRO
           END-IF

           PERFORM 1100-LER-MOVTOEST
           IF WS-TIPO-REG-MOVTO NOT = "H"
              MOVE "ARQUIVO SEM HEADER"    TO WS-MSG
              MOVE FS-MOVTOEST             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           MOVE WS-ANO-MES-REF-CAD         TO WS-ANO-MES-REF-CAD-AUX

           OPEN OUTPUT RELMOV01
           IF NOT SUCESSO-R
              MOVE "ERRO NA ABERTURA DE RELMOV01" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           PERFORM 1100-LER-MOVTOEST
           IF WS-TIPO-REG-MOVTO NOT = "D"
              MOVE "ARQUIVO SEM DATA"      TO WS-MSG
              MOVE FS-MOVTOEST             TO WS-FS                
              GO TO 9000-ERRO
           END-IF
           PERFORM 9000-FORMATA-DATA-HORA
           .      
       1100-LER-MOVTOEST.
           READ MOVTOEST                   INTO WS-REG-MOVTOEST
           IF SUCESSO-M
              ADD 1                        TO WS-CTLIDO          
           ELSE
              IF FIM-ARQUIVO-M
                 IF WS-CTLIDO > 0
                 MOVE "ARQUIVO NAO TEM TRAILER" TO WS-MSG
                 MOVE FS-MOVTOEST               TO WS-FS
                 GO TO 9000-ERRO
                 END-IF
              ELSE
                 MOVE "ERRO DE LEITURA"       TO WS-MSG
                 MOVE FS-MOVTOEST             TO WS-FS
                 GO TO 9000-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           IF WS-CTLIN > 49
              PERFORM 2100-IMPRIMIR-CABECALHO
           END-IF

           PERFORM 2200-IMPRIMIR-DETALHE

           IF WS-CTLIN = 48
              PERFORM 2300-IMPRIMIR-RODAPE
           
           PERFORM 1100-LER-MOVTOEST
           .
       2100-IMPRIMIR-CABECALHO.
           MOVE WS-DIA                     TO WS-DIA-F
           MOVE WS-MES                     TO WS-MES-F
           MOVE WS-ANO                     TO WS-ANO-F
           MOVE WS-HORA                    TO WS-HORA-F
           MOVE WS-MIN                     TO WS-MIN-F
           MOVE WS-SEG                     TO WS-SEG-F 
           MOVE WS-DATA-FORMATADA          TO WS-DATA-CABEC1
           MOVE WS-HORA-FORMATADA          TO WS-HORA-CABEC1
           MOVE WS-ANO-REF-CAD             TO WS-REF1-CABEC2
           MOVE WS-MES-REF-CAD             TO WS-REF2-CABEC2
           ADD 1                           TO WS-CTPAG
           MOVE WS-CTPAG                   TO WS-PAG-CABEC2

           WRITE RELMOV01                  FROM WS-CABEC1 AFTER PAGE 
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR CABECALHO1" TO WS-MSG 
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF

           WRITE RELMOV01                  FROM WS-CABEC2
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR CABECALHO2" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF

           WRITE RELMOV01                  FROM WS-CABEC3
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR CABECALHO3" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
                 
           WRITE RELMOV01                  FROM WS-CABEC4
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR CABECALHO4" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF

           WRITE RELMOV01                  FROM WS-CABEC5
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR CABECALHO5" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           MOVE 5                          TO WS-CTLIN
           .
       2200-IMPRIMIR-DETALHE.
           MOVE WS-NUMMOVTO                TO LD-NUMMOVTO
           MOVE WS-DATAMOVTO               TO LD-DATAMOVTO
           MOVE WS-HORAMOVTO               TO LD-HORAMOVTO
           MOVE WS-CODPRODMOVTO            TO LD-CODPRODMOVTO

           IF WS-TIPOMOVTO = 'E'
              MOVE WS-QTDMOVTO             TO LD-QTDMOVTO
           ELSE
              MULTIPLY WS-QTDMOVTO BY -1 GIVING LD-QTDMOVTO
           END-IF

           WRITE RELMOV01                  FROM WS-LINDET
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR DETALHE" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           ADD 1                           TO WS-CTLIN
           ADD 1                           TO WS-CTIMPRESSO
           .
       2300-IMPRIMIR-RODAPE.
           COMPUTE WS-PULA = WS-CTLIN - 48
           WRITE RELMOV01         FROM WS-RODAPE1 AFTER WS-PULA LINES
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR RODAPE1" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           WRITE RELMOV01         FROM WS-RODAPE2
           IF NOT SUCESSO-R
              MOVE "ERRO AO GRAVAR RODAPE2" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           MOVE 50                         TO WS-CTLIN
           .
       3000-TERMINO.
           PERFORM 9000-IMPRIME-DATA

           IF WS-CTLIN < 50
              PERFORM 2300-IMPRIMIR-RODAPE
           END-IF

           PERFORM 4000-FECHAMENTO-ARQUIVOS
           IF WS-QTDREG-MOVTOEST NOT EQUAL WS-CTLIN
              MOVE 12                      TO RETURN-CODE
              STOP RUN 
           END-IF

           ACCEPT WS-HORA-HORARIO-FINAL    FROM TIME
           PERFORM 9000-TEMPO-DE-PROCESSAMENTO           
           PERFORM 5000-EXIBIR-RESULTADOS
           .
       5000-EXIBIR-RESULTADOS.
           DISPLAY "==================================================="
           DISPLAY "            BOX COMPANY DO BRASIL                  "
           DISPLAY "==================================================="
           DISPLAY " TOTAL DE MOVIMENTOS LIDOS.....: " WS-CTLIDO
           DISPLAY " TOTAL DE PAGINAS IMPRESSAS....: " WS-CTPAG
           DISPLAY " TOTAL DE MOVIMENTOS IMPRESSOS.: " WS-CTIMPRESSO
           DISPLAY "==================================================="
           DISPLAY " TEMPO TOTAL DE PROCESSAMENTO  : " 
                                               WS-TEMPO-PROCESSAMENTO-F
           DISPLAY "==================================================="
           .
       4000-FECHAMENTO-ARQUIVOS.
           IF WRK-DEBUG-SIM
              DISPLAY "4000-FECHAMENTO-ARQUIVOS"
           END-IF
      *FECHAMENTO DO MOVTOEST
           CLOSE MOVTOEST     
           IF FS-MOVTOEST NOT = '00'
              MOVE "ERRO NO FECHAMENTO DE MOVTOEST" TO WS-MSG
              MOVE FS-MOVTOEST             TO WS-FS
              GO TO 9000-ERRO
           END-IF
      *FECHAMENTO DO RELMOV01
           CLOSE RELMOV01
           IF FS-RELMOV01 NOT = '00'
              MOVE "ERRO NO FECHAMENTO RELMOV01" TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
           COPY ROTERRO.
           COPY ROTPROSS.
           COPY ROTDATA.