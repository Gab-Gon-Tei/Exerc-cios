      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         EX004P11.
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
           SELECT CLIOLD ASSIGN          TO UT-S-CLIOLD
           FILE STATUS IS FS-CLIOLD
           .
           SELECT CLIMOV ASSIGN          TO UT-S-CLIMOV
           FILE STATUS IS FS-CLIMOV
           .
           SELECT CLINEW ASSIGN          TO UT-S-CLINEW
           FILE STATUS IS FS-CLINEW
           .
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *----------------------------------------------------------------*
       FD  CLIOLD
           RECORDING MODE IS F
           .
           COPY CPCLIOLD.
       FD  CLIMOV
           RECORDING MODE IS F
           .
           COPY CPCLIMOV.
       FD  CLINEW
           RECORDING MODE IS F
           .
           COPY CPCLINEW.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *              VARIAVEIS DEBUG
      *----------------------------------------------------------------*
      * FLAGS
       01  WRK-FLAGS-SWITCHES.
           05  WRK-DEBUG                   PIC  X(01)  VALUE 'S'.
               88  WRK-DEBUG-NAO                       VALUE 'N'.
               88  WRK-DEBUG-SIM                       VALUE 'S'.
      *----------------------------------------------------------------*
      *    VARIAVEIS DE TRABALHO
      *----------------------------------------------------------------*
       77  WS-CTLIDO-O                     PIC 9(04) COMP.
       77  WS-CTLIDO-M                     PIC 9(04) COMP.
       77  WS-CTGRAV                       PIC 9(04) COMP.
       77  WS-CTINC                        PIC 9(04) COMP.
       77  WS-CTEXC                        PIC 9(04) COMP.
       77  WS-CTALT                        PIC 9(04) COMP.
       77  WS-CTINV                        PIC 9(04) COMP.
       77  WS-CTPERM                       PIC 9(04) COMP.               
      *----------------------------------------------------------------*
      *    CONTADORES FORMATADOS
      *----------------------------------------------------------------*
       77  WS-CTLIDO-O-F                   PIC ZZZ9.
       77  WS-CTLIDO-M-F                   PIC ZZZ9.
       77  WS-CTGRAV-F                     PIC ZZZ9.
       77  WS-CTINC-F                      PIC ZZZ9.
       77  WS-CTEXC-F                      PIC ZZZ9.
       77  WS-CTALT-F                      PIC ZZZ9.
       77  WS-CTINV-F                      PIC ZZZ9.
       77  WS-CTPERM-F                     PIC ZZZ9.
      *----------------------------------------------------------------*
      *              VARIAVEIS ESPELHO 
      *----------------------------------------------------------------*
       01  WS-REG-CLIOLD.                                                
           05 WS-CODCLI-O                  PIC X(04).
           05 WS-NOMECLI-O                 PIC X(25). 
           05 WS-ENDCLI-O                  PIC X(30).
           05 WS-FONECLI-O                 PIC X(10).
           05 WS-TOTALDIVIDA-O             PIC 9(08)V99.
      *----------------------------------------------------------------*
       01  WS-REG-CLIMOV.                                                
           05 WS-CODCLI-M                  PIC X(04).
           05 WS-NOMECLI-M                 PIC X(25). 
           05 WS-ENDCLI-M                  PIC X(30).
           05 WS-FONECLI-M                 PIC X(10).
           05 WS-TOTALDIVIDA-M             PIC 9(08)V99.
           05 WS-TIPOMOV-M                 PIC X(01).
      *----------------------------------------------------------------*
       01  WS-REG-CLINEW.                                                
           05 WS-CODCLI-N                  PIC X(04).
           05 WS-NOMECLI-N                 PIC X(25). 
           05 WS-ENDCLI-N                  PIC X(30).
           05 WS-FONECLI-N                 PIC X(10).
           05 WS-TOTALDIVIDA-N             PIC 9(08)V99.
      *----------------------------------------------------------------*    
      *VARIAVEIS DE FILE STATUS
      *----------------------------------------------------------------*
       01  FS-CLIOLD                     PIC X(02).
           88 SUCESSO-O                  VALUE '00'.
           88 FIM-CLIOLD                 VALUE '10'.

       01  FS-CLIMOV                     PIC X(02).
           88 SUCESSO-M                  VALUE '00'.
           88 FIM-CLIMOV                 VALUE '10'.

       01  FS-CLINEW                     PIC X(02).
           88 SUCESSO-N                  VALUE '00'.
           88 FIM-CLINEW                 VALUE '10'.
      *----------------------------------------------------------------*
      *DECLARACAO DE VARIAVEIS DE DATA E TEMPO DE PROCESSAMENTO
      *----------------------------------------------------------------*
           COPY VARDATA.
           COPY VARPROSS.
      *----------------------------------------------------------------*
      * MENSAGENS DE ERRO
      *----------------------------------------------------------------*
       77  WS-MSG                          PIC X(60).
       77  WS-FS                           PIC X(02).

       77  WS-MSG01                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA CLIOLD".
       77  WS-MSG02                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA CLIMOV".
       77  WS-MSG03                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA CLINEW".
       77  WS-MSG04                        PIC X(60)
                                                   VALUE
           "ERRO DE LEITURA CLIOLD".
       77  WS-MSG05                        PIC X(60)
                                                   VALUE
           "ERRO DE LEITURA CLIMOV".
       77  WS-MSG06                        PIC X(60)
                                                   VALUE
           "ERRO DE GRAVACAO CLINEW".
       77  WS-MSG07                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO CLIOLD".
       77  WS-MSG08                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO CLIMOV".
       77  WS-MSG09                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO CLINEW".
      *----------------------------------------------------------------*
      * VARIAVEIS DE CHAMADA
      *----------------------------------------------------------------*
       77  WS-PROGRAMA                     PIC X(08) 
                                                   VALUE "PGMAUX02".
       01  WS-DADOS-ENVIADOS.
           05 WS-TOTALDIVIDA                  PIC 9(08)V99.
           05 WS-RESP                         PIC X(01).
           05 WS-DIVIDACALC                   PIC 9(08)V99.
      *----------------------------------------------------------------* 
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-EX004P11.
           IF WRK-DEBUG-SIM
              DISPLAY "0000-EX004P11"
           END-IF
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
               UNTIL FIM-CLIOLD
               AND   FIM-CLIMOV
           PERFORM 3000-TERMINO
           GOBACK
           .
      *
       1000-INICIALIZAR.
           IF WRK-DEBUG-SIM
              DISPLAY "1000-INICIALIZAR"
           END-IF
           ACCEPT WS-HORARIO-INICIAL FROM TIME
           MOVE ZEROS                      TO WS-CTLIDO-O
                                              WS-CTLIDO-M
                                              WS-CTGRAV
                                              WS-CTINC
                                              WS-CTEXC
                                              WS-CTALT
                                              WS-CTINV
                                              WS-CTPERM    
           PERFORM 1050-ABERTURA-ARQUIVOS
           PERFORM 1100-LER-CLIOLD
           IF FIM-CLIOLD
              MOVE WS-MSG04                TO WS-MSG
              MOVE FS-CLIOLD               TO WS-FS
              GO TO 9000-ERRO
           END-IF
           PERFORM 1200-LER-CLIMOV
           IF FIM-CLIMOV
              MOVE WS-MSG05                TO WS-MSG
              MOVE FS-CLIMOV               TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
       1050-ABERTURA-ARQUIVOS.
           IF WRK-DEBUG-SIM
              DISPLAY "1050-ABERTURA-ARQUIVOS"
           END-IF
           OPEN INPUT CLIOLD
           IF NOT SUCESSO-O
              MOVE WS-MSG01                TO WS-MSG
              MOVE FS-CLIOLD             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           OPEN INPUT CLIMOV
           IF NOT SUCESSO-M
              MOVE WS-MSG02                TO WS-MSG
              MOVE FS-CLIMOV             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           OPEN OUTPUT CLINEW
           IF NOT SUCESSO-N
              MOVE WS-MSG03                TO WS-MSG
              MOVE FS-CLINEW             TO WS-FS
              GO TO 9000-ERRO
           END-IF

           .      
       1100-LER-CLIOLD.
           IF WRK-DEBUG-SIM
              DISPLAY "1100-LER-CLIOLD"
           END-IF.
           READ CLIOLD                     INTO WS-REG-CLIOLD
           IF SUCESSO-O
              ADD 1                        TO WS-CTLIDO-O
           ELSE
              IF FIM-CLIOLD
                 MOVE HIGH-VALUES             TO WS-CODCLI-O
              ELSE
                 MOVE WS-MSG04                TO WS-MSG
                 MOVE FS-CLIOLD               TO WS-FS
                 GO TO 9000-ERRO
              END-IF
           END-IF
           .
       1200-LER-CLIMOV.
           IF WRK-DEBUG-SIM
              DISPLAY "1200-LER-CLIMOV"
           END-IF.
           READ CLIMOV                     INTO WS-REG-CLIMOV
           IF SUCESSO-O
              ADD 1                        TO WS-CTLIDO-M
           ELSE
              IF FIM-CLIMOV
                 MOVE HIGH-VALUES          TO WS-CODCLI-M
              ELSE
                 MOVE WS-MSG05             TO WS-MSG
                 MOVE FS-CLIMOV            TO WS-FS
                 GO TO 9000-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           IF WRK-DEBUG-SIM
              DISPLAY "2000-PROCESSAR"
           END-IF
           IF WS-CODCLI-O < WS-CODCLI-M
              ADD 1                        TO WS-CTINV
              PERFORM 1100-LER-CLIOLD
           ELSE
              IF WS-CODCLI-O > WS-CODCLI-M
                 PERFORM 2100-INCLUIR
                 PERFORM 1200-LER-CLIMOV
              ELSE
                 PERFORM 2200-ALT-EXC
                 PERFORM 1100-LER-CLIOLD
                 PERFORM 1200-LER-CLIMOV
              END-IF
           END-IF
           .
       2100-INCLUIR.
           IF WRK-DEBUG-SIM
              DISPLAY "2300-INCLUIR"
           END-IF
           IF WS-TIPOMOV-M = 'I'
              PERFORM 2400-GRAVA-M
              ADD 1                        TO WS-CTINC
           ELSE
              ADD 1                        TO WS-CTINV
           END-IF
           .
       2200-ALT-EXC.
           IF WRK-DEBUG-SIM
              DISPLAY "2200-ALT-EXC"
           END-IF
           IF WS-TIPOMOV-M = "A"
              PERFORM 2300-ALTERAR
           ELSE
              IF WS-TIPOMOV-M = "E"
                 ADD 1                     TO WS-CTEXC
              ELSE
                 ADD 1                     TO WS-CTINV
              END-IF
           END-IF
           .
       2300-ALTERAR.
           IF WRK-DEBUG-SIM
              DISPLAY "2300-ALTERAR"
           END-IF
           MOVE WS-TOTALDIVIDA-O           TO WS-TOTALDIVIDA
           CALL WS-PROGRAMA                USING WS-TOTALDIVIDA
                                                 WS-RESP
                                                 WS-DIVIDACALC
           CANCEL WS-PROGRAMA
           IF WS-RESP = "0"
              MOVE WS-DIVIDACALC           TO WS-TOTALDIVIDA-O
              ADD 1                        TO WS-CTALT
              PERFORM 2400-GRAVA-M
              MOVE SPACES                  TO WS-RESP
           ELSE
              ADD 1                        TO WS-CTINV
                                              WS-CTPERM
           .
       2400-GRAVA-M.
           IF WRK-DEBUG-SIM
              DISPLAY "2400-GRAVA-M"
           END-IF
           MOVE WS-CODCLI-M                TO WS-CODCLI-N
           IF WS-NOMECLI-M NOT EQUAL SPACES
              MOVE WS-NOMECLI-M            TO WS-NOMECLI-N
           ELSE
              MOVE WS-NOMECLI-O            TO WS-NOMECLI-N
           END-IF
           IF WS-ENDCLI-M NOT EQUAL SPACES
              MOVE WS-ENDCLI-M             TO WS-ENDCLI-N
           ELSE
              MOVE WS-ENDCLI-O             TO WS-ENDCLI-N
           END-IF
           IF WS-FONECLI-M NOT EQUAL SPACES
              MOVE WS-FONECLI-M            TO WS-FONECLI-N
           ELSE
              MOVE WS-FONECLI-O            TO WS-FONECLI-N
           END-IF
           IF WS-TOTALDIVIDA-M IS NUMERIC
              MOVE WS-TOTALDIVIDA-M        TO WS-TOTALDIVIDA-N
           ELSE
              MOVE WS-TOTALDIVIDA-O        TO WS-TOTALDIVIDA-N
           END-IF
           WRITE REG-CLINEW                FROM WS-REG-CLINEW
           IF NOT SUCESSO-N
              MOVE WS-MSG06                TO WS-MSG
              MOVE FS-CLINEW               TO WS-FS
              GO TO 9000-ERRO
           ELSE 
              ADD 1                        TO WS-CTGRAV 
           END-IF
           .
       3000-TERMINO.
           IF WRK-DEBUG-SIM
              DISPLAY "3000-TERMINO"
           END-IF
           PERFORM 9000-IMPRIME-DATA
           PERFORM 4000-FECHAMENTO-ARQUIVOS
           ACCEPT WS-HORARIO-FINAL         FROM TIME
           PERFORM 9000-TEMPO-DE-PROCESSAMENTO

           MOVE WS-CTLIDO-O                TO WS-CTLIDO-O-F
           MOVE WS-CTLIDO-M                TO WS-CTLIDO-M-F
           MOVE WS-CTGRAV                  TO WS-CTGRAV-F
           MOVE WS-CTINC                   TO WS-CTINC-F
           MOVE WS-CTEXC                   TO WS-CTEXC-F
           MOVE WS-CTALT                   TO WS-CTALT-F
           MOVE WS-CTINV                   TO WS-CTINV-F
           MOVE WS-CTPERM                  TO WS-CTPERM-F
           
           PERFORM 5000-EXIBIR-RESULTADOS
           .
       5000-EXIBIR-RESULTADOS.
           IF WRK-DEBUG-SIM
              DISPLAY "5000-EXIBIR-RESULTADOS"
           END-IF
           DISPLAY "==================================================="
           DISPLAY " TOTAL DE CADASTROS LIDOS EM CLIOLD.......: " 
                                           WS-CTLIDO-O-F
           DISPLAY " TOTAL DE MOVIMENTOS LIDOS EM CLIMOV......: " 
                                           WS-CTLIDO-M-F
           DISPLAY " TOTAL DE CADASTROS GRAVADOS EM CLINEW....: " 
                                           WS-CTGRAV-F
           DISPLAY " TOTAL DE CADASTROS INCLUIDOS EM CLINEW...: " 
                                           WS-CTINC-F
           DISPLAY " TOTAL DE CADASTROS EXCLUIDOS DE CLIOLD...: " 
                                           WS-CTEXC-F
           DISPLAY " TOTAL DE CADASTROS ALTERADOS PARA CLINEW.: " 
                                           WS-CTALT-F
           DISPLAY " TOTAL DE CADASTROS INVALIDOS.............: " 
                                           WS-CTINV-F
           DISPLAY " TOTAL DE MOVIMENTOS INVALIDOS............: " 
                                           WS-CTPERM-F
           DISPLAY " TEMPO TOTAL DE PROCESSAMENTO.............: " 
                                           WS-TEMPO-PROCESSAMENTO-F
           DISPLAY "==================================================="
           .
       4000-FECHAMENTO-ARQUIVOS.
           IF WRK-DEBUG-SIM
              DISPLAY "4000-FECHAMENTO-ARQUIVOS"
           END-IF
      *FECHAMENTO DO CLIOLD
           CLOSE CLIOLD     
           IF NOT SUCESSO-O
              MOVE WS-MSG07                TO WS-MSG
              MOVE FS-CLIOLD               TO WS-FS
              GO TO 9000-ERRO
           END-IF
      *FECHAMENTO DO CLIMOV
           CLOSE CLIMOV     
           IF NOT SUCESSO-M
              MOVE WS-MSG08                TO WS-MSG
              MOVE FS-CLIMOV               TO WS-FS
              GO TO 9000-ERRO
           END-IF
      *FECHAMENTO DO CLINEW
           CLOSE CLINEW
           IF NOT SUCESSO-N
              MOVE WS-MSG09                TO WS-MSG
              MOVE FS-CLINEW               TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
      *----------------------------------------------------------------*
           COPY ROTERRO.
           COPY ROTDATA.
           COPY ROTPROSS.