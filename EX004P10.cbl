      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         EX004P10.
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
           SELECT PSOLD ASSIGN          TO UT-S-PSOLD
           FILE STATUS IS FS-PSOLD
           .
           SELECT PSMOV ASSIGN          TO UT-S-PSMOV
           FILE STATUS IS FS-PSMOV
           .
           SELECT PSNEW ASSIGN          TO UT-S-PSNEW
           FILE STATUS IS FS-PSNEW
           .
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *----------------------------------------------------------------*
       FD  PSOLD
           RECORDING MODE IS F
           .
       01  REG-PSOLD                       PIC X(42).
      *----------------------------------------------------------------*
       FD  PSMOV
           RECORDING MODE IS F
           .
       01  REG-PSMOV                       PIC X(43).
      *----------------------------------------------------------------*
       FD  PSNEW
           RECORDING MODE IS F
           .
       01  REG-PSNEW                       PIC X(42).
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.         
      *----------------------------------------------------------------*
      *    VARIAVEIS ESPELHO 
      *----------------------------------------------------------------*
       01  WS-REG-PSOLD.
           05 WS-CODPS-O                   PIC X(05).
           05 WS-DESCPS-O                  PIC X(30).
           05 WS-VALORPS-O                 PIC 9(05)V99.
      *----------------------------------------------------------------*
       01  WS-REG-PSMOV.
           05 WS-CODPS-M                   PIC X(05).
           05 WS-DESCPS-M                  PIC X(30).
           05 WS-VALORPS-M                 PIC 9(05)V99.
           05 WS-TIPOMOV-M                 PIC X(01).
      *----------------------------------------------------------------*
       01  WS-REG-PSNEW.
           05 WS-CODPS-N                   PIC X(05).
           05 WS-DESCPS-N                  PIC X(30).
           05 WS-VALORPS-N                 PIC 9(05)V99.
      *----------------------------------------------------------------*     
      *VARIAVEIS DE FILE STATUS
      *----------------------------------------------------------------*
       01  FS-PSOLD                        PIC X(02).
           88 SUCESSO-O                    VALUE "00".
           88 FIM-ARQ-O                    VALUE "10".

       01  FS-PSMOV                        PIC X(02).
           88 SUCESSO-M                    VALUE "00".
           88 FIM-ARQ-M                    VALUE "10".

       01  FS-PSNEW                        PIC X(02).
           88 SUCESSO-N                    VALUE "00".
           88 FIM-ARQ-N                    VALUE "10".
      *----------------------------------------------------------------*
      *    CONTADORES E VARIAVEIS DE CONTROLE
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
      * MENSAGENS DE ERRO
      *----------------------------------------------------------------*
       77  WS-MSG                          PIC X(60).
       77  WS-FS                           PIC X(02).
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-EX004P09.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL FIM-ARQ-O
                                  AND   FIM-ARQ-M
           PERFORM 3000-TERMINO
           GOBACK
           .
      *
       1000-INICIALIZAR.
           MOVE ZEROS                      TO WS-CTLIDO-O 
                                              WS-CTLIDO-M
                                              WS-CTGRAV
                                              WS-CTINC
                                              WS-CTEXC
                                              WS-CTALT
                                              WS-CTINV
                                              WS-CTPERM                                                  
           PERFORM 1100-ABERTURA-ARQUIVOS
           PERFORM 1500-LER-PSOLD
           IF FIM-ARQ-O
              MOVE "PSOLD VAZIO"           TO WS-MSG
              MOVE FS-PSOLD                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           PERFORM 1600-LER-PSMOV
           IF FIM-ARQ-M
              MOVE "PSMOV VAZIO"           TO WS-MSG
              MOVE FS-PSMOV                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
       1100-ABERTURA-ARQUIVOS.
           OPEN INPUT PSOLD
           IF NOT SUCESSO-O
              MOVE "ERRO ABERTURA PSOLD"   TO WS-MSG
              MOVE FS-PSOLD                TO WS-FS
              GO TO 9000-ERRO
           END-IF

           OPEN INPUT PSMOV
           IF NOT SUCESSO-M
              MOVE "ERRO ABERTURA PSMOV"   TO WS-MSG
              MOVE FS-PSMOV                TO WS-FS
              GO TO 9000-ERRO
           END-IF

           OPEN OUTPUT PSNEW
           IF NOT SUCESSO-N
              MOVE "ERRO ABERTURA PSNEW"   TO WS-MSG
              MOVE FS-PSNEW                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .      
       1500-LER-PSOLD.
           READ PSOLD INTO WS-REG-PSOLD
           IF SUCESSO-O
              ADD 1                        TO WS-CTLIDO-O
           ELSE
              IF FIM-ARQ-O
                 MOVE HIGH-VALUES          TO WS-CODPS-O
              ELSE   
                 MOVE "ERRO LEITURA PSOLD" TO WS-MSG
                 MOVE FS-PSOLD             TO WS-FS
                 GO TO 9000-ERRO
              END-IF
           END-IF
           .
       1600-LER-PSMOV.
           READ PSMOV INTO WS-REG-PSMOV
           IF SUCESSO-M
              ADD 1                        TO WS-CTLIDO-M
           ELSE
              IF FIM-ARQ-M
                 MOVE HIGH-VALUES          TO WS-CODPS-M
              ELSE   
                 MOVE "ERRO LEITURA PSMOV" TO WS-MSG
                 MOVE FS-PSMOV             TO WS-FS
                 GO TO 9000-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           IF WS-CODPS-O < WS-CODPS-M
              PERFORM 2100-MANTER
              PERFORM 1500-LER-PSOLD
           ELSE 
              IF WS-CODPS-O > WS-CODPS-M
                 PERFORM 2200-INCLUIR
                 PERFORM 1600-LER-PSMOV
              ELSE
                 PERFORM 2300-ALT-EXC
                 PERFORM 1500-LER-PSOLD
                 PERFORM 1600-LER-PSMOV
              END-IF
           END-IF
           .
       2100-MANTER.
           PERFORM 2500-GRAVA-O
           ADD 1                           TO WS-CTPERM
           .
       2200-INCLUIR.
           IF WS-TIPOMOV-M = "I"
              PERFORM 2600-GRAVA-M
              ADD 1                     TO WS-CTINC
           ELSE
              ADD 1                     TO WS-CTINV
           END-IF
           .
       2300-ALT-EXC.
           IF WS-TIPOMOV-M = "A"
              PERFORM 2600-GRAVA-M
              ADD 1                     TO WS-CTALT
           ELSE
              IF WS-TIPOMOV-M = "E"
                 ADD 1              TO WS-CTEXC
              ELSE 
                 ADD 1             TO WS-CTINV
                 ADD 1             TO WS-CTPERM
                 PERFORM 2500-GRAVA-O
              END-IF
           END-IF
           .
       2500-GRAVA-O.
           MOVE WS-REG-PSOLD               TO WS-REG-PSNEW
           WRITE REG-PSNEW FROM WS-REG-PSNEW
           IF SUCESSO-N
              ADD 1                        TO WS-CTGRAV
           ELSE 
              MOVE "ERRO GRAVACAO PSNEW"   TO WS-MSG
              MOVE FS-PSNEW                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
       2600-GRAVA-M.
           MOVE WS-CODPS-M                 TO WS-CODPS-N
           IF WS-DESCPS-M NOT = SPACES
              MOVE WS-DESCPS-M             TO WS-DESCPS-N
           ELSE 
              MOVE WS-DESCPS-O             TO WS-DESCPS-N
           END-IF
           IF WS-VALORPS-M IS NUMERIC
              MOVE WS-VALORPS-M            TO WS-VALORPS-N
           ELSE 
              MOVE WS-VALORPS-O            TO WS-VALORPS-N
           END-IF

           WRITE REG-PSNEW                 FROM WS-REG-PSNEW
           IF NOT SUCESSO-N
              MOVE "ERRO GRAVACAO PSNEW"   TO WS-MSG
              MOVE FS-PSNEW                TO WS-FS
              GO TO 9000-ERRO
           ELSE
              ADD 1                        TO WS-CTGRAV
           END-IF
           .
       3000-TERMINO.
           MOVE WS-CTLIDO-O                TO WS-CTLIDO-O-F
           MOVE WS-CTLIDO-M                TO WS-CTLIDO-M-F
           MOVE WS-CTGRAV                  TO WS-CTGRAV-F
           MOVE WS-CTINC                   TO WS-CTINC-F
           MOVE WS-CTEXC                   TO WS-CTEXC-F
           MOVE WS-CTALT                   TO WS-CTALT-F
           MOVE WS-CTINV                   TO WS-CTINV-F
           MOVE WS-CTPERM                  TO WS-CTPERM-F
           PERFORM 5000-EXIBIR-RESULTADOS
           PERFORM 4000-FECHAMENTO-ARQUIVOS
           .
       5000-EXIBIR-RESULTADOS.
           DISPLAY "==================================================="
           DISPLAY " TOTAL DE REGISTROS LIDOS OLD: " WS-CTLIDO-O-F
           DISPLAY " TOTAL DE REGISTROS LIDOS MOV: " WS-CTLIDO-M-F
           DISPLAY " TOTAL DE REGISTROS GRAVADOS: " WS-CTGRAV-F
           DISPLAY " TOTAL DE REGISTROS INCLUIDOS: " WS-CTINC-F
           DISPLAY " TOTAL DE REGISTROS EXCLUIDOS: " WS-CTEXC-F
           DISPLAY " TOTAL DE REGISTROS ALTERADOS: " WS-CTALT-F
           DISPLAY " TOTAL DE REGISTROS INVALIDOS: " WS-CTINV-F
           DISPLAY " TOTAL DE REGISTROS PERMANENTES: " WS-CTPERM-F
           DISPLAY "==================================================="
           .
       4000-FECHAMENTO-ARQUIVOS.
           CLOSE PSOLD     
           IF NOT SUCESSO-O
              MOVE "ERRO FECHAMENTO PSOLD" TO WS-MSG
              MOVE FS-PSOLD                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           CLOSE PSMOV     
           IF NOT SUCESSO-O
              MOVE "ERRO FECHAMENTO PSMOV" TO WS-MSG
              MOVE FS-PSMOV                TO WS-FS
              GO TO 9000-ERRO
           END-IF
           CLOSE PSNEW     
           IF NOT SUCESSO-O
              MOVE "ERRO FECHAMENTO PSNEW" TO WS-MSG
              MOVE FS-PSNEW                TO WS-FS
              GO TO 9000-ERRO
           END-IF

           DISPLAY "==================================================="
           DISPLAY "          TERMINO NORMAL DO PROGRAMA               "
           DISPLAY "==================================================="
           .
           COPY ROTERRO.