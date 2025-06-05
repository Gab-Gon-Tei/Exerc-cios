      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         P104NX00.
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
           SELECT EXTTORD ASSIGN            TO UT-S-EXTTORD
           FILE STATUS IS FS-EXTTORD
           .
           SELECT EXTTDO24 ASSIGN            TO UT-S-EXTTDO24
           FILE STATUS IS FS-EXTTDO24
           .
           SELECT EXTTDO25 ASSIGN            TO UT-S-EXTTDO25
           FILE STATUS IS FS-EXTTDO25
           .
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *----------------------------------------------------------------*
       FD  EXTTORD
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS REG-EXTTORD
           .
           COPY CPEXTTOR REPLACING ==::== BY ====.
       FD  EXTTDO24
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS REG-EXTTDO24
           .
           COPY CPEXTT24 REPLACING ==::== BY ====.
       FD  EXTTDO25
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS REG-EXTTDO25
           .
           COPY CPEXTT25 REPLACING ==::== BY ====.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *              VARIAVEIS DEBUG
      *----------------------------------------------------------------*
      * FLAGS
       01  WRK-FLAGS-SWITCHES.
           05  WRK-DEBUG                    PIC  X(01)  VALUE 'S'.
               88  WRK-DEBUG-NAO                        VALUE 'N'.
               88  WRK-DEBUG-SIM                        VALUE 'S'.
      *----------------------------------------------------------------*
      *              VARIAVEIS ESPELHO
      *----------------------------------------------------------------*
           COPY CPEXTT24 REPLACING ==::== BY ==WS-==.
           COPY CPEXTT25 REPLACING ==::== BY ==WS-==.
           COPY CPEXTTOR REPLACING ==::== BY ==WS-==.
      *----------------------------------------------------------------*
      *    VARIAVEIS DE TRABALHO
      *----------------------------------------------------------------*
       01  CONTADORES.
           05 WS-CONTEXTTORD               PIC 9(03).
           05 WS-CONTEXTTDO24              PIC 9(03).
           05 WS-CONTEXTTDO25              PIC 9(03).
      *----------------------------------------------------------------*
      *VARIAVEIS DE FILE STATUS
      *----------------------------------------------------------------*
       77  FS-EXTTORD                      PIC X(02).
       77  FS-EXTTDO24                     PIC X(02).
       77  FS-EXTTDO25                     PIC X(02).
      *----------------------------------------------------------------*
      * MENSAGENS DE ERRO
      *----------------------------------------------------------------*
       77  WS-MSG                          PIC X(60).
       77  WS-FS                           PIC X(02).

       77  WS-MSG01                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA EXTTORD".
       77  WS-MSG02                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA EXTTDO24".
       77  WS-MSG03                        PIC X(60)
                                                   VALUE
           "ERRO DE ABERTURA EXTTDO25".
       77  WS-MSG04                        PIC X(60)
                                                   VALUE
           "ERRO DE LEITURA EXTTORD".
       77  WS-MSG05                        PIC X(60)
                                                   VALUE
           "ERRO DE GRAVACAO EXTTDO24".
       77  WS-MSG06                        PIC X(60)
                                                   VALUE
           "ERRO DE GRAVACAO EXTTDO25".
       77  WS-MSG07                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO EXTTORD".
       77  WS-MSG08                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO EXTTDO24".
       77  WS-MSG09                        PIC X(60)
                                                   VALUE
           "ERRO DE FECHAMENTO EXTTDO25".
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-P104NX00.
           IF WRK-DEBUG-SIM
              DISPLAY "0000-P104NX00"
           END-IF
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
               UNTIL FS-EXTTORD = '10'
           PERFORM 3000-TERMINO
           STOP RUN
           .
      *
       1000-INICIALIZAR.
           IF WRK-DEBUG-SIM
              DISPLAY "1000-INICIALIZAR"
           END-IF
           OPEN INPUT EXTTORD
           IF FS-EXTTORD NOT = '00'
              MOVE WS-MSG01                TO WS-MSG
              MOVE FS-EXTTORD              TO WS-FS
              GO TO 9000-ERRO
           END-IF
           OPEN OUTPUT EXTTDO24
           IF FS-EXTTDO24 NOT = '00'
              MOVE WS-MSG02                TO WS-MSG
              MOVE FS-EXTTDO24             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           OPEN OUTPUT EXTTDO25
           IF FS-EXTTDO25 NOT = '00'
              MOVE WS-MSG03                TO WS-MSG
              MOVE FS-EXTTDO25             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           PERFORM 1500-LER-EXTTORD
           .
      *
       1500-LER-EXTTORD.
           IF WRK-DEBUG-SIM
              DISPLAY "1500-LER-EXTTORD"
           END-IF.
           READ EXTTORD INTO WS-REG-EXTTORD
           IF FS-EXTTORD = '00'
              ADD 1                        TO WS-CONTEXTTORD
           ELSE
              IF FS-EXTTORD NOT = '10'
              MOVE WS-MSG04                TO WS-MSG
              MOVE FS-EXTTORD              TO WS-FS
              GO TO 9000-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           IF WRK-DEBUG-SIM
              DISPLAY "2000-PROCESSAR"
           END-IF
      
      *SEPARANDO POR ANO -> 24 OU 25 
           EVALUATE TRUE
              WHEN WS-ANO-ORD = 2024
                   MOVE WS-REG-EXTTORD     TO WS-REG-EXTTDO24
                   WRITE REG-EXTTDO24 FROM WS-REG-EXTTDO24
                   IF FS-EXTTDO24 = "00"
                      ADD 1                TO WS-CONTEXTTDO24
                   ELSE
                      MOVE WS-MSG05        TO WS-MSG
                      MOVE FS-EXTTDO24     TO WS-FS
                      GO TO 9000-ERRO
                   END-IF
              WHEN WS-ANO-ORD = 2025
                   MOVE WS-REG-EXTTORD     TO WS-REG-EXTTDO25
                   WRITE REG-EXTTDO25 FROM WS-REG-EXTTDO25
                   IF FS-EXTTDO25 = "00"
                      ADD 1                TO WS-CONTEXTTDO25
                   ELSE
                      MOVE WS-MSG06        TO WS-MSG
                      MOVE FS-EXTTDO25     TO WS-FS
                      GO TO 9000-ERRO
                   END-IF
           END-EVALUATE
           PERFORM 1500-LER-EXTTORD
           .
      *
       3000-TERMINO.
           IF WRK-DEBUG-SIM
              DISPLAY "3000-TERMINO"
           END-IF
           CLOSE EXTTORD
           IF FS-EXTTORD NOT = '00'
              MOVE WS-MSG07                TO WS-MSG
              MOVE FS-EXTTORD              TO WS-FS
              GO TO 9000-ERRO
           END-IF

           CLOSE EXTTDO24
           IF FS-EXTTDO24 NOT = '00'
              MOVE WS-MSG08                TO WS-MSG
              MOVE FS-EXTTDO24             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           CLOSE EXTTDO25
           IF FS-EXTTDO25 NOT = '00'
              MOVE WS-MSG09                TO WS-MSG
              MOVE FS-EXTTDO25             TO WS-FS
              GO TO 9000-ERRO
           END-IF
           .
           DISPLAY "==================================================="
           DISPLAY " REGISTROS ANALISADOS.....: " WS-CONTEXTTORD
           DISPLAY " REGISTROS 2024...........: " WS-CONTEXTTDO24
           DISPLAY " REGISTROS 2025...........: " WS-CONTEXTTDO25
           DISPLAY "==================================================="
           .
       COPY ROTERRO.
