      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         DATACHCK.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
       01  WS-SITUACAO-DATA                PIC X(01).
           88  DATA-INVALIDA                       VALUE '0'.
           88  DATA-VALIDA                         VALUE '1'.
       
       01  WS-CALCULO-BISSEXTO.
           05 WS-Q4                        PIC 9(06) COMP.
           05 WS-Q100                      PIC 9(06) COMP.
           05 WS-Q400                      PIC 9(06) COMP.
           05 WS-Q3600                     PIC 9(06) COMP.
           05 WS-R4                        PIC 9(06) COMP.
           05 WS-R100                      PIC 9(06) COMP.
           05 WS-R400                      PIC 9(06) COMP.
           05 WS-R3600                     PIC 9(06) COMP.
      *----------------------------------------------------------------*
       LINKAGE                             SECTION.                             
      *----------------------------------------------------------------*
       01  LS-PARAMETRO.
           05 LS-TAMANHO-PARAMETRO         PIC S9(04) COMP.
           05 LS-DATA-PARA-VALIDAR.
              10 LS-ANO                    PIC 9(04).
              10 LS-MES                    PIC 9(02).
              10 LS-DIA                    PIC 9(02).              
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
           USING LS-PARAMETRO.
      *----------------------------------------------------------------*
      *CONSIDERANDO DATA VALIDA     
           SET DATA-VALIDA TO TRUE.
      *TESTE 1 -> ANO ENTRE 1 E 2099
           IF DATA-VALIDA AND (LS-ANO < 1 OR > 2099)
                SET DATA-VALIDA TO TRUE
           END-IF
      *TESTE 2 -> MES ENTRE 1 E 12
           IF DATA-VALIDA AND (LS-MES < 1 OR > 12)
                SET DATA-VALIDA TO TRUE
           END-IF
      *TESTE 3 -> MES COM 30 OU 31 DIAS
           IF DATA-VALIDA AND (LS-MES = 1 OR 3 OR 5 OR 7 OR
                                        8 OR 10 OR 12) 
              IF LS-DIA < 1 OR > 31
                   SET DATA-VALIDA TO TRUE
              END-IF
           ELSE 
              IF LS-MES NOT = 2 AND (LS-DIA < 1 OR > 30)
                   SET DATA-VALIDA TO TRUE
              END-IF
           END-IF
      *TESTE 4 -> FEVEREIRO EM ANO BISSEXTO
           IF DATA-VALIDA AND LS-MES = 2 
              DIVIDE LS-ANO BY 4 GIVING WS-Q4      REMAINDER WS-R4
              DIVIDE LS-ANO BY 100 GIVING WS-Q100  REMAINDER WS-R100
              DIVIDE LS-ANO BY 400 GIVING WS-Q400  REMAINDER WS-R400
              DIVIDE LS-ANO BY 3600 GIVING WS-Q3600 REMAINDER WS-R3600

              IF WS-Q4 = 0 AND WS-Q100 NOT = 0 OR
                 WS-Q400 = 0 AND WS-Q3600 NOT = 0
                 IF LS-DIA < 1 OR > 29
                    SET DATA-VALIDA TO TRUE
                 END-IF
              ELSE
                 IF LS-DIA < 1 OR > 28
                    SET DATA-VALIDA TO TRUE
                 END-IF
              END-IF
           END-IF 
      *VERIFICANDO SE A DATA CONTINUA VALIDA
           IF DATA-VALIDA
              DISPLAY "DATA VALIDA =" LS-DATA-PARA-VALIDAR
           ELSE
              DISPLAY "DATA INVALIDA =" LS-DATA-PARA-VALIDAR
           END-IF
           GOBACK
           .
        *----------------------------------------------------------------*

           