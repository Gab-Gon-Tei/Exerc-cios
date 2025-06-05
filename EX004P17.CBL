      *--------------------------------------------------------------*  
       IDENTIFICATION                      DIVISION.                    
      *--------------------------------------------------------------*  
       PROGRAM-ID.                         EX004P17.                    
       AUTHOR.                             GABRIEL.                      
      *                                                                 
      *--------------------------------------------------------------*  
       ENVIRONMENT                         DIVISION.                    
      *--------------------------------------------------------------*  
       CONFIGURATION                       SECTION.                     
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA. 
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
           SELECT CADPROD ASSIGN           TO CADPRD
           FILE STATUS IS FS-CADPRD.
           .
           SELECT VCADPRD ASSIGN           TO VCADPRD
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-VCADPRD.
           RECORD KEY IS CODPROD-V
           .
      *--------------------------------------------------------------*  
       DATA                                DIVISION.                    
      *--------------------------------------------------------------*
       FILE                                SECTION.
       FD  CADPRD.
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 69 CHARACTERS
           DATA RECORD IS REG-CADPRD
           .
       01  REG-CADPRD.
           05 CODPRD                       PIC X(04).
           05 DESCPROD                     PIC X(20).
           05 UNIDPROD                     PIC X(02).
           05 LOCALPROD                    PIC X(05).
           05 QTDEST                       PIC 9(05).
           05 QTMAX                        PIC 9(05).
           05 QTMIN                        PIC 9(05).
           05 PRECOCOMPRA                  PIC 9(06)V99.
           05 PRECOVENDA                   PIC 9(06)V99.
           05 PERCOMIS                     PIC 9(02)V99.

       FD  VCADPRD.
           RECORD CONTAINS 69 CHARACTERS
           DATA RECORD IS REG-VCADPRD
           .
       01  REG-VCADPRD.
           05 CODPROD-V                     PIC X(04).
           05 DESCPROD-V                    PIC X(20).
           05 UNIDPROD-V                    PIC X(02).
           05 LOCALPROD-V                   PIC X(05).
           05 QTDEST-V                      PIC 9(05).
           05 QTMAX-V                       PIC 9(05).
           05 QTMIN-V                       PIC 9(05).
           05 PRECOCOMPRA-V                 PIC 9(06)V99.
           05 PRECOVENDA-V                  PIC 9(06)V99.
           05 PERCOMIS-V                    PIC 9(02)V99.
      *--------------------------------------------------------------*  
       WORKING-STORAGE                     SECTION.                     
      *--------------------------------------------------------------*  
       01  WS-REG-CADPRD.
           05 WS-CODPROD                     PIC X(04).
           05 WS-DESCPROD                    PIC X(20).
           05 WS-UNIDPROD                    PIC X(02).
           05 WS-LOCALPROD                   PIC X(05).
           05 WS-QTDEST                      PIC 9(05).
           05 WS-QTMAX                       PIC 9(05).
           05 WS-QTMIN                       PIC 9(05).
           05 WS-PRECOCOMPRA                 PIC 9(06)V99.
           05 WS-PRECOVENDA                  PIC 9(06)V99.
           05 WS-PERCOMIS                    PIC 9(02)V99. 

       01  WS-REG-VCADPRD.
           05 WS-CODPROD-V                   PIC X(04).
           05 WS-DESCPROD-V                  PIC X(20).
           05 WS-UNIDPROD-V                  PIC X(02).
           05 WS-LOCALPROD-V                 PIC X(05).
           05 WS-QTDEST-V                    PIC 9(05).
           05 WS-QTMAX-V                     PIC 9(05).
           05 WS-QTMIN-V                     PIC 9(05).
           05 WS-PRECOCOMPRA-V               PIC 9(06)V99.     
           05 WS-PRECOVENDA-V                PIC 9(06)V99.
           05 WS-PERCOMIS-V                  PIC 9(02)V99.
       
       01  WS-DATE-SYS.
           05 WS-ANO-SYS                     PIC 99.
           05 WS-MES-SYS                     PIC 99.
           05 WS-DIA-SYS                     PIC 99.

       01  WS-TIME-SYS.
           05 WS-HORA-SYS                    PIC 99.
           05 WS-MIN-SYS                     PIC 99.
           05 WS-SEG-SYS                     PIC 99.
           05 FILLER                         PIC XX.

       01  WS-DATA-FORMATADA.
           05 WS-DIA-FORMATADA               PIC 99.
           05 FILLER                         PIC X VALUE '/'.
           05 WS-MES-FORMATADA               PIC 99.
           05 FILLER                         PIC X VALUE '/20'.
           05 WS-ANO-FORMATADA               PIC 99.

       01  WS-TIME-FORMATADA.
           05 WS-HORA-FORMATADA              PIC 99.
           05 FILLER                         PIC X VALUE ':'.
           05 WS-MIN-FORMATADA               PIC 99.
           05 FILLER                         PIC X VALUE ':'.
           05 WS-SEG-FORMATADA               PIC 99.
      *--------------------------------------------------------------*  
       77  FS-CADPRD                         PIC X(02).
       77  FS-VCADPRD                        PIC X(02).
       77  WS-CTLIDO                         PIC 9(03).
       77  WS-CTGRAV                         PIC 9(03).
       77  WS-CTDUP                          PIC 9(03).
       77  WS-CTFORASEQ                      PIC 9(03).
       77  WS-MSG                            PIC X(60).
       77  WS-FS                             PIC X(02).
      *--------------------------------------------------------------*  
       PROCEDURE                           DIVISION.                    
      *--------------------------------------------------------------*  
       0000-EX004P17.
           PERFORM 9999-IMPRIME-DATA-HORA
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
                 UNTIL FS-CADPRD = '10'
           PERFORM 3000-TERMINO
           PERFORM 9999-IMPRIME-DATA-HORA
           GOBACK.
       1000-INICIALIZAR.
           MOVE ZERO                       TO WS-CTLIDO
                                              WS-CTGRAV
                                              WS-CTDUP
                                              WS-CTFORASEQ
           OPEN INPUT CADPRD
           IF FS-CADPRD NOT = '00'
               MOVE 'ERRO ABRINDO ARQUIVO CADPRD' TO WS-MSG
               MOVE FS-CADPRD              TO WS-FS
               PERFORM 9000-ERRO
           END-IF

           OPEN OUTPUT VCADPRD
           IF FS-VCADPRD NOT = '00'
               MOVE 'ERRO ABRINDO ARQUIVO VCADPRD' TO WS-MSG
               MOVE FS-VCADPRD             TO WS-FS
               PERFORM 9000-ERRO
           END-IF
           PERFORM 1500-LERCADPRD
           .
       1500-LERCADPRD.
           READ CADPRD INTO REG-CADPRD
           IF FS-CADPRD = '00'
               ADD 1 TO WS-CTLIDO
           ELSE 
              IF FS-CADPRD NOT = '10'
                  MOVE 'ERRO LENDO ARQUIVO CADPRD' TO WS-MSG
                  MOVE FS-CADPRD          TO WS-FS
                  PERFORM 9000-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           MOVE WS-REG-CADPRD              TO REG-VCADPRD
           WRITE REG-VCADPRD               FROM WS-REG-CADPRD
           
           EVALUATE FS-VCADPRD
               WHEN '00'
                   ADD 1 TO WS-CTGRAV
               WHEN '21'
                   ADD 1 TO WS-CTFORASEQ
               WHEN '22'
                   ADD 1 TO WS-CTDUP
               WHEN OTHER
                   MOVE 'ERRO GRAVANDO PRODUTO' TO WS-MSG
                   MOVE FS-VCADPRD         TO WS-FS
                   PERFORM 9000-ERRO
               END-EVALUATE
           PERFORM 1500-LERCADPRD
           .
       3000-TERMINO.
           DISPLAY '==================================================='
           DISPLAY 'TOTAL DE REGISTROS LIDOS: ' WS-CTLIDO
           DISPLAY 'TOTAL DE REGISTROS GRAVADOS: ' WS-CTGRAV
           DISPLAY 'TOTAL DE REGISTROS DUPLICADOS: ' WS-CTDUP
           DISPLAY 'TOTAL DE REGISTROS FORA SEQUENCIA: ' WS-CTFORASEQ
           DISPLAY '===================================================' 

           CLOSE CADPRD
           IF FS-CADPRD NOT = '00'
               MOVE 'ERRO FECHANDO ARQUIVO CADPRD' TO WS-MSG
               MOVE FS-CADPRD              TO WS-FS
               PERFORM 9000-ERRO
           END-IF

           CLOSE VCADPRD
           IF FS-VCADPRD NOT = '00'
               MOVE 'ERRO FECHANDO ARQUIVO VCADPRD' TO WS-MSG
               MOVE FS-VCADPRD             TO WS-FS
               PERFORM 9000-ERRO
           END-IF
           .
           9999-IMPRIME-DATA-HORA.
           ACCEPT WS-DATE-SYS FROM DATE 

           MOVE WS-ANO-SYS              TO WS-ANO-FORMATADA
           MOVE WS-MES-SYS              TO WS-MES-FORMATADA
           MOVE WS-DIA-SYS              TO WS-DIA-FORMATADA

           ACCEPT WS-TIME-SYS FROM TIME

           MOVE WS-HORA-SYS             TO WS-HORA-FORMATADA
           MOVE WS-MIN-SYS              TO WS-MIN-FORMATADA
           MOVE WS-SEG-SYS              TO WS-SEG-FORMATADA

           DISPLAY '*-------------------------------------------------*'
           DISPLAY 'DATA : ' WS-DATE-FORMATADA
                   ' HORA : ' WS-TIME-FORMATADA
           DISPLAY '*-------------------------------------------------*'
           .

           COPY ROTERRO.
