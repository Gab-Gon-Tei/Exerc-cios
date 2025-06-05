WORKING-STORAGE SECTION.
01 WS-REG-SYSIN          PIC X(10) VALUE SPACES.

0000-GPXXNN01.
	PERFORM 1000-INICIALIZAR
	PERFORM 2000-PROCESSAR 
			UNTIL WS-REG-SYSIN EQUALS ZEROES
	PERFORM 3000-TERMINO	
	STOP RUN
	.
1000-INICIALIZAR.
	MOVE ZERO TO WS-CONTSIM
				 WS-CONTSIMPROCESS
	PERFORM 1500-LER-SYSIN
	.
1500-LER-SYSIN.
	ACCEPT WS-REG-SYSIN FROM SYSIN
	IF WS-REG-SYSIN NOT EQUAL ZERO 
		ADD 1  TO WS-CONTSIM
	END-IF 
	.
2000-PROCESSAR. 
	COMPUTE WS-VAL-PARCELA = 
			(WS-VAL-EMPRESTIMO * WS-JUROS)/
			(1 - 1/(1 + WS-JUROS / 100)** WS-QTD-PARCELAS)
	COMPUTE WS-SOMA-PARCELAS = WS-VAL-PARCELA * WS-QTD-PARCELAS
	DISPLAY "************************************************************************"
	DISPLAY "NUMERO DA SIMULACAO.................: " WS-CONTSIM
	DISPLAY "VALOR DO EMPRESTIMO.................: " WS-VAL-EMPRESTIMO
	DISPLAY "JUROS...............................: " WS-JUROS
	DISPLAY "QTDE. DE PARCELAS...................: " WS-QTD-PARCELAS
	DISPLAY "VALOR DA PARCELA....................: " WS-VAL-PARCELA
	DISPLAY "VALOR TOTAL.........................: " WS-SOMA-PARCELAS
	DISPLAY "************************************************************************"
	ADD 1 TO WS-CONTSIMPROCESS 
	PERFORM 1500-LER-SYSIN
	.
3000-TERMINO.
	DISPLAY "************************************************************************"
	DISPLAY "TOTAL DE SIMULACOES LIDAS.............:" WS-CONTSIM
	DISPLAY "TOTAL DE SIMULACOES PROCESSADAS.......:" WS-CONTSIMPROCESS
	DISPLAY "************************************************************************"
*
	DISPLAY "************************************************************************"
	DISPLAY "*           TERMINO NORMAL DE PROCESSAMENTO DO GPXXNN01                *"
	DISPLAY "************************************************************************"
	.