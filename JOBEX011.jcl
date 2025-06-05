//FSYS004J JOB 'FSYS004',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,10)
//JOBLIB DD DISP=SHR,DSN=FS.GERAL.LOADLIB
//*******************************************************************
//* ORDENA ARQUIVO1
//*******************************************************************
//STEP001  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SORTIN   DD DSN=FS.FSYS998.CLIOLD,DISP=SHR
//SORTOUT  DD DSN=&&CLOLDORD,DISP=(NEW,PASS) 
//SYSIN    DD *
    SORT FIELDS=(1,04,CH,A)
//*******************************************************************
//* ORDENA ARQUIVO2
//*******************************************************************
//STEP002  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SORTIN   DD DSN=FS.FSYS998.CLIMOV,DISP=SHR
//SORTOUT  DD DSN=&&CLMOVORD,DISP=(NEW,PASS) 
//SYSIN    DD *
    SORT FIELDS=(1,04,CH,A)
//*******************************************************************
//* ESCREVE ARQUIVO
//*******************************************************************
//STEP003  EXEC PGM=P104NX00
//CLIOLD   DD  DSN=&&CLOLDORD,DISP=SHR
//CLIMOV   DD  DSN=&&CLMOVORD,DISP=SHR
//CLINEW   DD  DSN=FS.FSYS004.CLINEW,DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1),RLSE),UNIT=SYSALLDA,
//             DCB=(LRECL=79,RECFM=FB,DSORG=PS)