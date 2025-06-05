//FSYS004C JOB 'FSYS004',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,10)
//*******************************************************************
//* ESCREVE ARQUIVO
//*******************************************************************
//STEP001  EXEC PGM=EX004P10
//STEPLIB  DD  DISP=SHR,DSN=FS.GERAL.LOADLIB
//PSOLD    DD  DSN=FS.FSYS998.PSOLD,DISP=SHR
//PSMOV    DD  DSN=FS.FSYS998.PSMOV,DISP=SHR
//PSNEW    DD  DSN=FS.FSYS004.PSNEW,DISP=(OLD,CATLG,DELETE),
//             SPACE=(TRK,(1,1),RLSE),UNIT=SYSALLDA,
//             DCB=(LRECL=42,RECFM=FB,DSORG=PS,BLKSIZE=0)