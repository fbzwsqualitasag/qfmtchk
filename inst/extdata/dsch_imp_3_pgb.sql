
  /**
   * Importiert TVD-Schlachtdaten aus angegebenem File
   *
   * <p>
   *   DML        : YES<br/>
   *   Commit     : YES<br/>
   *   Rollback   : YES<br/>
   *   Autonom    : NO<br/>
   * </p>
   */
  PROCEDURE ImportSchlachtdatenTvd(psFileName IN VARCHAR2
                                 , psLogMail  IN VARCHAR2 DEFAULT NULL) IS
  /* ********************************************************************** */
  /* Datum        * Wer   * Kommentar                                       */
  /* ********************************************************************** */
  /* 06.2016      * DPO   * init                                            */
  /* ********************************************************************** */
    fhFileIn        SYS.UTL_FILE.FILE_TYPE;
    clLog           CLOB;
    rowSTV          TI_SCHLACHTDATENTVD%ROWTYPE;
    sLine           VARCHAR2(1000);
    nLineNum        BINARY_INTEGER;
    nErrCode        BINARY_INTEGER;
    sErrCode        VARCHAR2(128);
    bErr            BOOLEAN;
    nMailhandle     NUMBER;
    nCheckExisting  NUMBER;
  BEGIN
    nLineNum := 0;
    bErr := FALSE;
    DBMS_LOB.CREATETEMPORARY(clLog, FALSE, 10);
   BEGIN
     fhFileIn := SYS.UTL_FILE.FOPEN(PA_CONST.FILE_IN_DSCH, psFileName, 'R');
     <<Line_Loop>>
     WHILE PA_SYS.bGetLine(fhFileIn, sLine)
     LOOP
       rowSTV := NULL;
       nLineNum := nLineNum + 1;
       IF SUBSTR(sLine,1,3) != 'K11' THEN
         CONTINUE; --Falls Eintrag existiert wird er in Zukunft ignoriert
       END IF;

       SELECT COUNT(1)
         INTO nCheckExisting
         FROM TI_SCHLACHTDATENTVD
        WHERE STV_TIERID = TRIM(SUBSTR(sLine,79,14));

       IF nCheckExisting > 0 THEN
         CONTINUE; --Falls Eintrag existiert wird er in Zukunft ignoriert
       END IF;

       BEGIN
         rowSTV.STV_STATUS         := 0;
         rowSTV.STV_BTRAKTSTAO     := TRIM(SUBSTR(sLine,6,10));
         rowSTV.STV_BTRTVD         := TRIM(SUBSTR(sLine,16,7));
         rowSTV.STV_MUTTERID       := TRIM(SUBSTR(sLine,23,14));
         rowSTV.STV_MUTTERRC       := TRIM(SUBSTR(sLine,37,3));
         rowSTV.STV_TIERNAME       := TRIM(SUBSTR(sLine,40,12));
         rowSTV.STV_BTRABKALBUNG   := TRIM(SUBSTR(sLine,52,10));
         rowSTV.STV_BTRABKTVD      := TRIM(SUBSTR(sLine,62,7));
         rowSTV.STV_LAKTNR         := TRIM(SUBSTR(sLine,69,2));
         rowSTV.STV_GEBDAT         := TRIM(SUBSTR(sLine,71,8));
         rowSTV.STV_TIERID         := TRIM(SUBSTR(sLine,79,14));
         rowSTV.STV_TIERRC         := TRIM(SUBSTR(sLine,93,3));
         rowSTV.STV_SEX            := TRIM(SUBSTR(sLine,96,1));
         rowSTV.STV_MCODE          := TRIM(SUBSTR(sLine,97,1));
         rowSTV.STV_VATERID        := TRIM(SUBSTR(sLine,98,14));
         rowSTV.STV_VATERRC        := TRIM(SUBSTR(sLine,112,3));
         rowSTV.STV_ABORT          := TRIM(SUBSTR(sLine,115,1));
         rowSTV.STV_ZKZ            := TRIM(SUBSTR(sLine,116,3));
         rowSTV.STV_GEBVERL        := TRIM(SUBSTR(sLine,119,1));
         rowSTV.STV_VERENDET       := TRIM(SUBSTR(sLine,120,1));
         rowSTV.STV_GEBGEW         := TRIM(SUBSTR(sLine,121,2));
         rowSTV.STV_FARBE          := TRIM(SUBSTR(sLine,123,2));
         rowSTV.STV_AUSWEIS        := TRIM(SUBSTR(sLine,125,1));
         rowSTV.STV_IDGENMUTTER    := TRIM(SUBSTR(sLine,126,14));
         rowSTV.STV_RCGENMUTTER    := TRIM(SUBSTR(sLine,140,3));
         rowSTV.STV_BESDAT         := TRIM(SUBSTR(sLine,143,8));
         rowSTV.STV_KASTRIER       := TRIM(SUBSTR(sLine,151,1));
         rowSTV.STV_SCHLACHTBTR    := TRIM(SUBSTR(sLine,152,8));
         rowSTV.STV_SCHLACHTDAT    := TRIM(SUBSTR(sLine,160,8));
         rowSTV.STV_SCHLACHTGEW    := TRIM(SUBSTR(sLine,168,4));
         rowSTV.STV_KLASSIERER     := TRIM(SUBSTR(sLine,172,6));
         rowSTV.STV_SCHLACHTKAT    := TRIM(SUBSTR(sLine,178,2));
         rowSTV.STV_FLEISCHIGKEIT  := TRIM(SUBSTR(sLine,180,2));
         rowSTV.STV_FETTKLASSE     := TRIM(SUBSTR(sLine,182,1));

         INSERT INTO TI_SCHLACHTDATENTVD VALUES rowSTV RETURNING STV_ID INTO rowSTV.STV_ID;
       EXCEPTION
       WHEN VALUE_ERROR THEN
         PA_SYS.PUT_LINE(clLog, 'VALUE_ERROR@'||nLineNum||':   '||sLine);
         bErr := TRUE;
       WHEN OTHERS THEN
         nErrCode := SQLCODE;
         sErrCode := SUBSTR(SQLERRM, 1, 128);
         PA_SYS.PUT_LINE(clLog, 'OTHERS@'||nLineNum||' -> '||nErrCode||': '||sErrCode);
         PA_SYS.PUT_LINE(clLog, sLine);
         bErr := TRUE;
       END;
     END LOOP Line_Loop;
   EXCEPTION
   -- Filehandling, Parsing und Looping
   WHEN OTHERS THEN
     nErrCode := SQLCODE;
     sErrCode := SUBSTR(SQLERRM, 1, 128);
     IF nLineNum = 0 THEN
       PA_SYS.PUT_LINE(clLog, 'unable to open File -> '||nErrCode||': '||sErrCode);
     ELSE
       PA_SYS.PUT_LINE(clLog, 'OTHERS@'||nLineNum||' -> '||nErrCode||': '||sErrCode);
     END IF;
     bErr := TRUE;
   END;

   IF SYS.UTL_FILE.IS_OPEN(fhFileIn) THEN
     SYS.UTL_FILE.FCLOSE(fhFileIn);
   END IF;

   -- Ist ein Fehler beim Import aufgetreten, wird ein Mail generiert
   IF bErr THEN
     ROLLBACK;
     nMailhandle := PA_NET_MAIL.nGetMailhandle( psEmpfaenger   => COALESCE(psLogMail, PA_CONST.SYS_ADMINMAIL)
                                              , psCC           => NULL
                                              , psBetreff      => 'Fehler in der TVD-Schlachtdaten Importverarbeitung'
                                              , psText         => 'Fehler in der Verarbeitung des Files '||psFileName
                                              , psAbsender     => sPipe
                                              , pnAbsMandant   => PA_SYS.nGetMandantIdByContext
                                                );
     PA_NET_MAIL.addAttachment(pnMailhandle  => nMailhandle
                             , pclData       => clLog
                             , psFileName    => 'Import_Schlachtdaten-TVD'||TO_CHAR(SYSDATE,'YYYYMMDD')||'.log'
                              );
     bErr := PA_NET_MAIL.bSendMail(pnMailhandle  =>  nMailhandle
                                 , pbSchedule    =>  FALSE
                                  );
   ELSE
     COMMIT;
     SYS.UTL_FILE.FRENAME(PA_CONST.FILE_IN_DSCH, psFileName, PA_CONST.FILE_IN_DSCH, psFileName||'.OLD');
   END IF;

   DBMS_LOB.FREETEMPORARY(clLog);
 END ImportSchlachtdatenTvd;
