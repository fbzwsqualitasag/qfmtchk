CONNECT GESTHO/GESTHO@ARGUS;

  PROCEDURE ExportGAL IS
  /************************************************************************/
  /* Name         : Export                                                */
  /* Beschreibung : Exportiert die GAL                                    */
  /************************************************************************/
  /* Datum        * Wer   * Kommentar                                     */
  /************************************************************************/
  /* 01.04.2004   * aa    * Init                                          */
  /************************************************************************/
    fFile UTL_FILE.FILE_TYPE;
    fLogFile UTL_FILE.FILE_TYPE;

    -- Mandanten
    nMandantCTX         TR_CODE.COD_ID%TYPE := PA_SYS.nGetMandantIdByContext;
    nMandantSBZVID      TR_CODE.COD_ID%TYPE := PA_SYS.NGETCODEID(PA_CONST.CGR_MANDANT, PA_CONST.MANDANT_SBZV);
    nMandantSHBID       TR_CODE.COD_ID%TYPE := PA_SYS.NGETCODEID(PA_CONST.CGR_MANDANT, PA_CONST.MANDANT_FLECKVIEH);
    nMandantMuKuID      TR_CODE.COD_ID%TYPE := PA_SYS.NGETCODEID(PA_CONST.CGR_MANDANT, PA_CONST.MANDANT_MUTTERKUH);
    nGattungRindvieh    TR_CODE.COD_ID%TYPE := PA_SYS.NGETCODEID(PA_CONST.CGR_GATTUNG,PA_CONST.GATTUNG_RINDVIEH);

    bExport BOOLEAN := FALSE;

    nCountExport NUMBER :=0;
    nCountTier NUMBER :=0;
    nStierCode NUMBER;
    nPStier NUMBER;
    nKalbealterKuh NUMBER;
    nStaoKuhKalb NUMBER;
    nInsTypET NUMBER := PA_SYS.nGetCodeId(PA_CONST.CGR_INSEMTYP,PA_CONST.INSEMTYP_ET);
    nInsTypETErf NUMBER := PA_SYS.nGetCodeId(PA_CONST.CGR_INSEMTYP,PA_CONST.INSEMTYP_ET_ERFOLG);
    nTestStier NUMBER := PA_SYS.nGetCodeId(PA_CONST.CGR_STIEREINSATZ, PA_CONST.STIEREINSATZ_TEST);
    nKBStier NUMBER := PA_SYS.nGetCodeId(PA_CONST.CGR_STIEREINSATZ, PA_CONST.STIEREINSATZ_KB);
    nKeineMissbildung NUMBER := PA_SYS.nGetCodeId(PA_CONST.CGR_DERERBFEHLER, PA_CONST.DERERBFEHLER_KEIN);
    nETVater NUMBER;
    nETMutter NUMBER;
    nTesteinsatz NUMBER;
    dtGebDatETVater DATE;
    nCodeStierEinsatz NUMBER;

    sOutput VARCHAR2(400);
    sFilename VARCHAR2(200);
    sSex VARCHAR2(2) := PA_CONST.WEIBLICH;
    sFileoExt VARCHAR2(50) := 'gal'||TO_CHAR(nMandantCTX)||'_';
    sFileExt VARCHAR2(50) := '.dat';
    aErrCount PA_SYS.ARRAY_NUMBER;
--    dtMinDat DATE := TO_DATE('01.07.1993','DD.MM.YYYY');
    dtMinDat DATE := TO_DATE('01.01.1990','DD.MM.YYYY');
    nTemp NUMBER;
    nAlteLakt NUMBER := 0;
    bTierExportiert BOOLEAN;
    bOhneTierExportiert BOOLEAN;

    nIdAnimalBreak  NUMBER := 0;
    nAniAktId       NUMBER := 0;
    fFilein         UTL_FILE.FILE_TYPE;
    fFileout        UTL_FILE.FILE_TYPE;
    sIniFile        VARCHAR2(50) := 'zws_gal_'||TO_CHAR(nMandantCTX)||'.ini';
    sline           VARCHAR2(100);
    rowKalbeort     HFAEXPL%ROWTYPE;
    fex    BOOLEAN;
    flen   NUMBER;
    fbsize NUMBER;

    sInfoBtrNr        VARCHAR2(22);
    nInfoBtrStufe     NUMBER;
    sInfoBtrRegion    VARCHAR2(10);
    nStaoKuhKalbLast  NUMBER := NULL;

  BEGIN
    UTL_FILE.FGETATTR(PA_CONST.FILE_OUT_ZWS, sIniFile, fex, flen, fbsize);

    IF fex THEN
      fFilein := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS, sIniFile,'r',1000);
      PA_SYS.GET_LINE (fFilein,   sLine);
      nIdAnimalBreak := to_number(sLine);
      UTL_FILE.FClose(fFilein);
    ELSE
      nIdAnimalBreak := 0;
    END IF;

    sFilename := PA_SYS.sFileNameNumbered(PA_CONST.FILE_OUT_ZWS,sFileoExt,sFileExt);
    fFile := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS, sFilename, 'W');
    fLogFile := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS, 'gal_exp.log', 'a');
    /*Array f. FehlerCounter */
    aErrCount(2)  := 0;
    aErrCount(20) := 0;
    aErrCount(21) := 0;
    aErrCount(22) := 0;
    aErrCount(23) := 0;
    aErrCount(24) := 0; -- ET aber ohne Vater auf dem Kalb
    aErrCount(99) := 0;
    /* Selektion der Tiere (alle weiblichen Tiere, die nach dem dtMinDat gestorben sind, vorher sind keine Vollst?ndigen Daten vorhanden!) */
    FOR rowTier IN (SELECT ANIMAL.IDANIMAL
                          ,ANIMAL.IDPERE
                          ,ANIMAL.IDMERE
--                          ,PA_ANI.sTvdNrDataExport(ANIMAL.IDPAYS, ANIMAL.ANIMARQUEMETALLIQUE) TIER_ID -- Wird gar nicht verwendet
                          ,ANIMAL.ANIDATENAISSANCE
--                          ,PA_ANI.nGetBlutanteilBS64(ANIMAL.IDANIMAL) BS64 -- Wird nur zum Zeitpunkt des Exports berechnet
                      FROM ANIMAL
--                          ,ANIEXP
                     WHERE ANIMAL.ANISEX = sSex
                       AND ANIMAL.ANI_RACE_ID IN (SELECT IDRACE FROM TABLE(PA_ANI.tRaceByMandant(nMandantCTX)))
                       AND NVL(ANIMAL.ANIDATEINACTIF,SYSDATE) > dtMinDat
                       AND ANIMAL.IDANIMAL >= nIdAnimalBreak
                       AND ANIMAL.ANI_GATTUNG_ID = nGattungRindvieh
--                       AND ANI_AKT_ANIEXP = IDANIEXP
--                       AND IDEXPLOITATION = 965075612
--                      and animal.idanimal =  992133975--990872323
                     ORDER BY IDANIMAL
                    ) LOOP

      nAniAktId := rowTier.IDANIMAL;

      /*Stop-Signal abtesten, so kann Job abgebrochen werden, ohne dass Session gekillt werden muss.*/
      bStop := PA_SYS.bCheckStop(sPipe);
      IF bStop THEN
        EXIT;
      END IF;
      bExport := TRUE;
      bTierExportiert := FALSE;
      bOhneTierExportiert := FALSE;

      /* Nur Tiere mit >= 87.4 BV Blutanteil werden fur die ZWS exportiert */
      IF (nMandantCTX = nMandantSBZVID) THEN --F?r SBZV muss Blutanteil ?berpr?ft werden
        IF PA_ANI.nGetBlutanteilBV(nAniAktId) < 87.4 THEN
          bExport := FALSE;
          aErrCount(2) := aErrCount(2)+1;
        END IF;
      END IF;

      IF PA_SYS.bCheckCount(nCountTier,500000) THEN
        null;
      END IF;

      BEGIN
        IF bExport THEN

          /* File f?r jeweils 200000 Zeilen erstellen, wenn mehr -> neues File ?ffnen */
          IF PA_SYS.bCheckCount(nCountExport,200000) OR (NOT UTL_FILE.IS_OPEN(fFile)) THEN /* f?hrt Z?hler nach und gibt TRUE, wenn Grenze erreicht */
            IF UTL_FILE.IS_OPEN(fFile) THEN
              UTL_FILE.FFLUSH(fFile);
              UTL_FILE.FCLOSE(fFile);
            END IF;
            sFilename := PA_SYS.sFileNameNumbered(PA_CONST.FILE_OUT_ZWS,sFileoExt,sFileExt);
            fFile := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS, sFilename, 'W');
            PA_MEL.SendPipe(sPipe,'Stand Export Tiere '||nCountTier||' Zeilen: '||nCountExport|| ' neues File: '||sFilename,sSms);
          END IF;

          /* Geburtsabl?ufe selektieren */
          FOR rowGAL IN (SELECT LACGEB.LACNO
                               ,LACGEB.LACDATEVELAGE
                               ,INSEMINATION.IDINSEMINATION
                               ,INSEMINATION.IDTAUREAU
                               ,INSEMINATION.INSDATE
                               ,INSEMINATION.INSEM_INSEMTYP_ID
                               ,NVL(DEROULEMENTVELAGE.IDDEROULEMENTVELAGE,0) IDDEROULEMENTVELAGE
                               ,NVL(DEROULEMENTVELAGE.DERVELCODEAVORTE,0) DERVELCODEAVORTE
                               ,NVL(DEROULEMENTVELAGE.DERVELCODEJUMEAU,1) DERVELCODEJUMEAU
                               ,NVL(DEROULEMENTVELAGE.DERVELCODEMORT,1) DERVELCODEMORT
                               ,NVL(DEROULEMENTVELAGE.DER_FRUEHGEBURT_ID,0) DER_FRUEHGEBURT_ID
                               ,NVL(DEROULEMENTVELAGE.DERVELCODEVELAGE,2) DERVELCODEVELAGE
                               ,NVL(DEROULEMENTVELAGE.DERVELCODECESARIENNE,0) DERVELCODECESARIENNE
                               ,NVL(DEROULEMENTVELAGE.DERVELCODEPOSITION,0) DERVELCODEPOSITION
                               ,NVL(DEROULEMENTVELAGE.DERVELPOIDSNAISSANCE,0) DERVELPOIDSNAISSANCE
                               ,NVL(DEROULEMENTVELAGE.DER_ZITZEN,0) DER_ZITZEN
                               ,DECODE(NVL(DEROULEMENTVELAGE.DER_DERERBFEHLER_ID,0),0,0,nKeineMissbildung,0,1) ERBFEHLER
                               ,NVL(DEROULEMENTVELAGE.IDANIMAL,0) IDANIMAL
                               ,NVL(VATER.IDANIMAL,0) IDVATER
                               ,VATER.ANIDATENAISSANCE GEBDATVATER
                               ,NVL(DECODE(KALB.ANISEX,'F',2,'M',1,NULL),NVL(PA_SYS.nGetCodeIndex(DEROULEMENTVELAGE.DER_SEX_ID),'0')) ANISEX
                               ,KALB.IDPERE IDVATER2
                               ,KALB.IDMERE IDMUTTER2
                               ,LACBES.IDLACTATION
                               ,KALB.ANI_TVDABGANGSDATUM
                               ,KALB.ANI_TVDABGANG_ID
                           FROM LACTATION LACGEB
                               ,LACTATION LACBES
                               ,INSEMINATION
                               ,DEROULEMENTVELAGE
                               ,ANIMAL VATER
                               ,ANIMAL KALB
                          WHERE LACGEB.IDANIMAL = rowTier.IDANIMAL
                            AND LACBES.IDANIMAL = LACGEB.IDANIMAL
                            AND LACGEB.LACNO = LACBES.LACNO + 1
                            AND NVL(LACGEB.LACDATEVELAGE,dtMinDat-1) >= dtMinDat
                            AND LACBES.IDLACTATION = INSEMINATION.IDLACTATION
                            AND INSEMINATION.IDINSEMINATION = DEROULEMENTVELAGE.IDINSEMINATION
                            AND INSEMINATION.IDTAUREAU = VATER.IDANIMAL(+)
                            --AND (VATER.ANI_RACE_ID IN ( PA_CONST.RASSECODE_OB
                            --                           ,PA_CONST.RASSECODE_ROB
                            --                           ,PA_CONST.RASSECODE_BV
                            --                           ,PA_CONST.RASSECODE_BS) OR VATER.ANI_RACE_ID IS NULL)
                            AND (VATER.IDANIMAL IS NOT NULL OR INSEMINATION.INSEM_INSEMTYP_ID IN (nInstypET,nInsTypETErf))
                            AND DEROULEMENTVELAGE.IDANIMAL = KALB.IDANIMAL(+)
                            --AND NVL(INSEMINATION.INSEM_INSEMTYP_ID,-1) != nINSEMTYP_EIGENDEKLARATION
                            AND LACGEB.LACNO <= 20
                          ORDER BY LACBES.LACNO ASC, DEROULEMENTVELAGE.IDANIMAL, INSEMINATION.INSDATE DESC) LOOP


            IF nAlteLakt != rowGal.IDLACTATION THEN
              nAlteLakt := rowGAL.IDLACTATION;
              bTierExportiert := FALSE;
              bOhneTierExportiert := FALSE;
            END IF;

            /* Variable initialisieren */
            nKalbealterKuh := 0;
            nStaoKuhKalb := 0;
            dtGebDatETVater := NULL;
            rowKalbeort := NULL;

            /* verschiedene Z?hler f?hren f. Export von unvollst?ndigen Daten, z.T. in T_MELDUNG, sofern nicht zuviele Fehler */
            IF rowGAL.IDANIMAL = 0 THEN
              --PA_MEL.LogFehler(PA_CONST.PROZESS_ZWS_FBK,20,'Kein Besamer vorhanden!',rowTier.IDANIMAL,NULL,NULL);
              aErrCount(20) := aErrCount(20)+1;
              bExport := TRUE;
            ELSE
              /* Nur Geburtsablauf exportieren, wenn Kalb OK ist!! */
              IF PA_ANI.bTierIsOk(rowGAL.IDANIMAL) THEN
                bExport := TRUE;
              ELSE
                bExport := FALSE;
              END IF;
            END IF;

            /* Wenn Kalbedatum vor der Besamung nicht Exportieren */
            IF (rowGAL.LACDATEVELAGE < rowGAL.INSDATE) THEN
              PA_MEL.LogFehler(PA_CONST.PROZESS_ZWS_GAL,20,'Kalbedatum vor der Besamung! LacNo.'||TO_CHAR(rowGAL.LACNO),rowTier.IDANIMAL,NULL,NULL);
              bExport := FALSE;
            END IF;


            IF bExport THEN
              /* Check auf ET, wenn ja, gebdat Vater holen */
              IF rowGAL.INSEM_INSEMTYP_ID IN (nInstypET,nInsTypETErf) AND rowGAL.IDANIMAL != 0 THEN
                PA_SYS.PUT_LINE(fLogFile, 'ET Kalb: '||rowGAL.IDANIMAL||' / Vater: '||rowGAL.IDVATER2);
                UTL_FILE.FFLUSH(fLogFile);
                IF (rowGAL.IDVATER2 IS NOT NULL) THEN
                  SELECT ANIDATENAISSANCE
                    INTO dtGebDatETVater
                    FROM ANIMAL
                   WHERE IDANIMAL = rowGAL.IDVATER2;
                ELSE
                  aErrCount(24) := aErrCount(24)+1;
                  bExport := FALSE;
                END IF;
              ELSE
                rowGAL.IDMUTTER2 := NULL;
              END IF;
            END IF;

            IF bExport THEN

              IF (nMandantCTX = nMandantSBZVID) THEN
                nStaoKuhKalb   := PA_ANI.rowGetAktAniexp(pnAnimalID             => rowTier.IDANIMAL
                                                        ,pdDatum                => rowGAL.LACDATEVELAGE
                                                        ,pnMandantUnterscheiden => 0
                                                        ).IDEXPLOITATION;
                rowKalbeort    := pa_btr.rowgetbtr(nStaoKuhKalb);
                IF nvl(rowKalbeort.HEXP_MANDANT_ID,nMandantCTX) NOT IN (nMandantCTX,nMandantMuKuID) THEN
                  nStaoKuhKalb := NULL;
                  IF (nvl(rowKalbeort.HEXP_NRTVD,1) != 1) THEN
                    nStaoKuhKalb := PA_BTR.nGetBtrIdTVD(rowKalbeort.HEXP_NRTVD, nMandantCTX, 1); -- auch inaktive Betriebe
                    IF (nStaoKuhKalb IS NULL) THEN
                      nStaoKuhKalb := PA_BTR.nGetBtrIdTVD(rowKalbeort.HEXP_NRTVD, nMandantMuKuID, 1); -- auch inaktive Betriebe
                    END IF;
                  END IF;

                  IF (nStaoKuhKalb IS NULL) THEN
                    bExport := FALSE;
                    rowKalbeort := NULL;
                  ELSE
                    rowKalbeort := pa_btr.rowgetbtr(nStaoKuhKalb);
                  END IF;
                END IF;
              ELSE
                nStaoKuhKalb   := PA_ANI.nGetBtrId(rowTier.IDANIMAL,rowGAL.LACDATEVELAGE); -- Achtung sucht nur noch innerhalb selbem Mandant
                rowKalbeort    := pa_btr.rowgetbtr(nStaoKuhKalb);
                IF nvl(rowKalbeort.HEXP_MANDANT_ID,nMandantCTX) != nMandantCTX THEN
                  nStaoKuhKalb := NULL;
                  IF (nvl(rowKalbeort.HEXP_NRTVD,1) != 1) THEN
                    nStaoKuhKalb := PA_BTR.nGetBtrIdTVD(rowKalbeort.HEXP_NRTVD, nMandantCTX);
                  END IF;

                  IF (nStaoKuhKalb IS NULL) THEN
                    bExport := FALSE;
                    rowKalbeort := NULL;
                  ELSE
                    rowKalbeort := pa_btr.rowgetbtr(nStaoKuhKalb);
                  END IF;
                END IF;
              END IF;
            END IF;
            IF bExport THEN

              /* Betriebsinfos Standort Kuhkalb */
              IF NVL(nStaoKuhKalbLast,-1) != NVL(nStaoKuhKalb,-2) THEN
                nStaoKuhKalbLast := nStaoKuhKalb;
                IF nStaoKuhKalb IS NULL THEN
                  sInfoBtrNr := NULL;
                  nInfoBtrStufe := NULL;
                  sInfoBtrRegion := NULL; -- Wird gar nicht mehr verwendet deshalb nachfolgend auch nicht mehr nachgeschlagen
                ELSE
                  IF (nMandantCTX = nMandantSBZVID) THEN
                    sInfoBtrNr       := rowKalbeort.HEXP_NRTVD;
                  ELSE
                    sInfoBtrNr       := PA_BTR.sBtrNrDisplay(nStaoKuhKalb);
                  END IF;
                  IF (rowKalbeort.HEXP_MANDANT_ID = nMandantSHBID) THEN
                    CASE NVL(PA_SYS.nGetCodeIndex(rowKalbeort.hexp_stufe_id),1)
                    WHEN 1 THEN
                      nInfoBtrStufe := 0;
                    WHEN 3 THEN
                      nInfoBtrStufe := 1;
                    WHEN 5 THEN
                      nInfoBtrStufe := 2;
                    WHEN 7 THEN
                      nInfoBtrStufe := 3;
                    ELSE
                      nInfoBtrStufe := 0;
                    END CASE;
--                    SELECT SUBSTR(NVL(TO_CHAR(MAX(REG_NRDSCH)),' '),1,2)
--                      INTO sInfoBtrRegion
--                      FROM REGION
--                     WHERE IDREGION = rowKalbeort.HEXP_REGION_ID;
                  ELSE
                    nInfoBtrStufe    := PA_SYS.nGetCodeIndex(rowKalbeort.hexp_stufe_id);
--                    sInfoBtrRegion   := PA_BTR.nGetVzgRegion(nStaoKuhKalb);
                  END IF;
                END IF;
              END IF;

              /* verschiedene Daten holen */
              nKalbealterKuh := ROUND(MONTHS_BETWEEN(rowGAL.LACDATEVELAGE,rowTier.ANIDATENAISSANCE),1);
              nCodeStierEinsatz := PA_INS.nGetStiereinsatz(rowGAL.IDINSEMINATION);
              /* Es wird nur zwischen Teststier und kein Teststier unterschieden
              IF nCodeStierEinsatz = nTestStier THEN
                nCodeStierEinsatz := 1;
              ELSE
                nCodeStierEinsatz := 0;
              END IF;*/

              --> War der Stier ein Teststier?
              nCodeStierEinsatz := 0;
              --> Wir schauen nur nach, wenn der Stier zum Besamungsdatum max 4 Jahre alt war
              IF (rowGAL.GebDatVater IS NOT NULL) OR (dtGebDatETVater IS NOT NULL) THEN
                IF MONTHS_BETWEEN(rowGAL.INSDATE,NVL(rowGAL.GebDatVater,dtGebDatETVater)) < 48 THEN
                  --> Wir schauen, ob der Stier irgendwann im Testeinsatz war
                  FOR cSelect IN (SELECT STEI_STIEREINSATZ_ID
                                    FROM T_STIEREINSATZ
                                   WHERE STEI_ANIMAL_ID = NVL(rowGAL.IDVATER,rowGAL.IDVATER2) AND
                                         STEI_STIEREINSATZ_ID = nTestStier
                                  ) LOOP
                    --> Der Stier erf?llt die Bedingungen, welche su am 16.3.06 definiert hat, aa!
                    nCodeStierEinsatz := 1;
                  END LOOP;
                ELSE
                  --> Bei ?lteren Stieren schauen wir, ob ev. zum Besamungszeitpunkt ein Teststiereinsatz war
                  SELECT COUNT(*)
                    INTO nTemp
                    FROM T_STIEREINSATZ
                   WHERE STEI_ANIMAL_ID = NVL(rowGAL.IDVATER,rowGAL.IDVATER2) AND
                         STEI_STIEREINSATZ_ID = nTestStier AND
                         rowGAL.INSDATE BETWEEN STEI_VON AND STEI_BIS;
                  IF nTemp > 0 THEN
                    nCodeStierEinsatz := 1;
                  END IF;
                END IF;
              END IF;

              IF bTierExportiert AND NVL(rowGAL.IDANIMAL,0) = 0 THEN
                NULL; --> wir haben schon eine GAL zu der Laktation mit Kalb exportiert (eine 2. mit Kalb d?rften wir - Zwillinge)
                --DELETE FROM DEROULEMENTVELAGE WHERE IDDEROULEMENTVELAGE = rowGal.IDDEROULEMENTVELAGE;
                --nCountDel := nCountDel + 1;
              ELSIF rowGAL.IDANIMAL IS NULL AND bOhneTierExportiert THEN
                NULL; --> wir haben schon eine GAL zu der Laktation ohne Kalb exportiert
                --DELETE FROM DEROULEMENTVELAGE WHERE IDDEROULEMENTVELAGE = rowGal.IDDEROULEMENTVELAGE;
                --nCountDel := nCountDel + 1;
              ELSE
                /* String zusammensetzen */
                sOutput :=            PA_EXP.sFormat(rowTier.IDANIMAL                                 ,10,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowTier.ANIDATENAISSANCE,'YY')           ,3 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(PA_ANI.nGetBlutanteilBS64(rowTier.IDANIMAL)      ,3 ,PA_EXP.RECHTS,' ');
                /* Bei ET nicht dieselben Variablen wie sonst */
                IF rowGAL.INSEM_INSEMTYP_ID IN (nInstypET,nInsTypETErf) THEN
                  sOutput := sOutput || PA_EXP.sFormat(rowGAL.IDVATER2                                ,10,PA_EXP.RECHTS,' ');
                  sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(dtGebDatETVater,'YY')                  ,3 ,PA_EXP.RECHTS,' ');
                  sOutput := sOutput || PA_EXP.sFormat(PA_ANI.nGetBlutanteilBS64(rowGAL.IDVATER2)     ,3 ,PA_EXP.RECHTS,' ');
                ELSE
                  sOutput := sOutput || PA_EXP.sFormat(rowGAL.IDVATER                                 ,10,PA_EXP.RECHTS,' ');
                  sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowGAL.GEBDATVATER,'YY')               ,3 ,PA_EXP.RECHTS,' ');
                  sOutput := sOutput || PA_EXP.sFormat(PA_ANI.nGetBlutanteilBS64(rowGAL.IDVATER)      ,3 ,PA_EXP.RECHTS,' ');
                END IF;
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.LACNO                                     ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(nKalbealterKuh,'FM990.0')                ,5 ,PA_EXP.RECHTS,' ',' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.LACDATEVELAGE-rowGAL.INSDATE              ,4 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowGAL.LACDATEVELAGE,'MMYY')             ,4 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(sInfoBtrNr                                       ,7 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(nInfoBtrStufe                                    ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(sInfoBtrRegion                                   ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELCODEAVORTE                          ,2 ,PA_EXP.RECHTS,' ');
--                IF rowGAL.ANISEX = PA_CONST.WEIBLICH THEN
--                  sOutput := sOutput || PA_EXP.sFormat('2'                                            ,2 ,PA_EXP.RECHTS,' ');
--                ELSIF rowGAL.ANISEX = PA_CONST.MAENNLICH THEN
--                  sOutput := sOutput || PA_EXP.sFormat('1'                                            ,2 ,PA_EXP.RECHTS,' ');
--                ELSE
--                  sOutput := sOutput || PA_EXP.sFormat('0'                                            ,2 ,PA_EXP.RECHTS,' ');
--                END IF;
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.ANISEX                                    ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELCODEJUMEAU                          ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELCODEMORT                            ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(PA_SYS.nGetCodeIndex(rowGAL.DER_FRUEHGEBURT_ID)  ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELCODEVELAGE                          ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELCODEPOSITION                        ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DERVELPOIDSNAISSANCE                      ,3 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.DER_ZITZEN                                ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.ERBFEHLER                                 ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.IDANIMAL                                  ,10,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowGAL.LACDATEVELAGE,'YYYYMMDD')         ,8 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowGAL.INSDATE,'YYYYMMDD')               ,8 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(PA_SYS.sGetCodeDSCH(rowGAL.INSEM_INSEMTYP_ID)    ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(nCodeStierEinsatz                                ,2 ,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(rowGAL.IDMUTTER2                                 ,10,PA_EXP.RECHTS,' ');
                sOutput := sOutput || PA_EXP.sFormat(TO_CHAR(rowGAL.ANI_TVDABGANGSDATUM,'YYYYMMDD')   ,8 ,PA_EXP.RECHTS,' '); -- Abgangsdatum Kalb
                sOutput := sOutput || PA_EXP.sFormat(PA_SYS.nGetCodeIndex(rowGAL.ANI_TVDABGANG_ID)    ,2 ,PA_EXP.RECHTS,' '); --Status Abgang (tot, lebend, ausgefuehrt)

                PA_SYS.PUT_LINE(fFile, sOutput);
                UTL_FILE.FFLUSH(fFile);
                IF NVL(rowGAL.IDANIMAL,0) > 0 THEN
                  --> wir merken uns, dass wir zur Laktation ein GAL mit Kalb exportiert haben
                  bTierExportiert := TRUE;
                ELSE
                  --> GAL  ohne Kalb exportiert
                  bOhneTierExportiert := TRUE;
                END IF;
              END IF; -- bTierExportiert
            END IF; --bExport
          END LOOP;
        END IF; --bExport
      EXCEPTION
        WHEN OTHERS THEN
          PA_MEL.SendPipe(sPipe,'Fehler bei Tier '||rowTier.idanimal);
          PA_SYS.PUT_LINE(fLogFile,'Fehler bei Tier '||rowTier.idanimal);
          PA_SYS.PUT_LINE(fLogFile,'Abbruch: '||SQLERRM
                                    ||' / '||DBMS_UTILITY.FORMAT_ERROR_STACK
                                    ||' / '||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
          SYS.UTL_FILE.FFLUSH(fLogFile);
          aErrCount(99) := aErrCount(99) +1;
          IF aErrCount(99) >20 THEN
            RAISE;
          END IF;
      END;
      COMMIT;
    END LOOP;
    PA_MEL.LogFehler(PA_CONST.PROZESS_ZWS_GAL,2 ,'Anzahl <=87.4% BV      : '||aErrCount(2),NULL,NULL,NULL);
    nTemp := aErrCount.FIRST;
    WHILE aErrCount.EXISTS(nTemp) LOOP
      PA_SYS.PUT_LINE(fLogFile,'Index: '||nTemp||'  Wert: '||aErrCount(nTemp));
      nTemp := aErrCount.NEXT(nTemp);
    END LOOP;
    COMMIT;
    UTL_FILE.FFLUSH(fLogFile);
    UTL_FILE.FFLUSH(fFile);
    UTL_FILE.FCLOSE(fLogFile);
    UTL_FILE.FCLOSE(fFile);

    fFileout := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS,sIniFile,'W',1000);
    PA_SYS.PUT_LINE(fFileout, nAniAktId);
    UTL_FILE.FFLUSH(fFileout);
    UTL_FILE.FClose(fFileout);

  EXCEPTION
    WHEN OTHERS THEN
      fFileout := UTL_FILE.FOPEN(PA_CONST.FILE_OUT_ZWS,sIniFile,'W',1000);
      PA_SYS.PUT_LINE(fFileout, nAniAktId);
      UTL_FILE.FFLUSH(fFileout);
      UTL_FILE.FClose(fFileout);
      PA_SYS.PUT_LINE(fLogFile,SUBSTR(DBMS_UTILITY.FORMAT_ERROR_STACK||' - '||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE,200));

      PA_MEL.SendPipe(sPipe,'Abbruch ZWS-GAL-Export '||SUBSTR(SQLERRM,1,100),sSms);
      PA_MEL.LogFehler(PA_CONST.PROZESS_ZWS_GAL,2 ,'Anzahl <50% BV      : '||aErrCount(2),NULL,NULL,NULL);
      nTemp := aErrCount.FIRST;
      WHILE aErrCount.EXISTS(nTemp) LOOP
        PA_SYS.PUT_LINE(fLogFile,'Index: '||nTemp||'  Wert: '||aErrCount(nTemp));
        nTemp := aErrCount.NEXT(nTemp);
      END LOOP;
      COMMIT;
      UTL_FILE.FFLUSH(fLogFile);
      UTL_FILE.FFLUSH(fFile);
      UTL_FILE.FCLOSE(fLogFile);
      UTL_FILE.FCLOSE(fFile);

      RAISE;

  END ExportGAL;
