DROP TABLE t_uw_port;
COMMIT;

CREATE TABLE t_uw_port
AS
   SELECT   TRUNC (szerzdat, 'mm') AS idoszak,
            vonalkod,
            szerzazon,
            modkod,
            ugynokkod,
            ugynoknev,
            ktikod,
            ktinev,
            ertcsat,
            alirdat,
            erkdat,
            szerzdat,
            elutdat,
            stornodat,
            postdat,
            modtyp AS termcsop,
            CASE
               WHEN papir_tipus = 1 THEN 'Papír'
               WHEN papir_tipus = 2 THEN 'FE'
               WHEN papir_tipus = 3 THEN 'FE'
               WHEN papir_tipus = 4 THEN 'Ajpótló'
               WHEN papir_tipus = 5 THEN 'Elektra'
               WHEN papir_tipus = 6 THEN 'Elek'
               WHEN papir_tipus = 7 THEN 'MySigno'
               WHEN papir_tipus = 8 THEN 'Távért'
               WHEN papir_tipus = 9 THEN 'Enyil'
            END
               AS kotes_tipus,
            CASE
               WHEN papir_tipus IN (1, 70) THEN 'Papir'
               WHEN papir_tipus IN (5, 7) THEN 'Teljesen papir mentes'
               ELSE 'Részben papir mentes'
            END
               AS medium_tipus,
            CASE
               WHEN kimenet = 'ABLAK központi menesztés sikeres'
               THEN
                  'Sikeres kpm'
               WHEN kimenet <> 'ABLAK központi menesztés sikeres'
                    AND sikertelen_kpm = 'I'
               THEN
                  'Sikertelen kpm'
               ELSE
                  'Nincs kpm'
            END
               AS kimenet,
            szerzdat - erkdat - bnap_db@dl_peep (szerzdat, erkdat)
               AS ERK_SZERZ
     FROM   pss_201710_kieg@dl_peep a
    WHERE   TRUNC (szerzdat, 'mm') >= DATE '2017-10-01';
COMMIT;

DROP TABLE t_uw_kontakt_helper;
COMMIT;

CREATE TABLE t_uw_kontakt_helper
AS
   SELECT   a.*
     FROM   afc.t_afc_wflog_lin2 a, kontakt.t_lean_alirattipus b
    WHERE       f_ivk IN (SELECT   vonalkod FROM t_uw_port)
            AND a.f_alirattipusid = b.f_alirattipusid
            AND b.f_lean_tip = 'AL'
            AND afc.afc_wflog_intezkedes (a.f_ivkwfid, a.f_logid) IS NOT NULL
            AND UPPER (kontakt.basic.get_userid_login (a.f_userid)) NOT IN
                     ('MARKIB', 'SZERENCSEK');

COMMIT;
DROP INDEX port;

CREATE INDEX port
   ON t_uw_port (vonalkod);

COMMIT;
DROP INDEX kontakt;

CREATE INDEX kontakt
   ON t_uw_kontakt_helper (f_ivk);

COMMIT;
ALTER TABLE t_uw_port
ADD
(
feldolg_ag varchar2(40),
feldolg_ido_perc number
);
COMMIT;

UPDATE   t_uw_port a
   SET   feldolg_ag = 'Exception flow'
 WHERE   EXISTS
            (SELECT   1
               FROM   t_uw_kontakt_helper b
              WHERE   a.vonalkod = b.f_ivk
                      AND (afc.afc_wflog_intezkedes (b.f_ivkwfid, b.f_logid) LIKE
                              '%Várakoztatás szükséges%'
                           OR afc.afc_wflog_intezkedes (b.f_ivkwfid,
                                                        b.f_logid) LIKE
                                '%Nem zárható le/Reponálás funkció/Reponálás%'));

COMMIT;

UPDATE   t_uw_port a
   SET   feldolg_ag = 'Happy flow'
 WHERE   feldolg_ag IS NULL;

COMMIT;

UPDATE   t_uw_port a
   SET   feldolg_ido_perc =
            (SELECT   SUM (c.f_int_end - c.f_int_begin) * 1440
               FROM   t_uw_kontakt_helper c
              WHERE   a.vonalkod = c.f_ivk);

COMMIT;

UPDATE   t_uw_port a
   SET   feldolg_ido_perc = NULL
 WHERE   a.kimenet = 'Sikeres kpm' AND feldolg_ido_perc IS NOT NULL;

COMMIT;

UPDATE   t_uw_port a
   SET   feldolg_ag = 'Happy flow'
 WHERE   a.kimenet = 'Sikeres kpm' AND a.feldolg_ag = 'Exception flow';

COMMIT;



--INSERT INTO t_uw_history (idoszak,
--                          vonalkod,
--                          szerzazon,
--                          modkod,
--                          ugynokkod,
--                          ugynoknev,
--                          ktikod,
--                          ktinev,
--                          ertcsat,
--                          alirdat,
--                          erkdat,
--                          szerzdat,
--                          elutdat,
--                          stornodat,
--                          postdat,
--                          termcsop,
--                          kotes_tipus,
--                          medium_tipus,
--                          kimenet,
--                          erk_szerz,
--                          feldolg_ag,
--                          feldolg_ido_perc)
--     SELECT   * FROM t_uw_port;
--COMMIT;