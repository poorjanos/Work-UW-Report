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
     FROM   pss_201806_kieg@dl_peep a
    WHERE   TRUNC (szerzdat, 'mm') = DATE '2018-06-01';
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

--Premiums
ALTER TABLE t_uw_port
ADD(
dijbefizdat date,
dijerkdat date,
dijkonyvdat date);
COMMIT;


--Drop repeated rows
DELETE FROM   t_uw_port
      WHERE   vonalkod IN
                    (SELECT   vonalkod
                       FROM   (  SELECT   vonalkod, COUNT (vonalkod) AS db
                                   FROM   t_uw_port
                               GROUP BY   vonalkod
                               ORDER BY   COUNT (vonalkod) DESC)
                      WHERE   db > 1);

COMMIT;


--Drop erroneous sig dates
DELETE FROM t_uw_port WHERE erkdat-alirdat > 90;
COMMIT;


INSERT INTO t_uw_history (idoszak,
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
                          termcsop,
                          kotes_tipus,
                          medium_tipus,
                          kimenet,
                          erk_szerz,
                          feldolg_ag,
                          feldolg_ido_perc,
                          dijbefizdat,
                          dijerkdat,
                          dijkonyvdat
                          )
     SELECT   * FROM t_uw_port;
COMMIT;



--Premiums: refresh historic table
--Create helpers
DROP TABLE T_DIJ_HELPER_ABLAK;
COMMIT;


CREATE TABLE T_DIJ_HELPER_ABLAK as
SELECT   a.szerzazon,
         MIN (f_dijbeido) AS dijbefizdat,
         MIN (f_banknap) AS dijerkdat,
         MIN (f_datum) AS dijkonyvdat
  FROM   t_uw_history a, ab_t_dijtabla@dl_peep b
 WHERE   a.szerzazon = b.f_szerz_azon AND a.termcsop <> 'Life'
 GROUP BY a.szerzazon;
COMMIT;


DROP TABLE T_DIJ_HELPER_FUFI;
COMMIT;

CREATE TABLE T_DIJ_HELPER_FUFI
AS
     SELECT   c.szerzazon,
              MIN (b.payment_date) AS dijbefizdat,
              MIN (b.value_date) AS dijerkdat,
              MIN (a.application_date) AS dijkonyvdat
       FROM   fmoney_in_application@dl_peep a,
              (SELECT   DISTINCT money_in_idntfr,
                                 payment_mode,
                                 money_in_type,
                                 ifi_mozgaskod,
                                 payment_date,
                                 value_date
                 FROM   fmoney_in@dl_peep) b,
              t_uw_history c
      WHERE       c.vonalkod = a.proposal_idntfr
              AND a.money_in_idntfr = b.money_in_idntfr
              AND ref_entity_type = 'Premium'
              AND application_status = 'normal'
              AND a.cntry_flg = 'HU'
              AND a.currncy_code = 'HUF'
              AND money_in_type IN ('propprem', 'reguprem')
              AND c.termcsop = 'Life'
   GROUP BY   c.szerzazon;
COMMIT;

DROP TABLE T_DIJ_HELPER;
COMMIT;

--Merge helpers
CREATE TABLE T_DIJ_HELPER
AS
SELECT * from T_DIJ_HELPER_ABLAK
UNION 
SELECT * from T_DIJ_HELPER_FUFI;
COMMIT;

DROP INDEX uw;
CREATE INDEX uw
   ON t_uw_history (szerzazon);
COMMIT;


DROP INDEX dij;
CREATE INDEX dij
   ON T_DIJ_HELPER (szerzazon);

COMMIT;



--Add to main
UPDATE   t_uw_history a
   SET   (dijbefizdat,dijerkdat,dijkonyvdat) =
            (SELECT   dijbefizdat, dijerkdat, dijkonyvdat
               FROM   T_DIJ_HELPER b
              WHERE   a.szerzazon = b.szerzazon);

COMMIT;





DROP TABLE t_uw_history_r;
COMMIT;

CREATE TABLE t_uw_history_r
AS
   SELECT   idoszak,
            vonalkod,
            modkod,
            ertcsat,
            termcsop,
            kotes_tipus,
            medium_tipus,
            kimenet,
            feldolg_ag,
            ROUND (erkdat - alirdat, 2) AS alir_erk_nnap,
            ROUND (erkdat - alirdat - bnap_db@dl_peep (erkdat, alirdat), 2)
               AS alir_erk_mnap,
            ROUND (erk_szerz, 2) AS erk_szerz,
            ROUND (szerzdat - erkdat, 2) AS erk_szerz_nnap,
            ROUND (feldolg_ido_perc, 2) AS feldolg_ido_perc,
            ROUND (szerzdat - alirdat, 2) AS alir_szerz_nnap,
            ROUND (szerzdat - alirdat - bnap_db@dl_peep (alirdat, szerzdat),
                   2)
               AS alir_szerz_mnap,
            ROUND(dijbefizdat-alirdat,2) as alir_dijbefiz_nnap,
            ROUND(dijbefizdat-alirdat- bnap_db@dl_peep (alirdat, dijbefizdat),2) as alir_dijbefiz_mnap,
            ROUND (dijbefizdat - szerzdat, 2) AS szerz_dijbefiz_nnap,
            ROUND (
                 dijbefizdat
               - szerzdat
               - bnap_db@dl_peep (szerzdat, dijbefizdat),
               2
            )
               AS szerz_dijbefiz_mnap,
               
            ROUND (dijerkdat - szerzdat, 2) AS szerz_dijerk_nnap,
            ROUND (
                 dijerkdat
               - szerzdat
               - bnap_db@dl_peep (szerzdat, dijerkdat),
               2
            )
               AS szerz_dijerk_mnap,  
            ROUND (dijkonyvdat - szerzdat, 2) AS szerz_dijkonyv_nnap,
            ROUND (
                 dijkonyvdat
               - szerzdat
               - bnap_db@dl_peep (szerzdat, dijkonyvdat),
               2
            )
               AS szerz_dijkonyv_mnap,
            ROUND (dijkonyvdat - dijbefizdat, 2) AS dijbefiz_dijkonyv_nnap,
            ROUND (
                 dijkonyvdat
               - dijbefizdat
               - bnap_db@dl_peep (dijbefizdat, dijkonyvdat),
               2
            )
               AS dijbefiz_dijkonyv_mnap,
            ROUND (dijkonyvdat - dijerkdat, 2) AS dijerk_dijkonyv_nnap,
            ROUND (
                 dijkonyvdat
               - dijerkdat
               - bnap_db@dl_peep (dijerkdat, dijkonyvdat),
               2
            )
               AS dijerk_dijkonyv_mnap
     FROM   t_uw_history a
    WHERE   elutdat IS NULL AND stornodat IS NULL;

COMMIT;