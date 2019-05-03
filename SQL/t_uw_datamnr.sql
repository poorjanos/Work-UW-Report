/* Fetch portfolio data */
DROP TABLE t_uw_port;
commit;

CREATE TABLE t_uw_port
as select * from poorj.t_uw_port@dl_dijtart;
commit;

/* Compute kpm for life contracts */
UPDATE   t_uw_port a
   SET   kimenet = 'Nincs'
 WHERE   poorj.fufi_process_kpm_exists@dl_kontakt_poorj (vonalkod) =
            'központi menesztés nem futott'
         AND a.termcsop = 'Life';
COMMIT;


UPDATE   t_uw_port a
   SET   kimenet = 'Sikeres'
 WHERE  poorj.fufi_process_kpm_exists@dl_kontakt_poorj (vonalkod) =
            'központi menesztés futott'
         AND poorj.fufi_process_kpm_result@dl_kontakt_poorj (vonalkod) =
               'központi menesztés sikeres'
         AND a.termcsop = 'Life';
COMMIT;        
     
    
UPDATE   t_uw_port a
   SET   kimenet = 'Sikertelen'
 WHERE   poorj.fufi_process_kpm_exists@dl_kontakt_poorj (vonalkod) =
            'központi menesztés futott'
         AND poorj.fufi_process_kpm_result@dl_kontakt_poorj (vonalkod) =
               'központi menesztés sikertelen'
         AND a.termcsop = 'Life';
COMMIT;                  
         

DROP TABLE t_uw_kontakt_helper;
COMMIT;

CREATE TABLE t_uw_kontakt_helper
AS
   SELECT   a.*
     FROM   afc.t_afc_wflog_lin2@dl_kontakt_poorj a, kontakt.t_lean_alirattipus@dl_kontakt_poorj b
    WHERE       f_ivk IN (SELECT   vonalkod FROM t_uw_port)
            AND a.f_alirattipusid = b.f_alirattipusid
            AND b.f_lean_tip = 'AL'
            AND afc.afc_wflog_intezkedes@dl_kontakt_poorj (a.f_ivkwfid, a.f_logid) IS NOT NULL
            AND UPPER (kontakt.basic.get_userid_login@dl_kontakt_poorj (a.f_userid)) NOT IN
                     ('MARKIB', 'SZERENCSEK');

COMMIT;


CREATE INDEX port
   ON t_uw_port (vonalkod);

COMMIT;


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
                      AND (afc.afc_wflog_intezkedes@dl_kontakt_poorj (b.f_ivkwfid, b.f_logid) LIKE
                              '%Várakoztatás szükséges%'
                           OR afc.afc_wflog_intezkedes@dl_kontakt_poorj (b.f_ivkwfid,
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


--UPDATE   t_uw_port a
--   SET   feldolg_ido_perc = NULL
-- WHERE   a.kimenet = 'Sikeres kpm' AND feldolg_ido_perc IS NOT NULL;

--COMMIT;


UPDATE   t_uw_port a
   SET   feldolg_ag = 'Happy flow'
 WHERE   a.kimenet = 'Sikeres kpm' AND a.feldolg_ag = 'Exception flow';

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



--Add to history
INSERT INTO t_uw_history_r (idoszak,
                            vonalkod,
                            modkod,
                            ertcsat,
                            termcsop,
                            kotes_tipus,
                            medium_tipus,
                            kimenet,
                            feldolg_ag,
                            feldolg_ido_perc,
                            alir_erk_mnap,
                            alir_erk_nnap,
                            erk_szerz,
                            erk_szerz_nnap,
                            alir_szerz_mnap,
                            alir_szerz_nnap)
   SELECT   idoszak,
            vonalkod,
            modkod,
            ertcsat,
            termcsop,
            kotes_tipus,
            medium_tipus,
            kimenet,
            feldolg_ag,
            feldolg_ido_perc,
            alir_erk_mnap,
            alir_erk_nnap,
            erk_szerz,
            erk_szerz_nnap,
            alir_szerz_mnap,
            alir_szerz_nnap
     FROM   t_uw_port;

COMMIT;