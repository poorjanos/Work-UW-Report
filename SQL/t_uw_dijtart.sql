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
               ROUND (erkdat - alirdat, 2) AS alir_erk_nnap,
            ROUND (erkdat - alirdat - bnap_db(erkdat, alirdat), 2)
               AS alir_erk_mnap,
            ROUND (szerzdat - erkdat - bnap_db(szerzdat, erkdat), 2) AS erk_szerz,
            ROUND (szerzdat - erkdat, 2) AS erk_szerz_nnap,
            ROUND (szerzdat - alirdat, 2) AS alir_szerz_nnap,
            ROUND (szerzdat - alirdat - bnap_db(alirdat, szerzdat),
                   2)
               AS alir_szerz_mnap
     FROM   pss_201904_kieg a
    WHERE   TRUNC (szerzdat, 'mm') = DATE '2019-04-01';
COMMIT;