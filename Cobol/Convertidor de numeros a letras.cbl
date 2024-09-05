       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01EXAMENBIEN.
      *  Lector de cantidades EXAMEN
      *  Campero Granados Luis Daniel
      *  12 MAY 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VALORES.
           02 CADENASIN                    PIC X(30).
           02 CADENA                       PIC 9(30).
           02 VALOR                        REDEFINES CADENA VALUE ZEROS.
              03 OCHO-MILLON               PIC 9.
                   88 ES-NUMERO1           VALUE 1 THRU 9.
              03 SIETE-MILLON.
                   04 SIETE-MILLON100      PIC 9.
                   04 SIETE-MILLON010      PIC 9.
                   04 SIETE-MILLON001      PIC 9.
                   88 ES-NUMERO2           VALUE 0 THRU 999.
              03 SEIS-MILLON.
                   04 SEIS-MILLON100       PIC 9.
                   04 SEIS-MILLON010       PIC 9.
                   04 SEIS-MILLON001       PIC 9.
                   88 ES-NUMERO3           VALUE 0 THRU 999.
              03 CINCO-MILLON.
                   04 CINCO-MILLON100      PIC 9.
                   04 CINCO-MILLON010      PIC 9.
                   04 CINCO-MILLON001      PIC 9.
                   88 ES-NUMERO4           VALUE 0 THRU 999.
              03 CUATRO-MILLON.
                   04 CUATRO-MILLON100     PIC 9.
                   04 CUATRO-MILLON010     PIC 9.
                   04 CUATRO-MILLON001     PIC 9.
                   88 ES-NUMERO5           VALUE 0 THRU 999.
              03 TRES-MILLON.
                   04 TRES-MILLON100       PIC 9.
                   04 TRES-MILLON010       PIC 9.
                   04 TRES-MILLON001       PIC 9.
                   88 ES-NUMERO6           VALUE 0 THRU 999.
              03 DOS-MILLON.
                   04 DOS-MILLON100        PIC 9.
                   04 DOS-MILLON010        PIC 9.
                   04 DOS-MILLON001        PIC 9.
                   88 ES-NUMERO7           VALUE 0 THRU 999.
              03 MILLON.
                   04 MILLON100            PIC 9.
                   04 MILLON010            PIC 9.
                   04 MILLON001            PIC 9.
                   88 ES-NUMERO8           VALUE 0 THRU 999.
              03 MIL.
                   04 MIL100               PIC 9.
                   04 MIL010               PIC 9.
                   04 MIL001               PIC 9.
                   88 ES-NUMERO9           VALUE 0 THRU 999.
              03 CIEN.
                   04 CIEN100              PIC 9.
                   04 CIEN010              PIC 9.
                   04 CIEN001              PIC 9.
                   88 ES-NUMERO10          VALUE 0 THRU 999.
              03 CENTAVO                   PIC 99.
                   88 ES-NUMERO11          VALUE 0 THRU 99.
           02 VALOR-LETRA-UNIDADES.
              03 FILLER PIC X(15) VALUE "UNO".
              03 FILLER PIC X(15) VALUE "DOS".
              03 FILLER PIC X(15) VALUE "TRES".
              03 FILLER PIC X(15) VALUE "CUATRO".
              03 FILLER PIC X(15) VALUE "CINCO".
              03 FILLER PIC X(15) VALUE "SEIS".
              03 FILLER PIC X(15) VALUE "SIETE".
              03 FILLER PIC X(15) VALUE "OCHO".
              03 FILLER PIC X(15) VALUE "NUEVE".
           02 LETRA-UNIDADES REDEFINES VALOR-LETRA-UNIDADES
                                  PIC X(15) OCCURS 9 TIMES.
           02 VALOR-LETRA10.
              03 FILLER PIC X(15) VALUE "DIEZ".
              03 FILLER PIC X(15) VALUE "ONCE".
              03 FILLER PIC X(15) VALUE "DOCE".
              03 FILLER PIC X(15) VALUE "TRECE".
              03 FILLER PIC X(15) VALUE "CATORCE".
              03 FILLER PIC X(15) VALUE "QUINCE".
              03 FILLER PIC X(15) VALUE "DIECISEIS".
              03 FILLER PIC X(15) VALUE "DIECISIETE".
              03 FILLER PIC X(15) VALUE "DIECIOCHO".
              03 FILLER PIC X(15) VALUE "DIECINUEVE".
           02 LETRA-UNIDADES10 REDEFINES VALOR-LETRA10
                                  PIC X(15) OCCURS 10 TIMES.
           02 VALOR-LETRA-DECENAS.
              03 FILLER PIC X(15) VALUE "VEINTE".
              03 FILLER PIC X(15) VALUE "VEINTI".
              03 FILLER PIC X(15) VALUE "TREINTA".
              03 FILLER PIC X(15) VALUE "CUARENTA".
              03 FILLER PIC X(15) VALUE "CINCUENTA".
              03 FILLER PIC X(15) VALUE "SESENTA".
              03 FILLER PIC X(15) VALUE "SETENTA".
              03 FILLER PIC X(15) VALUE "OCHENTA".
              03 FILLER PIC X(15) VALUE "NOVENTA".
           02 LETRA-DECENAS REDEFINES VALOR-LETRA-DECENAS
                                  PIC X(15) OCCURS 9 TIMES.
           02 VALOR-LETRA-CENTENAS.
              03 FILLER PIC X(15) VALUE "CIEN".
              03 FILLER PIC X(15) VALUE "CIENTO".
              03 FILLER PIC X(15) VALUE "DOSCIENTOS".
              03 FILLER PIC X(15) VALUE "TRESCIENTOS".
              03 FILLER PIC X(15) VALUE "CUATROCIENTOS".
              03 FILLER PIC X(15) VALUE "QUINIENTOS".
              03 FILLER PIC X(15) VALUE "SEISCIENTOS".
              03 FILLER PIC X(15) VALUE "SETECIENTOS".
              03 FILLER PIC X(15) VALUE "OCHOCIENTOS".
              03 FILLER PIC X(15) VALUE "NOVECIENTOS".
           02 LETRA-CENTENAS REDEFINES VALOR-LETRA-CENTENAS
                                  PIC X(15) OCCURS 10 TIMES.
           02 VALOR-LETRA-MILLON.
              03 FILLER PIC X(15) VALUE 'PESOS'.
              03 FILLER PIC X(15) VALUE 'MIL'.
              03 FILLER PIC X(15) VALUE 'MILLONES'.
              03 FILLER PIC X(15) VALUE 'BILLONES'.
              03 FILLER PIC X(15) VALUE 'TRILLONES'.
              03 FILLER PIC X(15) VALUE 'CUATRILLONES'.
              03 FILLER PIC X(15) VALUE 'QUINTULLONES'.
              03 FILLER PIC X(15) VALUE 'SEXTILLONES'.
              03 FILLER PIC X(15) VALUE 'SEPTILLONES'.
              03 FILLER PIC X(15) VALUE 'OCTILLONES'.
           02 LETRA-MILLON REDEFINES VALOR-LETRA-MILLON
                                  PIC X(15) OCCURS 10 TIMES.
           02 VALOR-LETRA-MILLON1.
              03 FILLER PIC X(15) VALUE 'PESO'.
              03 FILLER PIC X(15) VALUE 'MIL'.
              03 FILLER PIC X(15) VALUE 'MILLON'.
              03 FILLER PIC X(15) VALUE 'BILLON'.
              03 FILLER PIC X(15) VALUE 'TRILLON'.
              03 FILLER PIC X(15) VALUE 'CUATRILLON'.
              03 FILLER PIC X(15) VALUE 'QUINTULLON'.
              03 FILLER PIC X(15) VALUE 'SEXTILLON'.
              03 FILLER PIC X(15) VALUE 'SEPTILLON'.
              03 FILLER PIC X(15) VALUE 'OCTILLON'.
           02 LETRA-MILLON1 REDEFINES VALOR-LETRA-MILLON1
                                  PIC X(15) OCCURS 10 TIMES.
      *     02 TVLETRA REDEFINES VALOR-LETRA PIC X(15) OCCURS 38 TIMES.
       01 INDICES.
           02 IND3.
               03 IND3SUB1         PIC 9 VALUE ZEROS.
               03 IND3SUB2         PIC 9 VALUE ZEROS.
           02 IND2.
               03 IND2SUB1         PIC 9 VALUE ZEROS.
               03 IND2SUB2         PIC 9 VALUE ZEROS.
           02 IND1.
               03 IND1SUB1         PIC 9 VALUE ZEROS.
               03 IND1SUB2         PIC 9 VALUE ZEROS.
           02 NOIND                PIC 99 VALUE ZEROS.
           02 NOIND2               PIC 99 VALUE ZEROS.
           02 POS                  PIC 99 VALUE ZEROS.
           02 K                    PIC 99 VALUE ZEROS.
           02 L                    PIC 99 VALUE ZEROS.
           02 CENTAVOS             PIC X(13) VALUE '/100 CENTAVOS'.
       01 VAL-UNIVERSAL.
           02 UNI.
               03 UNI100         PIC 9.
               03 UNI010         PIC 9.
               03 UNI001         PIC 9.

       PROCEDURE DIVISION.
       010-Inicio.
            DISPLAY 'Ingrese su cantidad: ' UPON CONSOLE.
            ACCEPT CADENASIN FROM CONSOLE
            COMPUTE L = 30
            PERFORM VARYING K FROM 30 BY -1 UNTIL K < 1
               IF CADENASIN(K:1) IS NOT NUMERIC
               ELSE
                   MOVE CADENASIN(K:1) TO CADENA(L:1)
                   COMPUTE L = L - 1
               END-IF
            END-PERFORM
            DISPLAY 'CADENA: ' CADENA

            DISPLAY OCHO-MILLON ',' SIETE-MILLON  ',' SEIS-MILLON
               ',' CINCO-MILLON ',' CUATRO-MILLON ',' TRES-MILLON
               ',' DOS-MILLON   ',' MILLON        ',' MIL',' CIEN
               '.' CENTAVO

            IF OCHO-MILLON > 000
                PERFORM 048-OCTILLON THRU 048-OCTILLON-SAL
                STOP RUN
            END-IF

            IF SIETE-MILLON > 000
                PERFORM 047-SEPTILLON THRU 047-SEPTILLON-SAL
                STOP RUN
            END-IF

            IF SEIS-MILLON > 000
                PERFORM 046-SEXTILLON THRU 046-SEXTILLON-SAL
                STOP RUN
            END-IF

            IF CINCO-MILLON > 000
                PERFORM 045-QUINTULLON THRU 045-QUINTULLON-SAL
                STOP RUN
            END-IF

            IF CUATRO-MILLON > 000
                PERFORM 044-CUATRILLON THRU 044-CUATRILLON-SAL
                STOP RUN
            END-IF

            IF TRES-MILLON > 000
                PERFORM 043-TRILLON THRU 043-TRILLON-SAL
                STOP RUN
            END-IF

            IF DOS-MILLON > 000
                PERFORM 042-BILLON THRU 042-BILLON-SAL
                STOP RUN
            END-IF

            IF MILLON > 000
                PERFORM 041-MILLON THRU 041-MILLON-SAL
                STOP RUN
            END-IF

            IF MIL > 000
                PERFORM 040-MILES THRU 040-MILES-SAL
                STOP RUN
            END-IF

            IF CIEN > 000
                MOVE 1 TO POS
                MOVE CIEN TO UNI
                PERFORM 030-UNIVER THRU
                   030-UNIVER-SAL
                DISPLAY 'CON 'CENTAVO CENTAVOS
                STOP RUN
            END-IF

            STOP RUN.

       030-UNIVER.
            PERFORM VARYING IND1SUB2 FROM 0 BY 1 UNTIL IND1SUB1 = 1
               COMPUTE IND2SUB1 = 0
               PERFORM VARYING IND2SUB2 FROM 0 BY 1 UNTIL IND2SUB1 = 1
                   COMPUTE IND3SUB1 = 0
                   PERFORM VARYING IND3SUB2 FROM 0 BY 1 UNTIL
                                   IND3SUB1 = 1
                       IF UNI100 = IND1SUB2
      *////////// SOLAMENTE 100 200 300 400 500 600 700 800 900 ///////////////
                           IF IND1SUB2 > 1 AND UNI100 > 1
                               AND IND1SUB2 = UNI100
                               COMPUTE NOIND = IND1SUB2 + 1
                               IF UNI010 = 0 AND IND2SUB2 = 0
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
      *////////// SOLAMENTE X10 X20 X30 X40 X50 X60 X70 X80 X90 ///////////////
                               IF UNI010 > 0 AND IND2SUB2 > 0
                                   AND UNI010 = IND2SUB2
                                   IF UNI010 = 1 AND IND2SUB2 = 1
                                       AND UNI010 = IND2SUB2
                                       IF UNI001 = 0 AND IND3SUB2 = 0
                                           DISPLAY LETRA-CENTENAS(NOIND)
                                               LETRA-UNIDADES10
                                               (IND2SUB2)
                                               LETRA-MILLON(POS)
                                           GO TO 030-UNIVER-SAL
                                       END-IF
      */////////////////////////// SOLAMENTE X1X //////////////////////////////
                                       IF UNI001 > 0 AND IND3SUB2 > 0
                                           AND UNI001 = IND3SUB2
                                           COMPUTE NOIND2 = IND3SUB2 + 1
                                           DISPLAY LETRA-CENTENAS(NOIND)
                                               LETRA-UNIDADES10(NOIND2)
                                               LETRA-MILLON(POS)
                                           GO TO 030-UNIVER-SAL
                                       END-IF
                                   END-IF
                                   IF UNI010 = 2 AND IND2SUB2 = 2
                                       AND UNI010 = IND2SUB2
                                       AND UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(1)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(IND2SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
      *///////////// SOLAMENTE X3X X4X X5X X6X X7X X8X X9X ////////////////////
                                   IF UNI001 > 0 AND IND3SUB2 > 0
                                       AND UNI001 = IND3SUB2
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(IND2SUB2)'Y '
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
      */////////////////////////// SOLAMENTE X2X //////////////////////////////
                               IF UNI010 = 2 AND IND2SUB2 = 2
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 > 0 AND IND3SUB2 > 0
                                       AND UNI001 = IND3SUB2
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(IND2SUB2)
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF

      */////////////////////////// SOLAMENTE X0X //////////////////////////////
                               IF UNI010 = 0 AND IND2SUB2 = 0
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 > 0 AND IND3SUB2 > 0
                                       AND UNI001 = IND3SUB2
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
                           END-IF
      *//////////////////// ENTRE 100 Y 199 ///////////////////////////
                           IF IND1SUB2 = 1 AND UNI100 = 1
                               IF UNI010 = 0 AND IND2SUB2 = 0
      *     DE 100
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(IND1SUB2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                                   IF UNI001 = IND3SUB2
      *     DE 101 A 109
                                       DISPLAY LETRA-CENTENAS(2)
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
                               COMPUTE NOIND = IND1SUB2 + 1
                               IF UNI010 = 1 AND IND2SUB2 = 1
      *     DE 110
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-UNIDADES10(1)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                                   COMPUTE NOIND2 = IND3SUB2 + 1
      *     DE 111 A 119
                                   IF UNI001 = IND3SUB2
                                       AND UNI001 > 0 AND IND3SUB2 > 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                       LETRA-UNIDADES10(NOIND2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
      *     DE 121 A 29
                               IF UNI010 = 2 AND IND2SUB2 = 2
                                   AND UNI001 > 0 AND IND3SUB2 > 0
                                   AND UNI001 = IND3SUB2
                                   DISPLAY LETRA-CENTENAS(NOIND)
                                       LETRA-DECENAS(IND2SUB2)
                                       LETRA-UNIDADES(IND3SUB2)
                                       LETRA-MILLON(POS)
                                   GO TO 030-UNIVER-SAL
                               END-IF
      *     DE 120
                               IF UNI010 = 2 AND IND2SUB2 = 2
                                   AND UNI001 = 0 AND IND3SUB2 = 0
                                   AND UNI001 = IND3SUB2
                                   COMPUTE NOIND2 = IND3SUB2 + 1
                                   DISPLAY LETRA-CENTENAS(NOIND)
                                       LETRA-DECENAS(NOIND2)
                                       LETRA-MILLON(POS)
                                   GO TO 030-UNIVER-SAL
                               END-IF
      *     DE 131 A 199
                               IF UNI010 > 2 AND IND2SUB2 > 2
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(IND2SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                                   IF UNI001 = IND3SUB2
                                       DISPLAY LETRA-CENTENAS(NOIND)
                                           LETRA-DECENAS(IND2SUB2)'Y '
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
                           END-IF
      *//////////////////// ENTRE 000 Y 099 ///////////////////////////
      *     DE 021 A 029
                           IF UNI100 = 0 AND IND1SUB2 = 0
                               IF UNI010 = 2 AND IND2SUB2 = 2
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 > 0 AND IND3SUB2 > 0 AND
                                       UNI001 = IND3SUB2
                                       COMPUTE NOIND2 = IND2SUB2 + 1
                                       DISPLAY LETRA-DECENAS(IND2SUB2)
                                           LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
      *     DE 020
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       AND IND3SUB2 = UNI001
                                       COMPUTE NOIND2 = IND2SUB2 - 1
                                       DISPLAY LETRA-DECENAS(NOIND2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
      *     DE 010
                               IF UNI010 = 1 AND IND2SUB2 = 1
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       AND UNI001 = IND3SUB2
                                      DISPLAY LETRA-UNIDADES10(IND2SUB2)
                                      LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
      *     DE 011 A 019
                                   IF UNI001 > 1 AND IND3SUB2 > 1
                                       AND UNI001 = IND3SUB2
                                       COMPUTE NOIND2 = IND3SUB2 + 1
                                       DISPLAY LETRA-UNIDADES10(NOIND2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
      *     DE 001 A 009
                               IF UNI010 = 0 AND IND2SUB2 = 0
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 > 0 AND IND3SUB2 > 0
                                       AND UNI001 = IND3SUB2
                                       IF UNI001 = 1 AND IND3SUB2 = 1
                                           AND UNI001 = IND3SUB2
                                           DISPLAY 'UN '
                                           LETRA-MILLON1(POS)
                                           GO TO 030-UNIVER-SAL
                                       END-IF
                                       DISPLAY LETRA-UNIDADES(IND3SUB2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF

                               END-IF
      *     DE 031 A 099
                               IF UNI010 > 2 AND IND2SUB2 > 2
                                   AND UNI010 = IND2SUB2
                                   IF UNI001 = 0 AND IND3SUB2 = 0
                                       DISPLAY LETRA-DECENAS(IND2SUB2)
                                       LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                                   IF UNI001 = IND3SUB2
                                       DISPLAY LETRA-DECENAS(IND2SUB2)
                                           'Y 'LETRA-UNIDADES(IND3SUB2)
                                           LETRA-MILLON(POS)
                                       GO TO 030-UNIVER-SAL
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                       IF IND3SUB2 = 9
                           COMPUTE IND3SUB1 = 1
                       END-IF
                   END-PERFORM
                   IF IND2SUB2 = 9
                       COMPUTE IND2SUB1 = 1
                   END-IF
               END-PERFORM
               IF IND1SUB2 = 9
                   COMPUTE IND1SUB1 = 1
               END-IF
            END-PERFORM.
       030-UNIVER-SAL.
            EXIT.

       040-MILES.
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER
            THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER
            THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       040-MILES-SAL.
            EXIT.

       041-MILLON.
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       041-MILLON-SAL.
            EXIT.

       042-BILLON.
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       042-BILLON-SAL.
            EXIT.
       043-TRILLON.
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       043-TRILLON-SAL.
            EXIT.

       044-CUATRILLON.
            MOVE 6 TO POS
            MOVE CUATRO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       044-CUATRILLON-SAL.
            EXIT.
       045-QUINTULLON.
            MOVE 7 TO POS
            MOVE CINCO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 6 TO POS
            MOVE CUATRO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       045-QUINTULLON-SAL.
            EXIT.
       046-SEXTILLON.
            MOVE 8 TO POS
            MOVE SEIS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 7 TO POS
            MOVE CINCO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 6 TO POS
            MOVE CUATRO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       046-SEXTILLON-SAL.
            EXIT.
       047-SEPTILLON.
            MOVE 9 TO POS
            MOVE SIETE-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 8 TO POS
            MOVE SEIS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 7 TO POS
            MOVE CINCO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 6 TO POS
            MOVE CUATRO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       047-SEPTILLON-SAL.
            EXIT.
       048-OCTILLON.
            MOVE 10 TO POS
            MOVE OCHO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 9 TO POS
            MOVE SIETE-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 8 TO POS
            MOVE SEIS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 7 TO POS
            MOVE CINCO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 6 TO POS
            MOVE CUATRO-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 5 TO POS
            MOVE TRES-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 4 TO POS
            MOVE DOS-MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 3 TO POS
            MOVE MILLON TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 2 TO POS
            MOVE MIL TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            MOVE 1 TO POS
            MOVE CIEN TO UNI
            PERFORM 030-UNIVER THRU 030-UNIVER-SAL
            DISPLAY 'CON 'CENTAVO CENTAVOS.
       048-OCTILLON-SAL.
            EXIT.

            STOP RUN.
       END PROGRAM PRG01EXAMENBIEN.
