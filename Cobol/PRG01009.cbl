       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01009.
      *  Uso de nivel 88, Uso de IF, Reciclado de codigo
      *  Campero Granados Luis Daniel
      *  05 MAY 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FECH.
           05 VAL-ANO                          PIC 9(4).
               88 ANO VALUE 0001 THRU 2100.
           05 VAL-MES                          PIC 9(2).
               88 MES VALUE 01 THRU 12.
               88 M31 VALUE 01, 03, 05, 07, 08, 10, 12.
               88 M30 VALUE 04, 06, 09, 11.
           05 VAL-DIA                          PIC 9(2).
               88 DIA VALUE 01 THRU 31.
       01 DIASTOT                              PIC 999 VALUE ZEROS.
       01 JULIANO.
           05 DIASACUMULADOS.
               10 FILLER                       PIC 9(6) VALUE 000000.
               10 FILLER                       PIC 9(6) VALUE 031032.
               10 FILLER                       PIC 9(6) VALUE 059060.
               10 FILLER                       PIC 9(6) VALUE 090091.
               10 FILLER                       PIC 9(6) VALUE 120121.
               10 FILLER                       PIC 9(6) VALUE 151152.
               10 FILLER                       PIC 9(6) VALUE 181182.
               10 FILLER                       PIC 9(6) VALUE 212213.
               10 FILLER                       PIC 9(6) VALUE 243244.
               10 FILLER                       PIC 9(6) VALUE 273274.
               10 FILLER                       PIC 9(6) VALUE 304305.
               10 FILLER                       PIC 9(6) VALUE 334335.
           05 DIASXMES REDEFINES DIASACUMULADOS.
               10 ACU-GRAL                     OCCURS 12.
                   15 ACU1                     PIC 999.
                   15 ACU2                     PIC 999.
       01 RESUL                                PIC 9999.
       01 RESIDUOS.
           05 RES-4                            PIC 9999.
       01 RESIDUO-R REDEFINES RESIDUOS.
           05 RES-TOT                          PIC 9(12).
               88 ES-RES-CERO                  VALUE ZEROS.

       PROCEDURE DIVISION.
       010-INICIO.
            DISPLAY ' ¿Ingrese su año?  (AAAA)' UPON CONSOLE.
            ACCEPT VAL-ANO FROM CONSOLE.
            DISPLAY ' ¿Ingrese su mes?  (MM)' UPON CONSOLE.
            ACCEPT VAL-MES FROM CONSOLE.
            DISPLAY ' ¿Ingrese su dia?  (DD)' UPON CONSOLE.
            ACCEPT VAL-DIA FROM CONSOLE.
            IF NOT ANO
                DISPLAY 'ERROR: Año incorrecto'
                STOP RUN
            END-IF.
            IF NOT MES
                DISPLAY 'ERROR: Mes incorrecto'
                STOP RUN
            END-IF.
            IF NOT DIA
                DISPLAY 'ERROR: Dia incorrecto'
                STOP RUN
            END-IF.
            DIVIDE VAL-ANO BY 4 GIVING RESUL
                   REMAINDER RES-4

            IF ES-RES-CERO
                DISPLAY 'Año bisiesto'
                IF VAL-MES = 02 AND VAL-DIA > 29
                    DISPLAY 'Año bisiesto pero febrero no puede tener '
                            'mas de 29 dias'
                    STOP RUN
                ELSE
                    COMPUTE DIASTOT = VAL-DIA + ACU2(VAL-MES)
                    DISPLAY 'Dias de año bisiesto: ' DIASTOT
                END-IF
            ELSE
                IF VAL-MES = 02 AND VAL-DIA > 28
                    DISPLAY 'Febrero no puede tener mas de 28 dias'
                    STOP RUN
                ELSE
                    COMPUTE DIASTOT = VAL-DIA + ACU1(VAL-MES)
                    DISPLAY 'Dias de año no bisiesto: ' DIASTOT
                END-IF
            END-IF.
            STOP RUN.
       END PROGRAM PRG01009.
