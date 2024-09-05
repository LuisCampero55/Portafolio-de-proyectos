       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01010.
      *  Diferencia entre dos fechas
      *  Primera fecha sea mayor a la segunda
      *  Validar ambas fechas
      *  Campero Granados Luis Daniel
      *  05 MAY 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FE1.
           02 FECHA1                   PIC 9(08).
           02 FECH1 REDEFINES FECHA1.
               03 ANO1                 PIC 9(04).
                   88 VAL-ANO1 VALUE 0001 THRU 2100.
               03 MES1                 PIC 9(02).
                   88 VAL-MES1 VALUE 01 THRU 12.
               03 DIA1                 PIC 9(02).
                   88 VAL-DIA1 VALUE 01 THRU 31.
       01 FE2.
           02 FECHA2                   PIC 9(08).
           02 FECH2 REDEFINES FECHA2.
               03 ANO2                 PIC 9(04).
                   88 VAL-ANO2 VALUE 0001 THRU 2100.
               03 MES2                 PIC 9(02).
                   88 VAL-MES2 VALUE 01 THRU 12.
               03 DIA2                 PIC 9(02).
                   88 VAL-DIA2 VALUE 01 THRU 31.
           02 DIATOT                   PIC 9(05)V999.
           02 MESTOT                   PIC 9(03)V999.
           02 ANOTOT                   PIC 9(02)V999.
           02 FEC1                     PIC 9(08).
           02 FEC2                     PIC 9(08).
       01 DIASTOTALES.
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
            DISPLAY '///////////////////////////////////////'.
            DISPLAY '///////       PRIMER FECHA      ///////'.
            DISPLAY ' ¿Ingrese su fecha?  (AAAAMMDD)' UPON CONSOLE.
            ACCEPT FECHA1 FROM CONSOLE.

            DISPLAY '///////////////////////////////////////'.
            DISPLAY '///////      SEGUNDA FECHA      ///////'.
            DISPLAY ' ¿Ingrese otra fecha?  (AAAAMMDD)' UPON CONSOLE.
            ACCEPT FECHA2 FROM CONSOLE.

            DIVIDE ANO1 BY 4 GIVING RESUL
                   REMAINDER RES-4

            MOVE 0 TO DIATOT


            IF ES-RES-CERO
                DISPLAY 'Año bisiesto para primer fecha'
                IF MES1 = 02 AND DIA1 > 29
                    DISPLAY 'Año bisiesto pero febrero no puede tener '
                            'mas de 29 dias'
                    STOP RUN
                ELSE
                    COMPUTE FEC1 = (ANO1 * 365.25) + ACU2(MES1)
                END-IF
            ELSE
                IF MES1 = 02 AND DIA1 > 28
                    DISPLAY 'Febrero no puede tener mas de 28 dias'
                    STOP RUN
                ELSE
                    COMPUTE FEC1 = (ANO1 * 365.25) + ACU1(MES1)
                END-IF
            END-IF.

            DIVIDE ANO2 BY 4 GIVING RESUL
                   REMAINDER RES-4

            IF ES-RES-CERO
                DISPLAY 'Año bisiesto para segunda fecha'
                IF MES2 = 02 AND DIA2 > 29
                    DISPLAY 'Año bisiesto pero febrero no puede tener '
                            'mas de 29 dias'
                    STOP RUN
                ELSE
                    COMPUTE FEC2 = (ANO2 * 365.25) + ACU2(MES2)
                END-IF
            ELSE
                IF MES2 = 02 AND DIA2 > 28
                    DISPLAY 'Febrero no puede tener mas de 28 dias'
                    STOP RUN
                ELSE
                    COMPUTE FEC2 = (ANO2 * 365.25) + ACU1(MES2)
                END-IF
            END-IF.

            IF FEC1 > FEC2

                DISPLAY 'PRIMERA FECHA MAYOR A LA SEGUNDA ' FEC1 ' / '
                           FEC2
                COMPUTE DIATOT = FEC1 - FEC2
                DISPLAY 'HAY UNA DIFERENCIA DE ' DIATOT ' DIAS'

            ELSE
                DISPLAY 'SEGUNDA FECHA MAYOR A LA PRIMERA ' FEC1 ' / '
                           FEC2
                STOP RUN.



            STOP RUN.
       END PROGRAM PRG01010.
