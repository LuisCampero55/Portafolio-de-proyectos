       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01007.
      *  Uso de nivel 88, Uso de IF, Reciclado de codigo
      *  Campero Granados Luis Daniel
      *  29 ABR 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAL.
           05 VAL-DIA                     PIC 99X.
               88 DIA VALUE '01' THRU '31'.
      *    Declaramos variable del mes
           05 VAL-MES                     PIC 99X VALUE ZEROS.
      *    Creamos una tabla para cada valor del mes
               88 ENE VALUE '01'.
               88 FEB VALUE '02'.
               88 MAR VALUE '03'.
               88 ABR VALUE '04'.
               88 MAY VALUE '05'.
               88 JUN VALUE '06'.
               88 JUL VALUE '07'.
               88 AGO VALUE '08'.
               88 SEP VALUE '09'.
               88 OCT VALUE '10'.
               88 NOV VALUE '11'.
               88 DIC VALUE '12'.
      *    Declaramos variable para el año bisiesto
           05 VAL-ANO                  PIC 99X VALUE ZEROS.
      *    Hacemos el uso del nivel 88 para validar años bisiestos
               88 BIS VALUES '00', '04', '08', '12', '16', '20', '24'
                       '28', '32', '36', '40', '44', '48', '52', '56'.

       PROCEDURE DIVISION.
       010-INICIO.
      *    Solicitamos al usuario ingresar los valores, dia, mes y año
            DISPLAY ' ¿Cual es su dia?  (DD)' UPON CONSOLE.
            ACCEPT VAL-DIA FROM CONSOLE.
            DISPLAY ' ¿Cual es su mes?  (MM)' UPON CONSOLE.
            ACCEPT VAL-MES FROM CONSOLE.
            DISPLAY ' ¿Cual es su año?  (AA)' UPON CONSOLE.
            ACCEPT VAL-ANO FROM CONSOLE.
      *    Empezamos con IF para validar si es un año bisiesto
            IF BIS THEN
      *    Si la condicion se cumple empezaremos con otro IF para
      *    validar si este es un mes con 31 dias
                IF ENE OR MAR OR MAY OR JUL OR AGO OR OCT OR DIC THEN
      *    Si la condicion se cumple tendremos otro IF para validar
      *    si  es un dia correcto, si es correcto tendremos el mensaje
      *    de la fecha ingresada
                    IF DIA THEN
                       DISPLAY 'Usted ingreso un año bisiesto'
                       DISPLAY 'Su fecha es: ' VAL-DIA '/ ' VAL-MES '/ '
                           VAL-ANO
                    END-IF
      *    Si el dia es mayor al rango de dias establecidos tendremos
      *    un error y se detentra el programa
                    IF VAL-DIA > 31 OR VAL-DIA < 01
                       DISPLAY 'Error en el dia'
                       STOP RUN
                    END-IF
                ELSE
      *    Tendremos la condicion para validar si es el mes de febrero
                    IF FEB THEN
      *    Validamos el dia correcto entre el rango establecido
                       IF DIA THEN
      *    Tendremos otra condicion en caso del que el dia sea
      *    incorrecto
                            IF VAL-DIA > 29 OR VAL-DIA < 01
                               DISPLAY 'Error en el dia'
                               STOP RUN
                            ELSE
      *    Si es correcto tendremos el mensaje de la fecha ingresada
                               DISPLAY 'Usted ingreso un año bisiesto'
                               DISPLAY 'Su fecha es: ' VAL-DIA '/ '
                                   VAL-MES '/ ' VAL-ANO
                            END-IF
                       End-IF
                    END-IF
                END-IF
      *    Ahora tendremos la condicion si es para los meses de 30 dias
      *    y se repite el proceso
                IF ABR OR JUN OR SEP OR NOV THEN
                    IF DIA THEN
                        IF VAL-DIA > 30 OR VAL-DIA < 01
                            DISPLAY 'Error en el dia'
                            STOP RUN
                        ELSE
                            DISPLAY 'Usted ingreso un año bisiesto'
                            DISPLAY 'Su fecha es: ' VAL-DIA '/ '
                               VAL-MES '/ ' VAL-ANO
                        END-IF
                    End-IF
                END-IF
                IF VAL-MES > 12 OR VAL-MES < 01
                   DISPLAY 'Mes incorrecto'
                   STOP RUN
                END-IF
            ELSE
      *    En caso de no cumplir con la condicion del año bisiesto
      *    pasaremos a la contra que seria para año no bisiesto
      *    el proceso se vuelve a repetir.
                IF ENE OR MAR OR MAY OR JUL OR AGO OR OCT OR DIC THEN
                    IF DIA THEN
                        DISPLAY 'Su fecha es: ' VAL-DIA '/ '
                                   VAL-MES '/ ' VAL-ANO
                    END-IF
                    IF VAL-DIA > 31 OR VAL-DIA < 01
                       DISPLAY 'Error en el dia'
                       STOP RUN
                    END-IF
                END-IF
                IF FEB THEN
                    IF DIA THEN
                        IF VAL-DIA > 28 OR VAL-DIA < 01
                            DISPLAY 'Error en el dia'
                            STOP RUN
                        ELSE
                            DISPLAY 'Su fecha es: ' VAL-DIA '/ '
                                   VAL-MES '/ ' VAL-ANO
                        END-IF
                    END-IF
                END-IF
                IF ABR OR JUN OR SEP OR NOV THEN
                    IF DIA THEN
                        IF VAL-DIA > 30 OR VAL-DIA < 01
                            DISPLAY 'Error en el dia'
                            STOP RUN
                        ELSE
                            DISPLAY 'Su fecha es: ' VAL-DIA '/ '
                                   VAL-MES '/ ' VAL-ANO
                        END-IF
                    End-IF
                END-IF
                IF VAL-MES > 12 OR VAL-MES < 01
                   DISPLAY 'Mes incorrecto'
                   STOP RUN
                END-IF
            END-IF
            IF VAL-ANO < 00 OR VAL-ANO > 54
               DISPLAY 'Solo validamos fechas desde el año 00 al'
                       'año 54'
            STOP RUN.
       END PROGRAM PRG01007.
