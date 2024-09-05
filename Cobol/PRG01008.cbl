       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01008.
      *  Manejo de Instrucciones Aritmeticas, Uso del nivel 88
      *  Manejo del GOTO, Reciclado de codigo
      *  Campero Granados Luis Daniel
      *  04 MAY 24
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  VARIABLES.
           05 A            PIC 9(5)   VALUE ZEROS.
               88 VAL-A VALUES 1 THRU 99999.
           05 B            PIC 9(5)   VALUE ZEROS.
               88 VAL-B VALUES 1 THRU 99999.
           05 X            PIC S9(10)  VALUE ZEROS.
           05 OP           PIC 9X      VALUE SPACES.
               88 ES-VALIDO VALUE '+', '-', '*', '/'.
               88 ES-SUMA VALUE '+'.
               88 ES-REST VALUE '-'.
               88 ES-MULT VALUE '*'.
               88 ES-DIVI VALUE '/'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY 'Ingresa el primer valor: ' UPON CONSOLE.
            ACCEPT A FROM CONSOLE.
            IF VAL-A THEN
               DISPLAY 'Numero valido'
            ELSE
               DISPLAY 'Numero invalido'
               STOP RUN
            END-IF
            DISPLAY 'Ingresa el segundo valor: ' UPON CONSOLE.
            ACCEPT B FROM CONSOLE.
            IF VAL-B THEN
               DISPLAY 'Numero valido'
            ELSE
               DISPLAY 'Numero invalido'
               STOP RUN
            END-IF
            DISPLAY 'Ingresa el tipo de operacion a realizar +, -, *, /'
            ACCEPT OP

            EVALUATE TRUE
               WHEN ES-SUMA
                   COMPUTE X = A + B
                   DISPLAY 'La Suma de ' A ' + ' B ' = ' X
               WHEN ES-REST
                   COMPUTE X = A - B
                   DISPLAY 'La Resta de ' A ' - ' B ' = ' X
               WHEN ES-MULT
                   COMPUTE X = A * B
                   DISPLAY 'La Multiplicacion de ' A ' * ' B ' = ' X
               WHEN ES-DIVI
                   COMPUTE X = A / B
                   DISPLAY 'La Division de ' A ' / ' B ' = ' X
               WHEN OTHER
                   DISPLAY 'Error en el operador.'
            END-EVALUATE
            EXIT.

       END PROGRAM PRG01008.
