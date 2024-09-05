       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01004.
      *  Manejo de Instrucciones Aritmeticas, Uso del nivel 88
      *  Campero Granados Luis Daniel
      *  21 ABR 24
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  VARIABLES.
           05 A            PIC 9(05)   VALUE ZEROS.
           05 B            PIC 9(05)   VALUE ZEROS.
           05 X            PIC S9(10)  VALUE ZEROS.
           05 OP           PIC 9X      VALUE SPACES.
               88 ES-VALIDO VALUE '+', '-', '*', '/'.
               88 ES-SUMA VALUE '+'.
               88 ES-REST VALUE '-'.
               88 ES-MULT VALUE '*'.
               88 ES-DIVI VALUE '/'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'Ingresa el tipo de operacion a realizar +, -, *, /'
            ACCEPT OP
            IF NOT ES-VALIDO
                DISPLAY 'Error de operador'
                STOP RUN.
            DISPLAY 'Ingresa el primer valor: ' UPON CONSOLE.
            ACCEPT A FROM CONSOLE.
            DISPLAY 'Ingresa el segundo valor: ' UPON CONSOLE.
            ACCEPT B FROM CONSOLE.
            IF ES-SUMA
                COMPUTE X = A + B
                DISPLAY 'La Suma de ' A ' + ' B ' = ' X.
            IF ES-REST
                COMPUTE X = A - B
                DISPLAY 'La Resta de ' A ' - ' B ' = ' X.
            IF ES-MULT
                COMPUTE X = A * B
                DISPLAY 'La Multiplicacion de ' A ' * ' B ' = ' X.
            IF ES-DIVI
                COMPUTE X = A / B
                DISPLAY 'La Division de ' A ' / ' B ' = ' X.

            STOP RUN.
       END PROGRAM PRG01004.
