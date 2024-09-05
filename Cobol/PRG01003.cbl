       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01003.
      *  Manejo de Instrucciones Aritmeticas e Instrucciones Anteriores
      *  Campero Granados Luis Daniel
      *  21 ABR 24
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 A            PIC 9(5).
           01 B            PIC 9(5).
           01 X            PIC 99.
       PROCEDURE DIVISION.
       010-INICIO.

            DISPLAY 'Ingresa el primer valor: ' UPON CONSOLE.
            ACCEPT A FROM CONSOLE.
            DISPLAY 'Ingresa el segundo valor: ' UPON CONSOLE.
            ACCEPT B FROM CONSOLE.

            DISPLAY 'Suma' UPON CONSOLE.
            ADD A TO B GIVING X.
            DISPLAY X.

            DISPLAY 'Resta' UPON CONSOLE.
            SUBTRACT A FROM B GIVING X.
            DISPLAY X.

            DISPLAY 'Multiplicacion' UPON CONSOLE.
            MULTIPLY A BY B GIVING X.
            DISPLAY X.

            DISPLAY 'Division' UPON CONSOLE.
            DIVIDE A BY B GIVING X.
            DISPLAY X.

            DISPLAY 'Compute General' UPON CONSOLE.
            DISPLAY 'Suma' UPON CONSOLE.
            COMPUTE X= A + B.
            DISPLAY X.

            DISPLAY 'Resta' UPON CONSOLE.
            COMPUTE X= A - B.
            DISPLAY X.

            DISPLAY 'Multiplicacion' UPON CONSOLE.
            COMPUTE X= A * B.
            DISPLAY X.

            DISPLAY 'Division' UPON CONSOLE.
            COMPUTE X= A / B.
            DISPLAY X.

            STOP RUN.
       END PROGRAM PRG01003.
