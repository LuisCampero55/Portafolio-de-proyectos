       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01006.
      *  Manejo de Instrucciones Aritmeticas, Uso del nivel 88
      *  Manejo del GOTO, Reciclado de codigo
      *  Campero Granados Luis Daniel
      *  28 ABR 24
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

            DISPLAY 'Ingresa el primer valor: ' UPON CONSOLE.
            ACCEPT A FROM CONSOLE.
            DISPLAY 'Ingresa el segundo valor: ' UPON CONSOLE.
            ACCEPT B FROM CONSOLE.
            DISPLAY 'Ingresa el tipo de operacion a realizar +, -, *, /'
            ACCEPT OP

            IF OP = '+'
                PERFORM 000-SUMA
                   THRU
                   000-SUMA-SAL
                DISPLAY 'La suma es: ' X
            ELSE IF OP = '-'
                PERFORM 010-RESTA
                   THRU
                   010-RESTA-SAL
                DISPLAY 'La resta es: ' X
            ELSE IF OP = '*'
                PERFORM 020-MULTIPLICACION
                   THRU
                   020-MULTIPLICACION-SAL
                DISPLAY 'La multiplicacion es: ' X
            ELSE IF OP = '/'
                PERFORM 030-DIVISION
                   THRU
                   030-DIVISION-SAL
                DISPLAY 'La division es: ' X
            ELSE
                DISPLAY 'Error en el operador.'
            END-IF.

       000-SUMA.
            ADD A TO B GIVING X ON SIZE ERROR
                MOVE ZEROS TO X
            END-ADD.
       000-SUMA-SAL.
            EXIT.
       010-RESTA.
            SUBTRACT A FROM B GIVING X ON SIZE ERROR
                MOVE ZEROS TO X
            END-SUBTRACT.
       010-RESTA-SAL.
            EXIT.
       020-MULTIPLICACION.
            MULTIPLY A BY B GIVING X ON SIZE ERROR
                MOVE ZEROS TO X
            END-MULTIPLY.
       020-MULTIPLICACION-SAL.
            EXIT.
       030-DIVISION.
            IF A = 0
                DISPLAY 'Error en el dividiendo'
                STOP RUN
            ELSE
                DIVIDE A BY B GIVING X ON SIZE ERROR
                    MOVE ZEROS TO X
                END-DIVIDE.
       030-DIVISION-SAL.
            EXIT.
       END PROGRAM PRG01006.
