       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01012.
      *  Cubo 3x3
      *  Primera fecha sea mayor a la segunda
      *  Validar ambas fechas
      *  Campero Granados Luis Daniel
      *  11 MAY 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TABLA.
           02 FILAS           OCCURS 3 TIMES.
               10 VAL-FILA    PIC 9(2) VALUE ZEROS.
               10 COLUMNAS    OCCURS 3 TIMES.
                   15 VAL-COL PIC 9(2) VALUE ZEROS.
                   15 PROFUNDIDAD   OCCURS 3 TIMES.
                       20 VAL-FON PIC 9(2) VALUE ZEROS.
       01 INDICES.
           05 IND1            PIC 9 VALUE ZEROS.
           05 IND2            PIC 9 VALUE ZEROS.
           05 IND3            PIC 9 VALUE ZEROS.
           05 VAL             PIC 99 VALUE ZEROS.
       01 OPC.
           05 BUSCAR          PIC 99 VALUE ZEROS.
               88 VAL-BUSCAR  VALUE 1 THRU 27.
           05 NUM1            PIC 99 VALUE ZEROS.
           05 NUM2            PIC 99 VALUE ZEROS.
           05 ANCHO           PIC 99 VALUE ZEROS.
               88 VAL-ANCHO   VALUE 1 THRU 3.
           05 LARGO           PIC 99 VALUE ZEROS.
               88 VAL-LARGO   VALUE 1 THRU 3.
           05 PROF            PIC 99 VALUE ZEROS.
               88 VAL-PROF    VALUE 1 THRU 3.
           05 OPCION          PIC 9 VALUE ZEROS.
               88 VAL-OPCION  VALUE 1 THRU 2.


       PROCEDURE DIVISION.
       010-INICIO.

            MOVE 1 TO VAL.
            PERFORM VARYING IND1 FROM 01 BY 01 UNTIL
                           IND1 > 03
                   PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                                  IND2 > 03
                       PERFORM VARYING IND3 FROM 01 BY 01 UNTIL
                                  IND3 > 03
                           MOVE VAL TO PROFUNDIDAD(IND1, IND2, IND3)
                           COMPUTE VAL = VAL + 1
                       END-PERFORM
                   END-PERFORM
            END-PERFORM.

            DISPLAY ' ¿Ingrese su la opcion 1/2? ' UPON CONSOLE.
            ACCEPT OPCION FROM CONSOLE.

            IF NOT VAL-OPCION
                DISPLAY 'Sabes leer?'
                GO TO 010-INICIO
            END-IF.

            PERFORM VARYING IND1 FROM 01 BY 01 UNTIL
                     IND1 > 3
                 PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                         IND2 > 3
                     PERFORM VARYING IND3 FROM 01 BY 01 UNTIL
                             IND3 > 3
                           DISPLAY 'Elemento '
                               PROFUNDIDAD(IND1, IND2, IND3)
                               ' Coordenadas: (' IND1','IND2 ','IND3 ')'
                     END-PERFORM
                 END-PERFORM
            END-PERFORM

            IF OPCION = 01
                PERFORM 020-OPC1
                THRU 020-OPC1-SAL
            END-IF.

            IF OPCION = 02
                PERFORM 030-OPC2
                THRU 030-OPC2-SAL
            END-IF.

       020-OPC1.
            IF OPCION = 01
                DISPLAY ' ¿Numero a buscar? ' UPON CONSOLE.
                ACCEPT BUSCAR FROM CONSOLE.
                IF VAL-BUSCAR THEN
                    PERFORM VARYING IND1 FROM 01 BY 01 UNTIL
                           IND1 > 3
                       PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                                  IND2 > 3
                           PERFORM VARYING IND3 FROM 01 BY 01 UNTIL
                                  IND3 > 3
                           IF PROFUNDIDAD(IND1, IND2, IND3) = BUSCAR
                               DISPLAY 'Aqui esta el numero que buscas'
                           DISPLAY 'Elemento '
                               PROFUNDIDAD(IND1, IND2, IND3)
                               ' Coordenadas: (' IND1','IND2 ','IND3 ')'
                           END-PERFORM
                       END-PERFORM
                   END-PERFORM
                STOP RUN
            ELSE
                DISPLAY 'Solo valores entre el 1 y 27'
                GO TO 020-OPC1
            END-IF.
       020-OPC1-SAL.
            EXIT.

       030-OPC2.
            IF OPCION = 02
                PERFORM 031-LARG
                THRU 031-LARG-SAL

                PERFORM 032-ANCH
                THRU 032-ANCH-SAL

                PERFORM 033-PROF
                THRU 033-PROF-SAL

                PERFORM 034-RESULTADO
                THRU 034-RESULTADO-SAL.

       030-OPC2-SAL.
            EXIT.

       031-LARG.
            DISPLAY ' ¿Ingrese su largo? ' UPON CONSOLE.
            ACCEPT LARGO FROM CONSOLE.
            IF NOT VAL-LARGO
                DISPLAY 'Solo entre el 1 y el 3'
                GO TO 031-LARG
            END-IF.
       031-LARG-SAL.
            EXIT.

       032-ANCH.
            DISPLAY ' ¿Ingrese su ancho? ' UPON CONSOLE.
            ACCEPT ANCHO FROM CONSOLE.
            IF NOT VAL-ANCHO
                DISPLAY 'Solamente entre el 1 y el 3'
                GO TO 032-ANCH
            END-IF.
       032-ANCH-SAL.
            EXIT.

       033-PROF.
            DISPLAY ' ¿Ingrese su profundidad? ' UPON CONSOLE.
            ACCEPT PROF FROM CONSOLE.
            IF NOT VAL-PROF
                DISPLAY 'QUE SOLAMENTE ENTRE EL 1 Y EL 3'
                GO TO 033-PROF
            END-IF.
       033-PROF-SAL.
            EXIT.

       034-RESULTADO.
            DISPLAY 'Las coordenadas en las que buscas son: ('
                        LARGO ',' ANCHO ',' PROF ') y es: '
                        PROFUNDIDAD(LARGO, ANCHO, PROF).
       034-RESULTADO-SAL.
            EXIT.

            STOP RUN.
       END PROGRAM PRG01012.
