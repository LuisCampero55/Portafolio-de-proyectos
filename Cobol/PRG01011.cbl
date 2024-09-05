       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01011.
      *  Tabla 5X5
      *  Campero Granados Luis Daniel
      *  11 MAY 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TABLA.
           02 FILAS           OCCURS 5 TIMES.
               10 VAL-FILA    PIC 9(2) VALUE ZEROS.
               10 COLUMNAS    OCCURS 5 TIMES.
                   15 VAL-COL PIC 9(2) VALUE ZEROS.
       01 INDICES.
           05 IND1            PIC 99 VALUE ZEROS.
           05 IND2            PIC 99 VALUE ZEROS.
           05 VAL             PIC 99 VALUE ZEROS.
       01 OPC.
           05 BUSCAR          PIC 99 VALUE ZEROS.
               88 VAL-BUSCAR  VALUE 1 THRU 25.
           05 NUM1            PIC 99 VALUE ZEROS.
           05 NUM2            PIC 99 VALUE ZEROS.
           05 ANCHO           PIC 99 VALUE ZEROS.
               88 VAL-ANCHO   VALUE 1 THRU 5.
           05 LARGO           PIC 99 VALUE ZEROS.
               88 VAL-LARGO   VALUE 1 THRU 5.
           05 OPCION          PIC 9 VALUE ZEROS.
               88 VAL-OPCION  VALUE 1 THRU 2.


       PROCEDURE DIVISION.
       010-INICIO.

            MOVE 1 TO VAL.
            PERFORM VARYING IND1 FROM 01 BY 01 UNTIL
                           IND1 > 05
                   PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                                  IND2 > 05
                       MOVE VAL TO COLUMNAS(IND1, IND2)
                       COMPUTE VAL = VAL + 1
                   END-PERFORM
            END-PERFORM.

            DISPLAY ' ¿Ingrese su la opcion 1/2? ' UPON CONSOLE.
            ACCEPT OPCION FROM CONSOLE.

            IF NOT VAL-OPCION
                GO TO 010-INICIO
            END-IF.

            PERFORM VARYING IND1 FROM 01 BY 01 UNTIL
                     IND1 > 5
                 PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                         IND2 > 5
                     DISPLAY 'Elemento '
                          COLUMNAS(IND1, IND2)
                          ' Coordenadas: (' IND1','IND2')'
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
                           IND1 > 05
                       PERFORM VARYING IND2 FROM 01 BY 01 UNTIL
                                  IND2 > 05
                           IF COLUMNAS(IND1, IND2) = BUSCAR
                               COMPUTE NUM1 = IND1
                               COMPUTE NUM2 = IND2 + 1
                               DISPLAY 'Tu valor a buscar esta en las '
                                   'coordenadas: (' NUM1 ','
                                   NUM2 ') y es: ' COLUMNAS(IND1, IND2)
                                   STOP RUN
                       END-PERFORM
                   END-PERFORM
                END-IF.
       020-OPC1-SAL.
            EXIT.

       030-OPC2.
            IF OPCION = 02
                PERFORM 031-LARG
                THRU 031-LARG-SAL

                PERFORM 032-ANCH
                THRU 032-ANCH-SAL

                PERFORM 033-RESULTADO
                THRU 033-RESULTADO-SAL.

       030-OPC2-SAL.
            EXIT.

       031-LARG.
            DISPLAY ' ¿Ingrese su largo? ' UPON CONSOLE.
            ACCEPT LARGO FROM CONSOLE.
            IF NOT VAL-LARGO
                DISPLAY 'Solo entre el 1 y el 5'
                GO TO 031-LARG
            END-IF.
       031-LARG-SAL.
            EXIT.

       032-ANCH.
            DISPLAY ' ¿Ingrese su ancho? ' UPON CONSOLE.
            ACCEPT ANCHO FROM CONSOLE.
            IF NOT VAL-ANCHO
                DISPLAY 'Solamente entre el 1 y el 5'
                GO TO 032-ANCH
            END-IF.
       032-ANCH-SAL.
            EXIT.

       033-RESULTADO.
            DISPLAY 'Las coordenadas en las que buscas son: ('
                        LARGO ',' ANCHO ') y es: '
                        COLUMNAS(LARGO, ANCHO).
       033-RESULTADO-SAL.
            EXIT.

            STOP RUN.
       END PROGRAM PRG01011.
