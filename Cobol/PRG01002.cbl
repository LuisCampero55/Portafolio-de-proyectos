       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01002.
      *  Manejo de Acept y Display
      *  Campero Granados Luis Daniel
      *  21 ABR 24
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 ApellidoPaterno      PIC X(20).
           01 ApellidoMaterno      PIC X(20).
           01 Nombre               PIC X(20).
           01 Dia                  PIC 99.
           01 Mes                  PIC 99.
           01 Anho                 PIC 9999.
       PROCEDURE DIVISION.
       010-INICIO.
            DISPLAY "Introduce tu apellido paterno: ".
            ACCEPT ApellidoPaterno.
            DISPLAY "Introduce tu apellido materno: ".
            ACCEPT ApellidoMaterno.
            DISPLAY "Introduce tu nombre: ".
            ACCEPT Nombre.
            DISPLAY "Introduce tu dia de nacimiento: ".
            ACCEPT Dia.
            DISPLAY "Introduce tu mes de nacimiento: ".
            ACCEPT Mes.
            DISPLAY "Introduce tu año de nacimiento: ".
            ACCEPT Anho.
            DISPLAY Nombre.
            DISPLAY ApellidoPaterno.
            DISPLAY ApellidoMaterno.
            Display Dia "/", Mes "/", Anho.
            STOP RUN.
       END PROGRAM PRG01002.
