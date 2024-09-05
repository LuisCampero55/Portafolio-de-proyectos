       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG01005.
      *  Manejo de instruccion MOVE, Uso primario de fechas
      *  Campero Granados Luis Daniel
      *  27 ABR 24
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Fechas.
           05 Fecha.
               10 Dia              PIC 99.
               10 Mes              PIC 99.
               10 Anho             PIC 99.
           05 Hora.
               10 Horas            PIC 99.
               10 Minutos          PIC 99.
               10 Segundos         PIC 99.
           05 Fecha-Sistema.
               10 Dia-Sis          PIC 99.
               10 Mes-Sis          PIC 99.
               10 Anho-Sis         PIC 99.
           05 Hora-Sistema.
               10 Horas-Sis        PIC 99.
               10 Minutos-Sis      PIC 99.
               10 Segundos-Sis     PIC 99.
       PROCEDURE DIVISION.
       010-Inicio.
            MOVE FUNCTION CURRENT-DATE TO Fecha-Sistema.
            ACCEPT Hora-Sistema FROM TIME
            MOVE Dia-Sis TO Dia.
            MOVE Mes-Sis TO Mes.
            MOVE Anho-Sis TO Anho.
            MOVE Horas-Sis TO Horas.
            MOVE Minutos-Sis TO Minutos.
            MOVE Segundos-Sis TO Segundos.

            DISPLAY 'Hora HHMMSS: ' Horas ":" Minutos ":" Segundos.
            DISPLAY 'Fecha DDMMAA: ' Dia "/" Mes "/" Anho.
            DISPLAY 'Fecha AAMMDD: ' Anho "/" Mes "/" Dia.
            DISPLAY 'Fecha MMDDAA: ' Mes "/" Dia "/" Anho.

            STOP RUN.

       END PROGRAM PRG01005.
