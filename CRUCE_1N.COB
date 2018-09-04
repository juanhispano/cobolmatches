      *----------------------------------------------------------------*
      *    OBJETIVO:  Este programa realiza un cruce 1n entre          *
      *    dos ficheros por una clave de un solo carácter, la última   *
      *    del registro.                                               *
      *    REQUISITOS: Los registros de lectura deben estar ordenados  *
      *    en ascendente por el código (último byte de registro).      *
      *----------------------------------------------------------------*


      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID. CRUCE_1N.

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT  TRA-ENTRADA  ASSIGN TO 'trabajadores.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TRA-ENTRADA.

           SELECT  EMP-ENTRADA ASSIGN TO 'empresas.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-EMP-ENTRADA.

           SELECT  CRUCE-SALIDA ASSIGN TO 'cruce-salida.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SALIDA.

      *================================================================*
       DATA DIVISION.
      *================================================================*
       FILE SECTION.

       FD  TRA-ENTRADA
           BLOCK CONTAINS 0 RECORDS.
       01  REG-TRA-ENTRADA                     PIC  X(10).

       FD  EMP-ENTRADA
           BLOCK CONTAINS 0 RECORDS.
       01  REG-PROF-SALIDA                     PIC  X(10).

       FD  CRUCE-SALIDA
           BLOCK CONTAINS 0 RECORDS.
       01  REG-SALIDA                          PIC  X(19).


       WORKING-STORAGE SECTION.

      *++++++++++++++++++*
      *    FILE STATUS   *
      *++++++++++++++++++*
       77  FS-TRA-ENTRADA                      PIC  X(2).
       77  FS-EMP-ENTRADA                      PIC  X(2).
       77  FS-SALIDA                           PIC  X(2).

      *++++++++++++++++++*
      *    SWITCHES      *
      *++++++++++++++++++*
       01  SWITCHES.
           05  SW-TRA-ENTRADA                  PIC  X(1)   VALUE  'N'.
               88  SI-FIN-TRA                              VALUE  'S'.
               88  NO-FIN-TRA                              VALUE  'N'.

           05  SW-EMP-ENTRADA                  PIC  X(1)   VALUE  'N'.
               88  SI-FIN-EMP                              VALUE  'S'.
               88  NO-FIN-EMP                              VALUE  'N'.

      *++++++++++++++++++*
      *    LITERALES     *
      *++++++++++++++++++*
       01  WS-ERROR.
           05  WS-ERROR-PARRAFO            PIC  X(30).
           05  WS-ERROR-ACCION             PIC  X(20).
           05  WS-ERROR-FICHERO            PIC  X(30).
           05  WS-ERROR-FS                 PIC  X(2).

      *+++++++++++++++++++++++++++++*
      *    VARIABLES DE TRABAJO     *
      *+++++++++++++++++++++++++++++*
       01  WS-CLAVE-TRA-ENT.
           05  WS-CLAVE-TRA-NOM            PIC  X(9).
           05  WS-CLAVE-TRA-COD            PIC  X(1).

       01  WS-CLAVE-EMP-ENT.
           05  WS-CLAVE-EMP-EMP            PIC  X(9).
           05  WS-CLAVE-EMP-COD            PIC  X(1).

       01  WS-SALIDA.
           05  WS-SALIDA-NOM               PIC  X(9).
           05  FILLER                      PIC  X(1)  VALUE  ' '.
           05  WS-SALIDA-EMP               PIC  X(9).

      *++++++++++++++++++++++++++++++++++++++++*
      *    VARIABLES DE CONTROL-CONTADORES     *
      *++++++++++++++++++++++++++++++++++++++++*
       01  WS-CONTADORES.
           05  WS-CONT-LEIDOS-TRA          PIC  9(2)  COMP-3
                                   VALUE ZEROES.
           05  WS-CONT-LEIDOS-EMP          PIC  9(2)  COMP-3
                                   VALUE ZEROES.
           05  WS-CONT-ESCRITOS-SAL        PIC  9(2)  COMP-3
                                   VALUE ZEROES.

       01  WS-TEXTO-PARRAFO.

           05  WS-1200-APERTURA-FICHEROS   PIC  X(30)
                                   VALUE  '1200-APERTURA-FICHEROS'.
           05  WS-1300-LECTURA-TRA-ENT     PIC  X(30)
                                   VALUE  '1300-LECT-TRA-ENT'.
           05  WS-1400-LECTURA-EMP-ENT    PIC  X(30)
                                   VALUE  '1400-LECT-EMP-ENT'.
           05  WS-2100-ESCRIBE-SALIDA      PIC  X(30)
                                   VALUE  '2100-ESCRIBE-SALIDA'.
           05  WS-5000-CIERRA-FICHEROS     PIC  X(30)
                                   VALUE  '5000-CIERRA-FICHEROS'.


      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

           PERFORM  1000-INICIO
           PERFORM  2000-PROCESO
           UNTIL    SI-FIN-TRA  AND  SI-FIN-EMP
           PERFORM  3000-FIN

           .


       1000-INICIO.

           PERFORM  1100-INICIALIZA-REGISTROS
           PERFORM  1200-APERTURA-FICHEROS
           PERFORM  1300-LECTURA-TRA-ENT
           PERFORM  1400-LECTURA-EMP-ENT

           .


       1100-INICIALIZA-REGISTROS.

           INITIALIZE  WS-ERROR
                       WS-CLAVE-TRA-ENT
                       WS-CLAVE-EMP-ENT
                       WS-SALIDA
                       WS-CONTADORES

           .


       1200-APERTURA-FICHEROS.

           OPEN  INPUT  TRA-ENTRADA
                        EMP-ENTRADA
                OUTPUT  CRUCE-SALIDA

           IF  FS-TRA-ENTRADA NOT = '00' AND FS-TRA-ENTRADA NOT = '10'
               MOVE  'ERROR DE APERTURA'          TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           IF  FS-EMP-ENTRADA NOT = '00' AND FS-EMP-ENTRADA NOT = '10'
               MOVE 'ERROR DE APERTURA'           TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           IF  FS-SALIDA NOT = '00' AND FS-SALIDA NOT = '10'
               MOVE 'ERROR DE APERTURA'           TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           .


       1300-LECTURA-TRA-ENT.

           READ  TRA-ENTRADA  INTO  WS-CLAVE-TRA-ENT

           EVALUATE  FS-TRA-ENTRADA

               WHEN  '00'
                     ADD  1  TO  WS-CONT-LEIDOS-TRA

               WHEN  '10'
                     MOVE  HIGH-VALUES  TO  WS-CLAVE-TRA-COD
                     SET   SI-FIN-TRA   TO  TRUE

               WHEN  OTHER
                     MOVE     WS-1300-LECTURA-TRA-ENT
                                        TO  WS-ERROR-PARRAFO
                     PERFORM  9500-ERROR-STATUS-FICHERO

           .


       1400-LECTURA-EMP-ENT.

           READ  EMP-ENTRADA  INTO  WS-CLAVE-EMP-ENT

           EVALUATE  FS-EMP-ENTRADA

               WHEN  '00'
                     ADD  1  TO  WS-CONT-LEIDOS-EMP

               WHEN  '10'
                     MOVE  HIGH-VALUES  TO  WS-CLAVE-EMP-COD
                     SET   SI-FIN-EMP   TO  TRUE

               WHEN  OTHER
                     MOVE     WS-1400-LECTURA-EMP-ENT
                                        TO  WS-ERROR-PARRAFO
                     PERFORM  9500-ERROR-STATUS-FICHERO

           .


       2000-PROCESO.

           IF  WS-CLAVE-TRA-COD  EQUAL TO  WS-CLAVE-EMP-COD

               MOVE  WS-CLAVE-TRA-NOM    TO  WS-SALIDA-NOM
               MOVE  WS-CLAVE-EMP-EMP    TO  WS-SALIDA-EMP

               PERFORM  2100-ESCRIBE-SALIDA
               PERFORM  1400-LECTURA-EMP-ENT

           ELSE

               IF  WS-CLAVE-TRA-COD  <  WS-CLAVE-EMP-COD

                   PERFORM  1300-LECTURA-TRA-ENT

               ELSE

                   PERFORM  1400-LECTURA-EMP-ENT

               END-IF

           END-IF

           .


       2100-ESCRIBE-SALIDA.

           WRITE  REG-SALIDA  FROM  WS-SALIDA

           EVALUATE  FS-SALIDA

               WHEN  '00'
                     ADD  1  TO  WS-CONT-ESCRITOS-SAL
               WHEN  '10'
                     CONTINUE
               WHEN  OTHER
                     MOVE  WS-2100-ESCRIBE-SALIDA  TO  WS-ERROR-PARRAFO
                     MOVE  'ERROR DE ESCRITURA'    TO  WS-ERROR-ACCION
                     PERFORM  9500-ERROR-STATUS-FICHERO

           .


       3000-FIN.

           PERFORM  5000-CIERRA-FICHEROS

           PERFORM  9999-MUESTRA-ESTADISTICAS

           STOP RUN

      *    END PROGRAM READ-WRITE.

           .


       5000-CIERRA-FICHEROS.

           CLOSE  TRA-ENTRADA
                  EMP-ENTRADA
                  CRUCE-SALIDA

           IF  FS-TRA-ENTRADA NOT = '00' AND FS-TRA-ENTRADA NOT = '10'
               MOVE  'ERROR DE APERTURA'          TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           IF  FS-EMP-ENTRADA NOT = '00' AND FS-EMP-ENTRADA NOT = '10'
               MOVE 'ERROR DE APERTURA'           TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           IF  FS-SALIDA NOT = '00' AND FS-SALIDA NOT = '10'
               MOVE 'ERROR DE APERTURA'           TO  WS-ERROR-ACCION
               MOVE  WS-1200-APERTURA-FICHEROS    TO  WS-ERROR-PARRAFO
               PERFORM 9500-ERROR-STATUS-FICHERO
           END-IF

           .


       9500-ERROR-STATUS-FICHERO.

           IF  FS-TRA-ENTRADA NOT EQUAL TO '00' AND
               FS-TRA-ENTRADA NOT EQUAL TO '10'

               MOVE  'TRA-ENTRADA   '      TO  WS-ERROR-FICHERO
               MOVE  FS-TRA-ENTRADA        TO  WS-ERROR-FS
               PERFORM  9900-ERROR-ABORTAR

           END-IF


           IF  FS-EMP-ENTRADA  NOT EQUAL TO '00' AND
               FS-EMP-ENTRADA  NOT EQUAL TO '10'

               MOVE  'EMP-ENTRADA   '     TO  WS-ERROR-FICHERO
               MOVE  FS-EMP-ENTRADA       TO  WS-ERROR-FS
               PERFORM  9900-ERROR-ABORTAR

           END-IF


           IF  FS-SALIDA  NOT EQUAL TO '00' AND
               FS-SALIDA  NOT EQUAL TO '10'

               MOVE  'SALIDA   '           TO  WS-ERROR-FICHERO
               MOVE  FS-SALIDA             TO  WS-ERROR-FS
               PERFORM  9900-ERROR-ABORTAR

           END-IF


           .


       9900-ERROR-ABORTAR.

           DISPLAY  '============================='
           DISPLAY  'ERROR EN PROG. CRUCE_1N. '
           DISPLAY  'PARRAFO DE ERROR:            '  WS-ERROR-PARRAFO
           DISPLAY  'TIPO DE ERROR:               '  WS-ERROR-ACCION
           DISPLAY  'FICHERO DE ERROR:            '  WS-ERROR-FICHERO
           DISPLAY  'FILE-STATUS DE FICHERO:      '  WS-ERROR-FS

           PERFORM  9999-MUESTRA-ESTADISTICAS

           STOP RUN

           .


       9999-MUESTRA-ESTADISTICAS.

           DISPLAY  ' '
           DISPLAY  '============================='
           DISPLAY  '        ESTADISTICAS         '
           DISPLAY  'REGISTROS LEIDOS TRA:        '
                     WS-CONT-LEIDOS-TRA
           DISPLAY  'REGISTROS LEIDOS EMP:        '
                     WS-CONT-LEIDOS-EMP
           DISPLAY  'REGISTROS ESCRITOS:          '
                     WS-CONT-ESCRITOS-SAL
           DISPLAY  '============================='

           .
