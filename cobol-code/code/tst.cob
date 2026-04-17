       IDENTIFICATION DIVISION.
       PROGRAM-ID. tst.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIL ASSIGN TO "../data/TST.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
      *    SELECT FIL-TRANS ASSIGN TO "../data/Transaktioner.txt"
      *        ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD FIL.
           01 BUTIK PIC X(20).
           01 TTYPE PIC X(20).

       WORKING-STORAGE SECTION. 
       
       PROCEDURE DIVISION.
       
           DISPLAY "ÆØÅ æøå"
           
           OPEN INPUT FIL
           
           PERFORM 10 TIMES
               READ FIL
               DISPLAY "'" BUTIK "' " "'" TTYPE "'"
           END-PERFORM

           CLOSE FIL

           STOP RUN.
           
