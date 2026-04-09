       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-6.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT KUNDE-FIL ASSIGN TO "../data/Kundeoplysninger.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD KUNDE-FIL.
       01 KUNDE-OPL.
           COPY "../data/KUNDER.cpy".

       WORKING-STORAGE SECTION. 
       01 EOF-FLAG PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT KUNDE-FIL
           PERFORM UNTIL EOF-FLAG = "Y"
               READ KUNDE-FIL INTO KUNDE-OPL
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       DISPLAY "Kunde-ID: " KUNDE-ID
                       DISPLAY "Navn: " FORNAVN " " EFTERNAVN
                       DISPLAY "Kontonummer: " KONTO-NUMMER
                       DISPLAY "Balance: " VALUTAKODE " " BALANCE

                       PERFORM DIS-LINE
                       DISPLAY ADDRESSE
                       PERFORM DIS-LINE
                       DISPLAY KONTAKT
               END-READ
           END-PERFORM

           CLOSE KUNDE-FIL
           STOP RUN.


       DIS-LINE.
           DISPLAY "----------------------------------------".
       
