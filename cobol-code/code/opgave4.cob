       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-4.

       DATA DIVISION.
           WORKING-STORAGE SECTION. 
      
           01 KUNDEOPL.
               02  KUNDE-ID     PIC X(10) VALUE SPACES.
               02  FORNAVN      PIC X(20) VALUE SPACES.
               02  EFTERNAVN    PIC X(20) VALUE SPACES.
           01  KONTOINFO.
               02  KONTO-NUMMER PIC X(20) VALUE SPACES.
               02  BALANCE      PIC 9(7)V9(2) VALUE 0.
               02  VALUTAKODE   PIC X(3) VALUE SPACES.

       PROCEDURE DIVISION.
           MOVE "1234567890" TO KUNDE-ID
           MOVE "Lars" TO FORNAVN
           MOVE "Hansen" TO EFTERNAVN
           MOVE "DK12345678912345" TO KONTO-NUMMER
           MOVE 2500.75 TO BALANCE
           MOVE "DKK" TO VALUTAKODE

           DISPLAY "----------------------------------------"
           DISPLAY "Kunde-ID: " KUNDE-ID
           DISPLAY "Navn: " FORNAVN " " EFTERNAVN
           DISPLAY "Kontonummer: " KONTO-NUMMER
           DISPLAY "Balance: "VALUTAKODE " " BALANCE

           DISPLAY "----------------------------------------"
           DISPLAY KUNDEOPL
           PERFORM DIS-LINE.
           DISPLAY KONTOINFO
           STOP RUN.

       DIS-LINE.
           DISPLAY "----------------------------------------".
       