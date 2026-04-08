       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-4A.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 

       01 KUNDEOPL
           02  KUNDE-ID     PIC X(10) VALUE SPACES.
           02  FORNAVN      PIC X(20) VALUE SPACES.
           02  EFTERNAVN    PIC X(20) VALUE SPACES.
       01  KONTOINFO
            02  KONTO-NUMMER PIC X(20) VALUE SPACES.
            02  BALANCE      PIC 9(7)V9(2) VALUE 0.
            02  VALUTAKODE   PIC X(3) VALUE SPACES.

       01  IX           PIC 9(2) VALUE 0.
       01  IX2          PIC 9(2) VALUE 0.
       01  CHAR-CURRENT PIC X VALUE SPACE.
       01  CHAR-LAST    PIC X VALUE SPACE.
       01  NAVN-TMP    PIC X(41) VALUE SPACES.
       01  NAVN           PIC X(41) VALUE SPACES.

       PROCEDURE DIVISION.
           MOVE "1234567890" TO KUNDEOPL.KUNDE-ID.
           MOVE "Lars" TO KUNDEOPL.FORNAVN.
           MOVE "Hansen" TO KUNDEOPL.EFTERNAVN.
           MOVE "DK12345678912345" TO KONTOINFO.KONTO-NUMMER.
           MOVE 2500.75 TO KONTOINFO.BALANCE.
           MOVE "DKK" TO KONTOINFO.VALUTAKODE.

           STRING KUNDEOPL.FORNAVN DELIMITED BY SIZE " "
              DELIMITED BY SIZE KUNDEOPL.EFTERNAVN 
              DELIMITED BY SIZE
              INTO NAVN-TMP.

           PERFORM VARYING IX FROM 0 BY 1 UNTIL IX > LENGTH OF NAVN-TMP
               MOVE NAVN-TMP(IX:1) TO CHAR-CURRENT
               IF CHAR-CURRENT NOT = SPACE OR CHAR-LAST NOT = SPACE THEN
                  MOVE CHAR-CURRENT TO NAVN(IX2:1)
                  ADD 1 TO IX2
               END-IF
               MOVE CHAR-CURRENT TO CHAR-LAST
           END-PERFORM.
           
           DISPLAY "Navn (med mellemrum): '" NAVN-TMP "'"
           DISPLAY "Navn (renset)       : '" NAVN "'"

           DISPLAY "----------------------------------------"
           DISPLAY "Kunde ID :  " KUNDE-ID
           DISPLAY "Navn (renset) : " NAVN
           DISPLAY "Kontonummer : " KONTO-NUMMER
           DISPLAY "Balance : " BALANCE " " VALUTAKODE
           DISPLAY "----------------------------------------"
           STOP RUN.
