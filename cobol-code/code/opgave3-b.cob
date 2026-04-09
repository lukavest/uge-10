       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-3B.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01  KUNDE-ID     PIC X(10) VALUE SPACES.
       01  FORNAVN      PIC X(20) VALUE SPACES.
       01  EFTERNAVN    PIC X(20) VALUE SPACES.

       01  KONTO-NUMMER PIC X(20) VALUE SPACES.
       01  BALANCE      PIC 9(7)V9(2) VALUE 0.
       01  VALUTAKODE   PIC X(3) VALUE SPACES.

       01  IX           PIC 9(2) VALUE 0.
       01  IX2          PIC 9(2) VALUE 0.
       01  CHAR-CURRENT PIC X VALUE SPACE.
       01  CHAR-LAST    PIC X VALUE SPACE.
       01  NAVN-TMP    PIC X(41) VALUE SPACES.
       01  NAVN           PIC X(41) VALUE SPACES.

       PROCEDURE DIVISION.
           MOVE "1234567890" TO KUNDE-ID.
           MOVE "Lars" TO FORNAVN.
           MOVE "Hansen" TO EFTERNAVN.
           MOVE "DK12345678912345" TO KONTO-NUMMER.
           MOVE 2500.75 TO BALANCE.
           MOVE "DKK" TO VALUTAKODE.

           STRING FORNAVN DELIMITED BY SIZE " "
              DELIMITED BY SIZE EFTERNAVN 
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
