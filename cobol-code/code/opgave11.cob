       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-10.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIL-TRANS ASSIGN TO "../data/Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD FIL-TRANS.
       01 TRANSAKTIONER.
           COPY "../copybooks/TRANSAKTIONER.cpy".

       WORKING-STORAGE SECTION.
       
    
       01 PENGE-SUM.
           02 SALDO            PIC S9(10)V99 VALUE 50000.
           02 PRETTY-NUMERIC   PIC -ZZZ,ZZZ,ZZ9.99.
           02 TMP-NUMERIC      PIC S9(10)V99.

       01 CURR-KONTO-ID    PIC X(14).
       01 CURR-REG-NR      PIC 9(06).
       01 FIELD-DESC       PIC X(15).
       01 FIELD-DESC-LONG  PIC X(30).
       01 FIELD-VAL    PIC X(50).
       01 INDENT       PIC X(60) VALUE SPACES.
       01 DASH-LINE    PIC X(100) VALUE ALL '-'.
       01 TIME-STMP.
           02 DATO     PIC X(10).
           02 FILLER   PIC X(01) VALUE "-".
           02 TID      PIC X(08).
           02 FILLER   PIC X(01) VALUE ".".
           02 MS       PIC 9(06).
       
       01 KURS-DKK.
           02 EUR PIC 9V99 VALUE 7.47.
           02 USD PIC 9V99 VALUE 6.34.
       01 NUM-TRANS    PIC 9(05) VALUE 100.
       01 IX           PIC 9(05) VALUE 1.
       01 TRANS-ARR OCCURS 0 TO 10000 TIMES DEPENDING ON NUM-TRANS.
           COPY "../copybooks/TRANSAKTIONER.cpy".
    

       01 KUNDE-IX PIC 9(05) VALUE 1.
       01 NUM-KUNDER PIC 9(04) VALUE 10.
       01 KUNDE-ARR OCCURS 0 TO 1000 TIMES DEPENDING ON NUM-KUNDER.
           02 KONTO-ID PIC X(14) VALUE SPACES.
           02 NAVN     PIC X(30) VALUE SPACES.
           02 SALDO    PIC S9(10)V99.
      *    02 SALDO    PIC -ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.

       PROCEDURE DIVISION.
           PERFORM TRANS-ARR-FILL
           PERFORM ITER-TRANSACTIONS
           SORT KUNDE-ARR ASCENDING SALDO OF KUNDE-ARR
           DISPLAY "Top kunder: "
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-KUNDER
               DISPLAY KUNDE-ARR(IX)
           END-PERFORM
           STOP RUN.
       
       COUNT-MONEY.
           MOVE BELØB OF TRANS-ARR(IX) TO TMP-NUMERIC
           EVALUATE VALUTA OF TRANS-ARR(IX)
               WHEN "EUR "
                   MULTIPLY EUR BY TMP-NUMERIC
               WHEN "USD "
                   MULTIPLY USD BY TMP-NUMERIC
               WHEN "DKK "
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Ukendt valuta: " VALUTA OF TRANS-ARR(IX)
                   MOVE ZEROS TO TMP-NUMERIC
           END-EVALUATE
           ADD TMP-NUMERIC TO SALDO OF PENGE-SUM.
       
       ITER-TRANSACTIONS.
           MOVE 1 TO IX
           PERFORM VARYING KUNDE-IX FROM 1 BY 1 
                   UNTIL KUNDE-IX > NUM-KUNDER
               MOVE KONTO-ID OF TRANS-ARR(IX) TO CURR-KONTO-ID

               MOVE CURR-KONTO-ID TO KONTO-ID OF KUNDE-ARR(KUNDE-IX)
               MOVE NAVN OF TRANS-ARR(IX) TO NAVN OF KUNDE-ARR(KUNDE-IX)
               
               PERFORM VARYING IX FROM IX BY 1 UNTIL IX > NUM-TRANS
                   IF KONTO-ID OF TRANS-ARR(IX) NOT = CURR-KONTO-ID
                       EXIT PERFORM
                   END-IF
                   PERFORM COUNT-MONEY
               END-PERFORM
               
               MOVE SALDO OF PENGE-SUM TO SALDO OF KUNDE-ARR(KUNDE-IX)

               DISPLAY KUNDE-IX " " KUNDE-ARR(KUNDE-IX)
               
               MOVE ZEROS TO PENGE-SUM
               MOVE 50000 TO SALDO OF PENGE-SUM
           END-PERFORM.
       
       TRANS-ARR-FILL.
           OPEN INPUT FIL-TRANS
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-TRANS
               READ FIL-TRANS INTO TRANS-ARR(IX)
                   AT END
                       SUBTRACT 1 FROM IX
                       MOVE IX TO NUM-TRANS
                       EXIT PERFORM
                END-READ
           END-PERFORM
           CLOSE FIL-TRANS
           DISPLAY "Antal transaktioner: " NUM-TRANS.

       