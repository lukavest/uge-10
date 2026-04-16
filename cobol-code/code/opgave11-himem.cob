       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-11-HIMEM.

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
       
    
       01 SALDO-SUM PIC S9(10)V99 VALUE 50000.

       01 PRETTY-NUMERIC   PIC -ZZZ,ZZZ,ZZ9.99.
       01 TMP-NUMERIC      PIC S9(10)V99.
       
       01 KURS-DKK.
           02 EUR PIC 9V99 VALUE 7.47.
           02 USD PIC 9V99 VALUE 6.34.
       01 NUM-TRANS    PIC 9(05) VALUE 10000.
       01 IX           PIC 9(05) VALUE 1.
       01 TRANS-ARR OCCURS 0 TO 10000 TIMES DEPENDING ON NUM-TRANS.
           COPY "../copybooks/TRANSAKTIONER.cpy".
    

       01 KUNDE-IX PIC 9(05) VALUE 1.
       01 NUM-KUNDER PIC 9(04) VALUE 1000.
       01 KUNDE-ARR OCCURS 0 TO 1000 TIMES DEPENDING ON NUM-KUNDER.
           COPY "../copybooks/STAT-KUNDE.cpy".
       01 CURR-KONTO-ID PIC X(14).
       01 TOP-N PIC 99 VALUE 3.

       PROCEDURE DIVISION.
           PERFORM TRANS-ARR-FILL
           DISPLAY NUM-TRANS " transaktioner"
           PERFORM ITER-TRANSACTIONS
           DISPLAY NUM-KUNDER  " kunder"
           SORT KUNDE-ARR DESCENDING SALDO OF KUNDE-ARR
           DISPLAY "Top kunder: "
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > TOP-N
               MOVE SALDO OF KUNDE-ARR(IX) TO PRETTY-NUMERIC
               DISPLAY KONTO-ID OF KUNDE-ARR(IX)
                       NAVN     OF KUNDE-ARR(IX)
                       PRETTY-NUMERIC
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
           ADD TMP-NUMERIC TO SALDO-SUM.
       
       ITER-TRANSACTIONS.
           MOVE 1 TO IX
           PERFORM VARYING KUNDE-IX FROM 1 BY 1 
                   UNTIL KUNDE-IX > NUM-KUNDER OR IX > NUM-TRANS
               MOVE KONTO-ID OF TRANS-ARR(IX) TO CURR-KONTO-ID

               MOVE CURR-KONTO-ID TO KONTO-ID OF KUNDE-ARR(KUNDE-IX)
               MOVE NAVN OF TRANS-ARR(IX) TO NAVN OF KUNDE-ARR(KUNDE-IX)
               
               PERFORM VARYING IX FROM IX BY 1 UNTIL IX > NUM-TRANS
                   IF KONTO-ID OF TRANS-ARR(IX) NOT = CURR-KONTO-ID
                       EXIT PERFORM
                   END-IF
                   PERFORM COUNT-MONEY
               END-PERFORM
               
               MOVE SALDO-SUM TO SALDO OF KUNDE-ARR(KUNDE-IX)
               MOVE 50000 TO SALDO-SUM
           END-PERFORM
           MOVE KUNDE-IX TO NUM-KUNDER
           SUBTRACT 1 FROM NUM-KUNDER.
       
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
           CLOSE FIL-TRANS.
           

       