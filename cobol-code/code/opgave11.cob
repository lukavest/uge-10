       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPG-11.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIL-TRANS ASSIGN TO "../data/Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FIL-TRANS.
       01 TRANSAKTION.
           COPY "../copybooks/TRANSAKTIONER.cpy".

       WORKING-STORAGE SECTION.

       01 KURS-DKK.
           02 EUR PIC 9V99 VALUE 7.47.
           02 USD PIC 9V99 VALUE 6.34.
       01 PRETTY-NUMERIC   PIC -ZZZ,ZZZ,ZZ9.99.
       01 TMP-NUMERIC      PIC S9(10)V99.
       01 CURR-KONTO-ID PIC X(14).
       
       01 IX   PIC 99 VALUE 1.
       01 NUM-KUNDER PIC 9(04) VALUE 0.
       
       01 KUNDE-ARR OCCURS 0 TO 1000 TIMES
           DEPENDING ON NUM-KUNDER.
           COPY "../copybooks/STAT-KUNDE.cpy".

       01 MÅNED-ARR OCCURS 12 TIMES.
           02 TOTAL-IND PIC S9(10)V99 VALUE 0.
           02 TOTAL-UD  PIC S9(10)V99 VALUE 0.
       
       01 NUM-BUTIK PIC 9(3) VALUE 0.

       01 BUTIK-ARR OCCURS 0 TO 100 TIMES DEPENDING ON NUM-BUTIK 
           INDEXED BY B-IX.
           02 BUTIK-T  PIC X(20).
           02 TÆLLER   PIC 9(04) VALUE 1.
           02 OMSÆTNING PIC S9(10)V99 VALUE 0.

       01 TOP-N PIC 99 VALUE 3.
  
       01 EOF-FLAG PIC X VALUE "N".
       
       01 TIME-STAMP.
           02 FILLER   PIC X(05).
           02 MÅNED    PIC 9(02).
           02 FILLER   PIC X(20).

       01 MÅNED-UDSKRIFT.
           02 FELT-MÅNED   PIC 99.
           02 FILLER       PIC X(10).
           02 FELT-IND     PIC -ZZZ,ZZZ,ZZ9.99.
           02 FILLER       PIC X(4).
           02 FELT-UD      PIC -ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
           PERFORM TST
      *    DISPLAY "Beginning"
      *    PERFORM ITER-TRANS
      *    DISPLAY "Iterating done"
      *    SORT KUNDE-ARR DESCENDING SALDO OF KUNDE-ARR
      *    
      *    DISPLAY NUM-KUNDER " kunder"
      *    DISPLAY "Top kunder:"
      *    
      *    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > TOP-N
      *        MOVE SALDO OF KUNDE-ARR(IX) TO PRETTY-NUMERIC
      *        DISPLAY KONTO-ID OF KUNDE-ARR(IX)
      *                NAVN     OF KUNDE-ARR(IX)
      *                PRETTY-NUMERIC
      *    END-PERFORM
      *    
      *    DISPLAY "Måned   Indbetalinger (DKK) Udbetalinger (DKK)"
      *    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
      *        MOVE IX TO FELT-MÅNED OF MÅNED-UDSKRIFT
      *        MOVE TOTAL-IND OF MÅNED-ARR(IX) 
      *           TO FELT-IND OF MÅNED-UDSKRIFT
      *        MOVE TOTAL-UD  OF MÅNED-ARR(IX) 
      *           TO FELT-UD  OF MÅNED-UDSKRIFT
      *        DISPLAY MÅNED-UDSKRIFT
      *    END-PERFORM
      *    DISPLAY "Butik    Antal transaktioner"
      *    PERFORM VARYING B-IX FROM 1 BY 1 UNTIL B-IX > NUM-BUTIK
      *        MOVE OMSÆTNING OF BUTIK-ARR(B-IX) TO PRETTY-NUMERIC
      *        DISPLAY BUTIK-T OF BUTIK-ARR(B-IX) 
      *            TÆLLER OF BUTIK-ARR(B-IX)
      *            PRETTY-NUMERIC
      *    END-PERFORM
      *    
      *    SORT BUTIK-ARR DESCENDING OMSÆTNING OF BUTIK-ARR
      *    DISPLAY "Top butikker"
      *    DISPLAY "Butik    Omsætning"
      *    PERFORM VARYING B-IX FROM 1 BY 1 UNTIL B-IX > 5
      *        MOVE OMSÆTNING OF BUTIK-ARR(B-IX) TO PRETTY-NUMERIC
      *        DISPLAY BUTIK-T OF BUTIK-ARR(B-IX) PRETTY-NUMERIC
      *    END-PERFORM
           STOP RUN.

       TST.
           OPEN INPUT FIL-TRANS
           PERFORM 10 TIMES
           READ FIL-TRANS
           DISPLAY "'"CPR"' " "'"ADRESSE"' " 
                   "'"FØDSELSDATO"' " "'"KONTO-ID OF TRANSAKTION"' "
                   "'"REG-NR"' " "'"BELØB"' "
                   "'"VALUTA"' " "'"TRANS-TYPE"' "
                   "'"BUTIK"' " "'"TIDSPUNKT"' "
           END-PERFORM

           CLOSE FIL-TRANS.

       ITER-TRANS.
           OPEN INPUT FIL-TRANS
      * Read first record
           READ FIL-TRANS
               AT END MOVE "Y" TO EOF-FLAG
           END-READ

           IF EOF-FLAG = "N"
               PERFORM NEXT-KUNDE
           END-IF

           PERFORM UNTIL EOF-FLAG = "Y"
               IF KONTO-ID OF TRANSAKTION = CURR-KONTO-ID
                   PERFORM PROCESS-TRANSACTION
               ELSE
      * New customer begins
                   PERFORM NEXT-KUNDE
                   PERFORM PROCESS-TRANSACTION
               END-IF
               READ FIL-TRANS
                   AT END MOVE "Y" TO EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE FIL-TRANS.

       NEXT-KUNDE.
           MOVE KONTO-ID OF TRANSAKTION TO CURR-KONTO-ID
           ADD 1 TO NUM-KUNDER
           MOVE CURR-KONTO-ID    TO KONTO-ID OF KUNDE-ARR(NUM-KUNDER)
           MOVE NAVN OF TRANSAKTION TO NAVN  OF KUNDE-ARR(NUM-KUNDER)
           MOVE 50000               TO SALDO OF KUNDE-ARR(NUM-KUNDER)
           
           DISPLAY NUM-KUNDER CURR-KONTO-ID NAVN OF TRANSAKTION.

       PROCESS-TRANSACTION.
           MOVE BELØB OF TRANSAKTION TO TMP-NUMERIC
           EVALUATE VALUTA OF TRANSAKTION
               WHEN "EUR "
                   MULTIPLY EUR BY TMP-NUMERIC
               WHEN "USD "
                   MULTIPLY USD BY TMP-NUMERIC
               WHEN "DKK "
                   CONTINUE
               WHEN OTHER
                   MOVE ZEROS TO TMP-NUMERIC
           END-EVALUATE
           ADD TMP-NUMERIC TO SALDO OF KUNDE-ARR(NUM-KUNDER)
           MOVE TIDSPUNKT OF TRANSAKTION TO TIME-STAMP

           IF TMP-NUMERIC > 0
               ADD TMP-NUMERIC TO TOTAL-IND OF MÅNED-ARR(MÅNED)
           ELSE
               ADD TMP-NUMERIC TO TOTAL-UD  OF MÅNED-ARR(MÅNED)
           END-IF
           
           MOVE 1 TO B-IX
           SEARCH BUTIK-ARR VARYING B-IX
               AT END
                   ADD 1 TO NUM-BUTIK
                   MOVE BUTIK TO BUTIK-T OF BUTIK-ARR(NUM-BUTIK)
                   MOVE FUNCTION ABS(TMP-NUMERIC) 
                       TO OMSÆTNING OF BUTIK-ARR(NUM-BUTIK)
               WHEN BUTIK-T OF BUTIK-ARR(B-IX) = BUTIK
                   ADD 1 TO TÆLLER OF BUTIK-ARR(B-IX)
                   ADD FUNCTION ABS(TMP-NUMERIC) 
                       TO OMSÆTNING OF BUTIK-ARR(B-IX)
           END-SEARCH.
           