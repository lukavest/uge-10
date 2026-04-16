       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-11-LOMEM.

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
    
        01 PRETTY-NUMERIC   PIC -ZZZ,ZZZ,ZZ9.99.
        01 TMP-NUMERIC      PIC S9(10)V99.
           
        01 KURS-DKK.
           02 EUR PIC 9V99 VALUE 7.47.
           02 USD PIC 9V99 VALUE 6.34.
       
        01 TRANSAKTION.
           COPY "../copybooks/TRANSAKTIONER.cpy".
    
        01 KUNDE-IX    PIC 9(4) VALUE 1.
        01 NUM-KUNDER  PIC 9(4) VALUE 1001.
        01 KUNDE.
           COPY "../copybooks/STAT-KUNDE.cpy".


        01 TOP-IX  PIC 99 VALUE 1.
        01 TOP-N   PIC 99 VALUE 3.

        01 TOP-KUNDER.
           02 TOP-ARR OCCURS 0 TO 10 TIMES DEPENDING ON TOP-N.
               COPY "../copybooks/STAT-KUNDE.cpy".

        01 MIN-THRESH PIC S9(10)V99 VALUE 0.

        01 CNT PIC 9(6) VALUE 1.
        01 EOF-FLAG PIC X VALUE "N".


       PROCEDURE DIVISION.
           PERFORM ITER-TRANSACTIONS
           DISPLAY CNT         " transaktioner"
           DISPLAY KUNDE-IX    " kunder"
           DISPLAY "Top " TOP-N " kunder: "
           PERFORM SHOW-TOP
           STOP RUN.

       SHOW-TOP.
           PERFORM VARYING TOP-IX FROM 1 BY 1 UNTIL TOP-IX > TOP-N
               MOVE SALDO OF TOP-ARR(TOP-IX) TO PRETTY-NUMERIC
               DISPLAY KONTO-ID OF TOP-ARR(TOP-IX)
                       NAVN     OF TOP-ARR(TOP-IX)
                       PRETTY-NUMERIC
           END-PERFORM.
       
       COUNT-MONEY.
           MOVE BELØB OF TRANSAKTION TO TMP-NUMERIC
           EVALUATE VALUTA OF TRANSAKTION
               WHEN "EUR "
                   MULTIPLY EUR BY TMP-NUMERIC
               WHEN "USD "
                   MULTIPLY USD BY TMP-NUMERIC
               WHEN "DKK "
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Ukendt valuta: " VALUTA OF TRANSAKTION
                   MOVE ZEROS TO TMP-NUMERIC
           END-EVALUATE
           ADD TMP-NUMERIC TO SALDO OF KUNDE.
       
       RANK.
           IF SALDO OF KUNDE > MIN-THRESH
               MOVE KUNDE TO TOP-ARR(TOP-N)
               SORT TOP-ARR DESCENDING SALDO OF TOP-ARR
               MOVE SALDO OF TOP-ARR(TOP-N) TO MIN-THRESH
           END-IF.

       ITER-TRANSACTIONS.
           OPEN INPUT FIL-TRANS
           READ FIL-TRANS INTO TRANSAKTION
           PERFORM VARYING KUNDE-IX FROM 1 BY 1 
                   UNTIL KUNDE-IX > NUM-KUNDER OR EOF-FLAG = "Y"
               MOVE KONTO-ID   OF TRANSAKTION TO KONTO-ID  OF KUNDE
               MOVE NAVN       OF TRANSAKTION TO NAVN      OF KUNDE
               MOVE 50000 TO SALDO OF KUNDE
               
               PERFORM
                 UNTIL KONTO-ID OF TRANSAKTION NOT = KONTO-ID OF KUNDE
                   PERFORM COUNT-MONEY
                   READ FIL-TRANS INTO TRANSAKTION
                       AT END
                           MOVE "Y" TO EOF-FLAG
                           SUBTRACT 1 FROM KUNDE-IX
                           EXIT PERFORM
                       NOT AT END
                           ADD 1 TO CNT
                   END-READ
               END-PERFORM
               PERFORM RANK
           END-PERFORM
           CLOSE FIL-TRANS.
       