       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-10.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIL-BANKER ASSIGN TO "../data/Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FIL-TRANS ASSIGN TO "../data/Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FIL-OUT ASSIGN TO "../data/KontoUdskrift.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
              
       DATA DIVISION.
       
       
       FILE SECTION.
       FD FIL-BANKER.
       01 BANKER.
           COPY "../data/BANKER.cpy".
       
       FD FIL-TRANS.
       01 TRANSAKTIONER.
           COPY "../data/TRANSAKTIONER.cpy".

       FD FIL-OUT.
       01 KONTOUDSKRIFT.
           02 OUTPUT-TEXT PIC X(100).

       WORKING-STORAGE SECTION. 
       01 NUM-BANKER   PIC 9(03) VALUE 100.
       01 NUM-TRANS    PIC 9(06) VALUE 30.
       01 IX           PIC 9(03) VALUE 1.
       01 CURR-KONTO-ID PIC X(14).
       01 CURR-REG-NR  PIC 9(6).
       01 FIELD-DESC   PIC X(15).
       01 FIELD-VAL    PIC X(50).
       01 INDENT       PIC X(30) VALUE SPACES.
       01 TIME-STMP.
           02 DATO     PIC X(10).
           02 FILLER   PIC X(01) VALUE "-".
           02 TID      PIC X(08).
           02 FILLER   PIC X(01) VALUE ".".
           02 MS       PIC 9(06).

      
       01 BANK-ARR OCCURS 0 TO 100 TIMES DEPENDING ON NUM-BANKER.
           COPY "../data/BANKER.cpy".

       01 TRANS-ARR OCCURS 0 TO 10000 TIMES DEPENDING ON NUM-TRANS.
           COPY "../data/TRANSAKTIONER.cpy".

       PROCEDURE DIVISION.
           PERFORM BANK-ARR-FILL
           PERFORM TRANS-ARR-FILL
           PERFORM ITER-TRANS
           STOP RUN.
       
       WRITE-P.
           WRITE KONTOUDSKRIFT
           MOVE SPACES TO OUTPUT-TEXT.

       WRITE-FIELD.
           STRING 
               FIELD-DESC  DELIMITED BY SIZE
               FIELD-VAL   DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-P.

       WRITE-FIELD-INDENTED.
           STRING
               INDENT      DELIMITED BY SIZE
               FIELD-DESC  DELIMITED BY SIZE
               FIELD-VAL   DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-P.

       WRITE-HEAD.
           MOVE "-------------------------------" TO OUTPUT-TEXT
           PERFORM WRITE-P
           MOVE "Kunde: " TO FIELD-DESC
           MOVE NAVN OF TRANS-ARR(IX) TO FIELD-VAL
           PERFORM WRITE-FIELD
           MOVE "Adresse: " TO FIELD-DESC
           MOVE ADRESSE OF TRANS-ARR(IX) TO FIELD-VAL
           PERFORM WRITE-FIELD
           
           MOVE "Reg.nr.: " TO FIELD-DESC
           MOVE REG-NR OF TRANS-ARR(IX) TO FIELD-VAL
           PERFORM WRITE-FIELD-INDENTED
           MOVE "Bankadresse: " TO FIELD-DESC
           MOVE BANKADRESSE OF BANK-ARR(CURR-REG-NR) TO FIELD-VAL
           PERFORM WRITE-FIELD-INDENTED
           MOVE "Telefon: " TO FIELD-DESC
           MOVE TELEFON OF BANK-ARR(CURR-REG-NR) TO FIELD-VAL
           PERFORM WRITE-FIELD-INDENTED
           MOVE "E-mail: " TO FIELD-DESC
           MOVE EMAIL OF BANK-ARR(CURR-REG-NR) TO FIELD-VAL
           PERFORM WRITE-FIELD-INDENTED

           STRING
               "Kontoudskrift for kontonr.: " DELIMITED BY SIZE
               CURR-KONTO-ID DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-P.

       WRITE-UDSKRIFT.
           MOVE TIDSPUNKT OF TRANS-ARR(IX) TO TIME-STMP
           STRING 
               DATO OF TIME-STMP DELIMITED BY SIZE
               SPACE DELIMITED BY SIZE
               TID OF TIME-STMP DELIMITED BY SIZE
               TRANSAKTIONSTYPE OF TRANS-ARR(IX) DELIMITED BY SIZE
               BELØB            OF TRANS-ARR(IX) DELIMITED BY SIZE
               VALUTA           OF TRANS-ARR(IX) DELIMITED BY SIZE
               BUTIK            OF TRANS-ARR(IX) DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-P.

       ITER-TRANS.
           OPEN OUTPUT FIL-OUT
           MOVE 1 TO IX
           PERFORM UNTIL IX > NUM-TRANS
               MOVE REG-NR OF TRANS-ARR(IX) TO CURR-REG-NR
               MOVE KONTO-ID OF TRANS-ARR(IX) TO CURR-KONTO-ID
               PERFORM WRITE-HEAD
               PERFORM VARYING IX FROM IX BY 1 UNTIL IX > NUM-TRANS
                   
                   IF KONTO-ID OF TRANS-ARR(IX) NOT = CURR-KONTO-ID
                       EXIT PERFORM
                   END-IF
                   PERFORM WRITE-UDSKRIFT
               END-PERFORM

           END-PERFORM

           CLOSE FIL-OUT.

       BANK-ARR-FILL.
           OPEN INPUT FIL-BANKER
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-BANKER
               READ FIL-BANKER INTO BANK-ARR(IX)
                   AT END
                       SUBTRACT 1 FROM IX
                       MOVE IX TO NUM-BANKER
                       EXIT PERFORM
               END-READ
           END-PERFORM
           CLOSE FIL-BANKER
           DISPLAY "Antal banker: " NUM-BANKER.

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

       