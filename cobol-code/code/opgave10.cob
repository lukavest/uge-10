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
           COPY "../copybooks/BANKER.cpy".
       
       FD FIL-TRANS.
       01 TRANSAKTIONER.
           COPY "../copybooks/TRANSAKTIONER.cpy".

       FD FIL-OUT.
       01 KONTOUDSKRIFT.
           02 OUTPUT-TEXT PIC X(150).

       WORKING-STORAGE SECTION.
       01 IX           PIC 9(05) VALUE 1.
       01 PENGE-SUM.
           02 SALDO            PIC S9(10)V99 VALUE 50000.
           02 TOTAL-INDBETALT  PIC S9(10)V99 VALUE ZEROS.
           02 TOTAL-UDBETALT   PIC S9(10)V99 VALUE ZEROS.
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
       
       01 KONTO-UDSKRIFT.
           02 DATO         PIC X(15).
           02 TID          PIC X(15).
           02 TRANS-TYPE   PIC X(20).
           02 BELØB-DKK    PIC X(15).
           02 FILLER       PIC X(02) VALUE SPACES.
           02 BELØB        PIC X(15).
           02 FILLER       PIC X(02) VALUE SPACES.
           02 VALUTA       PIC X(10).
           02 BUTIK        PIC X(20).
       01 NUM-BANKER   PIC 9(03) VALUE 100.
       01 BANK-ARR OCCURS 0 TO 100 TIMES DEPENDING ON NUM-BANKER.
           COPY "../copybooks/BANKER.cpy".
       01 NUM-TRANS    PIC 9(05) VALUE 10000.
       01 TRANS-ARR OCCURS 0 TO 10000 TIMES DEPENDING ON NUM-TRANS.
           COPY "../copybooks/TRANSAKTIONER.cpy".

       PROCEDURE DIVISION.
           PERFORM BANK-ARR-FILL
           PERFORM TRANS-ARR-FILL
           PERFORM ITER-TRANS
           STOP RUN.
       
       WRITE-LINE.
           WRITE KONTOUDSKRIFT
           MOVE SPACES TO OUTPUT-TEXT.
       
       WRITE-FIELD.
           STRING 
               FIELD-DESC  DELIMITED BY SIZE
               FIELD-VAL   DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-LINE.

       WRITE-FIELD-LONG.
           STRING 
               FIELD-DESC-LONG DELIMITED BY SIZE
               FIELD-VAL       DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-LINE.

       WRITE-FIELD-INDENTED.
           STRING
               INDENT      DELIMITED BY SIZE
               FIELD-DESC  DELIMITED BY SIZE
               FIELD-VAL   DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           PERFORM WRITE-LINE.

       WRITE-HEAD.
           MOVE DASH-LINE TO OUTPUT-TEXT
           PERFORM WRITE-LINE
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
           
           MOVE "Kontoudskrift for kontonr.: " TO FIELD-DESC-LONG
           MOVE CURR-KONTO-ID TO FIELD-VAL
           PERFORM WRITE-FIELD-LONG
           MOVE "Dato"             TO DATO         OF KONTO-UDSKRIFT
           MOVE "Tidspunkt"        TO TID          OF KONTO-UDSKRIFT
           MOVE "Transaktionstype" TO TRANS-TYPE   OF KONTO-UDSKRIFT
      *    "Beløb" with 'ø' corrupts OUTPUT :(
           MOVE "Beloeb (DKK)"     TO BELØB-DKK    OF KONTO-UDSKRIFT
           MOVE "Beloeb"           TO BELØB        OF KONTO-UDSKRIFT
           MOVE "Valuta"           TO VALUTA       OF KONTO-UDSKRIFT
           MOVE "Butik"            TO BUTIK        OF KONTO-UDSKRIFT
           MOVE KONTO-UDSKRIFT TO OUTPUT-TEXT
           PERFORM WRITE-LINE.
       
       CONVERT-VALUTA.
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
           IF TMP-NUMERIC < 0
               ADD TMP-NUMERIC TO TOTAL-UDBETALT
           ELSE
               ADD TMP-NUMERIC TO TOTAL-INDBETALT
           END-IF.

       WRITE-UDSKRIFT.
           MOVE TMP-NUMERIC TO PRETTY-NUMERIC
           MOVE PRETTY-NUMERIC TO BELØB-DKK OF KONTO-UDSKRIFT

           MOVE BELØB OF TRANS-ARR(IX)   TO PRETTY-NUMERIC
           MOVE PRETTY-NUMERIC           TO BELØB  OF KONTO-UDSKRIFT

           MOVE TIDSPUNKT  OF TRANS-ARR(IX) TO TIME-STMP
           MOVE DATO       OF TIME-STMP    TO DATO OF KONTO-UDSKRIFT
           MOVE TID        OF TIME-STMP    TO TID  OF KONTO-UDSKRIFT

           MOVE TRANS-TYPE OF TRANS-ARR(IX) 
             TO TRANS-TYPE OF KONTO-UDSKRIFT
           MOVE VALUTA OF TRANS-ARR(IX)  TO VALUTA OF KONTO-UDSKRIFT
           MOVE BUTIK OF TRANS-ARR(IX)   TO BUTIK  OF KONTO-UDSKRIFT

           MOVE KONTO-UDSKRIFT TO OUTPUT-TEXT
           PERFORM WRITE-LINE.
       
       WRITE-FOOT.
           MOVE "Totalt indbetalt: "   TO FIELD-DESC-LONG
           MOVE TOTAL-INDBETALT        TO PRETTY-NUMERIC
           MOVE PRETTY-NUMERIC         TO FIELD-VAL
           PERFORM WRITE-FIELD-LONG

           MOVE "Totalt udbetalt: "    TO FIELD-DESC-LONG
           MOVE TOTAL-UDBETALT         TO PRETTY-NUMERIC
           MOVE PRETTY-NUMERIC         TO FIELD-VAL
           PERFORM WRITE-FIELD-LONG

           
           MOVE "Saldo: "      TO FIELD-DESC-LONG
           MOVE SALDO          TO PRETTY-NUMERIC
           MOVE PRETTY-NUMERIC TO FIELD-VAL
           PERFORM WRITE-FIELD-LONG
           
           PERFORM WRITE-LINE *> Blank line
           MOVE "Med venlig hilsen" TO OUTPUT-TEXT
           PERFORM WRITE-LINE
           MOVE BANKNAVN OF BANK-ARR(CURR-REG-NR) TO OUTPUT-TEXT
           PERFORM WRITE-LINE.
           
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
                   PERFORM CONVERT-VALUTA
                   PERFORM WRITE-UDSKRIFT
               END-PERFORM
               PERFORM WRITE-LINE *> BLANK line
               ADD TOTAL-INDBETALT TO SALDO
               ADD TOTAL-UDBETALT  TO SALDO
               PERFORM WRITE-FOOT
               MOVE ZEROS TO PENGE-SUM
               MOVE 50000 TO SALDO
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

       