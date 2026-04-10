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
      *01 EOF-FLAG PIC X VALUE "N".
       01 NUM-BANKER PIC 9(3) VALUE 100.
       01 IX       PIC 9(3) VALUE 0.
       
       01 BANK-ARR OCCURS 0 TO 100 TIMES DEPENDING ON NUM-BANKER.
           COPY "../data/BANKER.cpy".

       01 FORMAT-FIELDS.
           05 FIELD-1 PIC X(50).
           05 FIELD-2 PIC X(50).
           05 FIELD-3 PIC X(50).
           05 FIELD-4 PIC X(50).
        

       PROCEDURE DIVISION.
           PERFORM BANK-ARR-FILL
           
           STOP RUN.
       
       BANK-ARR-FILL.
           OPEN INPUT FIL-BANKER
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-BANKER
               READ FIL-BANKER INTO BANK-ARR(IX)
                   AT END
                       SUBTRACT 1 FROM IX
                       MOVE IX TO NUM-BANKER
                       EXIT PERFORM
               END-READ
      *        DISPLAY IX " "
      *            REG-NR OF BANK-ARR(IX) " "
      *            BANKNAVN OF BANK-ARR(IX)
      *            BANKADRESSE OF BANK-ARR(IX)
      *            TELEFON OF BANK-ARR(IX)
      *            EMAIL OF BANK-ARR(IX)
           END-PERFORM
           CLOSE FIL-BANKER

           DISPLAY "Antal banker: " NUM-BANKER.
           
      *    DISPLAY BANKNAVN OF BANK-ARR(1) " , ... , " 
      *        BANKNAVN OF BANK-ARR(IX - 1)  " , " 
      *        BANKNAVN OF BANK-ARR(IX).
