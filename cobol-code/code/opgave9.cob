       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-9.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FIL-KUNDER ASSIGN TO "../data/Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FIL-KONTI ASSIGN TO "../data/KontoOpl.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FIL-OUT ASSIGN TO "../data/KUNDEKONTO-2.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
              
       DATA DIVISION.
       
       
       FILE SECTION.
       FD FIL-KUNDER.
       01 KUNDE-OPL.
           COPY "../copybooks/KUNDER.cpy".
       
       FD FIL-KONTI.
       01 KONTO-REKORD.
           COPY "../copybooks/KONTOOPL.cpy".

       FD FIL-OUT.
       01 KUNDEKONTO.
           02 OUTPUT-TEXT PIC X(100).

       WORKING-STORAGE SECTION. 
      *01 EOF-FLAG PIC X VALUE "N".
       01 ARR-SIZE PIC 9(2) VALUE 30.
       01 IX       PIC 9(2) VALUE 0.
       01 NUM-KONTI PIC 9(2) VALUE 0.
       01 KONTO-ARRAY OCCURS 0 TO 50 TIMES DEPENDING ON ARR-SIZE.
           COPY "../copybooks/KONTOOPL.cpy".

       01 FORMAT-FIELDS.
           05 FIELD-1 PIC X(50).
           05 FIELD-2 PIC X(50).
           05 FIELD-3 PIC X(50).
           05 FIELD-4 PIC X(50).
        

       PROCEDURE DIVISION.
           PERFORM KONTO-ARR-FILL
           
           OPEN INPUT FIL-KUNDER
           OPEN OUTPUT FIL-OUT
    
           PERFORM UNTIL 1 = 2
               READ FIL-KUNDER INTO KUNDE-OPL
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       PERFORM KUNDE-SKRIV
                       PERFORM KONTO-ITER
                       MOVE SPACES TO OUTPUT-TEXT
                       PERFORM WRITE-PARA
               END-READ
           END-PERFORM
    
           CLOSE FIL-KUNDER
           CLOSE FIL-OUT
           STOP RUN.
       KONTO-ARR-FILL.
           OPEN INPUT FIL-KONTI
           PERFORM VARYING IX FROM 0 BY 1 UNTIL IX = ARR-SIZE
               READ FIL-KONTI INTO KONTO-ARRAY(IX + 1)
                   AT END
                       EXIT PERFORM
               END-READ
           END-PERFORM
           CLOSE FIL-KONTI
           MOVE IX TO NUM-KONTI
           DISPLAY "Antal konti: " NUM-KONTI.

       WRITE-PARA.
           WRITE KUNDEKONTO
           MOVE SPACES TO OUTPUT-TEXT.

       FORMAT-LINE.
           MOVE SPACES TO OUTPUT-TEXT
           STRING
               FUNCTION TRIM(FIELD-1) DELIMITED BY SIZE
               " "     DELIMITED BY SIZE
               FUNCTION TRIM(FIELD-2) DELIMITED BY SIZE
               " "     DELIMITED BY SIZE
               FUNCTION TRIM(FIELD-3) DELIMITED BY SIZE
               " "     DELIMITED BY SIZE
                FUNCTION TRIM(FIELD-4) DELIMITED BY SIZE
               INTO OUTPUT-TEXT
           END-STRING
           PERFORM WRITE-PARA.

       
       FORMAT-KONTO.
           MOVE KONTO-ID   OF KONTO-ARRAY(IX) TO FIELD-1
           MOVE KONTO-TYPE OF KONTO-ARRAY(IX) TO FIELD-2
           MOVE BALANCE    OF KONTO-ARRAY(IX) TO FIELD-3
           MOVE VALUTA-KD  OF KONTO-ARRAY(IX) TO FIELD-4
           PERFORM FORMAT-LINE.

       KONTO-ITER.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-KONTI
               IF KUNDE-ID OF KONTO-ARRAY(IX) = KUNDE-ID OF KUNDE-OPL
                   PERFORM FORMAT-KONTO
               END-IF
           END-PERFORM.
      
       FORMAT-NAVN.
           MOVE FORNAVN   TO FIELD-1
           MOVE EFTERNAVN TO FIELD-2
           MOVE SPACES    TO FIELD-3 FIELD-4
           PERFORM FORMAT-LINE.

       FORMAT-ADR.
           MOVE VEJNAVN TO FIELD-1
           MOVE HUSNR   TO FIELD-2
           MOVE ETAGE   TO FIELD-3
           MOVE SIDE    TO FIELD-4
           PERFORM FORMAT-LINE.

       FORMAT-BY.
           MOVE POSTNR TO FIELD-1
           MOVE BY-ADR  TO FIELD-2
           MOVE SPACES  TO FIELD-3 FIELD-4
           PERFORM FORMAT-LINE.

       KUNDE-SKRIV.
           MOVE KUNDE-ID OF KUNDE-OPL TO OUTPUT-TEXT
           PERFORM WRITE-PARA
           
           PERFORM FORMAT-NAVN
           PERFORM FORMAT-ADR
           PERFORM FORMAT-BY
           
           MOVE TELEFON TO OUTPUT-TEXT
           PERFORM WRITE-PARA
           
           MOVE EMAIL TO OUTPUT-TEXT
           PERFORM WRITE-PARA.

           
