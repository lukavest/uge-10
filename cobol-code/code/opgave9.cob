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
           COPY "../data/KUNDER.cpy".
       
       FD FIL-KONTI.
       01 KONTO-REKORD.
           COPY "../data/KONTOOPL.cpy".

       FD FIL-OUT.
       01 KUNDEKONTO.
           02 OUTPUT-TEXT PIC X(100).

       WORKING-STORAGE SECTION. 
       01 EOF-FLAG PIC X VALUE "N".
       01 IX       PIC 9(2) VALUE 0.
       01 NUM-KONTI PIC 9(2) VALUE 0.
       01 KONTO-ARRAY OCCURS 30 TIMES.
           COPY "../data/KONTOOPL.cpy".

       PROCEDURE DIVISION.
           PERFORM KONTO-ARR-FILL
           
           OPEN INPUT FIL-KUNDER
           OPEN OUTPUT FIL-OUT
    
           PERFORM UNTIL EOF-FLAG = "Y"
               READ FIL-KUNDER INTO KUNDE-OPL
                   AT END
                       MOVE "Y" TO EOF-FLAG
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

       WRITE-PARA.
           WRITE KUNDEKONTO
           MOVE SPACES TO OUTPUT-TEXT.
       
       FORMAT-KONTO.
           STRING KONTO-ID OF KONTO-REKORD DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  KONTO-TYPE OF KONTO-REKORD DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  BALANCE OF KONTO-REKORD DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  VALUTA-KD OF KONTO-REKORD DELIMITED BY SPACE
                  INTO OUTPUT-TEXT
           END-STRING
           PERFORM WRITE-PARA.

       KONTO-ARR-FILL.
           OPEN INPUT FIL-KONTI
     
           PERFORM VARYING IX FROM 1 BY 1  UNTIL EOF-FLAG = "Y"
              READ FIL-KONTI INTO KONTO-REKORD
                AT END
                    MOVE "Y" TO EOF-FLAG
                NOT AT END
                    MOVE KONTO-REKORD TO KONTO-ARRAY(IX)
                   
              END-READ
           END-PERFORM
           MOVE "N" TO EOF-FLAG
           CLOSE FIL-KONTI
           
           MOVE IX TO NUM-KONTI
           ADD -2 TO NUM-KONTI.

       KONTO-ITER.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-KONTI
                IF KUNDE-ID OF KONTO-ARRAY(IX) = KUNDE-ID OF KUNDE-OPL
                   MOVE KONTO-ARRAY(IX) TO KONTO-REKORD
                   PERFORM FORMAT-KONTO
                   
                END-IF
           END-PERFORM.
      
       FORMAT-NAVN.
           STRING FORNAVN DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  EFTERNAVN DELIMITED BY SPACE
                  INTO OUTPUT-TEXT
           END-STRING
           PERFORM WRITE-PARA.

       FORMAT-ADR.
           STRING FUNCTION TRIM(VEJNAVN,TRAILING) 
                  DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  HUSNR DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  ETAGE DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  SIDE DELIMITED BY SPACE
                  INTO OUTPUT-TEXT
           END-STRING
           PERFORM WRITE-PARA.

       FORMAT-BY.
           STRING POSTNR DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               BY-ADR DELIMITED BY SIZE
               INTO OUTPUT-TEXT
           END-STRING
           PERFORM WRITE-PARA.
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

           
