       IDENTIFICATION DIVISION. 
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       01 VAR-TEXT PIC X(30) VALUE "HELLO med Variabel".

       PROCEDURE DIVISION.
       DISPLAY VAR-TEXT.
       IF "A" NOT = "B"
           DISPLAY "A er ikke lig med B"
       END-IF
       IF "A" NOT = "A"
           DISPLAY "A er ikke lig med A"
       END-IF

       STOP RUN.
