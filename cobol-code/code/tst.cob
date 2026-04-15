       IDENTIFICATION DIVISION. 
       PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT FIL-OUT ASSIGN TO "../data/HELLO.txt"
      *        ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

      *FILE SECTION.
      *FD FIL-OUT.
      *01 KONTOUDSKRIFT.
      *    02 OUTPUT-TEXT PIC X(100).
      
       WORKING-STORAGE SECTION. 
      *01 VAR-TEXT PIC N(30) USAGE NATIONAL.
      *01 TST  PIC X(100).
       
       01 A PIC S9(10)V99 VALUE ZEROS.
       01 B PIC S9(10)V99 VALUE ZEROS.
       01 C PIC S9(10)V99 VALUE ZEROS.

       01 P PIC ZZZZZZZZZ9.99.

       PROCEDURE DIVISION.
      *    DISPLAY "Danish characters: Æ, Ø, Å, æ, ø, å"
      *    MOVE "Danish characters: Æ, Ø, Å, æ, ø, å" TO VAR-TEXT
      *    DISPLAY VAR-TEXT
      *    OPEN OUTPUT FIL-OUT
      *    MOVE "Danish characters: Æ, Ø, Å, æ, ø, å" 
      *        TO OUTPUT-TEXT
      *    WRITE KONTOUDSKRIFT
      *    CLOSE FIL-OUT

           MOVE "96760.20" TO A
           MOVE "-41225.60" TO B
           MOVE A TO P
           DISPLAY P " " A
           MOVE B TO P
           DISPLAY P " " B
           
           ADD A TO B
           MOVE B TO P
      *    MOVE FUNCTION SUM(A, B) TO C
           DISPLAY P " " B
      *    DISPLAY FUNCTION SUM(A, B)
           
           MOVE "-41225.60" TO P
           IF P < 0
               DISPLAY P " < 0"
           END-IF

           IF P > 0
               DISPLAY P " > 0"
           END-IF
               
           

           
           STOP RUN.
