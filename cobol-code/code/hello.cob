       IDENTIFICATION DIVISION. 
       PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIL-OUT ASSIGN TO "../data/HELLO.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD FIL-OUT.
       01 UDSKRIFT.
           02 OUTPUT-TEXT PIC X(100).
      
       WORKING-STORAGE SECTION. 
      *01 VAR-TEXT PIC N(30) USAGE NATIONAL.
      *01 TST  PIC X(100).
       
      *01 A PIC S9(13) SIGN IS LEADING SEPARATE CHARACTER.
      *01 B PIC S9(13) SIGN IS LEADING SEPARATE CHARACTER.
      *01 C PIC S9(13) SIGN IS LEADING SEPARATE CHARACTER.
      *01 A PIC 9V99.
      *01 B PIC S9(10)V99.
      *01 C PIC -ZZZ,ZZZ,ZZ9.99.
      *01 P PIC Z(11)V99.

       PROCEDURE DIVISION.
           OPEN OUTPUT FIL-OUT

      *    DISPLAY R"Danish characters: Æ, Ø, Å, æ, ø, å"
      *    MOVE "Danish characters: Æ, Ø, Å, æ, ø, å" TO VAR-TEXT
      *    DISPLAY VAR-TEXT
      *    OPEN OUTPUT FIL-OUT
      *    MOVE "Danish characters: Æ, Ø, Å, æ, ø, å" 
      *        TO OUTPUT-TEXT
      *    WRITE KONTOUDSKRIFT
      *    CLOSE FIL-OUT
           DISPLAY FUNCTION 
               LENGTH("Danish characters: Æ, Ø, Å, æ, ø, å" )
           DISPLAY FUNCTION 
               LENGTH("Danish characters: X, X, X, X, X, X" )

           MOVE "Danish characters: Æ, Ø, Å, æ, ø, å" 
               TO OUTPUT-TEXT
           WRITE UDSKRIFT
           
           DISPLAY "Danish characters: Æ, Ø, Å, æ, ø, å"
           CLOSE FIL-OUT
           STOP RUN.
