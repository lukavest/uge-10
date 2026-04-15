       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OPG-5.

       DATA DIVISION.
           WORKING-STORAGE SECTION. 
      
           01 KUNDEOPL.
               COPY "../copybooks/KUNDER.cpy".

       PROCEDURE DIVISION.
           PERFORM FILL-VALUES.
           DISPLAY "Kunde-ID: " KUNDE-ID
           DISPLAY "Navn: " FORNAVN " " EFTERNAVN
           DISPLAY "Kontonummer: " KONTO-NUMMER
           DISPLAY "Balance: "VALUTAKODE " " BALANCE

           PERFORM DIS-LINE.
           DISPLAY ADDRESSE
           PERFORM DIS-LINE.
           DISPLAY KONTAKT
           STOP RUN.

       DIS-LINE.
           DISPLAY "----------------------------------------".
       
       FILL-VALUES.
           MOVE "1234567890" TO KUNDE-ID
           MOVE "Anders" TO FORNAVN
           MOVE "And" TO EFTERNAVN
           MOVE "DK12345678912345" TO KONTO-NUMMER
           MOVE 2500.75 TO BALANCE
           MOVE "DKK" TO VALUTAKODE
           
           MOVE "Paradisæblevej" TO VEJNAVN
           MOVE "111" TO HUSNR
           MOVE SPACES TO ETAGE
           MOVE "Andeby" TO BY-ADR
           MOVE "2800" TO POSTNR
           MOVE "DK" TO LANDE-KODE
           
           MOVE "12345678" TO TELEFON
           MOVE "anders@and.com" TO EMAIL.
