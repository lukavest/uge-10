      * early code with display debugging/output for opgave10.cob *
       ITER-TRANS.
           MOVE 1 TO IX
           PERFORM UNTIL IX > NUM-TRANS
               MOVE KONTO-ID OF TRANS-ARR(IX) TO CURR-KONTO-ID
               DISPLAY "-------------------------------"
               DISPLAY "IX: " IX
               DISPLAY "Kunde: " NAVN OF TRANS-ARR(IX)
               DISPLAY "Adresse: " ADRESSE OF TRANS-ARR(IX)
               MOVE REG-NR OF TRANS-ARR(IX) TO CURR-REG-NR
               DISPLAY "Reg.nr.: " CURR-REG-NR
               DISPLAY "Bankadresse: " 
                   BANKADRESSE OF BANK-ARR(CURR-REG-NR)
               DISPLAY "Telefon: "
                   TELEFON OF BANK-ARR(CURR-REG-NR)
               DISPLAY "Email: "
                   EMAIL OF BANK-ARR(CURR-REG-NR)
               DISPLAY "Kontoudskrift for kontonr.: " CURR-KONTO-ID
               PERFORM VARYING IX FROM IX BY 1 UNTIL IX > NUM-TRANS
                   IF KONTO-ID OF TRANS-ARR(IX) NOT = CURR-KONTO-ID
                       EXIT PERFORM
                   END-IF
                   DISPLAY "IX: " IX
                   DISPLAY
                       TIDSPUNKT           OF TRANS-ARR(IX)
                       TRANSAKTIONSTYPE    OF TRANS-ARR(IX)
                       BELØB               OF TRANS-ARR(IX)
                       VALUTA              OF TRANS-ARR(IX)
                       BUTIK               OF TRANS-ARR(IX)
                END-PERFORM

           END-PERFORM.
        BANK-ARR-FILL.
           OPEN INPUT FIL-BANKER
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > NUM-BANKER
               READ FIL-BANKER INTO BANK-ARR(IX)
                   AT END
                       SUBTRACT 1 FROM IX
                       MOVE IX TO NUM-BANKER
                       EXIT PERFORM
                   NOT AT END
                       DISPLAY IX " "
                       "'" REG-NR      OF BANK-ARR(IX) "'"
                       "'" BANKNAVN    OF BANK-ARR(IX) "'"
                       "'" BANKADRESSE OF BANK-ARR(IX) "'"
                       "'" TELEFON     OF BANK-ARR(IX) "'"
                       "'" EMAIL       OF BANK-ARR(IX) "'"
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
                   NOT AT END
                       DISPLAY IX " "
                       "'" CPR              OF TRANS-ARR(IX) "'"
                       "'" NAVN             OF TRANS-ARR(IX) "'"
                       "'" ADRESSE          OF TRANS-ARR(IX) "'"
                       "'" FØDSELSDATO      OF TRANS-ARR(IX) "'"
                       "'" KONTO-ID         OF TRANS-ARR(IX) "'"
                       "'" REG-NR           OF TRANS-ARR(IX) "'"
                       DISPLAY "    "
                       "'" BELØB            OF TRANS-ARR(IX) "'"
                       "'" VALUTA           OF TRANS-ARR(IX) "'"
                       "'" TRANSAKTIONSTYPE OF TRANS-ARR(IX) "'"
                       "'" BUTIK            OF TRANS-ARR(IX) "'"
                       "'" TIDSPUNKT        OF TRANS-ARR(IX) "'"
                       DISPLAY " "
                END-READ