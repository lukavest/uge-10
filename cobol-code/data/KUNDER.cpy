
           02  KUNDE-ID        PIC X(10) VALUE SPACES.
           02  FORNAVN         PIC X(20) VALUE SPACES.
           02  EFTERNAVN       PIC X(20) VALUE SPACES.
       
           02  KONTO-NUMMER    PIC X(20) VALUE SPACES.
           02  BALANCE         PIC 9(7)V9(2) VALUE 0.
           02  VALUTAKODE      PIC X(3) VALUE SPACES.

           02 ADDRESSE.
               03 ADR-FIELDS.
                   04 VEJNAVN      PIC X(30).
                   04 HUSNR        PIC X(5).
                   04 ETAGE        PIC X(5).
                   04 SIDE         PIC X(5) VALUE SPACES.
               03 BY-ADR       PIC X(20).
               03 POSTNR       PIC X(4).
               03 LANDE-KODE   PIC X(2).
           02 KONTAKT.
               03 TELEFON      PIC X(8).
               03 EMAIL        PIC X(50).
