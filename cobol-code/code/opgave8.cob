       identification division. 
       program-id. OPG-8.

       environment division.
       input-output section.
       file-control.
       select fil-kunder assign to "../data/Kundeoplysninger.txt"
               organization is line sequential.
           select fil-konti assign to "../data/KontoOpl.txt"
               organization is line sequential.
           select fil-out assign to "../data/KUNDEKONTO.txt"
               organization is line sequential.
              
       data division.
       

       file section.
       fd fil-kunder.
       01 kunde-opl.
           copy "../data/KUNDER.cpy".
       
       fd fil-konti.
       01 konto-rekord.
           copy "../data/KONTOOPL.cpy".

       fd fil-out.
       01 kundekonto.
           02 output-text pic x(100).

       working-storage section. 
       01 eof-flag pic x value "N".

       procedure division.
           open input fil-kunder
           open output fil-out
    
           perform until eof-flag = "Y"
               read fil-kunder into kunde-opl
                   at end
                       move "Y" to eof-flag
                   not at end
                       perform kunde-skriv
                       perform konto-iter
                       move spaces to output-text
                       perform write-para
               end-read
           end-perform
    
           close fil-kunder
           close fil-out
           stop run.
       
       format-konto.
           string KONTO-ID delimited by space
                  " " delimited by size
                  KONTO-TYPE delimited by space
                  " " delimited by size
                  BALANCE of konto-rekord delimited by space
                  " " delimited by size
                  VALUTA-KD delimited by space
                  into output-text
           end-string
           perform write-para.

       konto-iter.
           open input fil-konti
           perform until eof-flag = "Y"
             read fil-konti into konto-rekord
               at end
                   move "Y" to eof-flag
               not at end
                   if KUNDE-ID of konto-rekord = KUNDE-ID of kunde-opl
                       perform format-konto
                   end-if
             end-read
           end-perform
           move "N" to eof-flag
           close fil-konti.
           

       write-para.
           write kundekonto
           move spaces to output-text.
       
       format-navn.
           string FORNAVN delimited by space
                  " " delimited by size
                  EFTERNAVN delimited by space
                  into output-text
           end-string
           perform write-para.

       format-adr.
           string function trim(VEJNAVN,trailing) 
                  delimited by size
                  " " delimited by size
                  HUSNR delimited by space
                  " " delimited by size
                  ETAGE delimited by space
                  " " delimited by size
                  SIDE delimited by space
                  into output-text
           end-string
           perform write-para.

       format-by.
           string POSTNR delimited by space
               " " delimited by size
               BY-ADR delimited by size
               into output-text
           end-string
           perform write-para.
       kunde-skriv.
           move KUNDE-ID of kunde-opl to output-text
           perform write-para
           
           perform format-navn
           perform format-adr
           perform format-by
    
           move TELEFON to output-text
           perform write-para
    
           move EMAIL to output-text
           perform write-para.

           
