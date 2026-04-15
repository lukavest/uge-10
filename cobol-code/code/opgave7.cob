       identification division. 
       program-id. OPG-7.

       environment division.
       input-output section.
       file-control.
           select fil-in assign to "../data/Kundeoplysninger.txt"
               organization is line sequential.
           select fil-out assign to "../data/KunderReadable.txt"
               organization is line sequential.
              
       data division.
       

       file section.
       fd fil-in.
       01 kunde-opl.
           copy "../copybooks/KUNDER.cpy".
       
       fd fil-out.
       01 kunde-adr.
           02 navn-adr pic X(100).

       working-storage section. 
       01 eof-flag pic X value "N".

       procedure division.
           open input fil-in
           open output fil-out
    
           perform until eof-flag = "Y"
               read fil-in into kunde-opl
                   at end
                       move "Y" to eof-flag
                   not at end
                       move KUNDE-ID to navn-adr
                       perform write-para
                       
                       perform format-navn
                       perform format-adr
                       perform format-by

                       move TELEFON to navn-adr
                       perform write-para

                       move EMAIL to navn-adr
                       perform write-para

                       move spaces to navn-adr
                       perform write-para
                       
               end-read
           end-perform
    
           close fil-in
           close fil-out
           stop run.

       write-para.
           write kunde-adr
           move spaces to navn-adr.
       
       format-navn.
           string FORNAVN delimited by space
                  " " delimited by size
                  EFTERNAVN delimited by space
                  into navn-adr
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
                  into navn-adr
           end-string
           perform write-para.

       format-by.
           string POSTNR delimited by space
               " " delimited by size
               BY-ADR delimited by size
               into navn-adr
           end-string
           perform write-para.
