      $set sourceformat(free)
           IDENTIFICATION DIVISION.
           PROGRAM-ID. day1.
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select sample
                   assign to "../sample",
                   organization is line sequential,
                   file status is filestatus.

           DATA DIVISION.
           FILE SECTION.
           FD sample
               record contains 685 characters.
           01 sampleline pic x(685).

           WORKING-STORAGE SECTION.
           01 filestatus    pic xx.
           01 ws-delim      pic X    value ",".
           01 ws-len        pic 9(4) value 0.
           01 ws-start      pic 9(4) value 0.
           01 ws-end        pic 9(4) value 0.
           01 ws-count      pic 9(4) value 0.
           01 ws-char       pic x. 
           01 ws-field-len  pic 9(4) value 0.
           01 ws-table.
              05 ws-item occurs 700 times.
                 10 ws-text pic x(40). 

           PROCEDURE DIVISION.
      *> cobol-lint CL002 main-program
           main-program.
               perform init-file
               
               perform READ-CHARS

               perform close-file

               stop run.
           INIT-FILE.
               OPEN INPUT sample
               if FileStatus not = "00"
                   display "error opening file. status=" FileStatus
                   stop run
               end-if
               read sample
                   at end
                       display "no data in file."
                       close sample
                       stop run
               end-read
               .

           READ-CHARS.
               move function length(function trim (sampleline)) to ws-len
               move 1 to ws-start
               move 0 to ws-count
               
               perform varying ws-end from 1 by 1 until ws-end > ws-len
                   move sampleline(ws-end:1) to ws-char
                   if ws-char = ws-delim
                       add 1 to ws-count
                       compute ws-field-len = ws-end - ws-start
                       if ws-field-len > 0
                           move spaces to ws-text(ws-count)
                           move sampleline(ws-start:ws-field-len)
                               to ws-text(ws-count)
                       end-if
                       compute ws-start = ws-end + 1
                   else
                       if ws-end = ws-len
                           add 1 to ws-count
                           compute ws-field-len = ws-end - ws-start + 1
                           move spaces to ws-text(ws-count)
                           move sampleline(ws-start:ws-field-len)
                               to ws-text(ws-count)
                       end-if
                   end-if
               end-perform

               display "split results:"
               perform varying ws-end from 1 by 1 until ws-end > ws-count
                   display "item " ws-end ": " ws-text(ws-end)
               end-perform
               .


           close-file.
               close sample
               .
