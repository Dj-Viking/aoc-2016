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
              05 ws-item occurs 10 times.
                 10 ws-text pic x(40). 

           PROCEDURE DIVISION.
          *> cobol-lint CL002 main-program
           main-program.
               perform init-file
               
               perform read-file-to-array

               perform close-file

               stop run.
           init-file.
               open input sample
               if filestatus not = "00"
                   display "error opening file. status=" filestatus
                   stop run
               end-if
               read sample
                   at end
                       display "no data in file."
                       close sample
                       stop run
               end-read
               .

           read-file-to-array.
               display "test"
      *>         perform until eof-check = "Y"
      *>             read sample
      *>                 at end
      *>                     move "Y" to eof-check
      *>                 not at end
      *>                     display sampleline
      *>             end-read
      *>         end-perform
               .

           close-file.
               close sample
               .
