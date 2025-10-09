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
      *>       684 characters in one line 
      *>       plus null terminating char
           FD sample
               record contains 685 characters.
           01 sampleline pic x(685).
           WORKING-STORAGE SECTION.
           01 eof-check       pic X value "N". 
           01 filestatus      pic XX.
           01 answer          pic 9 value zero. 
           PROCEDURE DIVISION.

          *> cobol-lint CL002 main-program
           main-program.
               perform init-file
               
               perform read-file

               perform close-file

               stop run.
           init-file.
               open input sample
               if filestatus not = "00"
                   display "error opening file. status=" filestatus
                   stop run
               end-if
               .

      *> do something here per character
      *> of the line, maybe use a temp string
      *> to check what each set of 3 characters are
      *> and add up how far we went 
           read-file.
               perform until eof-check = "Y"
                   read sample
                       at end
                           move "Y" to eof-check
                       not at end
                           display sampleline
                   end-read
               end-perform
               .

           close-file.
               close sample
               .
