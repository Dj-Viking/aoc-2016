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
           01 filestatus      pic xx.
           01 ws-delim        pic XX    value ",".
           01 ws-len          pic 9(4)  value 0.
      *>     start by going north direction
           01 prev-instr      pic x     value spaces.
           01 ws-start        pic 9(4)  value 0.
           01 ws-field-len    pic 9(4)  value 0.
           01 ws-loop-str-end pic 9(4)  value 0.
           01 ws-count        pic 9(4)  value 0.
           01 ws-char         pic x. 
           01 ws-answer-num   pic 9(38) value 0.
           01 ws-unstring     pic X(3).
           01 ws-table.
              05 ws-item occurs 700 times.
                 10 ws-text   pic x(40). 

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
               
               perform varying ws-loop-str-end 
                       from 1 by 1 
                       until ws-loop-str-end > ws-len
                   move sampleline(ws-loop-str-end:1) 
                       to ws-char
      *>           scanning across string
      *>           found delim "," | start gathering chars to add
      *>           to table 
                   if ws-char = ws-delim
                       add 1 to ws-count
                       compute ws-field-len = ws-loop-str-end - ws-start
                       if ws-field-len > 0
                            
      *>                   the first iteration until the last item
      *>                   init table row text
                           move spaces to ws-text(ws-count)
                           display ws-start " " ws-field-len " - [" sampleline(ws-start:ws-field-len) "]"

      *>                     split the string of the current window
      *>                     we are looking at in the string
                           move spaces to ws-unstring
                           unstring sampleline(ws-start:ws-field-len) 
                               into ws-unstring;
      *>                   only this one is space at 3:1
      *>                   the rest have space at 1:1 
      *>                   part 1 just want to know the distance we went

      *>                   note: the direction matters according to
      *>                         cartesian coordinates! 

                           display "unstring1: [" ws-unstring(1:1) "]"
                           display "unstring2: [" ws-unstring(2:1) "]"
                           display "unstring3: [" ws-unstring(3:1) "]"

                           display "if block calculating...: " ws-answer-num

                           if ws-start = "0001"
                               display "very beginning of parsing"
      *>                         check direction as a means of adding distance
      *>                         lets try if your previous direction is the same
      *>                         as your current one to NOT add anything
                               if ws-unstring(1:1) = "R" and prev-instr is not = "R" 
                               then
                                   compute ws-answer-num = 
                                       ws-answer-num + function numval(ws-unstring(2:1)) 
                               else
                                   if ws-unstring(1:1) = "L" and prev-instr is not = "L" 
                                   then
                                       compute ws-answer-num = 
                                           ws-answer-num + function numval(ws-unstring(2:1)) 
                                   end-if
                               end-if

                               move ws-unstring(1:1) to prev-instr
                           else 
                               if ws-unstring(2:1) = "R" and prev-instr is not = "R" 
                               then
                                   compute ws-answer-num = 
                                       ws-answer-num + function numval(ws-unstring(3:1)) 
                               else 
                                   if ws-unstring(2:1) = "L" and prev-instr is not = "L" 
                                   then
                                       compute ws-answer-num = 
                                           ws-answer-num + function numval(ws-unstring(3:1)) 
                                   end-if
                               end-if
                               move ws-unstring(2:1) to prev-instr
                           end-if 

                           move sampleline(ws-start:ws-field-len)
                               to ws-text(ws-count)
                       end-if
                       compute ws-start = ws-loop-str-end + 1
                   else
                       if ws-loop-str-end = ws-len
      *>                 not the first iteration until the last item
                           add 1 to ws-count
                           compute ws-field-len = ws-loop-str-end - ws-start + 1
                           move spaces to ws-text(ws-count)
                           display ws-start " " ws-field-len " - [" sampleline(ws-start:ws-field-len) "]"

                           move spaces to ws-unstring
                           unstring sampleline(ws-start:ws-field-len)
                               into ws-unstring;

                           display "unstring1: [" ws-unstring(1:1) "]"
                           display "unstring2: [" ws-unstring(2:1) "]"
                           display "unstring3: [" ws-unstring(3:1) "]"

                           if ws-unstring(2:1) = "R" and prev-instr is not = "R" 
                           then
                               compute ws-answer-num = 
                                   ws-answer-num + function numval(ws-unstring(3:1)) 
                           else 
                               if ws-unstring(2:1) = "L" and prev-instr is not = "L" 
                               then
                                   compute ws-answer-num = 
                                       ws-answer-num + function numval(ws-unstring(3:1)) 
                               end-if
                           end-if
                           move ws-unstring(2:1) to prev-instr

                           display "else block calculating...: " ws-answer-num

                           move sampleline(ws-start:ws-field-len)
                               to ws-text(ws-count)
                       end-if
                   end-if
               end-perform

      *>       497 is too high!!!!! (adding every number)
      *>       253 is too low!!!!!! (not adding num if our previous direction 
      *>                             is the same as current direction)
      *>       able to add the numbers but i need to think about 
      *>       the direction!!
               display "answer: " ws-answer-num

      *>         perform varying ws-loop-str-end 
      *>                 from 1 by 1 
      *>                 until ws-loop-str-end > ws-count
      *>             display "item " ws-loop-str-end ":" ws-text(ws-loop-str-end)
      *>         end-perform
               .


           close-file.
               close sample
               .
