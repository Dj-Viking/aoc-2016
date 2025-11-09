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
           01 ws-samplelen    pic 9(4)  value 0.
      *>     coords
	       01 coord-x         pic S9(5) value 0.
	       01 coord-y         pic S9(5) value 0.
      *>     end-coords
      *>     directions stuff
           01 parsed-instr    pic x     value "R".
           01 parsed-distance pic 9(3)  value 0.
	       01 my-direction    pic x     value "N".
	       01 north           pic x     value "N".
	       01 south           pic x     value "S".
	       01 east            pic x     value "E".
	       01 west            pic x     value "W".
      *>     end-directions
           01 ws-start        pic 9(4)  value 0.
           01 ws-field-len    pic 9(4)  value 0.
           01 ws-loop-str-end pic 9(4)  value 0.
           01 ws-count        pic 9(4)  value 0.
           01 ws-char         pic x. 
           01 ws-answer-num   pic 9(5) value 0.
           01 ws-unstring     pic X(5).
           01 ws-table.
              05 ws-item occurs 700 times.
                 10 ws-text   pic x(40). 

           PROCEDURE DIVISION.
      *> cobol-lint CL002 main-program
           main-program.
               perform init-file
               
               perform read-chars

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
               move function length(function trim (sampleline)) to ws-samplelen
               move 1 to ws-start
               move 0 to ws-count
               
               perform varying ws-loop-str-end 
                       from 1 by 1 
                       until ws-loop-str-end > ws-samplelen
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
      *>                    
      *>                   note: split the string of the current window
      *>                         we are looking at in the string
                           move spaces to ws-unstring
                           unstring sampleline(ws-start:ws-field-len) 
                               into ws-unstring;
      *>                   only this one is space at 3:1
      *>                   the rest have space at 1:1 
      *>                   part 1 just want to know the distance we went
      *>                    
      *>                   note: the direction matters according to
      *>                         cartesian coordinates! 
      *>                         N = + to y 
      *>                         S = - to y 
      *>                         E = + to x 
      *>                         W = + to x 
                           display "========="
                           display "unstring1: [" ws-unstring(1:1) "]"
                           display "unstring2: [" ws-unstring(2:1) "]"
                           display "unstring3: [" ws-unstring(3:1) "]"
                           display "if block calculating...: "
                           display "========="

                           if ws-start = "0001"
                               display "very beginning of parsing"
                               move ws-unstring(1:1) 
                                   to parsed-instr
                               move function numval(ws-unstring(2:1)) 
                                   to parsed-distance
                           else 
                               move ws-unstring(2:1) 
                                   to parsed-instr
                                
                               move function numval(ws-unstring(3:ws-field-len - 2))
                                   to parsed-distance
                           end-if 

                           display "========="
                           display "parsed instr: " parsed-instr
                           display "parsed dist: " parsed-distance
                           display "========="
                           display "my current direction: " my-direction
                           display "coords so far x,y: " coord-x "," coord-y
                           display "========="

      *>                        note: check and
      *>                              change direction
                           evaluate true
                               when parsed-instr = "R"
                                   perform 
                                       evaluate true
                                           when my-direction = "N"
                                               perform
                                                   move "E"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "S"
                                               perform
                                                   move "W"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "E"
                                               perform
                                                   move "S"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "W"
                                               perform
                                                   move "N"
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                               when parsed-instr = "L"
                                   perform 
                                       evaluate true
                                           when my-direction = "N"
                                               perform
                                                   move "W"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "S"
                                               perform
                                                   move "E"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "E"
                                               perform
                                                   move "N"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "W"
                                               perform
                                                   move "S"
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                           end-evaluate
                           display "my new direction: " my-direction
                           display "========="

      *>                        note: move that direction
      *>                              by the unstringed number 
                           evaluate true
                               when my-direction = "N"
                                   perform 
                                       compute coord-y = 
                                           coord-y + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "S"
                                   perform 
                                       compute coord-y = 
                                           coord-y - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "E"
                                   perform 
                                       compute coord-x = 
                                           coord-x + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "W"
                                   perform 
                                       compute coord-x = 
                                           coord-x - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                           end-evaluate
                           display "========="
                           display "new coords: " coord-x "," coord-y

                           move sampleline(ws-start:ws-field-len)
                               to ws-text(ws-count)
                       end-if
                       compute ws-start = ws-loop-str-end + 1
                   else
                       if ws-loop-str-end = ws-samplelen
      *>                 not the first iteration until the last item
                           add 1 to ws-count
                           compute ws-field-len = ws-loop-str-end - ws-start + 1
                           move spaces to ws-text(ws-count)

                           display ws-start " " ws-field-len " - [" sampleline(ws-start:ws-field-len) "]"

                           move spaces to ws-unstring
                           unstring sampleline(ws-start:ws-field-len)
                               into ws-unstring;

                           display "========="
                           display "else block calculating...: " ws-answer-num

                           display "unstring1: [" ws-unstring(1:1) "]"
                           display "unstring2: [" ws-unstring(2:1) "]"
                           display "unstring3: [" ws-unstring(3:1) "]"
                           display "========="

                           move ws-unstring(2:1) 
                               to parsed-instr
                           move function numval(ws-unstring(3:1))
                               to parsed-distance

                           display "========="
                           display "parsed instr: " parsed-instr
                           display "parsed dist: " parsed-distance
                           display "========="
                           display "my direction: " my-direction
                           display "coords so far x,y: " coord-x "," coord-y
                           display "========="


      *>                        note: check and
      *>                              change direction
                           evaluate true
                               when parsed-instr = "R"
                                   perform 
                                       evaluate true
                                           when my-direction = "N"
                                               perform
                                                   move "E"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "S"
                                               perform
                                                   move "W"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "E"
                                               perform
                                                   move "S"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "W"
                                               perform
                                                   move "N"
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                               when parsed-instr = "L"
                                   perform 
                                       evaluate true
                                           when my-direction = "N"
                                               perform
                                                   move "W"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "S"
                                               perform
                                                   move "E"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "E"
                                               perform
                                                   move "N"
                                                   to my-direction
                                               end-perform
                                           when my-direction = "W"
                                               perform
                                                   move "S"
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                           end-evaluate

      *>                        note: move that direction
      *>                              by the unstringed number 
                           evaluate true
                               when my-direction = "N"
                                   perform 
                                       compute coord-y = 
                                           coord-y + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "S"
                                   perform 
                                       compute coord-y = 
                                           coord-y - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "E"
                                   perform 
                                       compute coord-x = 
                                           coord-x + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = "W"
                                   perform 
                                       compute coord-x = 
                                           coord-x - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                           end-evaluate

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

               display "end coords x: " coord-x " y: " coord-y
               compute ws-answer-num =
                   function abs(coord-x) + function abs(coord-y) 
               display "answer: " ws-answer-num

               .


           close-file.
               close sample
               .
