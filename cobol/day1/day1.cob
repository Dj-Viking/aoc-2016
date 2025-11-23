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
	       01 coord-2-x       pic S9(5) value 0.
           01 ustringed-coord pic x(13) value spaces.
	       01 coord-y         pic S9(5) value 0.
	       01 coord-2-y       pic S9(5) value 0.
           01 ws-coord-str    pic x(9)  value spaces.
           01 ws-x-coord-sign pic x(1)  value spaces.
           01 ws-y-coord-sign pic x(1)  value spaces.
      *>     end-coords
      *>     directions stuff
           01 parsed-instr    pic x     value spaces.
      *>     how many blocks away first place visited twice?  
           01 ws-answer2-num  pic S9(5) value 0.
           01 ws-left         pic x     value "L".
           01 ws-right        pic x     value "R".
           01 parsed-distance pic 9(3)  value 0.
	       01 my-direction    pic x     value "N".
	       01 north           pic x     value "N".
	       01 south           pic x     value "S".
	       01 east            pic x     value "E".
	       01 west            pic x     value "W".
      *>     end-directions
           01 ws-start        pic 9(4)  value 0.
           01 ws-field-len    pic 9(4)  value 0.
           01 ws-index        pic 9(4)  value 0.
           01 ws-loop-str-end pic 9(4)  value 0.
           01 ws-count        pic 9(4)  value 0.
           01 ws-char         pic x. 
           01 ws-answer-num   pic 9(5)  value 0.
           01 ws-unstring     pic X(5).
           01 ws-states-cnt   pic S9(5) value 0.
           01 states-table.
               05 filler      pic s9(5)  value 0. 
               05 filler      pic x(13)  value "+00000,+00000". 
      *>     visited coords table
           01 rdf-states-table redefines states-table.
               05 states-group occurs 700 times.
      *>       note: how many times we visited that coord
                   10 states-visited-cnt pic s9(5). 
      *>       note:     x , y
      *>               0000,0001
                   10 states-coord       pic x(13).  
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

           init-file.
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

           read-chars.
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
                           display "***** if block calculating...: "
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
                               when parsed-instr = ws-right
                                   perform 
                                       evaluate true
                                           when my-direction = north
                                               perform
                                                   move east
                                                   to my-direction
                                               end-perform
                                           when my-direction = south
                                               perform
                                                   move west
                                                   to my-direction
                                               end-perform
                                           when my-direction = east
                                               perform
                                                   move south
                                                   to my-direction
                                               end-perform
                                           when my-direction = west
                                               perform
                                                   move north
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                               when parsed-instr = ws-left
                                   perform 
                                       evaluate true
                                           when my-direction = north
                                               perform
                                                   move west
                                                   to my-direction
                                               end-perform
                                           when my-direction = south
                                               perform
                                                   move east
                                                   to my-direction
                                               end-perform
                                           when my-direction = east
                                               perform
                                                   move north
                                                   to my-direction
                                               end-perform
                                           when my-direction = west
                                               perform
                                                   move south
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                           end-evaluate
                           display "my new direction: " my-direction
                           display "========="
*> 
      *>                        note: move that direction
      *>                              by the unstringed number 
                           evaluate true
                               when my-direction = north
                                   perform 
                                       compute coord-y = 
                                           coord-y + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = south
                                   perform 
                                       compute coord-y = 
                                           coord-y - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = east
                                   perform 
                                       compute coord-x = 
                                           coord-x + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = west
                                   perform 
                                       compute coord-x = 
                                           coord-x - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                           end-evaluate
                           display "========="
                           display "new coords: " coord-x "," coord-y


      *>                 note: get the sign char for our current coords

      *>                     zero has positive sign (shrugs)
                           if coord-x = zeros 
      *>                         display "x = zero"
                               move "+"
                                   to ws-x-coord-sign
                           end-if
                           if coord-y = zeros 
      *>                         display "y = zero"
                               move "+"
                                   to ws-y-coord-sign
                           end-if

                           if coord-x < 0
      *>                         display "x lt zero"
                               move "-"
                                   to ws-x-coord-sign
                           else 
                               if coord-x > 0
      *>                             display "x gt zero"
                                   move "+"
                                       to ws-x-coord-sign
                               end-if
                           end-if

                           if coord-y < 0
      *>                         display "y lt zero"
                               move "-" 
                                   to ws-y-coord-sign 
                           else 
                               if coord-y > 0
                                   move "+" 
                                       to ws-y-coord-sign 
      *>                             display "y gt zero"
                               end-if
                           end-if

      *>                     note: create the coordstring 

                           move function concatenate(ws-x-coord-sign,
                                                     function abs(coord-x),
                                                     ",",
                                                     ws-y-coord-sign,
                                                     function abs(coord-y)) 
                               to states-coord(ws-count)

      *>                     note: initialize the coord count if we havent
      *>                           been there yet
                           if states-visited-cnt(ws-count) = zero
                           and states-coord(ws-count) not  = spaces
                               move 1 
                                   to states-visited-cnt(ws-count)
                           end-if

                           move spaces to ustringed-coord

      *>                     note: check visited need to plot every point visited
      *>                           on the graph including the starting point of each
      *>                           movement

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
                           display "***** else block calculating...: " ws-answer-num
*> 
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
                               when parsed-instr = ws-right
                                   perform 
                                       evaluate true
                                           when my-direction = north
                                               perform
                                                   move east
                                                   to my-direction
                                               end-perform
                                           when my-direction = south
                                               perform
                                                   move west
                                                   to my-direction
                                               end-perform
                                           when my-direction = east
                                               perform
                                                   move south
                                                   to my-direction
                                               end-perform
                                           when my-direction = west
                                               perform
                                                   move north
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                               when parsed-instr = ws-left
                                   perform 
                                       evaluate true
                                           when my-direction = north
                                               perform
                                                   move west
                                                   to my-direction
                                               end-perform
                                           when my-direction = south
                                               perform
                                                   move east
                                                   to my-direction
                                               end-perform
                                           when my-direction = east
                                               perform
                                                   move north
                                                   to my-direction
                                               end-perform
                                           when my-direction = west
                                               perform
                                                   move south
                                                   to my-direction
                                               end-perform
                                       end-evaluate
                                   end-perform
                           end-evaluate

      *>                        note: move that direction
      *>                              by the unstringed number 
                           evaluate true
                               when my-direction = north
                                   perform 
                                       compute coord-y = 
                                           coord-y + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = south
                                   perform 
                                       compute coord-y = 
                                           coord-y - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = east
                                   perform 
                                       compute coord-x = 
                                           coord-x + 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                               when my-direction = west
                                   perform 
                                       compute coord-x = 
                                           coord-x - 
                                           function numval(
                                               parsed-distance) 
                                   end-perform
                           end-evaluate

                           display "========="
                           display "new coords: " coord-x "," coord-y

      *>                 note: get the sign char for our current coords

      *>                     zero has positive sign (shrugs)
                           if coord-x = zeros 
      *>                         display "x = zero"
                               move "+"
                                   to ws-x-coord-sign
                           end-if
                           if coord-y = zeros 
      *>                         display "y = zero"
                               move "+"
                                   to ws-y-coord-sign
                           end-if

                           if coord-x < 0
      *>                         display "x lt zero"
                               move "-"
                                   to ws-x-coord-sign
                           else 
                               if coord-x > 0
      *>                             display "x gt zero"
                                   move "+"
                                       to ws-x-coord-sign
                               end-if
                           end-if

                           if coord-y < 0
      *>                         display "y lt zero"
                               move "-"
                                   to ws-y-coord-sign
                           else 
                               if coord-y > 0
      *>                             display "y gt zero"
                                   move "+"
                                       to ws-y-coord-sign
                               end-if
                           end-if


      *>                     note: create the coordstring 

                           move function concatenate(ws-x-coord-sign,
                                                     function abs(coord-x),
                                                     ",",
                                                     ws-y-coord-sign,
                                                     function abs(coord-y))
                               to states-coord(ws-count)

      *>                     note: initialize the coord count if we havent
      *>                           been there yet
                           if states-visited-cnt(ws-count) = zero
                           and states-coord(ws-count) not  = spaces 
                               move 1 
                                   to states-visited-cnt(ws-count)
                           end-if

                           display "index: " ws-count "coord: " states-coord(ws-count)
                           display "visited times: " states-visited-cnt(ws-count)

                           move spaces to ustringed-coord

      *>                     note: check visited need to plot every point visited
      *>                           on the graph including the starting point of each
      *>                           movement

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

      *>         display "end coords x: " coord-x " y: " coord-y
               compute ws-answer-num =
      *>           manhatten distance
                   function abs(coord-x) + function abs(coord-y) 
               display "answer: " ws-answer-num

      *>         note: loop through the table to check when the visited count
      *>               becomes 2 and then k

      *>         4 is not correct!
               display "answer2: " ws-answer2-num

               .


           close-file.
               close sample
               .
