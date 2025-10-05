>>SOURCE FORMAT IS FREE
          IDENTIFICATION DIVISION.
          PROGRAM-ID. math.
          ENVIRONMENT DIVISION.
          DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 vars-to-add.
      *>     single digit (01)
             05 a    pic 9(01) value 2.
             05 b    pic 9(01) value 2.
          PROCEDURE DIVISION.
             add a to b.
             display "Add result -> " b. *> should display 4
             stop run.
