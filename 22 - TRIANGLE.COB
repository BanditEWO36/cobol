IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIANGLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Incoming
       01 WS-SIDES PIC X(20).
       01 WS-PROPERTY PIC X(11).
       01 WS-A         PIC 9V9.
       01 WS-B         PIC 9V9.
       01 WS-C         PIC 9V9.
       01 WS-SUM-AB    PIC 999V9.
       01 WS-SUM-AC    PIC 999V9.
       01 WS-SUM-BC    PIC 999V9.
       01 WS-TRIANGLE  PIC 9.
      *Outgoing
       01 WS-RESULT PIC 9.
       PROCEDURE DIVISION.
       TRIANGLE.
      * WS-SIDES IS FORMATTED: "A,B,C" WHERE A, B, AND C COULD BE 
      * INTEGERS OR DECIMAL NUMBERS
      * WS-PROPERTY IS WHERE THE TEST OPTION GOES:
      * OPTIONS: "equilateral", "isosceles", "scalene"
      * WS-RESULT CAN BE: 1 (TRUE) OR 0 (FALSE) 
           MOVE 0 TO WS-RESULT
           UNSTRING WS-SIDES DELIMITED BY ','
              INTO WS-A, WS-B, WS-C 
           DISPLAY "A: " WS-A 
           DISPLAY "B: " WS-B 
           DISPLAY "C: " WS-C
           COMPUTE WS-SUM-AB = WS-A + 
               WS-B 
           COMPUTE WS-SUM-AC = WS-A + 
               WS-C
           COMPUTE WS-SUM-BC = WS-B + 
               WS-C
           DISPLAY "A + B: " WS-SUM-AB 
           DISPLAY "A + C: " WS-SUM-AC 
           DISPLAY "B + C: " WS-SUM-BC
           IF WS-SUM-AB > WS-C AND 
               WS-SUM-AC > WS-B AND 
               WS-SUM-BC > WS-A THEN
                 MOVE 1 TO WS-TRIANGLE
           ELSE MOVE 0 TO WS-TRIANGLE 
           END-IF 
      *     PERFORM IS-TRIANGLE
           DISPLAY "IS IT A TRIANGLE? "
           IF WS-TRIANGLE = 1 THEN
              DISPLAY "YES" 
              IF WS-PROPERTY = "equilateral" THEN
                 IF WS-A = WS-B AND WS-B = WS-C THEN
                    DISPLAY "EQUILATERAL"
                    MOVE 1 TO WS-RESULT 
                 ELSE 
                    DISPLAY "NOT EQUILATERAL"
                    MOVE 0 TO WS-RESULT
                 END-IF 
              ELSE IF WS-PROPERTY = "isosceles" THEN
                 IF WS-A = WS-B OR WS-A = WS-C OR WS-B = WS-C THEN
                       DISPLAY "ISOSCELES"
                       MOVE 1 TO WS-RESULT 
                 ELSE
                       DISPLAY "NOT ISOSCELES"
                       MOVE 0 TO WS-RESULT
                 END-IF 
              ELSE IF WS-PROPERTY = "scalene" THEN
                 IF WS-A = WS-B OR WS-A = WS-C OR WS-B = WS-C THEN
                    DISPLAY "NOT SCALENE"
                    MOVE 0 TO WS-RESULT 
                 ELSE 
                    DISPLAY "SCALENE"
                    MOVE 1 TO WS-RESULT
                 END-IF 
              END-IF 
           ELSE DISPLAY "NO"
           END-IF
           DISPLAY "WS-RESULT: " WS-RESULT.