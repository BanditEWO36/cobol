﻿IDENTIFICATION DIVISION.
       PROGRAM-ID. collatz-conjecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER     PIC S9(8).
       01 WS-STEPS      PIC 9(4).
       01 WS-ERROR      PIC X(35).
       01 N             PIC S9(8).
       01 WS-REMAINDER  PIC 9.
       
       PROCEDURE DIVISION.
       COLLATZ-CONJECTURE.
         IF WS-NUMBER IS LESS THAN 1 THEN
            MOVE 0 TO WS-STEPS
            MOVE "Only positive integers are allowed" TO WS-ERROR
         ELSE
            IF WS-NUMBER = 1 THEN 
               MOVE 0 TO WS-STEPS
            ELSE
               MOVE 0 TO WS-STEPS
               PERFORM UNTIL WS-NUMBER EQUAL 1
                  ADD 1 TO WS-STEPS GIVING WS-STEPS
                  DIVIDE WS-NUMBER BY 2 GIVING N REMAINDER WS-REMAINDER
                  IF WS-REMAINDER = 0 THEN
                     COMPUTE WS-NUMBER = WS-NUMBER / 2
                  ELSE
                     COMPUTE WS-NUMBER = 3 * WS-NUMBER + 1
                  END-IF
               END-PERFORM
            END-IF
         END-IF         
      
         DISPLAY WS-STEPS.
         DISPLAY WS-ERROR.