IDENTIFICATION DIVISION.
       PROGRAM-ID. difference-of-squares.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIFFERENCE-OF-SQUARES  PIC 9(8).
       01 WS-SUM-OF-SQUARES         PIC 9(8).
       01 WS-SQUARE-OF-SUM          PIC 9(8).
       01 WS-NUMBER                 PIC 9(8).
       01 WS-COUNTER                PIC 9(8).
       01 WS-SUM                    PIC 9(8).


       PROCEDURE DIVISION.
       
       SQUARE-OF-SUM.
         MOVE 1 TO WS-COUNTER
         MOVE 0 TO WS-SUM
      * GET SUM
         PERFORM WS-NUMBER TIMES
            COMPUTE WS-SUM = WS-SUM + WS-COUNTER
            ADD 1 TO WS-COUNTER GIVING WS-COUNTER
         END-PERFORM
         COMPUTE WS-SQUARE-OF-SUM = WS-SUM * WS-SUM.
       
       SUM-OF-SQUARES.
         MOVE 1 TO WS-COUNTER
         MOVE 0 TO WS-SUM-OF-SQUARES
      * GET SUM OF SQUARES
         PERFORM WS-NUMBER TIMES
            COMPUTE WS-SUM-OF-SQUARES = WS-SUM-OF-SQUARES + 
               (WS-COUNTER * WS-COUNTER)
            ADD 1 TO WS-COUNTER GIVING WS-COUNTER
         END-PERFORM.
       
       DIFFERENCE-OF-SQUARES.
         COMPUTE WS-DIFFERENCE-OF-SQUARES = WS-SQUARE-OF-SUM - 
            WS-SUM-OF-SQUARES.