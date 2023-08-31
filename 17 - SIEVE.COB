IDENTIFICATION DIVISION.
       PROGRAM-ID. SIEVE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * WS-LIMIT IS THE GREATEST NUMBER BEING CONSIDERED
       01 WS-LIMIT        PIC 9999.
       01 WS-LIST.
      * WS-RESULT WILL BE THE LIST OF PRIME NUMBERS
          05 WS-RESULT    PIC 999 VALUE 0 OCCURS 1000 TIMES.
      * WS-COMPOSITE INDICATES IF THE INDEX IS (0) PRIME OR (1) COMPOSITE
          05 WS-COMPOSITE PIC 9 VALUE 0 OCCURS 1000 TIMES.
       01 WS-COUNT        PIC 9999 VALUE 0.
       01 WS-INDEX1       PIC 9999.
       01 WS-INDEX2       PIC 9999.
       01 WS-DIVISOR      PIC 9999.
      
       PROCEDURE DIVISION.
       SIEVE.
          IF WS-LIMIT < 2 THEN 
      * NO PRIME NUMBERS LESS THAN 2
              CONTINUE
          ELSE IF WS-LIMIT = 2 THEN 
              MOVE 2 TO WS-RESULT(1)
              MOVE 1 TO WS-COUNT
          ELSE
              MOVE 2 TO WS-RESULT(1)
              MOVE 1 TO WS-COMPOSITE(2)
              MOVE 2 TO WS-COUNT
              MOVE 3 TO WS-INDEX1
              MOVE 2 TO WS-INDEX2
              MOVE 3 TO WS-DIVISOR
              PERFORM UNTIL WS-INDEX1 > WS-LIMIT 
      * MAKE A LIST OF NUMBERS UP TO WS-LIMIT
      * LABEL ALL PRIMES AS 1 IN WS-PRIME
                 IF FUNCTION MOD(WS-INDEX1, 2) = 0 THEN
                    ADD 1 TO WS-INDEX1 GIVING WS-INDEX1 
                 ELSE 
                    MOVE 3 TO WS-DIVISOR
                    PERFORM UNTIL WS-DIVISOR >= WS-LIMIT 
                       IF (FUNCTION MOD(WS-INDEX1, WS-DIVISOR) = 0) 
                          AND (WS-INDEX1 IS NOT = WS-DIVISOR) THEN 
                             EXIT PERFORM
                       ELSE 
      * ADD 2 BECAUSE ONLY ODD NUMBERS ARE LEFT
                          ADD 2 TO WS-DIVISOR GIVING WS-DIVISOR
                       END-IF
                       IF WS-DIVISOR >= WS-INDEX1 THEN 
                          COMPUTE WS-COUNT = WS-COUNT + 1
                          MOVE WS-INDEX1 TO WS-RESULT(WS-INDEX2)
                          MOVE 1 TO WS-COMPOSITE(WS-INDEX1)
                          ADD 1 TO WS-INDEX2 GIVING WS-INDEX2 
                          MOVE WS-LIMIT TO WS-DIVISOR
                       END-IF
                    END-PERFORM
                    ADD 1 TO WS-INDEX1 GIVING WS-INDEX1
                 END-IF
              END-PERFORM
              MOVE 1 TO WS-INDEX1
              PERFORM WS-LIMIT TIMES
                 ADD 1 TO WS-INDEX1 GIVING WS-INDEX1
              END-PERFORM
          END-IF
          .