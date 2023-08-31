IDENTIFICATION DIVISION.
       PROGRAM-ID. hamming.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA-1 PIC X(32).
       01 WS-DNA-2 PIC X(32).
       01 WS-HAMMING PIC 9(2).
       01 WS-ERROR PIC X(31).
       01 WS-COUNTER PIC 99.


       PROCEDURE DIVISION.
       HAMMING.
      * BOTH EMPTY STRANDS
         IF WS-DNA-1 = " " AND WS-DNA-2 = " " THEN
            MOVE 0 TO WS-HAMMING
      * NOT BOTH EMPTY STRANDS
         ELSE
      * DIFFERENT LENGTH STRANDS
            IF FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1, TRAILING))
               IS NOT EQUAL
               FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-2, TRAILING)) THEN
                  MOVE 0 TO WS-HAMMING
                  MOVE "Strands must be of equal length" TO WS-ERROR
      * SAME LENGTH
            ELSE
      * THEY ARE THE SAME
               IF WS-DNA-1 = WS-DNA-2 THEN
                  MOVE 0 TO WS-HAMMING
               ELSE
      * THEY ARE DIFFERENT
                  MOVE 0 TO WS-HAMMING
                  MOVE 1 TO WS-COUNTER
                  PERFORM
                   FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1, TRAILING))
                   TIMES
                     IF WS-DNA-1(WS-COUNTER:1) IS NOT EQUAL 
                        WS-DNA-2(WS-COUNTER:1) THEN
                           ADD 1 TO WS-HAMMING GIVING WS-HAMMING
                     END-IF
                     ADD 1 TO WS-COUNTER GIVING WS-COUNTER
                  END-PERFORM
               END-IF
            END-IF
         END-IF
      


       DISPLAY "Hamming Distance is: " WS-HAMMING.
       DISPLAY WS-ERROR.