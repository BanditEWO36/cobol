IDENTIFICATION DIVISION.
       PROGRAM-ID. luhn.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CARD-NUMBER    PIC X(32).
       01 WS-CARD-DIGITS    PIC X(32) VALUE SPACES.
       01 WS-CARD-REVERSED  PIC X(32) VALUE SPACES.
       01 WS-DIGIT-TO-CHECK PIC 9.
       01 WS-CHECKSUM       PIC 9(2).
       01 WS-VALID          PIC X(5).
       01 WS-LENGTH         PIC 99.
       01 WS-INDEX1         PIC 99.
       01 WS-INDEX2         PIC 99.
       01 WS-COUNTER        PIC 99.
       01 WS-SUM            PIC 9999 VALUE 0.
       
       PROCEDURE DIVISION.
       LUHN.
           PERFORM CONVERT-NUMBER.
           MOVE FUNCTION TRIM(
            FUNCTION REVERSE(WS-CARD-DIGITS), LEADING)
            TO WS-CARD-REVERSED
           IF WS-VALID = "TRUE" THEN
              PERFORM VALIDATE-NUMBER
           END-IF
           STOP RUN.


        VALIDATE-NUMBER.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(
             WS-CARD-REVERSED))
      * LOOK AT EVERY OTHER DIGIT (EX. 2ND, 4TH, 6TH, ETC.)
           PERFORM VARYING WS-COUNTER FROM 2 BY 2 UNTIL 
            WS-COUNTER > WS-LENGTH
              MOVE WS-CARD-REVERSED(WS-COUNTER:1) TO WS-DIGIT-TO-CHECK
               IF WS-DIGIT-TO-CHECK * 2 > 9 THEN
                 COMPUTE WS-DIGIT-TO-CHECK = 
                     WS-DIGIT-TO-CHECK * 2 - 9
                 MOVE WS-DIGIT-TO-CHECK TO 
                    WS-CARD-REVERSED(WS-COUNTER:1)
               ELSE
                  COMPUTE WS-DIGIT-TO-CHECK = WS-DIGIT-TO-CHECK * 2
               END-IF
               COMPUTE WS-SUM = WS-SUM + WS-DIGIT-TO-CHECK
           END-PERFORM
      * IF THE SUM IS EVENLY DIVISIBLE BY 10, THEN THE NUMBER IS VALID
      * IF NOT, IT IS A FALSE NUMBER
           IF FUNCTION MOD(WS-SUM, 10) = 0 THEN
              MOVE "VALID" TO WS-VALID
           ELSE
              MOVE "FALSE" TO WS-VALID
           END-IF.
      
       CONVERT-NUMBER.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(
            FUNCTION TRIM(WS-CARD-NUMBER))
           MOVE 1 TO WS-INDEX2
      * GET RID OF ANY SPACES
           PERFORM VARYING WS-INDEX1 FROM 1 BY 1 UNTIL 
              WS-INDEX1 > WS-LENGTH
                    IF WS-CARD-NUMBER(WS-INDEX1:1) IS NOT = SPACES 
                       THEN MOVE WS-CARD-NUMBER(WS-INDEX1:1) TO
                          WS-CARD-DIGITS(WS-INDEX2:1)
                       ADD 1 TO WS-INDEX2 GIVING WS-INDEX2 
                    END-IF             
           END-PERFORM.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(
            WS-CARD-DIGITS))
      * SEE IF THERE ARE ANY CHARACTERS OTHER THAN DIGITS OR
      * IF IT'S ONLY GOT 1 CHARACTER
           IF FUNCTION TRIM(WS-CARD-DIGITS) IS NOT NUMERIC OR 
              WS-LENGTH < 2 THEN 
                 MOVE "FALSE" TO WS-VALID
           ELSE
      * JUST DIGITS
              IF WS-LENGTH < 2 THEN
      * TOO SHORT
                 MOVE "FALSE" TO WS-VALID
              ELSE
      * GOOD TO GO
                 MOVE "TRUE" TO WS-VALID
              END-IF
           END-IF.