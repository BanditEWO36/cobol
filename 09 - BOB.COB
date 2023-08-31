IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEYBOB                PIC X(60).
       01 WS-RESULT                PIC X(40).
       01 WS-LENGTH                PIC 99.
       
       PROCEDURE DIVISION.
       BOB.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION 
              TRIM(WS-HEYBOB, TRAILING))
           IF WS-HEYBOB = " " THEN
              MOVE "Fine. Be that way!" TO WS-RESULT
           ELSE
              IF WS-HEYBOB = FUNCTION UPPER-CASE(WS-HEYBOB)
                 IF WS-HEYBOB(WS-LENGTH:1) = "?" THEN
                    MOVE "Calm down, I know what I'm doing!" TO 
                    WS-RESULT
                 ELSE
                    MOVE "Whoa, chill out!" TO WS-RESULT
                 END-IF
              ELSE
                 IF WS-HEYBOB(WS-LENGTH:1) = "?" THEN
                    MOVE "Sure." TO WS-RESULT
                 ELSE
                    MOVE "Whatever." TO WS-RESULT
                 END-IF
              END-IF
           END-IF
           DISPLAY WS-RESULT.
           GOBACK.