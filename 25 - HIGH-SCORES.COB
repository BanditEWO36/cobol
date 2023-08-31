IDENTIFICATION DIVISION.
       PROGRAM-ID. high-scores.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PROPERTY       PIC A(20).
       01 WS-SCORES.
           02 SCORES        PIC X(3) OCCURS 20 TIMES
                            INDEXED BY IDX.
         
       01 WS-RESULT-STRING  PIC X(60) VALUE SPACES.
       01 WS-RESULT-VALUE   PIC 999.
       01 WS-COUNTER         PIC 99.
       01 WS-HIGH-SCORE      PIC X(3).
       01 WS-2ND-HIGH        PIC X(3).
       01 WS-3RD-HIGH        PIC X(3).
       01 WS-LATEST          PIC X(3).
       01 WS-LENGTH          PIC 99.
       01 WS-INDEX           PIC 99.


       PROCEDURE DIVISION.
       HIGH-SCORES.
           EVALUATE WS-PROPERTY 
           WHEN "scores"
              MOVE WS-SCORES TO WS-RESULT-STRING 
              DISPLAY "ALL SCORES: " WS-RESULT-STRING
           WHEN "latest"
              PERFORM GET-LATEST 
              MOVE WS-LATEST TO WS-RESULT-VALUE 
              DISPLAY "LATEST: " WS-RESULT-VALUE
           WHEN "personalBest"
              PERFORM GET-TOP-THREE
              MOVE WS-HIGH-SCORE TO WS-RESULT-VALUE 
              DISPLAY "PERSONAL BEST: " WS-RESULT-VALUE
           WHEN "personalTopThree"
              PERFORM GET-TOP-THREE
              MOVE SPACES TO WS-RESULT-STRING
              MOVE WS-HIGH-SCORE TO WS-RESULT-STRING(1:3)
              MOVE WS-2ND-HIGH TO WS-RESULT-STRING(4:3)
              MOVE WS-3RD-HIGH TO WS-RESULT-STRING(7:3)
              DISPLAY "TOP THREE: " 
                 FUNCTION TRIM(WS-RESULT-STRING, TRAILING) " :"
           END-EVALUATE.
       
       GET-LATEST.
           COMPUTE WS-LENGTH = FUNCTION LENGTH(
           FUNCTION TRIM(WS-SCORES, TRAILING))
           DISPLAY "LENGTH: " WS-LENGTH
           COMPUTE WS-INDEX = WS-LENGTH / 3
           DISPLAY "INDEX: " WS-INDEX 
           MOVE SCORES(WS-INDEX) TO WS-LATEST.


       GET-TOP-THREE.
           MOVE SCORES(1) TO WS-HIGH-SCORE
           MOVE "   " TO WS-2ND-HIGH 
           MOVE "   " TO WS-3RD-HIGH 
           MOVE SCORES(1) TO WS-RESULT-STRING(1:3)
           PERFORM VARYING WS-COUNTER FROM 2 BY 1 UNTIL WS-COUNTER > 20
              DISPLAY "CURRENT HIGH SCORE: " WS-HIGH-SCORE 
              IF SCORES(WS-COUNTER) > WS-HIGH-SCORE THEN 
                 MOVE WS-2ND-HIGH TO WS-3RD-HIGH
                 MOVE WS-HIGH-SCORE TO WS-2ND-HIGH 
                 MOVE SCORES(WS-COUNTER) TO WS-HIGH-SCORE
              ELSE 
                 IF SCORES(WS-COUNTER) > WS-2ND-HIGH THEN 
                    MOVE WS-2ND-HIGH TO WS-3RD-HIGH 
                    MOVE SCORES(WS-COUNTER) TO WS-2ND-HIGH 
                 ELSE 
                    IF SCORES(WS-COUNTER) > WS-3RD-HIGH THEN
                       MOVE SCORES(WS-COUNTER) TO WS-3RD-HIGH 
                    END-IF
                 END-IF
              END-IF 
              DISPLAY "SCORE #" WS-COUNTER ": " SCORES(WS-COUNTER)
              DISPLAY "NEW HIGH SCORE: " WS-HIGH-SCORE
              DISPLAY "2ND HIGH SCORE: " WS-2ND-HIGH 
              DISPLAY "3RD HIGH SCORE: " WS-3RD-HIGH 
              IF SCORES(WS-COUNTER) EQUAL TO SPACE THEN
                 CONTINUE 
              ELSE 
                 MOVE SCORES(WS-COUNTER) TO
                    WS-RESULT-STRING(WS-COUNTER * 3 - 2:3)
              END-IF
              DISPLAY ":" WS-RESULT-STRING ":"
           END-PERFORM
           DISPLAY ":" FUNCTION TRIM(WS-RESULT-STRING, TRAILING) ":"
           .