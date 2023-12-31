﻿IDENTIFICATION DIVISION.
       PROGRAM-ID. SCRABBLE-SCORE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Inputs
       01 WS-WORD   PIC X(60).
      *Internal Variables
       01 WS-COUNTER   PIC 99.
       01 WS-LENGTH    PIC 99.
      *Outputs
       01 WS-RESULT PIC 99.
      
      * SCORE THE WORD:
      * A, E, I, O, U, L, N, R, S, T = 1,
      * D, G = 2
      * B, C, M, P = 3
      * F, H, V, W, Y = 4
      * K = 5
      * J, X = 8
      * Q, Z = 10


       PROCEDURE DIVISION.
       SCRABBLE-SCORE.
           MOVE 0 TO WS-RESULT
           COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(WS-WORD))
           DISPLAY "LENGTH: " WS-LENGTH
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL 
              WS-COUNTER > WS-LENGTH
                 IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = "A" OR 
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = "E" OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = "I" OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = "O" OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = "U" OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'L' OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'N' OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'R' OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'S' OR
                    FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'T' THEN
                       ADD 1 TO WS-RESULT GIVING WS-RESULT 
                       DISPLAY WS-WORD(WS-COUNTER:1) ": 1"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'D' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'G' 
                        THEN
                             ADD 2 TO WS-RESULT GIVING WS-RESULT 
                             DISPLAY WS-WORD(WS-COUNTER:1) ": 2"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'B' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'C' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'M' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'P' 
                        THEN
                             ADD 3 TO WS-RESULT GIVING WS-RESULT 
                             DISPLAY WS-WORD(WS-COUNTER:1) ": 3"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'F' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'H' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'V' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'W' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'Y' 
                        THEN
                             ADD 4 TO WS-RESULT GIVING WS-RESULT 
                             DISPLAY WS-WORD(WS-COUNTER:1) ": 4"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'K' 
                        THEN
                          ADD 5 TO WS-RESULT GIVING WS-RESULT 
                          DISPLAY WS-WORD(WS-COUNTER:1) ": 5"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'J' 
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'X' 
                        THEN
                             ADD 8 TO WS-RESULT GIVING WS-RESULT 
                             DISPLAY WS-WORD(WS-COUNTER:1) ": 8"
                 ELSE IF FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'Q'
                        OR
                         FUNCTION UPPER-CASE(WS-WORD(WS-COUNTER:1)) = 'Z' 
                        THEN 
                             ADD 10 TO WS-RESULT GIVING WS-RESULT 
                             DISPLAY WS-WORD(WS-COUNTER:1) ": 10"
                      END-IF
                     END-IF 
                    END-IF 
                   END-IF 
                  END-IF 
                  END-IF
                 END-IF 
           END-PERFORM
           DISPLAY "TOTAL: " WS-RESULT 


           .