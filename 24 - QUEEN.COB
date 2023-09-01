IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEEN-ATTACK.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Inputs
       01 WS-QUEEN PIC X(9).
       01 WS-WHITE_QUEEN PIC X(9).
       01 WS-BLACK_QUEEN PIC X(9).
       01 WS-PROPERTY PIC X(11).
      * WS-PROPERTY IS "create" OR "canAttack"
      
      * Internal Variables
       01 WS-LENGTH    PIC 9.
       01 WS-CHARS     PIC 9.
       01 WS-X         PIC 9.
       01 WS-Y         PIC 9.
       01 WS-X1        PIC 9.
       01 WS-Y1        PIC 9.
       01 WS-X2        PIC 9.
       01 WS-Y2        PIC 9.
       01 WS-RISE      PIC 9.
       01 WS-RUN       PIC 9.
      
      *Outputs
       01 WS-RESULT PIC 9.


       PROCEDURE DIVISION.
       QUEEN-ATTACK.
           MOVE 0 TO WS-CHARS 
           MOVE 0 TO WS-X 
           MOVE 0 TO WS-Y 
           MOVE 0 TO WS-X1
           MOVE 0 TO WS-Y1 
           MOVE 0 TO WS-X2 
           MOVE 0 TO WS-Y2 
      * IF THERE'S A NEGATIVE NUMBER, IT'S NOT ALLOWED
           INSPECT WS-QUEEN TALLYING WS-CHARS FOR ALL "-"
           DISPLAY "QUEEN: " WS-QUEEN 
           DISPLAY "WHITE QUEEN: (" FUNCTION TRIM(WS-WHITE_QUEEN) ")"
           DISPLAY "BLACK QUEEN: (" FUNCTION TRIM(WS-BLACK_QUEEN) ")"
      * IF ANY X OR Y VALUE IS NEGATIVE, THE COORDINATE IS INVALID
           IF WS-CHARS > 0 THEN
              DISPLAY "NEGATIVE NUMBER DETECTED; INVALID INPUT"
              MOVE 0 TO WS-RESULT 
           ELSE
      * NO NEGATIVES, SO CAN CONTINUE
              COMPUTE WS-LENGTH = FUNCTION LENGTH(
                 FUNCTION TRIM(WS-QUEEN))
              DISPLAY "LENGTH: " WS-LENGTH
              MOVE WS-QUEEN(1:1) TO WS-X 
              DISPLAY "BEFORE THE COMMA: " WS-X  
              MOVE WS-QUEEN(3:1) TO WS-Y  
              DISPLAY "AFTER THE COMMA: " WS-Y 
              DISPLAY ":" FUNCTION UPPER-CASE(
                 FUNCTION TRIM(WS-PROPERTY)) ":"
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-PROPERTY)) = 
                 "CREATE" THEN
                    DISPLAY "CREATE"
      * I THINK THE LIMIT SHOULD BE 8, BUT THE TESTS ONLY ALLOW 7
                    IF WS-X > 7 OR WS-Y > 7 THEN
                       DISPLAY "INVALID POSITION"
                       DISPLAY "GREATER THAN 7"
                       MOVE 0 TO WS-RESULT 
                    ELSE 
                       DISPLAY "VALID POSITION"
                       MOVE 1 TO WS-RESULT 
                    END-IF
              ELSE
      * THIS SECTION DETERMINES WHETHER THE QUEENS CAN ATTACK EACH OTHER
                 DISPLAY "CAN-ATTACK"
                 MOVE WS-WHITE_QUEEN(1:1) TO WS-X1 
                 DISPLAY "BEFORE THE COMMA: " WS-X1
                 MOVE WS-WHITE_QUEEN(3:1) TO WS-Y1 
                 DISPLAY "AFTER THE COMMA: " WS-Y1 
                 MOVE WS-BLACK_QUEEN(1:1) TO WS-X2
                 DISPLAY "BEFORE THE COMMA: " WS-X2
                 MOVE WS-BLACK_QUEEN(3:1) TO WS-Y2
                 DISPLAY "AFTER THE COMMA: " WS-Y2 
                 DISPLAY "White: (" WS-X1 ", " WS-Y1 ")"
                 DISPLAY "Black: (" WS-X2 ", " WS-Y2 ")"
      * IF X OR Y VALUES ARE EQUAL, THE QUEENS CAN ATTACK EACH OTHER
                 IF WS-X1 = WS-X2 OR WS-Y1 = WS-Y2 THEN
                    MOVE 1 TO WS-RESULT
                    DISPLAY "RESULT: " WS-RESULT ": SAME ROW OR COLUMN"
                 ELSE
      * IF THEY DON'T HAVE SAME X OR Y, WE MUST DETERMINE THE RISE AND RUN
      * MUST ACCOUNT FOR NEGATIVE NUMBER RESULTS BY USING ABSOLUTE VALUE
                    IF (WS-Y2 - WS-Y1) < 0 THEN
                       COMPUTE WS-RISE = FUNCTION ABS(WS-Y2 - WS-Y1)
                       DISPLAY "WS-Y2 - WS-Y1 = -" WS-RISE
                    ELSE 
                       COMPUTE WS-RISE = WS-Y2 - WS-Y1
                       DISPLAY "WS-Y2 - WS-Y1 = " WS-RISE
                    END-IF
      * MUST ACCOUNT FOR NEGATIVE NUMBER RESULTS BY USING ABSOLUTE VALUE
                    IF (WS-X2 - WS-X1) < 0 THEN
                       COMPUTE WS-RUN = FUNCTION ABS(WS-X2 - WS-X1) 
                       DISPLAY "WS-X2 - WS-X1 = -" WS-RUN
                    ELSE 
                       COMPUTE WS-RUN = WS-X2 - WS-X1 
                       DISPLAY "WS-X2 - WS-X1 = " WS-RUN 
                    END-IF
      * IF THE RISE = RUN THEN THE QUEENS CAN ATTACK EACH OTHER         
                    IF (WS-RISE = WS-RUN) THEN
                          DISPLAY "RISE EQUALS RUN"
                          MOVE 1 TO WS-RESULT
                    ELSE
                       DISPLAY "RISE DOES NOT EQUAL RUN"
                       MOVE 0 TO WS-RESULT
                    END-IF 
                    DISPLAY "RESULT: " WS-RESULT 
                 END-IF
              END-IF
           END-IF 


           .