IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X      PIC 99V9.
       01 WS-Y      PIC 99V9.
       01 WS-C      PIC 99V99.
       01 WS-RESULT PIC 99.
       PROCEDURE DIVISION.
       DARTS.
      * SIMPLY TAKE THE COORDINATES AND FIND THE HYPOTENUSE USING THE PYTHAGOREAN THEOREM
      * WHERE A = WS-X, B = WS-Y, AND C = WS-C
      * WS-C = SQRT(WS-X^2 + WS-Y^2)
      * WS-C IS THE RADIUS AT THE POINT (WS-X, WS-Y)
       COMPUTE WS-C ROUNDED = FUNCTION SQRT(WS-X ** 2 + WS-Y ** 2)
       IF WS-C <= 1 THEN
         MOVE 10 TO WS-RESULT
       ELSE
         IF WS-C > 1 AND WS-C <= 5 THEN
            MOVE 5 TO WS-RESULT
         ELSE
            IF WS-C > 5 AND WS-C <= 10 THEN
               MOVE 1 TO WS-RESULT
            ELSE
               MOVE 0 TO WS-RESULT
            END-IF
         END-IF
       END-IF.