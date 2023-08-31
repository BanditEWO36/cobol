IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       01 WS-DICESTRING PIC X(5).
       01 WS-COUNTER PIC 9(1).
       01 WS-ONES PIC 9(1).
       01 WS-TWOS PIC 9(1).
       01 WS-THREES PIC 9(1).
       01 WS-FOURS PIC 9(1).
       01 WS-FIVES PIC 9(1).
       01 WS-SIXES PIC 9(1).
      
       PROCEDURE DIVISION.
       YACHT.
      * Initialize the various counters by setting them all to 0. 
       MOVE 0 TO WS-ONES.
       MOVE 0 TO WS-TWOS.
       MOVE 0 TO WS-THREES.
       MOVE 0 TO WS-FOURS.
       MOVE 0 TO WS-FIVES.
       MOVE 0 TO WS-SIXES.
      * Change the Dice numeral to a string so we can easily look at each letter.
       MOVE WS-DICE TO WS-DICESTRING.
      * Call the function that will count the various dice.
       PERFORM DICE-TYPE.
      * Change the WS-CATEGORY input to all uppercase to evaluate it.
       EVALUATE FUNCTION UPPER-CASE(WS-CATEGORY)
         WHEN "ONES"
      * SCORE = NUMBER OF ONES
            MOVE WS-ONES TO WS-RESULT
         WHEN "TWOS"
      * SCORE = 2 * NUMBER OF TWOS
            COMPUTE WS-RESULT = 2 * WS-TWOS
         WHEN "THREES"
      * SCORE = 3 * NUMBER OF THREES
            COMPUTE WS-RESULT = 3 * WS-THREES
         WHEN "FOURS"
      * SCORE = 4 * NUMBER OF FOURS
                  COMPUTE WS-RESULT = 4 * WS-FOURS
         WHEN "FIVES"
      * SCORE = 5 * NUMBER OF FIVES
            COMPUTE WS-RESULT = 5 * WS-FIVES
         WHEN "SIXES"
      * SCORE = 6 * NUMBER OF SIXES
            COMPUTE WS-RESULT = 6 * WS-SIXES
         WHEN "FULL HOUSE"
      * NEED THREE OF ONE NUMBER AND TWO OF ANOTHER NUMBER; SCORE = TOTAL OF DICE
            IF WS-ONES = 3 OR WS-TWOS = 3 OR WS-THREES = 3 OR
               WS-FOURS = 3 OR WS-FIVES = 3 OR WS-SIXES = 3 AND(
               WS-ONES = 2 OR WS-TWOS = 2 OR WS-THREES = 2 OR
               WS-FOURS = 2 OR WS-FIVES = 2 OR WS-SIXES = 2) THEN
                     COMPUTE WS-RESULT = WS-ONES + (2 * WS-TWOS) +
                        (3 * WS-THREES) + (4 * WS-FOURS) +
                        (5 * WS-FIVES) + (6 * WS-SIXES)
            ELSE MOVE 0 TO WS-RESULT
            END-IF
         WHEN "FOUR OF A KIND"
      * FOUR OF THE SAME NUMBER; SCORE = TOTAL OF THE FOUR IDENTICAL DICE
      * Could be five identical dice, but will only count 4.
            IF WS-ONES >= 4 OR WS-TWOS >= 4 OR WS-THREES >= 4 OR
               WS-FOURS >= 4 OR WS-FIVES >= 4 OR WS-SIXES >= 4 THEN
                  IF WS-ONES >= 4 THEN MOVE 4 TO WS-RESULT
                  ELSE IF WS-TWOS >= 4 THEN
                     MOVE 8 TO WS-RESULT
                  ELSE IF WS-THREES >= 4 THEN
                     MOVE 12 TO WS-RESULT
                  ELSE IF WS-FOURS >= 4 THEN
                     MOVE 16 TO WS-RESULT
                  ELSE IF WS-FIVES >= 4 THEN
                     MOVE 20 TO WS-RESULT
                  ELSE MOVE 24 TO WS-RESULT
                  END-IF
            ELSE MOVE 0 TO WS-RESULT
            END-IF
         WHEN "LITTLE STRAIGHT"
      * Has to be 1-2-3-4-5; SCORE = 30
            IF WS-ONES = 1 AND WS-TWOS = 1 AND WS-THREES = 1 AND
               WS-FOURS = 1 AND WS-FIVES = 1 THEN
                  MOVE 30 TO WS-RESULT
            ELSE MOVE 0 TO WS-RESULT
            END-IF
         WHEN "BIG STRAIGHT"
      * Has to be 2-3-4-5-6; SCORE = 30
            IF WS-TWOS = 1 AND WS-THREES = 1 AND WS-FOURS = 1 
               AND WS-FIVES = 1 AND WS-SIXES = 1 THEN
                  MOVE 30 TO WS-RESULT
            ELSE MOVE 0 TO WS-RESULT
            END-IF
         WHEN "CHOICE"
      * ANY COMBINATION; SCORE = SUM OF THE DICE
            COMPUTE WS-RESULT = WS-ONES + (2 * WS-TWOS) + (3 * WS-THREES)
                  + (4 * WS-FOURS) + (5 * WS-FIVES) + (6 * WS-SIXES)
         WHEN "YACHT"
      * ALL FIVE ARE THE SAME; SCORE = 50
            IF WS-ONES = 5 OR WS-TWOS = 5 OR WS-THREES = 5 OR
               WS-FOURS = 5 OR WS-FIVES = 5 OR WS-SIXES = 5 THEN
                  MOVE 50 TO WS-RESULT
            ELSE MOVE 0 TO WS-RESULT
            END-IF
       END-EVALUATE.
       END-YACHT.


       DICE-TYPE.
      * This function counts how many of each numeral are in the dice string.
      * First index of the string is 1, so count from 1 to 5; exit the loop when WS-COUNTER becomes 6.
         PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER EQUALS
         6
            EVALUATE WS-DICESTRING(WS-COUNTER:1)
               WHEN "1"
                  ADD 1 TO WS-ONES GIVING WS-ONES
               WHEN "2"
                  ADD 1 TO WS-TWOS GIVING WS-TWOS
               WHEN "3"
                  ADD 1 TO WS-THREES GIVING WS-THREES
               WHEN "4"
                  ADD 1 TO WS-FOURS GIVING WS-FOURS
               WHEN "5"
                  ADD 1 TO WS-FIVES GIVING WS-FIVES
               WHEN "6"
                  ADD 1 TO WS-SIXES GIVING WS-SIXES
            END-EVALUATE
         END-PERFORM.
       END-DICE-TYPE.


       EXIT.