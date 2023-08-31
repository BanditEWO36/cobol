IDENTIFICATION DIVISION.
       PROGRAM-ID. ALRGY.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE        PIC 999.
       01 WS-ITEM         PIC X(12).
       01 WS-RESULT       PIC A.
       01 WS-RESULT-LIST  PIC X(108) VALUE SPACES.
       01 WS-NEW-SCORE    PIC 999.
       01 WS-TEMP-LIST    PIC X(108) VALUE SPACES.
       01 WS-LENGTH       PIC 999.
       01 WS-TEST         PIC X(8).
       01 WS-COUNTER      PIC 9.
       01 WS-CHARS        PIC 99.
       01 WS-ALLERGEN-LIST.
           05 WS-VALUE    PIC 999 OCCURS 8 TIMES.
           05 WS-ALLERGEN PIC X(12) OCCURS 8 TIMES.


       PROCEDURE DIVISION.
       MAIN.
      * THIS SECTION IS INPUT FROM THE USER ******
           MOVE "cats" TO WS-ITEM
           MOVE 683 TO WS-SCORE
           MOVE "allergic" TO WS-TEST
      ********************************************
      * THIS SECTION POPULATES THE ALLERGEN LIST
           MOVE 1   TO WS-VALUE(1)
           MOVE 2   TO WS-VALUE(2)
           MOVE 4   TO WS-VALUE(3)
           MOVE 8   TO WS-VALUE(4)
           MOVE 16  TO WS-VALUE(5)
           MOVE 32  TO WS-VALUE(6)
           MOVE 64  TO WS-VALUE(7)
           MOVE 128 TO WS-VALUE(8)
           MOVE "eggs"          TO WS-ALLERGEN(1)
           MOVE "peanuts"       TO WS-ALLERGEN(2)
           MOVE "shellfish"     TO WS-ALLERGEN(3)
           MOVE "strawberries"  TO WS-ALLERGEN(4)
           MOVE "tomatoes"      TO WS-ALLERGEN(5)
           MOVE "chocolate"     TO WS-ALLERGEN(6)
           MOVE "pollen"        TO WS-ALLERGEN(7)
           MOVE "cats"          TO WS-ALLERGEN(8)
      ********************************************
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER = 9
              DISPLAY "ALLERGEN: " WS-ALLERGEN(WS-COUNTER)
                 "    POWER: " WS-VALUE(WS-COUNTER)
           END-PERFORM
           IF WS-TEST = "allergic" THEN
              DISPLAY "TEST: ALLERGIC TO " WS-ITEM
              PERFORM ALLERGIC-TO
              DISPLAY WS-RESULT
           ELSE
              DISPLAY "TEST: ALLERGEN LIST"
              PERFORM LIST-ALLERGENS
           END-IF


           STOP RUN.


       ALLERGIC-TO.
      * This function returns a Y or N to the question:
      * Is the patient allergic to WS-ITEM given their WS-SCORE
           IF WS-SCORE = 0 THEN
              MOVE "N" TO WS-RESULT
           ELSE
              PERFORM DISECT-THE-SCORE
              MOVE 0 TO WS-COUNTER
              INSPECT WS-RESULT-LIST TALLYING WS-COUNTER FOR ALL WS-ITEM
              IF WS-COUNTER = 0 THEN
                 MOVE "N" TO WS-RESULT
              ELSE
                 MOVE "Y" TO WS-RESULT
              END-IF
           END-IF.


       LIST-ALLERGENS.
      * Code this paragraph
           IF WS-SCORE = 0 THEN
              MOVE SPACES TO WS-RESULT-LIST
           ELSE
              PERFORM DISECT-THE-SCORE
           END-IF.


      * The list of items (and their value) that were tested are:
      * eggs (1)
      * peanuts (2)
      * shellfish (4)
      * strawberries (8)
      * tomatoes (16)
      * chocolate (32)
      * pollen (64)
      * cats (128)
      * NOTE: Each value is a power of 2: 1 = 2^0, 2 = 2^1, 4 = 2^2 ...


      * WS-ITEM is the description, ex. "strawberries"
      * WS-SCORE is the patient's score.
      * If WS-SCORE = 0, the patient is not allergic to anything
      * WS-RESULT is "Y" or "N"


       DISECT-THE-SCORE.
           DISPLAY "SCORE: " WS-SCORE
           MOVE WS-SCORE TO WS-NEW-SCORE
           MOVE 0 TO WS-LENGTH
           MOVE 0 TO WS-CHARS
      * 2^8 = 256 which is higher than anything in our list
      *     PERFORM UNTIL WS-NEW-SCORE = 0
              IF WS-NEW-SCORE >= 256 THEN
                 PERFORM UNTIL WS-NEW-SCORE < 256
                    COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 256
                    DISPLAY "MUST GET THE SCORE BELOW 256 FOR THIS LIST"
                    DISPLAY "NEW SCORE: " WS-NEW-SCORE
                 END-PERFORM
              END-IF
              PERFORM VARYING WS-COUNTER FROM 8 BY -1 UNTIL
               WS-NEW-SCORE < 1
                 IF WS-NEW-SCORE >= WS-VALUE(WS-COUNTER) THEN
                    PERFORM UNTIL WS-NEW-SCORE < WS-VALUE(WS-COUNTER)
                       COMPUTE WS-NEW-SCORE = WS-NEW-SCORE -
                          WS-VALUE(WS-COUNTER)
                       DISPLAY WS-VALUE(WS-COUNTER) " "
                          WS-ALLERGEN(WS-COUNTER) " IS AN ALLERGEN"
                       DISPLAY "NEW SCORE: " WS-NEW-SCORE
                    END-PERFORM
                    COMPUTE WS-CHARS = FUNCTION LENGTH(
                       FUNCTION TRIM(WS-ALLERGEN(WS-COUNTER)))
                    DISPLAY "CHARS: " WS-CHARS
                    MOVE FUNCTION REVERSE(
                       FUNCTION TRIM(WS-ALLERGEN(WS-COUNTER)))
                       TO WS-TEMP-LIST(WS-LENGTH + 1:WS-CHARS)
                    IF WS-NEW-SCORE >= 1 THEN
                       COMPUTE WS-LENGTH = WS-LENGTH + WS-CHARS + 1
                       MOVE "," TO WS-TEMP-LIST(WS-LENGTH:1)
                    ELSE
                       COMPUTE WS-LENGTH = WS-LENGTH + WS-CHARS
                    END-IF
                    DISPLAY WS-TEMP-LIST
                 ELSE
                    DISPLAY WS-VALUE(WS-COUNTER) " "
                       WS-ALLERGEN(WS-COUNTER) " IS NOT AN ALLERGEN"
                 END-IF
              END-PERFORM
      *        IF WS-NEW-SCORE >= 64 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 64
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 64
      *              DISPLAY "64 (POLLEN) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "nellop," TO WS-TEMP-LIST(WS-LENGTH + 1:7)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 7
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "64 (POLLEN) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 32 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 32
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 32
      *              DISPLAY "32 (CHOCOLATE) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "etalocohc," TO WS-TEMP-LIST(WS-LENGTH + 1:10)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 10
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "32 (CHOCOLATE) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 16 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 16
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 16
      *              DISPLAY "16 (TOMATOES) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "seotamot," TO WS-TEMP-LIST(WS-LENGTH + 1:9)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 9
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "16 (TOMATOES) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 8 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 8
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 8
      *              DISPLAY "8 (STRAWBERRIES) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "seirrebwarts," TO WS-TEMP-LIST(WS-LENGTH + 1:13)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 13
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "8 (STRAWBERRIES) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 4 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 4
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 4
      *              DISPLAY "4 (SHELLFISH) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "hsifllehs," TO WS-TEMP-LIST(WS-LENGTH + 1:10)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 10
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "4 (SHELLFISH) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 2 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 2
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 2
      *              DISPLAY "2 (PEANUTS) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "stunaep," TO WS-TEMP-LIST(WS-LENGTH + 1:8)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 8
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "2 (PEANUTS) IS NOT AN ALLERGEN"
      *        END-IF
      *        IF WS-NEW-SCORE >= 1 THEN
      *           PERFORM UNTIL WS-NEW-SCORE < 1
      *              COMPUTE WS-NEW-SCORE = WS-NEW-SCORE - 1
      *              DISPLAY "1 (EGGS) IS AN ALLERGEN"
      *              DISPLAY "NEW SCORE: " WS-NEW-SCORE
      *           END-PERFORM
      *           MOVE "sgge" TO WS-TEMP-LIST(WS-LENGTH + 1:4)
      *           COMPUTE WS-LENGTH = WS-LENGTH + 4
      *           DISPLAY WS-TEMP-LIST
      *        ELSE
      *           DISPLAY "1 (EGGS) IS NOT AN ALLERGEN"
      *        END-IF
      *     END-PERFORM
           MOVE FUNCTION TRIM(FUNCTION REVERSE(WS-TEMP-LIST)) TO
            WS-RESULT-LIST
           DISPLAY WS-RESULT-LIST.