﻿IDENTIFICATION DIVISION.
       PROGRAM-ID. two-fer.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME    PIC X(16).
       01 WS-RESULT  PIC X(64).
       01 WS-S1      PIC X(8)  VALUE "One for ".
       01 WS-S2      PIC X(13) VALUE ", one for me.".
       
       PROCEDURE DIVISION.
       TWO-FER.
         IF WS-NAME = " " THEN
            MOVE "One for you, one for me." TO WS-RESULT
         ELSE
            STRING WS-S1 DELIMITED BY SIZE
                   WS-NAME DELIMITED BY SPACE
                   WS-S2 
            INTO WS-RESULT
         END-IF.
         EXIT.