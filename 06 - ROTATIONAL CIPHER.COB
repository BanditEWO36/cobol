﻿IDENTIFICATION DIVISION.
       PROGRAM-ID. rotational-cipher.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-KEY PIC 9(2).
       01 WS-TEXT    PIC X(128).
       01 WS-CIPHER  PIC X(128).
       01 WS-ALPHA   PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01 WS-SHIFTED PIC X(26).
       01 WS-COUNTER PIC 999.
       01 NEW-ORD    PIC 99.


       PROCEDURE DIVISION.
       ROTATIONAL-CIPHER.
         MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-CIPHER
         PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL
            WS-COUNTER EQUAL 27
               COMPUTE NEW-ORD = WS-COUNTER + WS-KEY
               IF NEW-ORD IS GREATER THAN 26 THEN
                  COMPUTE NEW-ORD = NEW-ORD - 26
               END-IF
               MOVE WS-ALPHA(NEW-ORD:1) TO
                  WS-SHIFTED(WS-COUNTER:1)
         END-PERFORM
         INSPECT WS-CIPHER CONVERTING WS-ALPHA TO WS-SHIFTED.