IDENTIFICATION DIVISION.
        PROGRAM-ID. PANGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SENTENCE PIC X(60).
        01 WS-RESULT PIC 9.
        01 WS-SHORTER PIC X(60).
        01 WS-ACOUNT PIC 9.
        01 WS-BCOUNT PIC 9.
        01 WS-CCOUNT PIC 9.
        01 WS-DCOUNT PIC 9.
        01 WS-ECOUNT PIC 9.
        01 WS-FCOUNT PIC 9.
        01 WS-GCOUNT PIC 9.
        01 WS-HCOUNT PIC 9.
        01 WS-ICOUNT PIC 9.
        01 WS-JCOUNT PIC 9.
        01 WS-KCOUNT PIC 9.
        01 WS-LCOUNT PIC 9.
        01 WS-MCOUNT PIC 9.
        01 WS-NCOUNT PIC 9.
        01 WS-OCOUNT PIC 9.
        01 WS-PCOUNT PIC 9.
        01 WS-QCOUNT PIC 9.
        01 WS-RCOUNT PIC 9.
        01 WS-SCOUNT PIC 9.
        01 WS-TCOUNT PIC 9.
        01 WS-UCOUNT PIC 9.
        01 WS-VCOUNT PIC 9.
        01 WS-WCOUNT PIC 9.
        01 WS-XCOUNT PIC 9.
        01 WS-YCOUNT PIC 9.
        01 WS-ZCOUNT PIC 9.
        01 WS-COUNTER PIC 99.
        01 WS-TOTAL PIC 99.
        01 WS-STRING-LENGTH PIC 99.
      
        PROCEDURE DIVISION.
        PANGRAM.
        PERFORM INIT-COUNTERS
        MOVE FUNCTION TRIM(WS-SENTENCE) TO WS-SHORTER
        MOVE FUNCTION LENGTH(WS-SHORTER) TO WS-STRING-LENGTH
        IF WS-STRING-LENGTH > 25 THEN 
           PERFORM READ-STRING
           IF WS-TOTAL = 26 THEN MOVE 1 TO WS-RESULT
           ELSE MOVE 0 TO WS-RESULT
           END-IF
        ELSE MOVE 0 TO WS-RESULT
        END-IF.
        END-PANGRAM.


        EXIT.


        INIT-COUNTERS.
        MOVE 0 TO WS-RESULT.
        MOVE 0 TO WS-ACOUNT.
        MOVE 0 TO WS-BCOUNT.
        MOVE 0 TO WS-CCOUNT.
        MOVE 0 TO WS-DCOUNT.
        MOVE 0 TO WS-ECOUNT.
        MOVE 0 TO WS-FCOUNT.
        MOVE 0 TO WS-GCOUNT.
        MOVE 0 TO WS-HCOUNT.
        MOVE 0 TO WS-ICOUNT.
        MOVE 0 TO WS-JCOUNT.
        MOVE 0 TO WS-KCOUNT.
        MOVE 0 TO WS-LCOUNT.
        MOVE 0 TO WS-MCOUNT.
        MOVE 0 TO WS-NCOUNT.
        MOVE 0 TO WS-OCOUNT.
        MOVE 0 TO WS-PCOUNT.
        MOVE 0 TO WS-QCOUNT.
        MOVE 0 TO WS-RCOUNT.
        MOVE 0 TO WS-SCOUNT.
        MOVE 0 TO WS-TCOUNT.
        MOVE 0 TO WS-UCOUNT.
        MOVE 0 TO WS-VCOUNT.
        MOVE 0 TO WS-WCOUNT.
        MOVE 0 TO WS-XCOUNT.
        MOVE 0 TO WS-YCOUNT.
        MOVE 0 TO WS-ZCOUNT.
        MOVE 0 TO WS-COUNTER.
        MOVE 0 TO WS-TOTAL.
        END-INIT-COUNTERS.


        READ-STRING.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER
            EQUALS WS-STRING-LENGTH + 1
               EVALUATE FUNCTION UPPER-CASE(WS-SHORTER(WS-COUNTER:1))
                  WHEN "A"
                     MOVE 1 TO WS-ACOUNT
                  WHEN "B"
                     MOVE 1 TO WS-BCOUNT
                  WHEN "C"
                     MOVE 1 TO WS-CCOUNT
                  WHEN "D"
                     MOVE 1 TO WS-DCOUNT
                  WHEN "E"
                     MOVE 1 TO WS-ECOUNT
                  WHEN "F"
                     MOVE 1 TO WS-FCOUNT
                  WHEN "G"
                     MOVE 1 TO WS-GCOUNT
                  WHEN "H"
                     MOVE 1 TO WS-HCOUNT
                  WHEN "I"
                     MOVE 1 TO WS-ICOUNT
                  WHEN "J"
                     MOVE 1 TO WS-JCOUNT
                  WHEN "K"
                     MOVE 1 TO WS-KCOUNT
                  WHEN "L"
                     MOVE 1 TO WS-LCOUNT
                  WHEN "M"
                     MOVE 1 TO WS-MCOUNT
                  WHEN "N"
                     MOVE 1 TO WS-NCOUNT
                  WHEN "O"
                     MOVE 1 TO WS-OCOUNT
                  WHEN "P"
                     MOVE 1 TO WS-PCOUNT
                  WHEN "Q"
                     MOVE 1 TO WS-QCOUNT
                  WHEN "R"
                     MOVE 1 TO WS-RCOUNT
                  WHEN "S"
                     MOVE 1 TO WS-SCOUNT
                  WHEN "T"
                     MOVE 1 TO WS-TCOUNT
                  WHEN "U"
                     MOVE 1 TO WS-UCOUNT
                  WHEN "V"
                     MOVE 1 TO WS-VCOUNT
                  WHEN "W"
                     MOVE 1 TO WS-WCOUNT
                  WHEN "X"
                     MOVE 1 TO WS-XCOUNT
                  WHEN "Y"
                     MOVE 1 TO WS-YCOUNT
                  WHEN "Z"
                     MOVE 1 TO WS-ZCOUNT
               END-EVALUATE
         END-PERFORM
         COMPUTE WS-TOTAL = WS-ACOUNT + WS-BCOUNT + WS-CCOUNT +
            WS-DCOUNT + WS-ECOUNT + WS-FCOUNT + WS-GCOUNT + WS-HCOUNT +
            WS-ICOUNT + WS-JCOUNT + WS-KCOUNT + WS-LCOUNT + WS-MCOUNT +
            WS-NCOUNT + WS-OCOUNT + WS-PCOUNT + WS-QCOUNT + WS-RCOUNT +
            WS-SCOUNT + WS-TCOUNT + WS-UCOUNT + WS-VCOUNT + WS-WCOUNT +
            WS-XCOUNT + WS-YCOUNT + WS-ZCOUNT.
        END-READ-STRING.