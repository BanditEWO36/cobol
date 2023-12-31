﻿IDENTIFICATION DIVISION.
        PROGRAM-ID. ISOGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-PHRASE PIC X(60).
        01 WS-RESULT PIC 99.
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
        01 WS-STRING-LENGTH PIC 99.
      
        PROCEDURE DIVISION.
        ISOGRAM.
           PERFORM INIT-COUNTERS.
           MOVE FUNCTION TRIM(WS-PHRASE) TO WS-SHORTER.
           MOVE FUNCTION LENGTH(WS-SHORTER) TO WS-STRING-LENGTH.
           PERFORM READ-STRING.
        END-ISOGRAM.


        EXIT.


        INIT-COUNTERS.
        MOVE 1 TO WS-RESULT.
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
        END-INIT-COUNTERS.


        READ-STRING.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER
            EQUALS WS-STRING-LENGTH + 1
               EVALUATE FUNCTION UPPER-CASE(WS-SHORTER(WS-COUNTER:1))
                  WHEN "A"
                     ADD 1 TO WS-ACOUNT GIVING WS-ACOUNT
                     IF WS-ACOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "B"
                     ADD 1 TO WS-BCOUNT GIVING WS-BCOUNT
                     IF WS-BCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "C"
                     ADD 1 TO WS-CCOUNT GIVING WS-CCOUNT
                     IF WS-CCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "D"
                     ADD 1 TO WS-DCOUNT GIVING WS-DCOUNT
                     IF WS-DCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "E"
                     ADD 1 TO WS-ECOUNT GIVING WS-ECOUNT
                     IF WS-ECOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "F"
                     ADD 1 TO WS-FCOUNT GIVING WS-FCOUNT
                     IF WS-FCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "G"
                     ADD 1 TO WS-GCOUNT GIVING WS-GCOUNT
                     IF WS-GCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "H"
                     ADD 1 TO WS-HCOUNT GIVING WS-HCOUNT
                     IF WS-HCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "I"
                     ADD 1 TO WS-ICOUNT GIVING WS-ICOUNT
                     IF WS-ICOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "J"
                     ADD 1 TO WS-JCOUNT GIVING WS-JCOUNT
                     IF WS-JCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "K"
                     ADD 1 TO WS-KCOUNT GIVING WS-KCOUNT
                     IF WS-KCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "L"
                     ADD 1 TO WS-LCOUNT GIVING WS-LCOUNT
                     IF WS-LCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "M"
                     ADD 1 TO WS-MCOUNT GIVING WS-MCOUNT
                     IF WS-MCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "N"
                     ADD 1 TO WS-NCOUNT GIVING WS-NCOUNT
                     IF WS-NCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "O"
                     ADD 1 TO WS-OCOUNT GIVING WS-OCOUNT
                     IF WS-OCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "P"
                     ADD 1 TO WS-PCOUNT GIVING WS-PCOUNT
                     IF WS-PCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "Q"
                     ADD 1 TO WS-QCOUNT GIVING WS-QCOUNT
                     IF WS-QCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "R"
                     ADD 1 TO WS-RCOUNT GIVING WS-RCOUNT
                     IF WS-RCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "S"
                     ADD 1 TO WS-SCOUNT GIVING WS-SCOUNT
                     IF WS-SCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "T"
                     ADD 1 TO WS-TCOUNT GIVING WS-TCOUNT
                     IF WS-TCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "U"
                     ADD 1 TO WS-UCOUNT GIVING WS-UCOUNT
                     IF WS-UCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "V"
                     ADD 1 TO WS-VCOUNT GIVING WS-VCOUNT
                     IF WS-VCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "W"
                     ADD 1 TO WS-WCOUNT GIVING WS-WCOUNT
                     IF WS-WCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "X"
                     ADD 1 TO WS-XCOUNT GIVING WS-XCOUNT
                     IF WS-XCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "Y"
                     ADD 1 TO WS-YCOUNT GIVING WS-YCOUNT
                     IF WS-YCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
                  WHEN "Z"
                     ADD 1 TO WS-ZCOUNT GIVING WS-ZCOUNT
                     IF WS-ZCOUNT > 1 MOVE 0 TO WS-RESULT
                     END-IF
               END-EVALUATE
           END-PERFORM.
        END-READ-STRING.