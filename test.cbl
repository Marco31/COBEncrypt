      ************************************************************

      *  Tests for COBEncrypt                                    *
      
      *  compile with :                                          *
      *    cobc -x test.cbl main.cbl -o build/test               *

      *----------------------------------------------------------*
      ************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBENCRYPT-TEST.
       AUTHOR. MARC VEYSSEYRE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           Console is Name-Input.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  TESTS-STAT.
           05  TESTS-RUN           PIC 9(2) VALUE ZEROES.
           05  TESTS-PASSES        PIC 9(2) VALUE ZEROES.
           05  TESTS-FAILURES      PIC 9(2) VALUE ZEROES.
       01  WS-PROG                 PIC X(20).
       01  WS-MODULE               PIC X(20).
       01  WS-AREA-CAESAR.
           05  WS-PLAINTXT         PIC X(200).
           05  WS-ENCRYPTTXT       PIC X(200).
           05  WS-CAESAR-SETTING   PIC 9(2).
           05  WS-EXCEPTED-ENCRYPTTXT  PIC X(200).

       PROCEDURE DIVISION.
       MD01 SECTION.

       MD01-INIT-BG.
           PERFORM MD01-TEST-CAESAR.
      *     PERFORM MD01-TEST-LFSR.
           DISPLAY "Tests run: " TESTS-RUN.
           DISPLAY "Tests passed: " TESTS-PASSES.
           DISPLAY "Tests failed: " TESTS-FAILURES.
           STOP RUN.

       MD01-TEST-CAESAR.
           ADD 1 TO TESTS-RUN
           MOVE "Hello" TO WS-PLAINTXT.
           MOVE "MJQQT" TO WS-EXCEPTED-ENCRYPTTXT.
           MOVE 5 TO WS-CAESAR-SETTING.
           MOVE "COBENCRYPT" TO WS-PROG
           CANCEL  WS-PROG
           MOVE "SM-CAESAR" TO WS-MODULE
           DISPLAY "CALL MD10-CAESAR-ENCRYPTION"
           CALL WS-MODULE USING WS-PLAINTXT, WS-ENCRYPTTXT, 
               WS-CAESAR-SETTING
           END-CALL.
      *     DISPLAY WS-ENCRYPTTXT.
           IF WS-ENCRYPTTXT = WS-EXCEPTED-ENCRYPTTXT
               THEN
                   ADD 1 TO TESTS-PASSES
               ELSE
                   ADD 1 TO TESTS-FAILURES
                   DISPLAY "Test failed"
           END-IF
           EXIT.
      
      *MD01-TEST-LFSR
      *     EXIT.
