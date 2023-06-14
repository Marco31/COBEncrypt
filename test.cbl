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
      * 01  WS-AREA-LFSR.
      *     05  WS-LFSR-A-STATIC.
      *         10  WS-INTERNAL-STATE    PIC 9(1) COMP OCCURS 8 TIMES
      *                                         INDEXED BY IND-IS.
      *         10  WS-NB-XOR  PIC 9(1).
      *         10  WS-I   PIC 9(2).
      *         10  WS-J   PIC 9(2).
      *         10  WS-NB-GEN  PIC 9(4).
      *         10  WS-WS-BIT  PIC 9(1).
      *     05  WS-LFSR-A-STREAM.
      *         10  WS-LFSR-STREAM     PIC 9(1) OCCURS 1 TO 9999
      *                                         DEPENDING ON NB-GEN.
      *         10  WS-LFSR-STREAM-ENC     PIC 9(1) OCCURS 1 TO 9999
      *                                         DEPENDING ON NB-GEN.
      * 01 WS-AREA-LFSR-2.
      *     05  WS-LFSR2-A-STATIC.
      *         10  WS-IND-XB  PIC 9(2).
      *     05  WS-LFSR2-A-XOR.
      *         10  WS-XOR-BITS    PIC 9(1) COMP OCCURS 1 TO 8
      *                      DEPENDING ON NB-XOR.


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

           MOVE "PRE-TEST" TO WS-MODULE
           DISPLAY "CALL MD10-PRE-TEST"
           CALL WS-MODULE
           END-CALL.

           MOVE "TEST-CAESAR" TO WS-MODULE
           DISPLAY "CALL MD10-CAESAR-ENCRYPTION"
           CALL WS-MODULE USING WS-PLAINTXT, WS-ENCRYPTTXT, 
               WS-CAESAR-SETTING
           END-CALL.

           IF WS-ENCRYPTTXT = WS-EXCEPTED-ENCRYPTTXT
               THEN
                   ADD 1 TO TESTS-PASSES
               ELSE
                   ADD 1 TO TESTS-FAILURES
                   DISPLAY "Test failed"
           END-IF
           EXIT.
      
      * MD01-TEST-LFSR
      *     ADD 1 TO TESTS-RUN
      *     MOVE 0 TO WS-INTERNAL-STATE(1)
      *     MOVE 0 TO WS-INTERNAL-STATE(2)
      *     MOVE 1 TO WS-INTERNAL-STATE(3)
      *     MOVE 0 TO WS-INTERNAL-STATE(4)
      *     MOVE 1 TO WS-INTERNAL-STATE(5)
      *     MOVE 0 TO WS-INTERNAL-STATE(6)
      *     MOVE 0 TO WS-INTERNAL-STATE(7)
      *     MOVE 1 TO WS-INTERNAL-STATE(8)

      *     MOVE 4 TO WS-NB-GEN

      *     MOVE 0 TO WS-XOR-BITS(1)
      *     MOVE 1 TO WS-XOR-BITS(2)
      *     MOVE 0 TO WS-XOR-BITS(3)
      *     MOVE 0 TO WS-XOR-BITS(4)
      *     MOVE 1 TO WS-XOR-BITS(5)
      *     MOVE 0 TO WS-XOR-BITS(6)
      *     MOVE 0 TO WS-XOR-BITS(7)
      *     MOVE 0 TO WS-XOR-BITS(8)

      *     MOVE 1 TO WS-LFSR-STREAM-ENC(1)
      *     MOVE 0 TO WS-LFSR-STREAM-ENC(2)
      *     MOVE 0 TO WS-LFSR-STREAM-ENC(3)
      *     MOVE 1 TO WS-LFSR-STREAM-ENC(4)
           
      *     MOVE "COBENCRYPT" TO WS-PROG
      *     CANCEL  WS-PROG
      *     MOVE "TEST-CAESAR" TO WS-MODULE
      *     DISPLAY "CALL MD11-LFSR"
      *     CALL WS-MODULE USING WS-INTERNAL-STATE, WS-NB-GEN, 
      *         WS-XOR-BITS
      *     END-CALL.

      *     IF WS-ENCRYPTTXT = WS-EXCEPTED-ENCRYPTTXT
      *         THEN
      *             ADD 1 TO TESTS-PASSES
      *         ELSE
      *            ADD 1 TO TESTS-FAILURES
      *             DISPLAY "Test failed"
      *     END-IF
      *     EXIT.
