      ************************************************************

      *  Application to Encrypt string                             *
      
      *  compile with : cobc -x main.cbl
      *----------------------------------------------------------*

      ************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBENCRYPT.
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
       01  INPUT-MENU       PIC 9(1).
       01  ZONE-CAESAR.
           05  PLAINTXT    PIC X(200).
           05  ENCRYPTTXT  PIC X(200).
           05  CAESAR-SETTING  PIC 9(2).

       PROCEDURE DIVISION.
       MD01 SECTION.

       MD01-INIT-BG.
      *     DISPLAY "MD01-INIT-BG"
           PERFORM MD01-INIT-MENU.
           STOP RUN.

       MD01-INIT-MENU.
       DISPLAY "***************Menu***************"
       DISPLAY "1-          Caesar cipher" 
       DISPLAY "2-          EXIT" 
       ACCEPT INPUT-MENU from Name-Input.
      * DISPLAY INPUT-MENU
       EVALUATE INPUT-MENU
           WHEN 1
               PERFORM MD10-CAESAR
           WHEN 2
               DISPLAY "EXITING..."
           WHEN OTHER
               DISPLAY "UNKNOWN INPUT"
       END-EVALUATE
       EXIT.

       MD10-CAESAR.
       DISPLAY "***************CAESAR***************"
       DISPLAY "Input Ceaser setting"
       ACCEPT CAESAR-SETTING from Name-Input.
       DISPLAY "Input string"
       ACCEPT PLAINTXT from Name-Input.
       DISPLAY CAESAR-SETTING
       DISPLAY PLAINTXT
       PERFORM MD10-CAESAR-ENCRYPTION
       DISPLAY ENCRYPTTXT        
       EXIT.

       MD10-CAESAR-ENCRYPTION.
       MOVE PLAINTXT TO ENCRYPTTXT
       INSPECT ENCRYPTTXT
           CONVERTING "abcdefghijklmnopqrstuvwxyz"
           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       EVALUATE CAESAR-SETTING
           WHEN 0
               MOVE PLAINTXT TO ENCRYPTTXT
           WHEN 1
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "BCDEFGHIJKLMNOPQRSTUVWXYZA"
           WHEN 2
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "CDEFGHIJKLMNOPQRSTUVWXYZAB"
           WHEN 3
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "DEFGHIJKLMNOPQRSTUVWXYZABC"
           WHEN 4
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "EFGHIJKLMNOPQRSTUVWXYZABCD"
           WHEN 5
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "FGHIJKLMNOPQRSTUVWXYZABCDE"
           WHEN 6
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "GHIJKLMNOPQRSTUVWXYZABCDEF"
           WHEN 7
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "HIJKLMNOPQRSTUVWXYZABCDEFG"
           WHEN 8
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "IJKLMNOPQRSTUVWXYZABCDEFGH"
           WHEN 9
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "JKLMNOPQRSTUVWXYZABCDEFGHI"
           WHEN 10
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "KLMNOPQRSTUVWXYZABCDEFGHIJ"
           WHEN 11
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "LMNOPQRSTUVWXYZABCDEFGHIJK"
           WHEN 12
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "MNOPQRSTUVWXYZABCDEFGHIJKL"
           WHEN 13
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "NOPQRSTUVWXYZABCDEFGHIJKLM"
           WHEN 14
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "OPQRSTUVWXYZABCDEFGHIJKLMN"
           WHEN 15
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "PQRSTUVWXYZABCDEFGHIJKLMNO"
           WHEN 16
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "QRSTUVWXYZABCDEFGHIJKLMNOP"
           WHEN 17
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "RSTUVWXYZABCDEFGHIJKLMNOPQ"
           WHEN 18
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "STUVWXYZABCDEFGHIJKLMNOPQR"
           WHEN 19
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "TUVWXYZABCDEFGHIJKLMNOPQRS"
           WHEN 20
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "UVWXYZABCDEFGHIJKLMNOPQRST"
           WHEN 21
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "VWXYZABCDEFGHIJKLMNOPQRSTU"
           WHEN 22
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "WXYZABCDEFGHIJKLMNOPQRSTUV"
           WHEN 23
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "XYZABCDEFGHIJKLMNOPQRSTUVW"
           WHEN 24
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "YZABCDEFGHIJKLMNOPQRSTUVWX"
           WHEN 25
               INSPECT ENCRYPTTXT
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "ZABCDEFGHIJKLMNOPQRSTUVWXY"
           WHEN OTHER
               MOVE PLAINTXT TO ENCRYPTTXT
       EXIT.