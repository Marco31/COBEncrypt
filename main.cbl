      ************************************************************

      *  Toolbox for cryptography                             *
      
      *  compile with : cobc -x main.cbl -o build/main           *

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
       01  AREA-CAESAR.
           05  PLAINTXT    PIC X(200).
           05  ENCRYPTTXT  PIC X(200).
           05  CAESAR-SETTING  PIC 9(2).
      * 8-bit LFSR
       01  AREA-LFSR.
           05  LFSR-A-STATIC.
               10  INTERNAL-STATE    PIC 9(1) COMP OCCURS 8 TIMES
                                               INDEXED BY IND-IS.
               10  NB-XOR  PIC 9(1).
               10  I   PIC 9(2).
               10  J   PIC 9(2).
               10  NB-GEN  PIC 9(4).
               10  WS-BIT  PIC 9(1).
           05  LFSR-A-STREAM.
               10  LFSR-STREAM     PIC 9(1) OCCURS 1 TO 9999
                                               DEPENDING ON NB-GEN.
       01 AREA-LFSR-2.
           05  LFSR2-A-STATIC.
               10  IND-XB  PIC 9(2).
           05  LFSR2-A-XOR.
               10  XOR-BITS    PIC 9(1) COMP OCCURS 1 TO 8
                            DEPENDING ON NB-XOR.

       01  AREA-XOR.
           05  XOR-INPUT-1 PIC 9(1) COMP.
           05  XOR-INPUT-2 PIC 9(1) COMP.
           05  XOR-OUTPUT PIC 9(1) COMP.

       PROCEDURE DIVISION.
       MD01 SECTION.

       MD01-INIT-BG.
           PERFORM MD01-INIT-MENU.
           STOP RUN.

       MD01-INIT-MENU.
       DISPLAY "***************Menu***************"
       DISPLAY "1-         Caesar cipher" 
       DISPLAY "2-             LFSR" 
       DISPLAY "3-             EXIT" 
       ACCEPT INPUT-MENU from Name-Input.
       EVALUATE INPUT-MENU
           WHEN 1
               PERFORM MD10-CAESAR
           WHEN 2
               PERFORM MD11-LFSR
           WHEN 4
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

       MD11-LFSR.
      * Initilization of INTERNAL-STATE
       PERFORM     VARYING I
                   FROM 0 BY 1
                   UNTIL I >= 8
               MOVE 0 TO INTERNAL-STATE(I)
       END-PERFORM

       DISPLAY "***************LFSR***************"
       DISPLAY "Number Bit to XOR [1;8]"
       ACCEPT NB-XOR from Name-Input.

       PERFORM     VARYING I
                   FROM 0 BY 1
                   UNTIL I >= NB-XOR

           DISPLAY "Which bits to XOR (Start at 1 from the left)"
           ACCEPT WS-BIT from Name-Input
           MOVE 1 TO XOR-BITS(WS-BIT)
       END-PERFORM

       DISPLAY "How many bits generate?"
       ACCEPT NB-GEN from Name-Input.

       DISPLAY "Input initial state"
       PERFORM     VARYING I
                   FROM 1 BY 1
                   UNTIL I >= 9

           DISPLAY "BIT :" I 
           ACCEPT WS-BIT from Name-Input
           MOVE WS-BIT TO INTERNAL-STATE(I)
       END-PERFORM

       DISPLAY "Here is your Internal State"
       PERFORM     VARYING I
                   FROM 0 BY 1
                   UNTIL I >= 8
           DISPLAY INTERNAL-STATE(I)
       END-PERFORM

       PERFORM MD11-LFRS-STREAM
       DISPLAY "Here is your LFSR Stream"
       PERFORM     VARYING I
                   FROM 0 BY 1
                   UNTIL I >= NB-GEN
               DISPLAY LFSR-STREAM(I)
       END-PERFORM
       EXIT.

       MD11-LFRS-STREAM.
       PERFORM     VARYING I
                   FROM 0 BY 1
                   UNTIL I >= NB-GEN

      * PERFORM XOR
           MOVE 0 TO XOR-INPUT-1
           PERFORM     VARYING J
                       FROM 0 BY 1
                       UNTIL J >= NB-XOR
               MOVE INTERNAL-STATE(XOR-BITS(J)) TO XOR-INPUT-2
               PERFORM MD12-XOR
               MOVE XOR-OUTPUT TO XOR-INPUT-1
           END-PERFORM
           MOVE INTERNAL-STATE(8) TO LFSR-STREAM(I)
           
           PERFORM     VARYING J
                       FROM 8 BY -1
                       UNTIL J = 1
      * We need a "register" to memorize J+1
               MOVE J TO IND-XB
               ADD 1 TO IND-XB
               MOVE INTERNAL-STATE(J) TO INTERNAL-STATE(IND-XB)
           END-PERFORM
           
           MOVE XOR-INPUT-1 TO INTERNAL-STATE(1)
       END-PERFORM
       EXIT.
       
       MD12-XOR.
       EVALUATE XOR-INPUT-1 ALSO XOR-INPUT-2
           WHEN    0 ALSO 0
               MOVE 0 TO XOR-OUTPUT
           WHEN    0 ALSO 1
               MOVE 1 TO XOR-OUTPUT
           WHEN    1 ALSO 0
               MOVE 1 TO XOR-OUTPUT
           WHEN    1 ALSO 1
               MOVE 0 TO XOR-OUTPUT
       END-EVALUATE
       EXIT.
