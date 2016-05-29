;;; as
;;; 
;;; an assembler for the DCPU-16 assembly language
;;; 
;;; accepts a file of no more than 512 words indicating the sectors on disk that hold a particular file.
;;; then reads those sectors in the order given, treating them as assembler source
;;; then assembles said source to produce a DCPU-16 binary.
;;; the supported binary format is an image that can be loaded directly from a disk by a bootloader:
;;; it contains an 'initial sector' which is written to the first part of the disk and that loads the rest
;;; of the binary.
;;;
;;; Copyright (c) 2014 Miles Rout
;;; SEE THE LICENSE FILE IN THE ROOT DIRECTORY
	
;;; I suppose I will end up hand-assembling this for the proper 'bootstrapping' feel. eugh.

;;; LINKED LIST OF TOKENS
;;; 
;;; structure:
;;; +0: tok_type
;;; +1: next
;;; +2: prev
;;; +3: tok_data - string for TOK_STRING and TOK_IDENTIFIER, number for TOK_NUMBER, lstring for TOK_LSTRING
;;;
;;; initial: prev=0
;;; final:   next=0

;;; hardcoded into a particular place in memory at this stage
;;; first item is always empty (for code clarity and laziness, TODO: fix this)
list_create:
	SET [0x4000], 0
	SET [0x4001], 0
	SET [0x4002], 0
	SET [0x4003], 0
	SET A, 0x4000

;;; A:      address of list
;;; [SP]:   <return address>
;;; [SP+1]: tok_type
;;; [SP+2]: tok_data
list_append:
	SET A, 0x4000		; TEMPORARY, until I can be bothered having more than one list
list_append_loop:	
	IFE [A+1], 0		; IF  (node->next != NULL)
	    SET PC, list_append_found
	SET A, [A+1]
	SET PC, list_append_loop
list_append_found:
	SET B, A
	SET A, [A+1]
	SET [A], PICK 1
	SET [A+1], 0
	SET [A+2], B		; new->prev = old
	SET [A+3], PICK 2
	SET PC, POP

;;; OPCODES
	
;;; finds the basic opcode corresponding to a certain string
;;; A is the address of the string 
check_basic_opcode:
	SET B, basic_opcodes
check_basic_opcode_loop:	
	IFE [A], [B]
	    IFE [A+1], [B+1]
	        IFE [A+2], [B+2]
	            SET PC, check_basic_opcode_found
	ADD B, 4
	IFL B, basic_opcodes_end
	    SET PC, check_basic_opcode_loop
	SET A, 0		; it wasn't found in the basic opcodes
	SET PC, POP
check_basic_opcode_found:
	SET A, [B+3]
	SET PC, POP

	
;;; finds the special opcode corresponding to a certain string
;;; A is the address of the string 
check_special_opcode:
	SET B, special_opcodes
check_special_opcode_loop:
	IFE [A], [B]
	    IFE [A+1], [B+1]
	        IFE [A+2], [B+2]
	            SET PC, check_special_opcode_found
	ADD B, 4
	IFL B, special_opcodes_end
	    SET PC, check_special_opcode_loop
	SET A, 0		; it wasn't found in the special opcodes
	SET PC, POP
check_special_opcode_found:
	SET A, [B+3]
	SET PC, POP
	
lookahead:
	SET A, [Z]
	SET PC, POP
	
;;; STATE MACHINE FOR LEXICAL ANALYSIS

;;; TOKEN TYPES:
;;;  0: invalid token
;;;  1: identifier
;;;  2: lstring (length-prefixed string)
;;;  3: cstring (null-terminated C-style string)
;;;  4: colon
;;;  5: lbrack
;;;  6: rbrack
;;;  7: bin_number
;;;  8: oct_number
;;;  9: dec_number
;;; 10: hex_number
;;; 11: pstring (packed string)

	SET Z, 0		; Z: stream pointer
	SET J, 0x8000		; J: string pointer
state_0:	
	JSR lookahead
	;; stay in same state
	IFE A, 0x5B		; OR   A == '['
	    SET PC, lex_tr_0_to_0
	IFE A, 0x5D		; OR   A == ']'
	    SET PC, lex_tr_0_to_0
	IFE A, 0x3A		; IF   A == ':'
	    SET PC, lex_tr_0_to_0

	;; go to string recognition states
	IFE A, 0x22		; "
	    SET PC, lex_tr_0_to_1
	IFE A, 0x4C		; L
	    SET PC, lex_tr_0_to_2

	;; go to identifier recognition states
	;; note that this doesn't actually include L despite
	;; appearances, because L is dealt with above.
	IFG A, 0x40		; IF  (A >= 'A' AND A <= 'Z')
	    IFL A, 0x5B  	; 
	        SET PC, lex_tr_0_to_4
	IFG A, 0x60		; OR  (A >= 'a' AND A <= 'z')
            IFL A, 0x7B
	        SET PC, lex_tr_0_to_4
	IFE A, 0x5F		; OR   A == '_'
	    SET PC, lex_tr_0_to_4

	;; go to non-decimal number recognition states
	IFE A, 0x30		; IF   A == '0'
	    SET PC, lex_tr_0_to_5

	;; go to decimal number recognition states
	IFG A, 0x30		; IF  (A >= '1' AND A <= '9')
	    IFL A, 0x3A
	        SET PC, lex_tr_0_to_9
state_1:
	JSR lookahead
	;; if this is a ", end our string recognition
	IFE A, 0x22		; "
	    SET PC, lex_tr_1_to_0
	;; otherwise continue recognising the string
	SET PC, lex_tr_1_to_1
state_2:
	JSR lookahead
	;; stay in same state
	IFE A, 0x20		; space
	    SET PC, lex_tr_2_to_2
	IFE A, 0x09		; horizontal tab
	    SET PC, lex_tr_2_to_2
	IFE A, 0x0A		; line feed (new line)
	    SET PC, lex_tr_2_to_2
	IFE A, 0x0B		; vertical tab
	    SET PC, lex_tr_2_to_2
	IFE A, 0x0C		; form feed (new page)
	    SET PC, lex_tr_2_to_2
	IFE A, 0x0D		; carriage return
	    SET PC, lex_tr_2_to_2

	;; quotation mark: start reading a string
	IFE A, 0x22		; "
	    SET PC, lex_tr_2_to_3
	
	;; oops, that L is actually the start of an
	;; identifier, not an lstring. better head back over
	;; to identifier recognition
	IFG A, 0x40		; IF  (A >= 'A' AND A <= 'Z')
	    IFL A, 0x5B  	; 
	        SET PC, lex_tr_2_to_4
	IFG A, 0x60		; OR  (A >= 'a' AND A <= 'z')
            IFL A, 0x7B
	        SET PC, lex_tr_2_to_4
	IFG A, 0x2F		; OR  (A >= '0' AND A <= '9')
            IFL A, 0x3A
	        SET PC, lex_tr_2_to_4
	IFE A, 0x5F		; OR   A == '_'
	    SET PC, lex_tr_2_to_4
state_3:
	JSR lookahead
	;; if this is a ", end our string recognition
	IFE A, 0x22		; "
	    SET PC, lex_tr_3_to_0
	;; otherwise continue recognising the string
	SET PC, lex_tr_3_to_3
state_4:
	JSR lookahead
	;; continue reading identifier
	IFG A, 0x40		; IF  (A >= 'A' AND A <= 'Z')
	    IFL A, 0x5B
	        SET PC, lex_tr_4_to_4
	IFG A, 0x60		; OR  (A >= 'a' AND A <= 'z')
            IFL A, 0x7B
	        SET PC, lex_tr_4_to_4
	IFG A, 0x2F		; OR  (A >= '0' AND A <= '9')
            IFL A, 0x3A
	        SET PC, lex_tr_4_to_4
	IFE A, 0x5F		; OR   A == '_'
	    SET PC, lex_tr_4_to_4

	;; otherwise do a non-consuming production transition back to zero
	SET PC, lex_tr_4_to_0
state_5:
	JSR lookahead
	;; octal number
	IFG A, 0x2F		; OR  (A >= '0' AND A <= '7')
            IFL A, 0x38
	        SET PC, lex_tr_5_to_6
	;; hexadecimal number
	IFE A, 0x78
	    SET PC, lex_tr_5_to_7
	;; binary number
	IFE A, 0x62
	    SET PC, lex_tr_5_to_8

	;; otherwise this is just a single zero, so we should produce and go back to zero.
	SET PC, lex_tr_5_to_0
state_6:
	JSR lookahead
	;; continue reading number
	IFG A, 0x2F		; OR  (A >= '0' AND A <= '7')
            IFL A, 0x38
	        SET PC, lex_tr_6_to_6
	
	;; otherwise do a non-consuming production transition back to zero
	SET PC, lex_tr_6_to_0	
state_7:
	JSR lookahead
	;; continue reading number
	IFG A, 0x40		; IF  (A >= 'A' AND A <= 'F')
	    IFL A, 46
	        SET PC, lex_tr_7_to_7
	IFG A, 0x60		; OR  (A >= 'a' AND A <= 'f')
            IFL A, 0x66
	        SET PC, lex_tr_7_to_7
	IFG A, 0x2F		; OR  (A >= '0' AND A <= '9')
            IFL A, 0x3A
	        SET PC, lex_tr_7_to_7
	
	;; otherwise do a non-consuming production transition back to zero
	SET PC, lex_tr_6_to_0	
state_8:
	JSR lookahead
	IFE A, 0x30		; '0'
	    SET PC, lex_tr_8_to_8
	IFE A, 0x31		; '1'
	    SET PC, lex_tr_8_to_8

	;; otherwise do a non-consuming production transition back to zero
	SET PC, lex_tr_8_to_0
state_9:
	JSR lookahead
	IFG A, 0x2F		; IF  (A >= '0' AND A <= '9')
            IFL A, 0x3A
	        SET PC, lex_tr_9_to_9

	;; otherwise do a non-consuming production transition back to zero
	SET PC, lex_tr_9_to_0

;;; Strings and other such data will be stored in memory at a fixed location for now, until I can come up
;;; with some more compelling memory allocation code.
;;; At the beginning set up a pointer in a register to a fixed location in memory. 0x8000 might be appropriate,
;;; because it is nowhere near 0x4000 (where our token list is) and also won't interfere with the stack.
;;; The register is set above the states along with the character stream pointer, and is the register J, because
;;; of the *very* useful STI instruction. Y additionally always points at the start of the most recent string.
	
;;; TRANSITIONS
lex_tr_0_to_0:
	SET PUSH, A		; the token data is the char literal
	IFE A, 0x3A		; :
	    SET PUSH, 4
	IFE A, 0x5B		; [
	    SET PUSH, 5
	IFE A, 0x5D		; ]
	    SET PUSH, 6
	JSR list_append		; append pushed token
	ADD SP, 1		; get rid of rubbish values
	ADD Z, 1		; move to next character
	SET PC, state_0		; move to relevant state
lex_tr_0_to_1:
	SET Y, J		; save the start of the string being built
	;; note that we don't want to push the first character yet, because we don't yet have it: we only have a quotation mark.
	ADD Z, 1		; move to next character
	SET PC, state_1		; move to relevant state
lex_tr_0_to_2:
	STI Y, J		; save the start of the string being built and save a space for it in the storage space
        ;; note that Y points to this space (which has a garbage value), not to the first character
	;; also note that we don't want to push the first character yet, because we don't have it: we only have
	;; L, which could be the start of an lstring or the first character of an identifier.
	ADD Z, 1		; move to next character
	SET PC, state_2		; move to relevant state
lex_tr_0_to_4:
	SET Y, J	        ; save the start of the string being built
	STI [J], A		; append the first character to the string
	ADD Z, 1		; move to next character
	SET PC, state_4		; move to relevant state
lex_tr_0_to_5:
	SET Y, J		; save the start of the string being built
	STI [J], A		; append the first character to the string
	ADD Z, 1		; move to next character
	SET PC, state_5		; move to relevant state
lex_tr_0_to_9:
	;; build a string of digit characters. these will be turned into the actual NUMBER later.
	SET Y, J		; save the start of the string being built
	STI [J], A		; append the first character to the string
	ADD Z, 1		; move to next character
	SET PC, state_9		; move to relevant state
lex_tr_1_to_0:
	;; currently Y is pointing at the start of the string.
        ;; J is pointing one past the end of the string.
	;; the string hasn't been null-terminated.
	STI [J], 0		; null-terminate the string
	SET PUSH, Y		; push tok_data
	SET PUSH, 3		; push tok_type: 3 (cstring)
	JSR list_append
	ADD SP, 2		; clear rubbish values from stack
	ADD Z, 1		; move to next character
	SET PC, state_0		; move to relevant state
lex_tr_1_to_1:
	STI [J], A		; append the next character to the string
	ADD Z, 1		; move to next character
	SET PC, state_1		; move to relevant state
lex_tr_2_to_2:
	;; ignore whitespace
	ADD Z, 1		; move to next character
	SET PC, state_2		; move to relevant state
lex_tr_2_to_3:
	;; start reading the string as usual
	SET [Y], 0		; initialise the string length to zero.
	;; we still don't want to add a character, but this definitely is an lstring.
	ADD Z, 1		; move to next character
	SET PC, state_3		; move to relevant state
lex_tr_2_to_4:
	;; oops, this is actually an identifier. we're a bit behind.
	;; we need to add the L character in and the new character.
	SET [Y], 0x4C		; 'L' - put it where we would otherwise have put the lstring's length
	STI [J], A		; append the next character to the string
	ADD Z, 1		; move to next character
	SET PC, state_4		; move to relevant state
lex_tr_3_to_0:
	;; currently Y is pointing at the length of the string, J is pointing one past the end.
	;; the string hasn't been and doesn't need to be null-terminated.
	SET PUSH, Y		; push tok_data
	SET PUSH, 2		; push tok_type: 2 (lstring)
	JSR list_append
	ADD SP, 2		; clear rubbish values from stack
	ADD Z, 1		; move to next character
	SET PC, state_0		; move to relevant state
lex_tr_3_to_3:
	ADD [Y], 1		; increment string length
	STI [J], A		; append character to string
	ADD Z, 1		; move to next character
	SET PC, state_3		; move to relevant state
lex_tr_4_to_0:
	;; currently Y is pointing at the first character of the string. 
        ;; J is pointing one past the end.
	;; the string needs to be null-terminated.
	STI [J], 0		; null-terminate the string.
	SET PUSH, Y		; push tok_data
	SET PUSH, 1		; push tok_type: 1 (identifier)
	JSR list_append
	ADD SP, 2		; clear rubbish values from the stack
	ADD Z, 1		; move to next character
	SET PC, state_0		; move to relevant state
lex_tr_4_to_4:
	STI [J], A		; append the next character to the string
	ADD Z, 1		; move to next character
	SET PC, state_4		; move to relevant state
lex_tr_5_to_0:
	;; currently Y is pointing at the first character of the string. 
        ;; J is pointing one past the end of the string.
	;; the string needs to be null-terminated.
	STI [J], 0		; null-terminate the string.
	SET PUSH, Y		; push tok_data
	SET PUSH, 8		; push tok_type: 8 (oct_number)
	JSR list_append
	ADD SP, 2		; clear rubbish values from stack
	ADD Z, 1		; move to next character
	SET PC, state_0		; move to relevant state
lex_tr_5_to_6:
	STI [J], A		; append the character to the string
	ADD Z, 1		; move to next character
	SET PC, state_6		; move to relevant state
lex_tr_5_to_7:
	STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_7         ; move to relevant state
lex_tr_5_to_8:
        STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_8         ; move to relevant state
lex_tr_6_to_0:
        ;; currently Y is pointing at the first character of the string.
        ;; the string needs to be null-terminated.
        STI [J], 0              ; null-terminate the string
        SET PUSH, Y             ; push tok_data
        SET PUSH, 8             ; push tok_type: 8 (oct_number)
        JSR list_append
        ADD SP, 2               ; clear rubbish values from stack
        ADD Z, 1                ; move to next character
        SET PC, state_0         ; move to relevant state
lex_tr_6_to_6:
        STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_6         ; move to relevant state
lex_tr_7_to_0:
        ;; currently Y is pointing at the first character of the string.
        ;; the string needs to be null-terminated.
        STI [J], 0              ; null-terminate the string
        SET PUSH, Y             ; push tok_data
        SET PUSH, 10            ; push tok_type: 10 (hex_number)
        JSR list_append
        ADD SP, 2               ; clear rubbish values from stack
        ADD Z, 1                ; move to next character
        SET PC, state_0         ; move to relevant state
lex_tr_7_to_7:
        STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_7         ; move to relevant state
lex_tr_8_to_0:
        ;; currently Y is pointing at the first character of the string
        ;; the string needs to be null-terminated.
        STI [J], 0              ; null-terminate the string
        SET PUSH, Y             ; push tok_data
        SET PUSH, 7             ; push tok_type: 7 (bin_number)
        JSR list_append
        ADD SP, 2               ; clear rubbish values from stack
        ADD Z, 1                ; move to next character
        SET PC, state_0         ; move to relevant state
lex_tr_8_to_8:
        STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_8         ; move to relevant state
lex_tr_9_to_0:
        ;; currently Y is pointing at the first character of the string
        ;; the string needs to be null-terminated.
        STI [J], 0              ; null-terminate the string
        SET PUSH, Y             ; push tok_data
        SET PUSH, 9             ; push tok_type: 9 (dec_number)
        JSR list_append
        ADD SP, 2               ; clear rubbish values from stack
        ADD Z, 1                ; move to next character
        SET PC, state_0         ; move to relevant state
lex_tr_9_to_9:	
        STI [J], A              ; append the character to the string
        ADD Z, 1                ; move to next character
        SET PC, state_9         ; move to relevant state
	
;;; BASIC OPCODES
basic_opcodes:
	DAT 0x53, 0x45, 0x54, 0x01	;SET  1h
	DAT 0x41, 0x44, 0x44, 0x02	;ADD  2h
	DAT 0x53, 0x55, 0x42, 0x03	;SUB  3h
	DAT 0x4D, 0x55, 0x4C, 0x04	;MUL  4h
	DAT 0x4D, 0x4C, 0x49, 0x05	;MLI  5h
	DAT 0x44, 0x49, 0x56, 0x06	;DIV  6h
	DAT 0x44, 0x56, 0x49, 0x07	;DVI  7h
	DAT 0x4D, 0x4F, 0x44, 0x08	;MOD  8h
	DAT 0x4D, 0x44, 0x49, 0x09	;MDI  9h
	DAT 0x41, 0x4E, 0x44, 0x0A	;AND  Ah
	DAT 0x42, 0x4F, 0x52, 0x0B	;BOR  Bh
	DAT 0x58, 0x4F, 0x52, 0x0C	;XOR  Ch
	DAT 0x53, 0x48, 0x52, 0x0D	;SHR  Dh
	DAT 0x41, 0x53, 0x52, 0x0E	;ASR  Eh
	DAT 0x53, 0x48, 0x4C, 0x0F	;SHL  Fh
	DAT 0x49, 0x46, 0x42, 0x10	;IFB 10h
	DAT 0x49, 0x46, 0x43, 0x11	;IFC 11h
	DAT 0x49, 0x46, 0x45, 0x12	;IFE 12h
	DAT 0x49, 0x46, 0x4E, 0x13	;IFN 13h
	DAT 0x49, 0x46, 0x47, 0x14	;IFG 14h
	DAT 0x49, 0x46, 0x41, 0x15	;IFA 15h
	DAT 0x49, 0x46, 0x4C, 0x16	;IFL 16h
	DAT 0x49, 0x46, 0x55, 0x17	;IFU 17h
	;; empty
	;; empty
	DAT 0x41, 0x44, 0x52, 0x1A      ;ADX 1Ah
	DAT 0x53, 0x42, 0x52, 0x1B	;SBX 1Bh
	;; empty
	;; empty
	DAT 0x53, 0x54, 0x49, 0x1E	;STI 1Eh
	DAT 0x53, 0x54, 0x44, 0x1F	;STD 1Fh
basic_opcodes_end:
	DAT 0x00, 0x00, 0x00, 0x00 	;sentinel

;;; SPECIAL OPCODES
special_opcodes:	
	DAT 0x4A, 0x53, 0x52, 0x01 	;JSR  1h
	;; empty
	;; empty
	;; empty
	;; empty
	;; empty	
	;; empty
	DAT 0x49, 0x4E, 0x54, 0x08 	;INT  8h
	DAT 0x49, 0x41, 0x47, 0x09	;IAG  9h
	DAT 0x49, 0x41, 0x53, 0x0A	;IAS  Ah
	DAT 0x52, 0x46, 0x49, 0x0B	;RFI  Bh
	DAT 0x49, 0x41, 0x51, 0x0C	;IAQ  Ch
	;; empty
	;; empty
	;; empty
	DAT 0x48, 0x57, 0x4E, 0x10 	;HWN 10h
	DAT 0x48, 0x57, 0x51, 0x11	;HWQ 11h
	DAT 0x48, 0x57, 0x49, 0x12	;HWI 12h
special_opcodes_end:
	DAT 0x00, 0x00, 0x00, 0x00 	;sentinel
