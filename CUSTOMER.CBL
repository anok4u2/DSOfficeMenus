      $SET ans85
       IDENTIFICATION DIVISION.
       PROGRAM-ID. customer.


       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT customer-file ASSIGN "cust.ism"
           ORGANIZATION IS INDEXED
           RECORD KEY IS file-c-code
           ACCESS IS DYNAMIC
           lock MODE IS AUTOMATIC.


       DATA DIVISION.

       FILE SECTION.
       FD  customer-file.
       01  customer-record.
           03  file-c-code             PIC X(5).
           03  file-c-name             PIC X(15).
           03  file-c-addr1            PIC X(15).
           03  file-c-addr2            PIC X(15).
           03  file-c-addr3            PIC X(15).
           03  file-c-addr4            PIC X(15).
           03  file-c-limit            PIC 9(4) COMP.
           03  file-c-area             PIC X.
           03  file-c-order.
             78  no-of-orders              VALUE 10.
               05  file-c-order-entry OCCURS no-of-orders.
                   07  file-ord-no     PIC 9(6).
                   07  file-ord-date   PIC 9(6).
                   07  file-ord-val    PIC 9(4)V99 COMP.
                   07  file-pay-val    PIC 9(4)V99 COMP.

       WORKING-STORAGE SECTION.

           COPY "ds-cntrl.v1".
           COPY "customer.cpb".

       78  refresh-text-and-data-proc      VALUE 255.
       78  dialog-system                   VALUE "dsgrun".

       77  array-ind                   PIC 9(4) COMP.
       77  display-error-no            PIC 9(4).


       PROCEDURE DIVISION.

       Main-Process SECTION.

           DISPLAY ERASE
           PERFORM Program-Initialize
           PERFORM Program-Body UNTIL customer-exit-flg-true
           PERFORM Program-Terminate.

      *---------------------------------------------------------------*

       Program-Initialize SECTION.

           INITIALIZE ds-control-block
           INITIALIZE customer-data-block
           MOVE customer-data-block-version-no
                                    TO ds-data-block-version-no
           MOVE customer-version-no TO ds-version-no
           OPEN I-O customer-file
           PERFORM Load-Screenset.

      *---------------------------------------------------------------*

       Program-Body SECTION.

           IF ds-exit-field-true
               PERFORM Derivations
               PERFORM Set-Up-For-Refresh-Screen
           END-IF
           EVALUATE TRUE
               WHEN customer-del-flg-true
                   PERFORM Delete-Record
               WHEN customer-load-flg-true
                   PERFORM Load-Record
               WHEN customer-save-flg-true
                   PERFORM Save-Record
               WHEN customer-clr-flg-true
                   PERFORM Clear-Record
           END-EVALUATE
           PERFORM Clear-Flags
           PERFORM Call-Dialog-System.

      *---------------------------------------------------------------*

       Program-Terminate SECTION.

           CLOSE customer-file
           STOP RUN.

      *--------------------------------------------------------------*

       Delete-Record SECTION.

           MOVE customer-c-code TO file-c-code
           DELETE customer-file
           PERFORM Clear-Record.

      *---------------------------------------------------------------*

       Load-Record SECTION.

           INITIALIZE customer-record
           MOVE customer-c-code TO file-c-code
           IF file-c-code NOT = SPACES
               READ customer-file
                   INVALID KEY
                       INITIALIZE customer-data-block
                       MOVE file-c-code TO customer-c-code
                   NOT INVALID KEY
                       PERFORM Fill-Screen-From-Record
                       PERFORM Derivations
               END-READ
           ELSE
               INITIALIZE customer-data-block
           END-IF
           PERFORM Set-Up-For-Refresh-Screen.

      *---------------------------------------------------------------*

       Save-Record SECTION.

           PERFORM Fill-Record-From-Screen
           REWRITE customer-record
               INVALID KEY
                   WRITE customer-record
               END-WRITE
           END-REWRITE.

      *---------------------------------------------------------------*

       Clear-Flags SECTION.

           INITIALIZE customer-group-002.

      *---------------------------------------------------------------*

       Clear-Record SECTION.

           INITIALIZE customer-record
           INITIALIZE customer-data-block
           PERFORM Set-Up-For-Refresh-Screen.

      *---------------------------------------------------------------*

       Fill-Record-From-Screen SECTION.

           MOVE customer-c-code  TO file-c-code
           MOVE customer-c-name  TO file-c-name
           MOVE customer-c-addr1 TO file-c-addr1
           MOVE customer-c-addr2 TO file-c-addr2
           MOVE customer-c-addr3 TO file-c-addr3
           MOVE customer-c-addr4 TO file-c-addr4
           MOVE customer-c-limit TO file-c-limit
           MOVE customer-c-area  TO file-c-area
           PERFORM VARYING Array-Ind FROM 1 BY 1
                           UNTIL array-ind > no-of-orders
               MOVE customer-ord-no(array-ind) TO file-ord-no(array-ind)
               MOVE customer-ord-date(array-ind) TO
                                           file-ord-date(array-ind)
               MOVE customer-ord-val(array-ind) TO
                                           file-ord-val(array-ind)
               MOVE customer-pay-val(array-ind) TO
                                           file-pay-val(array-ind)
           END-PERFORM.

      *---------------------------------------------------------------*

       Fill-Screen-From-Record SECTION.

           MOVE file-c-code  TO customer-c-code
           MOVE file-c-name  TO customer-c-name
           MOVE file-c-addr1 TO customer-c-addr1
           MOVE file-c-addr2 TO customer-c-addr2
           MOVE file-c-addr3 TO customer-c-addr3
           MOVE file-c-addr4 TO customer-c-addr4
           MOVE file-c-limit TO customer-c-limit
           MOVE file-c-area  TO customer-c-area
           PERFORM VARYING Array-Ind FROM 1 BY 1
                                          UNTIL array-ind > no-of-orders
               MOVE file-ord-no(array-ind) TO customer-ord-no(array-ind)
               MOVE file-ord-date(array-ind) TO
                                           customer-ord-date(array-ind)
               MOVE file-ord-val(array-ind) TO
                                           customer-ord-val(array-ind)
               MOVE file-pay-val(array-ind) TO
                                           customer-pay-val(array-ind)
           END-PERFORM.

      *---------------------------------------------------------------*

       Set-Up-For-Refresh-Screen SECTION.

           MOVE refresh-text-and-data-proc TO ds-proc-no.

      *---------------------------------------------------------------*

       Derivations SECTION.

           MOVE 0 TO customer-c-bal
           PERFORM VARYING Array-Ind
                              FROM 1 BY 1 UNTIL array-ind > no-of-orders
               COMPUTE customer-ord-bal(array-ind) =
                     customer-ord-val(array-ind) -
                                           customer-pay-val(array-ind)
               ADD customer-ord-bal(array-ind) TO customer-c-bal
           END-PERFORM.

      *---------------------------------------------------------------*

       Load-Screenset SECTION.

           MOVE ds-new-set TO ds-control
           MOVE "customer" TO ds-set-name
           PERFORM Call-Dialog-System.

      *---------------------------------------------------------------*

       Call-Dialog-System SECTION.

           CALL dialog-system USING ds-control-block,
                                    customer-data-block
           IF NOT ds-no-error
               MOVE ds-system-error-no TO display-error-no
               DISPLAY "DS ERROR NO:   " display-error-no
               PERFORM Program-Terminate
           END-IF.
