@openapi.openedge.export FILE(type="REST", executionMode="external", useReturnValue="true", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : item2.p
    Purpose     : Improvement of item.p program, to use HTTP Status Code
    Syntax      :
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Jul 09 12:55:18 BRT 2023
    Notes       : Added:
                    - http status code
                    - finnaly block 
                    - qty of records to the target table (item)
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING AppServer.Utils.TableRowsCounter.

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD ItemNum    LIKE Item.ItemNum 
    FIELD ItemName   LIKE Item.ItemName 
    FIELD Price      LIKE Item.Price 
    INDEX ItemNum IS PRIMARY IS UNIQUE ItemNum.
    
DEFINE DATA-SOURCE srcItem FOR Item.
DEFINE DATASET dsItem FOR ttItem.
DEFINE QUERY qItem FOR ttItem.

DEFINE VARIABLE tableRowsCounter AS AppServer.Utils.TableRowsCounter NO-UNDO.
DEFINE VARIABLE cStatusCode      AS CHARACTER NO-UNDO INIT "400".

DEFINE INPUT PARAM              iQty       AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM       pRowId     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM             iTotalRows AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAM             cStatus    AS CHARACTER NO-UNDO INIT "Unknown error!".
DEFINE OUTPUT PARAM DATASET FOR dsItem.

/* ***************************  Main Block  *************************** */

/* Clear dataset */
DATASET dsItem:EMPTY-DATASET ().

/* Initial validation */
IF iQty = ? THEN DO:
    ASSIGN
        cStatus = "Quantity parameter must be informed!".
    RETURN cStatusCode. // bad request
END.
ELSE IF iQty <= 0 THEN DO:
    ASSIGN
        cStatus = "Quantity parameter must be greater than 0!".
    RETURN cStatusCode. // bad request
END.
ELSE IF iQty > 200 THEN
DO:
    ASSIGN
        cStatus = "Not alowed! Maximum of 200 records per request!".
    RETURN cStatusCode. // bad request
END.

/* Attaches the srcItem datasource to the ttItem buffer */
BUFFER ttItem:ATTACH-DATA-SOURCE(DATA-SOURCE srcItem:HANDLE).

/* Set the RESTART-ROWID using the pRowId passed */
IF LENGTH(pRowId) > 0 THEN
  DATA-SOURCE srcItem:RESTART-ROWID = TO-ROWID(pRowId).

/* Set the batch size */
BUFFER ttItem:BATCH-SIZE = iQty.

/* Empties the table before the FILL operation begins */
BUFFER ttItem:FILL-MODE = "EMPTY".

/* Fill dataset */
DATASET dsItem:FILL().

/* Get the NEXT-ROWID of DATA-SOURCE to return */
pRowId = STRING(DATA-SOURCE srcItem:NEXT-ROWID).

/* Detach the datasource */
BUFFER ttItem:DETACH-DATA-SOURCE().

ASSIGN
    tableRowsCounter = NEW TableRowsCounter()
    iTotalRows = tableRowsCounter:CountRows('sports2020.item')
    cStatus = "Success!"
    cStatusCode = "200".

CATCH err AS Progress.Lang.Error :
    DEFINE VARIABLE iMessage AS INTEGER NO-UNDO.
    ASSIGN 
        cStatusCode = "500".
        
    DO WHILE iMessage < err:NumMessages:
        ASSIGN 
            cStatus = SUBSTITUTE ("&1~n&2", cStatus, err:GetMessage(iMessage))
            iMessage = iMessage + 1.
    END.
    IF err:CallStack <> ? THEN DO:
        ASSIGN 
            cStatus = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cStatus, err:CallStack).
    END.
END CATCH.
FINALLY:
    RETURN cStatusCode.    
END FINALLY.