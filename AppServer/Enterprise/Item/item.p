@openapi.openedge.export FILE(type="REST", executionMode="external", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : item.p
    Purpose     : Expose the items with pagination in a REST API - PASOE
                  Limit of 200 records per request
    Syntax      :
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Jun 18 12:26:23 BRT 2023
    Notes       : Based on 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD ItemNum    LIKE Item.ItemNum 
    FIELD ItemName   LIKE Item.ItemName 
    FIELD Price      LIKE Item.Price 
    INDEX ItemNum IS PRIMARY IS UNIQUE ItemNum.
    
DEFINE DATA-SOURCE srcItem FOR Item.
DEFINE DATASET dsItem FOR ttItem.
DEFINE QUERY qItem FOR ttItem.

DEFINE INPUT PARAM              iQty    AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM       pRowId  AS CHARACTER     NO-UNDO.
DEFINE OUTPUT PARAM             cStatus AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM DATASET FOR dsItem.

/* ***************************  Main Block  *************************** */

/* Clear dataset */
DATASET dsItem:EMPTY-DATASET ().

/* Initial validation */
IF iQty = 0 THEN RETURN.
IF iQty > 200 THEN
DO:
    ASSIGN
        cStatus = "Not alowed! Maximum of 200 records per request!".
    RETURN.
END.

ASSIGN
    cStatus = "Unknown error!".

/* Load batch based on ROWID passed */
BUFFER ttItem:ATTACH-DATA-SOURCE(DATA-SOURCE srcItem:HANDLE).

/* Set the RESTART-ROWID using the rowid passed back from server. */
IF LENGTH(pRowId) > 0 THEN
  DATA-SOURCE srcItem:RESTART-ROWID = TO-ROWID(pRowId).

/* Configura a quantidade de registros */
BUFFER ttItem:BATCH-SIZE = iQty.

/* Set FILL-MODE to EMPTY if this code is running persistently.  */
IF THIS-PROCEDURE:PERSISTENT THEN
  BUFFER ttItem:FILL-MODE = "EMPTY".

/* Fill dataset using above configurations. */
DATASET dsItem:FILL().

/* Get the NEXT-ROWID of DATA-SOURCE to return */
pRowId = STRING(DATA-SOURCE srcItem:NEXT-ROWID).

BUFFER ttItem:DETACH-DATA-SOURCE().

ASSIGN
    cStatus = "Success!".

CATCH err AS Progress.Lang.Error :
    DEFINE VARIABLE iMessage AS INTEGER NO-UNDO.
    
    DO WHILE iMessage LT err:NumMessages:
        cStatus = SUBSTITUTE ("&1~n&2", cStatus, err:GetMessage(iMessage)).
        iMessage = iMessage + 1.
    END.
    IF err:CallStack <> ? THEN DO:
        cStatus = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cStatus, err:CallStack).
    END.
END CATCH.