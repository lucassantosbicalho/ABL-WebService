 
 /*------------------------------------------------------------------------
    File        : tableRowsQty
    Purpose     : Return the quantity of rows given a specific table
    Syntax      : 
    Description : Table's quantity of rows
    Author(s)   : Lucas Bicalho
    Created     : Sun Jul 09 13:16:13 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS AppServer.Utils.TableRowsCounter: 

    DEFINE PUBLIC PROPERTY iQtyRows AS INTEGER NO-UNDO 
    GET.
    PRIVATE SET.     
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC TableRowsCounter (  ):
        SUPER ().
        
        THIS-OBJECT:Clear().
        
    END CONSTRUCTOR.
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PRIVATE VOID Clear(  ):
        
        iQtyRows = 0.

    END METHOD.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC INTEGER CountRows(INPUT ipcTable AS CHARACTER):

        DEF VAR h_predicate AS CHAR     NO-UNDO.
        DEF VAR h_qry       AS HANDLE   NO-UNDO.
        DEF VAR h_buffer    AS HANDLE   NO-UNDO.
        h_predicate = SUBSTITUTE("PRESELECT EACH &1 NO-LOCK", ipcTable).
        CREATE BUFFER h_buffer FOR TABLE ipcTable.
        CREATE QUERY h_qry.
        h_qry:SET-BUFFERS( h_buffer ).
        h_qry:QUERY-PREPARE( h_predicate ).
        h_qry:QUERY-OPEN().
    
        ASSIGN 
            iQtyRows = h_qry:NUM-RESULTS.
        
        DELETE OBJECT h_qry.
        DELETE OBJECT h_buffer.
        
        RETURN iQtyRows.

    END METHOD.  

END CLASS.