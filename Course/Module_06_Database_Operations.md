# Module 6: Database Operations - Expert Level Guide

## 🎯 Complete Database Mastery
From basic SQL to advanced database optimization techniques used in high-performance enterprise SAP systems.

---

## 📖 Table of Contents
1. [Advanced SELECT Statements](#advanced-select-statements)
2. [Database Modifications](#database-modifications)
3. [Transaction Management](#transaction-management)
4. [Performance Optimization](#performance-optimization)
5. [Database Procedures](#database-procedures)
6. [Locking Strategies](#locking-strategies)
7. [Error Handling](#error-handling)
8. [Enterprise Patterns](#enterprise-patterns)

---

## 1. Advanced SELECT Statements

### Complex JOIN Operations

#### **Multi-table Joins with Performance Optimization**
```abap
" Complex business scenario: Customer order analysis
SELECT 
  c~kunnr AS customer_id,
  c~name1 AS customer_name,
  c~land1 AS country,
  o~vbeln AS order_number,
  o~erdat AS order_date,
  o~netwr AS order_value,
  i~posnr AS item_number,
  i~matnr AS material,
  i~kwmeng AS quantity,
  m~mtart AS material_type,
  m~matkl AS material_group,
  p~vprs AS standard_price
  
FROM kna1 AS c
INNER JOIN vbak AS o ON c~kunnr = o~kunnr
INNER JOIN vbap AS i ON o~vbeln = i~vbeln
INNER JOIN mara AS m ON i~matnr = m~matnr
LEFT OUTER JOIN mbew AS p ON i~matnr = p~matnr 
                        AND p~bwkey = 'IDES'
                        
WHERE c~land1 IN @lr_countries
  AND o~erdat >= @lv_date_from
  AND o~erdat <= @lv_date_to
  AND o~vbtyp = 'C'                    " Sales orders only
  AND c~loevm = ''                     " Not marked for deletion
  AND m~lvorm = ''                     " Material not deleted
  
ORDER BY c~kunnr, o~erdat DESC, i~posnr
INTO TABLE @DATA(lt_order_analysis).

" Alternative with CTE for better performance
WITH RECURSIVE order_hierarchy AS (
  SELECT vbeln, vbeln_parent, hierarchy_level
  FROM custom_order_tree
  WHERE vbeln_parent IS NULL
  
  UNION ALL
  
  SELECT t.vbeln, t.vbeln_parent, h.hierarchy_level + 1
  FROM custom_order_tree t
  INNER JOIN order_hierarchy h ON t.vbeln_parent = h.vbeln
)
SELECT * FROM order_hierarchy;
```

#### **Subqueries and EXISTS/NOT EXISTS**
```abap
" Find customers with no orders in specific period
SELECT kunnr, name1, land1
FROM kna1
WHERE land1 = 'DE'
  AND NOT EXISTS (
    SELECT 1 FROM vbak 
    WHERE kunnr = kna1~kunnr 
      AND erdat >= '20240101'
      AND erdat <= '20241231'
  )
INTO TABLE @DATA(lt_inactive_customers).

" Correlated subquery with aggregation
SELECT 
  kunnr,
  name1,
  ( SELECT COUNT(*) FROM vbak WHERE kunnr = kna1~kunnr ) AS total_orders,
  ( SELECT SUM( netwr ) FROM vbak WHERE kunnr = kna1~kunnr 
    AND erdat >= '20240101' ) AS ytd_revenue
FROM kna1
WHERE land1 IN ( 'DE', 'US', 'GB' )
INTO TABLE @DATA(lt_customer_stats).

" Window functions (ABAP 7.5+)
SELECT 
  kunnr,
  vbeln,
  erdat,
  netwr,
  SUM( netwr ) OVER( PARTITION BY kunnr 
                     ORDER BY erdat 
                     ROWS UNBOUNDED PRECEDING ) AS cumulative_revenue,
  ROW_NUMBER() OVER( PARTITION BY kunnr 
                     ORDER BY erdat DESC ) AS order_rank,
  LAG( netwr, 1, 0 ) OVER( PARTITION BY kunnr 
                           ORDER BY erdat ) AS previous_order_value
FROM vbak
WHERE erdat >= '20240101'
ORDER BY kunnr, erdat
INTO TABLE @DATA(lt_order_analysis).
```

### Dynamic SQL Construction

#### **Safe Dynamic SQL Builder**
```abap
CLASS zcl_dynamic_sql_builder DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             add_table
               IMPORTING iv_table TYPE string
               RETURNING VALUE(ro_builder) TYPE REF TO zcl_dynamic_sql_builder,
             add_field  
               IMPORTING iv_field TYPE string
                         iv_alias TYPE string OPTIONAL
               RETURNING VALUE(ro_builder) TYPE REF TO zcl_dynamic_sql_builder,
             add_join
               IMPORTING iv_join_type TYPE string DEFAULT 'INNER JOIN'
                         iv_table TYPE string
                         iv_condition TYPE string
               RETURNING VALUE(ro_builder) TYPE REF TO zcl_dynamic_sql_builder,
             add_where
               IMPORTING iv_condition TYPE string
               RETURNING VALUE(ro_builder) TYPE REF TO zcl_dynamic_sql_builder,
             build
               RETURNING VALUE(rv_sql) TYPE string,
             execute
               RETURNING VALUE(rr_result) TYPE REF TO data.
               
  PRIVATE SECTION.
    DATA: mv_select_clause TYPE string,
          mv_from_clause   TYPE string,
          mv_join_clause   TYPE string,
          mv_where_clause  TYPE string,
          mt_parameters    TYPE TABLE OF string.
          
    METHODS: escape_identifier
               IMPORTING iv_identifier TYPE string
               RETURNING VALUE(rv_escaped) TYPE string,
             validate_sql_injection
               IMPORTING iv_input TYPE string
               RETURNING VALUE(rv_safe) TYPE abap_bool.
ENDCLASS.

CLASS zcl_dynamic_sql_builder IMPLEMENTATION.
  METHOD constructor.
    mv_select_clause = 'SELECT'.
    mv_from_clause = 'FROM'.
  ENDMETHOD.
  
  METHOD add_table.
    IF validate_sql_injection( iv_table ) = abap_false.
      RAISE EXCEPTION TYPE zcx_sql_injection.
    ENDIF.
    
    mv_from_clause = |{ mv_from_clause } { escape_identifier( iv_table ) }|.
    ro_builder = me.
  ENDMETHOD.
  
  METHOD add_field.
    DATA: lv_field_clause TYPE string.
    
    IF validate_sql_injection( iv_field ) = abap_false.
      RAISE EXCEPTION TYPE zcx_sql_injection.
    ENDIF.
    
    lv_field_clause = escape_identifier( iv_field ).
    
    IF iv_alias IS NOT INITIAL.
      lv_field_clause = |{ lv_field_clause } AS { escape_identifier( iv_alias ) }|.
    ENDIF.
    
    IF mv_select_clause = 'SELECT'.
      mv_select_clause = |{ mv_select_clause } { lv_field_clause }|.
    ELSE.
      mv_select_clause = |{ mv_select_clause }, { lv_field_clause }|.
    ENDIF.
    
    ro_builder = me.
  ENDMETHOD.
  
  METHOD build.
    rv_sql = |{ mv_select_clause } { mv_from_clause } { mv_join_clause } { mv_where_clause }|.
  ENDMETHOD.
  
  METHOD escape_identifier.
    " Add proper escaping for SQL identifiers
    rv_escaped = |"{ replace( val = iv_identifier sub = '"' with = '""' occ = 0 ) }"|.
  ENDMETHOD.
  
  METHOD validate_sql_injection.
    " Basic SQL injection prevention
    DATA: lt_dangerous_patterns TYPE TABLE OF string.
    
    lt_dangerous_patterns = VALUE #( 
      ( |'| ) ( |;| ) ( |--| ) ( |/*| ) ( |*/| )
      ( |DROP| ) ( |DELETE| ) ( |INSERT| ) ( |UPDATE| )
      ( |EXEC| ) ( |EXECUTE| ) ( |UNION| )
    ).
    
    DATA(lv_upper_input) = to_upper( iv_input ).
    
    LOOP AT lt_dangerous_patterns INTO DATA(lv_pattern).
      IF lv_upper_input CS lv_pattern.
        rv_safe = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
    
    rv_safe = abap_true.
  ENDMETHOD.
ENDCLASS.

" Usage example:
DATA(lo_builder) = NEW zcl_dynamic_sql_builder( ).

DATA(lv_sql) = lo_builder->add_table( 'KNA1' )
                         ->add_field( 'KUNNR', 'customer_id' )
                         ->add_field( 'NAME1', 'customer_name' )
                         ->add_join( iv_join_type = 'LEFT JOIN'
                                    iv_table = 'VBAK'
                                    iv_condition = 'KNA1~KUNNR = VBAK~KUNNR' )
                         ->add_where( 'KNA1~LAND1 = ''DE''' )
                         ->build( ).
```

### Bulk Data Processing

#### **Chunked SELECT with Cursor Processing**
```abap
" Process large datasets without memory issues
CLASS zcl_bulk_processor DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: c_chunk_size TYPE i VALUE 50000.
    
    METHODS: process_large_table
               IMPORTING iv_table_name TYPE string
                         iv_where_clause TYPE string OPTIONAL,
                         
             process_with_cursor
               IMPORTING iv_table_name TYPE string.
               
  PRIVATE SECTION.
    METHODS: process_chunk
               IMPORTING it_data TYPE STANDARD TABLE.
ENDCLASS.

CLASS zcl_bulk_processor IMPLEMENTATION.
  METHOD process_large_table.
    DATA: lv_offset TYPE i VALUE 0,
          lt_chunk  TYPE STANDARD TABLE,
          lv_sql    TYPE string.
          
    " Build dynamic SQL
    lv_sql = |SELECT * FROM { iv_table_name }|.
    IF iv_where_clause IS NOT INITIAL.
      lv_sql = |{ lv_sql } WHERE { iv_where_clause }|.
    ENDIF.
    lv_sql = |{ lv_sql } ORDER BY mandt, primary_key|.
    
    DO.
      " Clear chunk for reuse
      CLEAR lt_chunk.
      
      " Select chunk with OFFSET and LIMIT
      SELECT * FROM (iv_table_name)
        ORDER BY mandt, primary_key
        INTO TABLE lt_chunk
        OFFSET lv_offset
        UP TO c_chunk_size ROWS.
        
      IF sy-subrc <> 0 OR lines( lt_chunk ) = 0.
        EXIT.  " No more data
      ENDIF.
      
      " Process chunk
      process_chunk( lt_chunk ).
      
      " Commit after each chunk
      COMMIT WORK.
      
      " Update offset for next chunk
      lv_offset += c_chunk_size.
      
      " Optional: Progress indicator
      MESSAGE |Processing records { lv_offset - c_chunk_size + 1 } to { lv_offset }| TYPE 'I'.
    ENDDO.
  ENDMETHOD.
  
  METHOD process_with_cursor.
    " Alternative: Using database cursor (HANA)
    DATA: lo_cursor TYPE REF TO cl_sql_result_set,
          lr_data   TYPE REF TO data.
          
    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
    
    " Create cursor for large dataset
    DATA(lo_connection) = cl_sql_connection=>get_connection( ).
    
    DATA(lv_sql) = |SELECT * FROM { iv_table_name } ORDER BY mandt, primary_key|.
    
    lo_cursor = lo_connection->create_statement( )->execute_query( lv_sql ).
    
    " Create dynamic internal table
    CREATE DATA lr_data TYPE STANDARD TABLE.
    ASSIGN lr_data->* TO <lt_data>.
    
    " Fetch in chunks
    WHILE lo_cursor->next_package( EXPORTING package_size = c_chunk_size
                                   IMPORTING et_data = <lt_data> ) > 0.
      " Process chunk
      process_chunk( <lt_data> ).
      
      " Clear for next iteration
      CLEAR <lt_data>.
    ENDWHILE.
    
    lo_cursor->close( ).
  ENDMETHOD.
  
  METHOD process_chunk.
    " Implement business logic for each chunk
    DATA(lv_lines) = lines( it_data ).
    " Processing logic here...
  ENDMETHOD.
ENDCLASS.
```

---

## 2. Database Modifications

### Atomic Operations

#### **Safe INSERT Operations**
```abap
" Insert with conflict resolution
CLASS zcl_safe_insert DEFINITION.
  PUBLIC SECTION.
    METHODS: insert_customer
               IMPORTING is_customer TYPE zcustomer
               RAISING   zcx_duplicate_key
                         zcx_database_error,
                         
             bulk_insert_customers
               IMPORTING it_customers TYPE ztt_customers
               EXPORTING et_errors TYPE ztt_insert_errors.
               
  PRIVATE SECTION.
    METHODS: check_duplicate
               IMPORTING iv_customer_id TYPE kunnr
               RETURNING VALUE(rv_exists) TYPE abap_bool,
               
             log_insert_error
               IMPORTING is_customer TYPE zcustomer
                         ix_error TYPE REF TO cx_root.
ENDCLASS.

CLASS zcl_safe_insert IMPLEMENTATION.
  METHOD insert_customer.
    " Check for duplicates first
    IF check_duplicate( is_customer-customer_id ) = abap_true.
      RAISE EXCEPTION TYPE zcx_duplicate_key
        EXPORTING customer_id = is_customer-customer_id.
    ENDIF.
    
    " Insert with error handling
    TRY.
        INSERT zcustomer_master FROM is_customer.
        
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_database_error
            EXPORTING operation = 'INSERT'
                      table_name = 'ZCUSTOMER_MASTER'
                      sql_code = sy-subrc.
        ENDIF.
        
      CATCH cx_sy_open_sql_db INTO DATA(lx_db_error).
        RAISE EXCEPTION TYPE zcx_database_error
          EXPORTING previous = lx_db_error.
    ENDTRY.
  ENDMETHOD.
  
  METHOD bulk_insert_customers.
    DATA: ls_error TYPE zinsert_error.
    
    CLEAR et_errors.
    
    LOOP AT it_customers INTO DATA(ls_customer).
      TRY.
          insert_customer( ls_customer ).
          
        CATCH zcx_duplicate_key INTO DATA(lx_dup).
          ls_error = VALUE #( 
            customer_id = ls_customer-customer_id
            error_type = 'DUPLICATE'
            error_message = lx_dup->get_text( )
          ).
          APPEND ls_error TO et_errors.
          
        CATCH zcx_database_error INTO DATA(lx_db).
          ls_error = VALUE #( 
            customer_id = ls_customer-customer_id
            error_type = 'DATABASE'
            error_message = lx_db->get_text( )
          ).
          APPEND ls_error TO et_errors.
          
          " Log error for analysis
          log_insert_error( is_customer = ls_customer
                            ix_error = lx_db ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD check_duplicate.
    SELECT SINGLE customer_id FROM zcustomer_master
      INTO @DATA(lv_existing)
      WHERE customer_id = @iv_customer_id.
      
    rv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.
```

#### **UPDATE with Optimistic Locking**
```abap
" Implement optimistic locking pattern
CLASS zcl_optimistic_update DEFINITION.
  PUBLIC SECTION.
    METHODS: update_customer
               IMPORTING is_customer TYPE zcustomer
               RAISING   zcx_record_changed
                         zcx_record_not_found,
                         
             update_with_retry
               IMPORTING is_customer TYPE zcustomer
                         iv_max_retries TYPE i DEFAULT 3
               RAISING   zcx_update_failed.
               
  PRIVATE SECTION.
    METHODS: get_current_version
               IMPORTING iv_customer_id TYPE kunnr
               RETURNING VALUE(rv_version) TYPE timestamp
               RAISING   zcx_record_not_found.
ENDCLASS.

CLASS zcl_optimistic_update IMPLEMENTATION.
  METHOD update_customer.
    " Get current version from database
    DATA(lv_current_version) = get_current_version( is_customer-customer_id ).
    
    " Check if record was modified by another user
    IF lv_current_version <> is_customer-last_changed_at.
      RAISE EXCEPTION TYPE zcx_record_changed
        EXPORTING customer_id = is_customer-customer_id
                  expected_version = is_customer-last_changed_at
                  actual_version = lv_current_version.
    ENDIF.
    
    " Update with new timestamp
    GET TIME STAMP FIELD DATA(lv_new_timestamp).
    
    UPDATE zcustomer_master 
      SET name = is_customer-name,
          city = is_customer-city,
          last_changed_at = lv_new_timestamp,
          last_changed_by = sy-uname
    WHERE customer_id = is_customer-customer_id
      AND last_changed_at = is_customer-last_changed_at.
      
    IF sy-subrc <> 0.
      " Record was changed between version check and update
      RAISE EXCEPTION TYPE zcx_record_changed
        EXPORTING customer_id = is_customer-customer_id.
    ENDIF.
  ENDMETHOD.
  
  METHOD update_with_retry.
    DATA: lv_retry_count TYPE i.
    
    DO iv_max_retries TIMES.
      TRY.
          " Get fresh data before retry
          SELECT SINGLE * FROM zcustomer_master
            INTO CORRESPONDING FIELDS OF is_customer
            WHERE customer_id = is_customer-customer_id.
            
          " Apply changes to fresh data
          " (Business logic to merge changes)
          
          " Attempt update
          update_customer( is_customer ).
          
          " Success - exit retry loop
          EXIT.
          
        CATCH zcx_record_changed.
          lv_retry_count += 1.
          
          IF lv_retry_count >= iv_max_retries.
            RAISE EXCEPTION TYPE zcx_update_failed
              EXPORTING customer_id = is_customer-customer_id
                        retry_count = lv_retry_count.
          ENDIF.
          
          " Wait before retry (exponential backoff)
          WAIT UP TO ( lv_retry_count * 100 ) MILLISECONDS.
      ENDTRY.
    ENDDO.
  ENDMETHOD.
  
  METHOD get_current_version.
    SELECT SINGLE last_changed_at FROM zcustomer_master
      INTO rv_version
      WHERE customer_id = iv_customer_id.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_record_not_found
        EXPORTING customer_id = iv_customer_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### Bulk Operations

#### **High-Performance Bulk Updates**
```abap
" Optimize bulk updates for large datasets
CLASS zcl_bulk_updater DEFINITION.
  PUBLIC SECTION.
    METHODS: bulk_update_prices
               IMPORTING it_price_updates TYPE ztt_price_updates
               EXPORTING ev_updated_count TYPE i
                         et_errors TYPE ztt_update_errors.
                         
  PRIVATE SECTION.
    CONSTANTS: c_batch_size TYPE i VALUE 10000.
    
    METHODS: update_batch
               IMPORTING it_batch TYPE ztt_price_updates
               EXPORTING ev_updated TYPE i
                         et_batch_errors TYPE ztt_update_errors.
ENDCLASS.

CLASS zcl_bulk_updater IMPLEMENTATION.
  METHOD bulk_update_prices.
    DATA: lv_batch_start TYPE i VALUE 1,
          lv_batch_end   TYPE i,
          lt_batch       TYPE ztt_price_updates,
          lv_batch_updated TYPE i,
          lt_batch_errors TYPE ztt_update_errors.
          
    CLEAR: ev_updated_count, et_errors.
    
    DATA(lv_total_records) = lines( it_price_updates ).
    
    " Process in batches
    WHILE lv_batch_start <= lv_total_records.
      lv_batch_end = lv_batch_start + c_batch_size - 1.
      IF lv_batch_end > lv_total_records.
        lv_batch_end = lv_total_records.
      ENDIF.
      
      " Extract batch
      CLEAR lt_batch.
      LOOP AT it_price_updates INTO DATA(ls_update) FROM lv_batch_start TO lv_batch_end.
        APPEND ls_update TO lt_batch.
      ENDLOOP.
      
      " Update batch
      update_batch( EXPORTING it_batch = lt_batch
                    IMPORTING ev_updated = lv_batch_updated
                              et_batch_errors = lt_batch_errors ).
      
      " Accumulate results
      ev_updated_count += lv_batch_updated.
      APPEND LINES OF lt_batch_errors TO et_errors.
      
      " Commit after each batch
      COMMIT WORK.
      
      lv_batch_start = lv_batch_end + 1.
    ENDWHILE.
  ENDMETHOD.
  
  METHOD update_batch.
    " Use ABAP SQL for bulk update
    CLEAR: ev_updated, et_batch_errors.
    
    " Build UPDATE statement for batch
    DATA: lt_material_updates TYPE TABLE OF zmaterial_price.
    
    " Prepare data for bulk update
    LOOP AT it_batch INTO DATA(ls_update).
      APPEND VALUE #( 
        material = ls_update-material
        plant = ls_update-plant
        price = ls_update-new_price
        valid_from = ls_update-valid_from
        last_changed_by = sy-uname
        last_changed_at = sy-datum
      ) TO lt_material_updates.
    ENDLOOP.
    
    " Perform bulk update using MODIFY
    TRY.
        MODIFY zmaterial_price FROM TABLE lt_material_updates.
        ev_updated = sy-dbcnt.
        
      CATCH cx_sy_open_sql_db INTO DATA(lx_db_error).
        " Handle batch-level errors
        LOOP AT it_batch INTO ls_update.
          APPEND VALUE #( 
            material = ls_update-material
            plant = ls_update-plant
            error_message = lx_db_error->get_text( )
          ) TO et_batch_errors.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

---

## 3. Transaction Management

### Advanced Transaction Control

#### **Nested Transaction Handler**
```abap
" Manage complex transaction scenarios
CLASS zcl_transaction_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: begin_transaction
               IMPORTING iv_transaction_id TYPE string OPTIONAL,
               
             commit_transaction
               RAISING zcx_transaction_error,
               
             rollback_transaction,
             
             create_savepoint
               IMPORTING iv_savepoint_name TYPE string
               RETURNING VALUE(rv_savepoint_id) TYPE string,
               
             rollback_to_savepoint
               IMPORTING iv_savepoint_id TYPE string
               RAISING   zcx_savepoint_not_found.
               
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_savepoint,
             savepoint_id   TYPE string,
             savepoint_name TYPE string,
             created_at     TYPE timestamp,
           END OF ty_savepoint.
           
    DATA: mv_transaction_active TYPE abap_bool,
          mv_transaction_id      TYPE string,
          mt_savepoints          TYPE TABLE OF ty_savepoint,
          mv_savepoint_counter   TYPE i.
ENDCLASS.

CLASS zcl_transaction_manager IMPLEMENTATION.
  METHOD begin_transaction.
    IF mv_transaction_active = abap_true.
      " Nested transaction - use savepoint
      DATA(lv_savepoint_id) = create_savepoint( |NESTED_{ mv_savepoint_counter }| ).
    ELSE.
      " Start new transaction
      mv_transaction_active = abap_true.
      mv_transaction_id = COND #( WHEN iv_transaction_id IS NOT INITIAL 
                                  THEN iv_transaction_id 
                                  ELSE |TRANS_{ sy-datum }{ sy-uzeit }{ sy-tabix }| ).
    ENDIF.
  ENDMETHOD.
  
  METHOD commit_transaction.
    IF mv_transaction_active = abap_false.
      RAISE EXCEPTION TYPE zcx_transaction_error
        EXPORTING message = 'No active transaction to commit'.
    ENDIF.
    
    " Validate data consistency before commit
    IF validate_transaction_consistency( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_transaction_error
        EXPORTING message = 'Transaction consistency check failed'.
    ENDIF.
    
    " Commit work
    COMMIT WORK AND WAIT.
    
    " Reset transaction state
    mv_transaction_active = abap_false.
    CLEAR: mv_transaction_id, mt_savepoints, mv_savepoint_counter.
  ENDMETHOD.
  
  METHOD rollback_transaction.
    IF mv_transaction_active = abap_true.
      ROLLBACK WORK.
      mv_transaction_active = abap_false.
      CLEAR: mv_transaction_id, mt_savepoints, mv_savepoint_counter.
    ENDIF.
  ENDMETHOD.
  
  METHOD create_savepoint.
    mv_savepoint_counter += 1.
    
    GET TIME STAMP FIELD DATA(lv_timestamp).
    
    rv_savepoint_id = |SP_{ mv_savepoint_counter }_{ lv_timestamp }|.
    
    " Store savepoint info
    APPEND VALUE #( 
      savepoint_id = rv_savepoint_id
      savepoint_name = iv_savepoint_name
      created_at = lv_timestamp
    ) TO mt_savepoints.
    
    " Create actual database savepoint (if supported)
    " Note: ABAP doesn't support nested transactions natively
    " This is a logical implementation for business logic
  ENDMETHOD.
  
  METHOD rollback_to_savepoint.
    READ TABLE mt_savepoints INTO DATA(ls_savepoint)
      WITH KEY savepoint_id = iv_savepoint_id.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_savepoint_not_found
        EXPORTING savepoint_id = iv_savepoint_id.
    ENDIF.
    
    " Remove savepoints created after this one
    DELETE mt_savepoints WHERE created_at > ls_savepoint-created_at.
    
    " Logical rollback implementation
    " (Business-specific compensation logic here)
  ENDMETHOD.
  
  METHODS: validate_transaction_consistency
             RETURNING VALUE(rv_valid) TYPE abap_bool.
             
  METHOD validate_transaction_consistency.
    " Implement business-specific validation
    " Example: Check referential integrity, business rules, etc.
    rv_valid = abap_true.  " Placeholder
  ENDMETHOD.
ENDCLASS.
```

#### **Distributed Transaction Coordinator**
```abap
" Coordinate transactions across multiple systems
CLASS zcl_distributed_transaction DEFINITION.
  PUBLIC SECTION.
    METHODS: begin_distributed_transaction
               IMPORTING it_systems TYPE ztt_system_connections,
               
             prepare_phase
               RETURNING VALUE(rv_all_prepared) TYPE abap_bool,
               
             commit_phase
               RETURNING VALUE(rv_all_committed) TYPE abap_bool,
               
             abort_transaction.
               
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_transaction_participant,
             system_id     TYPE string,
             connection    TYPE REF TO object,
             prepared      TYPE abap_bool,
             committed     TYPE abap_bool,
             error_message TYPE string,
           END OF ty_transaction_participant.
           
    DATA: mt_participants TYPE TABLE OF ty_transaction_participant,
          mv_transaction_id TYPE string.
          
    METHODS: prepare_participant
               IMPORTING is_participant TYPE ty_transaction_participant
               RETURNING VALUE(rv_prepared) TYPE abap_bool,
               
             commit_participant
               IMPORTING is_participant TYPE ty_transaction_participant
               RETURNING VALUE(rv_committed) TYPE abap_bool.
ENDCLASS.

CLASS zcl_distributed_transaction IMPLEMENTATION.
  METHOD begin_distributed_transaction.
    " Initialize participants
    CLEAR mt_participants.
    
    LOOP AT it_systems INTO DATA(ls_system).
      APPEND VALUE #( 
        system_id = ls_system-system_id
        connection = ls_system-connection
        prepared = abap_false
        committed = abap_false
      ) TO mt_participants.
    ENDLOOP.
    
    " Generate unique transaction ID
    GET TIME STAMP FIELD DATA(lv_timestamp).
    mv_transaction_id = |DIST_{ lv_timestamp }|.
  ENDMETHOD.
  
  METHOD prepare_phase.
    " Two-phase commit: Phase 1 - Prepare
    rv_all_prepared = abap_true.
    
    LOOP AT mt_participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      DATA(lv_prepared) = prepare_participant( <ls_participant> ).
      <ls_participant>-prepared = lv_prepared.
      
      IF lv_prepared = abap_false.
        rv_all_prepared = abap_false.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD commit_phase.
    " Two-phase commit: Phase 2 - Commit
    rv_all_committed = abap_true.
    
    LOOP AT mt_participants ASSIGNING FIELD-SYMBOL(<ls_participant>)
      WHERE prepared = abap_true.
      
      DATA(lv_committed) = commit_participant( <ls_participant> ).
      <ls_participant>-committed = lv_committed.
      
      IF lv_committed = abap_false.
        rv_all_committed = abap_false.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD abort_transaction.
    " Rollback all participants
    LOOP AT mt_participants ASSIGNING FIELD-SYMBOL(<ls_participant>).
      " Send rollback command to each participant
      " Implementation depends on connection type (RFC, HTTP, etc.)
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

---

## 4. Performance Optimization

### SQL Optimization Techniques

#### **Query Performance Analyzer**
```abap
CLASS zcl_sql_performance_analyzer DEFINITION.
  PUBLIC SECTION.
    METHODS: analyze_query
               IMPORTING iv_sql TYPE string
               RETURNING VALUE(rs_analysis) TYPE zquery_analysis,
               
             optimize_where_clause
               IMPORTING it_conditions TYPE ztt_where_conditions
               RETURNING VALUE(rv_optimized) TYPE string,
               
             suggest_index
               IMPORTING iv_table TYPE string
                         it_where_fields TYPE ztt_field_names
               RETURNING VALUE(rs_index_suggestion) TYPE zindex_suggestion.
               
  PRIVATE SECTION.
    METHODS: parse_sql_statement
               IMPORTING iv_sql TYPE string
               RETURNING VALUE(rs_parsed) TYPE zsql_parse_result,
               
             estimate_cost
               IMPORTING is_parsed_sql TYPE zsql_parse_result
               RETURNING VALUE(rv_cost) TYPE i,
               
             check_index_usage
               IMPORTING iv_table TYPE string
                         it_where_fields TYPE ztt_field_names
               RETURNING VALUE(rt_available_indexes) TYPE ztt_index_info.
ENDCLASS.

CLASS zcl_sql_performance_analyzer IMPLEMENTATION.
  METHOD analyze_query.
    " Parse SQL statement
    DATA(ls_parsed) = parse_sql_statement( iv_sql ).
    
    " Estimate execution cost
    DATA(lv_cost) = estimate_cost( ls_parsed ).
    
    " Check for performance issues
    DATA: lt_issues TYPE TABLE OF string.
    
    " Check for missing WHERE clause
    IF ls_parsed-has_where = abap_false.
      APPEND 'Missing WHERE clause - full table scan' TO lt_issues.
    ENDIF.
    
    " Check for functions in WHERE clause
    IF ls_parsed-has_functions_in_where = abap_true.
      APPEND 'Functions in WHERE clause prevent index usage' TO lt_issues.
    ENDIF.
    
    " Check for OR conditions
    IF ls_parsed-has_or_conditions = abap_true.
      APPEND 'OR conditions may prevent optimal index usage' TO lt_issues.
    ENDIF.
    
    rs_analysis = VALUE #( 
      estimated_cost = lv_cost
      performance_issues = lt_issues
      optimization_suggestions = get_optimization_suggestions( ls_parsed )
    ).
  ENDMETHOD.
  
  METHOD optimize_where_clause.
    " Reorder conditions by selectivity
    DATA: lt_optimized TYPE TABLE OF string.
    
    " Sort conditions by estimated selectivity (most selective first)
    SORT it_conditions BY selectivity_score DESCENDING.
    
    " Build optimized WHERE clause
    LOOP AT it_conditions INTO DATA(ls_condition).
      IF sy-tabix = 1.
        rv_optimized = |WHERE { ls_condition-condition }|.
      ELSE.
        rv_optimized = |{ rv_optimized } AND { ls_condition-condition }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD suggest_index.
    " Analyze current indexes
    DATA(lt_existing_indexes) = check_index_usage( 
      iv_table = iv_table
      it_where_fields = it_where_fields
    ).
    
    " Suggest new index if beneficial
    IF lines( lt_existing_indexes ) = 0.
      rs_index_suggestion = VALUE #( 
        table_name = iv_table
        suggested_fields = it_where_fields
        index_type = 'BTREE'
        estimated_benefit = 'HIGH'
        create_statement = build_index_statement( iv_table, it_where_fields )
      ).
    ENDIF.
  ENDMETHOD.
  
  METHODS: get_optimization_suggestions
             IMPORTING is_parsed TYPE zsql_parse_result
             RETURNING VALUE(rt_suggestions) TYPE TABLE OF string,
             
           build_index_statement
             IMPORTING iv_table TYPE string
                       it_fields TYPE ztt_field_names
             RETURNING VALUE(rv_statement) TYPE string.
             
  METHOD get_optimization_suggestions.
    " Implementation for suggestions based on parsed SQL
    IF is_parsed-has_select_star = abap_true.
      APPEND 'Replace SELECT * with specific field list' TO rt_suggestions.
    ENDIF.
    
    IF is_parsed-has_subqueries = abap_true.
      APPEND 'Consider using JOINs instead of subqueries' TO rt_suggestions.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

#### **Database Connection Pool**
```abap
" Manage database connections efficiently
CLASS zcl_connection_pool DEFINITION.
  PUBLIC SECTION.
    METHODS: get_connection
               RETURNING VALUE(ro_connection) TYPE REF TO cl_sql_connection,
               
             return_connection
               IMPORTING io_connection TYPE REF TO cl_sql_connection,
               
             cleanup_idle_connections.
               
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_pooled_connection,
             connection TYPE REF TO cl_sql_connection,
             in_use     TYPE abap_bool,
             last_used  TYPE timestamp,
             created_at TYPE timestamp,
           END OF ty_pooled_connection.
           
    DATA: mt_pool TYPE TABLE OF ty_pooled_connection,
          mv_max_connections TYPE i VALUE 20,
          mv_idle_timeout TYPE i VALUE 300.  " 5 minutes
          
    METHODS: create_new_connection
               RETURNING VALUE(ro_connection) TYPE REF TO cl_sql_connection,
               
             is_connection_valid
               IMPORTING io_connection TYPE REF TO cl_sql_connection
               RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS zcl_connection_pool IMPLEMENTATION.
  METHOD get_connection.
    " Look for available connection in pool
    LOOP AT mt_pool ASSIGNING FIELD-SYMBOL(<ls_pool_entry>)
      WHERE in_use = abap_false.
      
      " Validate connection is still active
      IF is_connection_valid( <ls_pool_entry>-connection ).
        <ls_pool_entry>-in_use = abap_true.
        GET TIME STAMP FIELD <ls_pool_entry>-last_used.
        ro_connection = <ls_pool_entry>-connection.
        RETURN.
      ELSE.
        " Remove invalid connection
        DELETE mt_pool INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    
    " No available connection - create new one if under limit
    IF lines( mt_pool ) < mv_max_connections.
      ro_connection = create_new_connection( ).
      
      GET TIME STAMP FIELD DATA(lv_timestamp).
      
      APPEND VALUE #( 
        connection = ro_connection
        in_use = abap_true
        last_used = lv_timestamp
        created_at = lv_timestamp
      ) TO mt_pool.
    ELSE.
      " Pool exhausted - wait and retry or throw exception
      RAISE EXCEPTION TYPE zcx_pool_exhausted.
    ENDIF.
  ENDMETHOD.
  
  METHOD return_connection.
    " Mark connection as available
    LOOP AT mt_pool ASSIGNING FIELD-SYMBOL(<ls_pool_entry>)
      WHERE connection = io_connection.
      
      <ls_pool_entry>-in_use = abap_false.
      GET TIME STAMP FIELD <ls_pool_entry>-last_used.
      EXIT.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD cleanup_idle_connections.
    GET TIME STAMP FIELD DATA(lv_current_time).
    
    " Remove idle connections
    LOOP AT mt_pool ASSIGNING FIELD-SYMBOL(<ls_pool_entry>)
      WHERE in_use = abap_false.
      
      DATA(lv_idle_time) = cl_abap_tstmp=>subtract( 
        tstmp1 = lv_current_time
        tstmp2 = <ls_pool_entry>-last_used
      ).
      
      IF lv_idle_time > mv_idle_timeout.
        " Close and remove connection
        <ls_pool_entry>-connection->close( ).
        DELETE mt_pool INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

This comprehensive database operations module covers everything from basic SQL to advanced enterprise patterns for database management in SAP ABAP systems. The techniques shown are used in high-performance, mission-critical SAP environments.

---

**Next Module**: [Module 7: Reports Development](Module_07_Reports_Development.md)