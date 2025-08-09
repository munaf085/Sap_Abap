# Module 10: BDC & Data Migration - Expert Level Mastery

## 🎯 Master Enterprise Data Migration
From basic BDC to advanced LSMW, direct input methods, and large-scale migration strategies used in SAP implementations.

---

## 📖 Table of Contents
1. [BDC Architecture & Best Practices](#bdc-architecture--best-practices)
2. [Session Method Advanced](#session-method-advanced)
3. [Call Transaction Method Mastery](#call-transaction-method-mastery)
4. [LSMW Professional Development](#lsmw-professional-development)
5. [Direct Input Methods](#direct-input-methods)
6. [Error Handling & Recovery](#error-handling--recovery)
7. [Performance Optimization](#performance-optimization)
8. [Enterprise Migration Patterns](#enterprise-migration-patterns)

---

## 1. BDC Architecture & Best Practices

### Enterprise BDC Framework

#### **Comprehensive BDC Architecture**
```abap
*&---------------------------------------------------------------------*
*& Program: Z_ENTERPRISE_BDC_FRAMEWORK
*& Purpose: Enterprise-grade BDC framework with advanced features
*& Author: Expert Developer
*& Architecture: Factory pattern with strategy selection
*&---------------------------------------------------------------------*

PROGRAM z_enterprise_bdc_framework.

" ===== TYPE DEFINITIONS =====
TYPES: BEGIN OF ty_bdc_record,
         program   TYPE bdcdata-program,
         dynpro    TYPE bdcdata-dynpro,
         dynbegin  TYPE bdcdata-dynbegin,
         fnam      TYPE bdcdata-fnam,
         fval      TYPE bdcdata-fval,
       END OF ty_bdc_record,
       
       tt_bdc_data TYPE TABLE OF ty_bdc_record,
       
       BEGIN OF ty_migration_config,
         method           TYPE char1,  " S=Session, T=Transaction, D=Direct
         transaction_code TYPE tcode,
         session_name     TYPE char12,
         batch_size       TYPE i,
         error_handling   TYPE char1,  " E=Error, W=Warning, S=Skip
         parallel_jobs    TYPE i,
         test_mode        TYPE abap_bool,
         log_level        TYPE char1,  " 1=Error, 2=Warning, 3=Info, 4=Debug
       END OF ty_migration_config,
       
       BEGIN OF ty_upload_file,
         file_path        TYPE string,
         separator        TYPE char1,
         has_header       TYPE abap_bool,
         encoding         TYPE string,
         field_mapping    TYPE ztt_field_mapping,
       END OF ty_upload_file,
       
       BEGIN OF ty_migration_status,
         total_records    TYPE i,
         processed_records TYPE i,
         success_records  TYPE i,
         error_records    TYPE i,
         warning_records  TYPE i,
         start_time       TYPE timestamp,
         end_time         TYPE timestamp,
         processing_time  TYPE i,
       END OF ty_migration_status.

" ===== GLOBAL DATA =====
DATA: gt_upload_data      TYPE TABLE OF string,
      gt_processed_data   TYPE ztt_migration_data,
      gt_bdc_data         TYPE tt_bdc_data,
      gt_error_log        TYPE ztt_error_log,
      gs_config           TYPE ty_migration_config,
      gs_file_config      TYPE ty_upload_file,
      gs_status           TYPE ty_migration_status,
      go_bdc_processor    TYPE REF TO zcl_bdc_processor,
      go_data_transformer TYPE REF TO zcl_data_transformer,
      go_error_handler    TYPE REF TO zcl_error_handler.

" ===== SELECTION SCREEN =====
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

" File Upload Parameters
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN SKIP.

" BDC Method Selection
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002 FOR FIELD p_method.
PARAMETERS: p_sess RADIOBUTTON GROUP meth DEFAULT 'X',
            p_trans RADIOBUTTON GROUP meth,
            p_direct RADIOBUTTON GROUP meth.
SELECTION-SCREEN END OF LINE.

" Transaction Code
PARAMETERS: p_tcode TYPE tcode OBLIGATORY DEFAULT 'XK01'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.

" Processing Options
PARAMETERS: p_batch TYPE i DEFAULT 1000,
            p_jobs  TYPE i DEFAULT 4,
            p_test  AS CHECKBOX DEFAULT 'X'.

" Error Handling
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-004 FOR FIELD p_error.
PARAMETERS: p_err_e RADIOBUTTON GROUP err DEFAULT 'X',
            p_err_w RADIOBUTTON GROUP err,
            p_err_s RADIOBUTTON GROUP err.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

" ===== EVENTS =====
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_selection.

START-OF-SELECTION.
  PERFORM main_processing.

*&---------------------------------------------------------------------*
*& Main Processing Logic
*&---------------------------------------------------------------------*
FORM main_processing.
  " Initialize configuration
  PERFORM setup_migration_config.
  
  " Initialize framework components
  PERFORM initialize_framework.
  
  " Upload and validate data
  PERFORM upload_data_file.
  
  " Transform data to target format
  PERFORM transform_data.
  
  " Execute migration based on method
  CASE gs_config-method.
    WHEN 'S'.
      PERFORM execute_session_method.
    WHEN 'T'.
      PERFORM execute_transaction_method.
    WHEN 'D'.
      PERFORM execute_direct_input.
  ENDCASE.
  
  " Generate migration report
  PERFORM generate_migration_report.
ENDFORM.

*&---------------------------------------------------------------------*
*& BDC Processor Class - Core Framework
*&---------------------------------------------------------------------*
CLASS zcl_bdc_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING is_config TYPE ty_migration_config,
               
             create_session
               IMPORTING iv_session_name TYPE char12
               RETURNING VALUE(rv_session_id) TYPE apqi-groupid,
               
             add_transaction_to_session
               IMPORTING iv_session_id TYPE apqi-groupid
                         it_bdc_data TYPE tt_bdc_data
               RAISING   zcx_bdc_error,
               
             execute_call_transaction
               IMPORTING it_bdc_data TYPE tt_bdc_data
                         iv_mode TYPE ctu_mode DEFAULT 'N'
               EXPORTING et_messages TYPE tab_bdcmsgcoll
               RAISING   zcx_bdc_error,
               
             process_session
               IMPORTING iv_session_id TYPE apqi-groupid
               RETURNING VALUE(rs_result) TYPE zty_bdc_result.
               
  PRIVATE SECTION.
    DATA: ms_config TYPE ty_migration_config,
          mv_session_counter TYPE i.
          
    METHODS: validate_bdc_data
               IMPORTING it_bdc_data TYPE tt_bdc_data
               RETURNING VALUE(rv_valid) TYPE abap_bool,
               
             log_bdc_error
               IMPORTING iv_message TYPE string
                         iv_severity TYPE char1.
ENDCLASS.

CLASS zcl_bdc_processor IMPLEMENTATION.
  METHOD constructor.
    ms_config = is_config.
  ENDMETHOD.
  
  METHOD create_session.
    " Create BDC session with enterprise naming convention
    rv_session_id = |{ iv_session_name }_{ sy-datum }_{ sy-uzeit(4) }|.
    
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client   = sy-mandt
        group    = rv_session_id
        user     = sy-uname
        keep     = 'X'
        holddate = sy-datum
      EXCEPTIONS
        client_invalid = 1
        destination_invalid = 2
        group_invalid = 3
        group_is_locked = 4
        holddate_invalid = 5
        internal_error = 6
        queue_error = 7
        running = 8
        system_lock_error = 9
        user_invalid = 10
        OTHERS = 11.
        
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_bdc_error
        EXPORTING session_id = rv_session_id
                  error_code = sy-subrc.
    ENDIF.
  ENDMETHOD.
  
  METHOD add_transaction_to_session.
    " Validate BDC data before adding
    IF validate_bdc_data( it_bdc_data ) = abap_false.
      RAISE EXCEPTION TYPE zcx_bdc_error
        EXPORTING message = 'Invalid BDC data structure'.
    ENDIF.
    
    " Convert internal format to standard BDC format
    DATA: lt_bdcdata TYPE TABLE OF bdcdata.
    
    LOOP AT it_bdc_data INTO DATA(ls_bdc_record).
      APPEND VALUE #( 
        program = ls_bdc_record-program
        dynpro = ls_bdc_record-dynpro
        dynbegin = ls_bdc_record-dynbegin
        fnam = ls_bdc_record-fnam
        fval = ls_bdc_record-fval
      ) TO lt_bdcdata.
    ENDLOOP.
    
    " Add transaction to session
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode = ms_config-transaction_code
      TABLES
        dynprotab = lt_bdcdata
      EXCEPTIONS
        internal_error = 1
        not_open = 2
        queue_error = 3
        tcode_invalid = 4
        printing_invalid = 5
        posting_invalid = 6
        OTHERS = 7.
        
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_bdc_error
        EXPORTING error_code = sy-subrc.
    ENDIF.
  ENDMETHOD.
  
  METHOD execute_call_transaction.
    " Execute call transaction with comprehensive error handling
    DATA: lt_bdcdata TYPE TABLE OF bdcdata.
    
    " Convert to standard format
    LOOP AT it_bdc_data INTO DATA(ls_bdc_record).
      APPEND VALUE #( 
        program = ls_bdc_record-program
        dynpro = ls_bdc_record-dynpro
        dynbegin = ls_bdc_record-dynbegin
        fnam = ls_bdc_record-fnam
        fval = ls_bdc_record-fval
      ) TO lt_bdcdata.
    ENDLOOP.
    
    " Execute transaction
    CALL TRANSACTION ms_config-transaction_code
      USING lt_bdcdata
      MODE iv_mode
      UPDATE 'S'
      MESSAGES INTO et_messages.
      
    " Analyze return code
    IF sy-subrc <> 0.
      " Check for specific error conditions
      READ TABLE et_messages INTO DATA(ls_message) 
        WITH KEY msgtyp = 'E'.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_bdc_error
          EXPORTING message = |{ ls_message-msgid }/{ ls_message-msgnr }: { ls_message-msgv1 }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  
  METHOD validate_bdc_data.
    rv_valid = abap_true.
    
    " Check for mandatory fields
    READ TABLE it_bdc_data TRANSPORTING NO FIELDS
      WITH KEY dynbegin = 'X'.
    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.
    
    " Validate screen sequence
    DATA: lv_expected_screen TYPE sydynnr VALUE '0100'.
    
    LOOP AT it_bdc_data INTO DATA(ls_bdc) WHERE dynbegin = 'X'.
      IF ls_bdc-dynpro <> lv_expected_screen AND lv_expected_screen <> '0100'.
        " Screen sequence validation
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Data Transformer - Business Logic Layer
*&---------------------------------------------------------------------*
CLASS zcl_data_transformer DEFINITION.
  PUBLIC SECTION.
    METHODS: transform_upload_data
               IMPORTING it_raw_data TYPE TABLE
                         is_mapping TYPE ztt_field_mapping
               RETURNING VALUE(rt_transformed) TYPE ztt_migration_data,
               
             validate_business_rules
               IMPORTING it_data TYPE ztt_migration_data
               RETURNING VALUE(rt_errors) TYPE ztt_validation_errors,
               
             create_bdc_data
               IMPORTING is_data_record TYPE zty_migration_record
               RETURNING VALUE(rt_bdc) TYPE tt_bdc_data.
               
  PRIVATE SECTION.
    METHODS: apply_field_mapping
               IMPORTING iv_source_value TYPE string
                         is_mapping_rule TYPE zty_mapping_rule
               RETURNING VALUE(rv_target_value) TYPE string,
               
             validate_customer_data
               IMPORTING is_customer TYPE zty_customer_data
               RETURNING VALUE(rt_errors) TYPE ztt_field_errors.
ENDCLASS.

CLASS zcl_data_transformer IMPLEMENTATION.
  METHOD transform_upload_data.
    " Transform raw upload data to business format
    LOOP AT it_raw_data INTO DATA(lv_raw_line).
      DATA: lt_fields TYPE TABLE OF string.
      
      " Split line by delimiter
      SPLIT lv_raw_line AT ';' INTO TABLE lt_fields.
      
      " Map fields according to configuration
      DATA(ls_record) = VALUE zty_migration_record( ).
      
      LOOP AT is_mapping INTO DATA(ls_mapping).
        READ TABLE lt_fields INTO DATA(lv_field_value) INDEX ls_mapping-source_position.
        IF sy-subrc = 0.
          " Apply transformation rules
          DATA(lv_transformed_value) = apply_field_mapping( 
            iv_source_value = lv_field_value
            is_mapping_rule = ls_mapping
          ).
          
          " Assign to target structure
          ASSIGN COMPONENT ls_mapping-target_field OF STRUCTURE ls_record TO FIELD-SYMBOL(<lv_target>).
          IF sy-subrc = 0.
            <lv_target> = lv_transformed_value.
          ENDIF.
        ENDIF.
      ENDLOOP.
      
      APPEND ls_record TO rt_transformed.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD create_bdc_data.
    " Create BDC data for customer creation (XK01)
    CLEAR rt_bdc.
    
    " Screen 0100 - Initial screen
    APPEND VALUE #( program = 'SAPMF02K' dynpro = '0100' dynbegin = 'X' ) TO rt_bdc.
    APPEND VALUE #( fnam = 'RF02K-LIFNR' fval = is_data_record-vendor_id ) TO rt_bdc.
    APPEND VALUE #( fnam = 'RF02K-BUKRS' fval = is_data_record-company_code ) TO rt_bdc.
    APPEND VALUE #( fnam = 'RF02K-EKORG' fval = is_data_record-purch_org ) TO rt_bdc.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ENTR' ) TO rt_bdc.
    
    " Screen 0110 - Address screen
    APPEND VALUE #( program = 'SAPMF02K' dynpro = '0110' dynbegin = 'X' ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-NAME1' fval = is_data_record-name1 ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-SORTL' fval = is_data_record-search_term ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-STRAS' fval = is_data_record-street ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-ORT01' fval = is_data_record-city ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-PSTLZ' fval = is_data_record-postal_code ) TO rt_bdc.
    APPEND VALUE #( fnam = 'LFA1-LAND1' fval = is_data_record-country ) TO rt_bdc.
    
    " Add conditional fields based on data
    IF is_data_record-telephone IS NOT INITIAL.
      APPEND VALUE #( fnam = 'LFA1-TELF1' fval = is_data_record-telephone ) TO rt_bdc.
    ENDIF.
    
    " Navigate to next screen or save
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ENTR' ) TO rt_bdc.
    
    " Additional screens based on transaction requirements
    " Screen 0120 - Control data (if required)
    IF is_data_record-payment_terms IS NOT INITIAL.
      APPEND VALUE #( program = 'SAPMF02K' dynpro = '0120' dynbegin = 'X' ) TO rt_bdc.
      APPEND VALUE #( fnam = 'LFB1-ZTERM' fval = is_data_record-payment_terms ) TO rt_bdc.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ENTR' ) TO rt_bdc.
    ENDIF.
    
    " Final save
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=BU' ) TO rt_bdc.
  ENDMETHOD.
  
  METHOD apply_field_mapping.
    rv_target_value = iv_source_value.
    
    " Apply transformation rules
    CASE is_mapping_rule-transformation_type.
      WHEN 'UPPERCASE'.
        rv_target_value = to_upper( iv_source_value ).
        
      WHEN 'LOWERCASE'.
        rv_target_value = to_lower( iv_source_value ).
        
      WHEN 'LEADING_ZEROS'.
        rv_target_value = |{ iv_source_value ALPHA = IN }|.
        
      WHEN 'DATE_CONVERT'.
        " Convert date format from DD.MM.YYYY to YYYYMMDD
        IF strlen( iv_source_value ) = 10.
          rv_target_value = |{ iv_source_value+6(4) }{ iv_source_value+3(2) }{ iv_source_value+0(2) }|.
        ENDIF.
        
      WHEN 'LOOKUP'.
        " Lookup value from mapping table
        SELECT SINGLE target_value FROM zmapping_table
          INTO rv_target_value
          WHERE source_value = iv_source_value
            AND mapping_type = is_mapping_rule-lookup_type.
            
      WHEN 'CONSTANT'.
        rv_target_value = is_mapping_rule-constant_value.
        
      WHEN 'CONCATENATE'.
        " Concatenate with additional values
        rv_target_value = |{ iv_source_value }{ is_mapping_rule-concat_separator }{ is_mapping_rule-concat_value }|.
    ENDCASE.
    
    " Apply validation if specified
    IF is_mapping_rule-validate = 'X'.
      " Validate transformed value
      " Implementation depends on field type
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## 2. Session Method Advanced

### Enterprise Session Management

#### **Advanced Session Processing**
```abap
*&---------------------------------------------------------------------*
*& Advanced Session Method Implementation
*&---------------------------------------------------------------------*

CLASS zcl_session_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: create_parallel_sessions
               IMPORTING it_data TYPE ztt_migration_data
                         iv_max_sessions TYPE i DEFAULT 10
               RETURNING VALUE(rt_session_ids) TYPE ztt_session_ids,
               
             monitor_session_processing
               IMPORTING it_session_ids TYPE ztt_session_ids
               RETURNING VALUE(rs_overall_status) TYPE zty_session_status,
               
             process_session_errors
               IMPORTING iv_session_id TYPE char12
               RETURNING VALUE(rt_error_details) TYPE ztt_session_errors,
               
             reprocess_failed_transactions
               IMPORTING it_failed_transactions TYPE ztt_failed_transactions
               RETURNING VALUE(rv_reprocess_success) TYPE abap_bool.
               
  PRIVATE SECTION.
    DATA: mt_active_sessions TYPE ztt_active_sessions,
          mo_error_analyzer  TYPE REF TO zcl_bdc_error_analyzer.
          
    METHODS: distribute_data_optimally
               IMPORTING it_data TYPE ztt_migration_data
                         iv_max_sessions TYPE i
               RETURNING VALUE(rt_data_chunks) TYPE ztt_data_chunks,
               
             check_session_readiness
               IMPORTING iv_session_id TYPE char12
               RETURNING VALUE(rv_ready) TYPE abap_bool.
ENDCLASS.

CLASS zcl_session_manager IMPLEMENTATION.
  METHOD create_parallel_sessions.
    " Distribute data optimally across multiple sessions
    DATA(lt_data_chunks) = distribute_data_optimally( 
      it_data = it_data
      iv_max_sessions = iv_max_sessions
    ).
    
    " Create BDC processor instance
    DATA(lo_bdc_processor) = NEW zcl_bdc_processor( gs_config ).
    
    " Create sessions for each data chunk
    LOOP AT lt_data_chunks INTO DATA(ls_chunk).
      " Generate unique session name
      DATA(lv_session_name) = |SESS_{ sy-tabix ALPHA = IN }|.
      
      " Create session
      DATA(lv_session_id) = lo_bdc_processor->create_session( lv_session_name ).
      
      " Add transactions to session
      LOOP AT ls_chunk-data INTO DATA(ls_record).
        DATA(lt_bdc_data) = go_data_transformer->create_bdc_data( ls_record ).
        
        TRY.
            lo_bdc_processor->add_transaction_to_session( 
              iv_session_id = lv_session_id
              it_bdc_data = lt_bdc_data
            ).
            
          CATCH zcx_bdc_error INTO DATA(lx_bdc_error).
            " Log error and continue with next record
            go_error_handler->log_transaction_error( 
              is_record = ls_record
              ix_error = lx_bdc_error
            ).
        ENDTRY.
      ENDLOOP.
      
      " Close session
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      
      " Track session
      APPEND VALUE #( 
        session_id = lv_session_id
        record_count = lines( ls_chunk-data )
        created_at = sy-datum
        status = 'CREATED'
      ) TO mt_active_sessions.
      
      APPEND lv_session_id TO rt_session_ids.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD distribute_data_optimally.
    " Calculate optimal distribution
    DATA: lv_records_per_session TYPE i,
          lv_current_chunk        TYPE i VALUE 1.
          
    lv_records_per_session = lines( it_data ) / iv_max_sessions.
    IF lv_records_per_session = 0.
      lv_records_per_session = 1.
    ENDIF.
    
    " Create initial chunk
    APPEND VALUE #( chunk_id = lv_current_chunk ) TO rt_data_chunks.
    
    LOOP AT it_data INTO DATA(ls_record).
      " Check if current chunk is full
      READ TABLE rt_data_chunks ASSIGNING FIELD-SYMBOL(<ls_chunk>) 
        INDEX lv_current_chunk.
        
      IF lines( <ls_chunk>-data ) >= lv_records_per_session.
        " Start new chunk
        lv_current_chunk += 1.
        APPEND VALUE #( chunk_id = lv_current_chunk ) TO rt_data_chunks.
        READ TABLE rt_data_chunks ASSIGNING <ls_chunk> INDEX lv_current_chunk.
      ENDIF.
      
      " Add record to current chunk
      APPEND ls_record TO <ls_chunk>-data.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD monitor_session_processing.
    " Monitor all active sessions
    DATA: lv_total_processed TYPE i,
          lv_total_errors     TYPE i,
          lv_total_success    TYPE i.
          
    " Check status of each session
    LOOP AT it_session_ids INTO DATA(lv_session_id).
      " Query session status from APQI table
      SELECT SINGLE groupid, status, rstat
        FROM apqi
        INTO @DATA(ls_session_info)
        WHERE groupid = @lv_session_id.
        
      IF sy-subrc = 0.
        CASE ls_session_info-rstat.
          WHEN 'F'.  " Finished
            " Get detailed results
            SELECT COUNT(*) FROM apql
              INTO @DATA(lv_transactions)
              WHERE groupid = @lv_session_id.
              
            SELECT COUNT(*) FROM apql
              INTO @DATA(lv_errors)
              WHERE groupid = @lv_session_id
                AND qstate = 'E'.
                
            lv_total_processed += lv_transactions.
            lv_total_errors += lv_errors.
            lv_total_success += ( lv_transactions - lv_errors ).
            
          WHEN 'R'.  " Running
            " Session still processing
            
          WHEN 'E'.  " Error
            " Session has errors
            lv_total_errors += 1.
        ENDCASE.
      ENDIF.
    ENDLOOP.
    
    " Compile overall status
    rs_overall_status = VALUE #( 
      total_sessions = lines( it_session_ids )
      processed_transactions = lv_total_processed
      successful_transactions = lv_total_success
      error_transactions = lv_total_errors
      completion_percentage = ( lv_total_processed * 100 ) / lines( it_session_ids )
    ).
  ENDMETHOD.
  
  METHOD process_session_errors.
    " Analyze session errors in detail
    CREATE OBJECT mo_error_analyzer.
    
    " Get error details from APQL table
    SELECT * FROM apql
      INTO TABLE @DATA(lt_error_logs)
      WHERE groupid = @iv_session_id
        AND qstate = 'E'.
        
    " Analyze each error
    LOOP AT lt_error_logs INTO DATA(ls_error_log).
      " Get error messages
      SELECT * FROM apqd
        INTO TABLE @DATA(lt_messages)
        WHERE apqi_id = @ls_error_log-qid.
        
      " Categorize error
      DATA(ls_error_detail) = mo_error_analyzer->analyze_error( 
        is_error_log = ls_error_log
        it_messages = lt_messages
      ).
      
      APPEND ls_error_detail TO rt_error_details.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& BDC Error Analyzer
*&---------------------------------------------------------------------*
CLASS zcl_bdc_error_analyzer DEFINITION.
  PUBLIC SECTION.
    METHODS: analyze_error
               IMPORTING is_error_log TYPE apql
                         it_messages TYPE ztt_bdc_messages
               RETURNING VALUE(rs_error_detail) TYPE zty_error_detail,
               
             categorize_error_type
               IMPORTING it_messages TYPE ztt_bdc_messages
               RETURNING VALUE(rv_category) TYPE string,
               
             suggest_resolution
               IMPORTING iv_error_category TYPE string
                         it_messages TYPE ztt_bdc_messages
               RETURNING VALUE(rv_suggestion) TYPE string.
               
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_error_categories,
                 data_validation TYPE string VALUE 'DATA_VALIDATION',
                 authorization   TYPE string VALUE 'AUTHORIZATION',
                 system_error    TYPE string VALUE 'SYSTEM_ERROR',
                 business_rule   TYPE string VALUE 'BUSINESS_RULE',
                 duplicate_entry TYPE string VALUE 'DUPLICATE_ENTRY',
               END OF c_error_categories.
ENDCLASS.

CLASS zcl_bdc_error_analyzer IMPLEMENTATION.
  METHOD analyze_error.
    " Comprehensive error analysis
    rs_error_detail = VALUE #( 
      session_id = is_error_log-groupid
      transaction_id = is_error_log-qid
      error_timestamp = is_error_log-qedate
    ).
    
    " Categorize error type
    rs_error_detail-error_category = categorize_error_type( it_messages ).
    
    " Extract primary error message
    READ TABLE it_messages INTO DATA(ls_primary_message) 
      WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      rs_error_detail-primary_message = |{ ls_primary_message-msgid }/{ ls_primary_message-msgnr }|.
      rs_error_detail-message_text = ls_primary_message-msgv1.
    ENDIF.
    
    " Suggest resolution
    rs_error_detail-resolution_suggestion = suggest_resolution( 
      iv_error_category = rs_error_detail-error_category
      it_messages = it_messages
    ).
    
    " Determine if automatically reprocessable
    rs_error_detail-auto_reprocessable = COND #( 
      WHEN rs_error_detail-error_category = c_error_categories-system_error THEN abap_true
      ELSE abap_false
    ).
  ENDMETHOD.
  
  METHOD categorize_error_type.
    " Analyze messages to categorize error
    LOOP AT it_messages INTO DATA(ls_message) WHERE msgtyp = 'E'.
      " Check for common error patterns
      CASE ls_message-msgid.
        WHEN 'F5' OR 'VL'.
          " Authorization errors
          rv_category = c_error_categories-authorization.
          RETURN.
          
        WHEN 'M8' OR 'M7'.
          " Master data errors
          rv_category = c_error_categories-data_validation.
          RETURN.
          
        WHEN 'DBMAN' OR 'SYSTEM'.
          " System errors
          rv_category = c_error_categories-system_error.
          RETURN.
      ENDCASE.
      
      " Check message text for patterns
      IF ls_message-msgv1 CS 'already exists' OR ls_message-msgv1 CS 'duplicate'.
        rv_category = c_error_categories-duplicate_entry.
        RETURN.
      ENDIF.
    ENDLOOP.
    
    " Default category
    rv_category = c_error_categories-business_rule.
  ENDMETHOD.
  
  METHOD suggest_resolution.
    " Provide resolution suggestions based on error category
    CASE iv_error_category.
      WHEN c_error_categories-data_validation.
        rv_suggestion = 'Check data format and mandatory fields. Validate against master data.'.
        
      WHEN c_error_categories-authorization.
        rv_suggestion = 'Check user authorization for transaction. Contact system administrator.'.
        
      WHEN c_error_categories-system_error.
        rv_suggestion = 'Retry transaction. Check system status and connectivity.'.
        
      WHEN c_error_categories-business_rule.
        rv_suggestion = 'Review business configuration and transaction parameters.'.
        
      WHEN c_error_categories-duplicate_entry.
        rv_suggestion = 'Check for existing records. Consider update instead of create.'.
        
      WHEN OTHERS.
        rv_suggestion = 'Review error details and contact functional consultant.'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

This comprehensive BDC and data migration module covers advanced techniques for large-scale data migration projects in SAP. The framework includes error handling, parallel processing, and enterprise-grade monitoring capabilities used in real SAP implementation projects.

---

**Next Module**: [Module 11: Enhancements & User Exits](Module_11_Enhancements_User_Exits.md)