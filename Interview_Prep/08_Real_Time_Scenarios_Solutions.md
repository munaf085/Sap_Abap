# Real-Time Scenarios & Solutions 🚨

## Production Emergency Scenarios

### 🔥 **Scenario 1: Production System Slow Performance**
**Situation:** "Users complaining that a critical sales report is taking 15+ minutes to run in production, but it works fine in development."

**How to Resolve:**

**Step 1: Immediate Analysis**
```abap
" Check runtime analysis
" Go to SE30/SAT and analyze the program
" Look for expensive database operations

" Check ST05 (SQL Trace) for database bottlenecks
" Transaction: ST05
" Turn on trace → Run program → Turn off trace → Display trace
```

**Step 2: Common Root Causes & Solutions**
```abap
" Root Cause 1: Missing WHERE clause optimization
" ❌ Problematic code
SELECT * FROM vbak INTO TABLE lt_orders.
LOOP AT lt_orders INTO ls_order WHERE erdat = sy-datum.
  " Process only today's orders
ENDLOOP.

" ✅ Solution
SELECT * FROM vbak INTO TABLE lt_orders 
  WHERE erdat = sy-datum.

" Root Cause 2: Nested SELECT statements
" ❌ Problematic code  
LOOP AT lt_orders INTO ls_order.
  SELECT SINGLE kunnr FROM vbpa INTO lv_customer
    WHERE vbeln = ls_order-vbeln.
ENDLOOP.

" ✅ Solution using JOIN
SELECT o~vbeln, o~erdat, p~kunnr
  FROM vbak AS o
  INNER JOIN vbpa AS p ON o~vbeln = p~vbeln
  INTO TABLE lt_result
  WHERE o~erdat = sy-datum
    AND p~parvw = 'AG'.
```

**Step 3: Long-term Solution**
```abap
" Implement performance monitoring
CLASS cl_performance_monitor DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start_monitoring 
      RETURNING VALUE(rv_start_time) TYPE timestampl,
      end_monitoring 
      IMPORTING iv_start_time TYPE timestampl
                iv_operation TYPE string.
ENDCLASS.

CLASS cl_performance_monitor IMPLEMENTATION.
  METHOD start_monitoring.
    GET TIME STAMP FIELD rv_start_time.
  ENDMETHOD.
  
  METHOD end_monitoring.
    DATA: lv_end_time TYPE timestampl,
          lv_duration TYPE p DECIMALS 3.
    
    GET TIME STAMP FIELD lv_end_time.
    lv_duration = lv_end_time - iv_start_time.
    
    " Log if execution takes more than 5 seconds
    IF lv_duration > 5.
      " Log to monitoring table or send alert
      INSERT INTO zperformance_log VALUES (
        operation = iv_operation
        duration = lv_duration
        user = sy-uname
        timestamp = lv_end_time ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

### 🔥 **Scenario 2: Memory Dump in Production**
**Situation:** "A batch job is failing with TSV_TNEW_PAGE_ALLOC_FAILED memory dump every night."

**How to Resolve:**

**Step 1: Analyze the Dump (ST22)**
```abap
" Key information to gather:
" - Program name and line number where dump occurred
" - Memory consumption at time of dump
" - Input parameters that caused the issue
" - Time and frequency of occurrence
```

**Step 2: Identify Memory Leak**
```abap
" Common memory leak patterns:

" Problem 1: Loading entire large table
" ❌ Memory killer
DATA: lt_huge_table TYPE TABLE OF billing_document.
SELECT * FROM vbrk INTO TABLE lt_huge_table.  " 10M+ records

" ✅ Solution: Use PACKAGE SIZE
DATA: lt_package TYPE TABLE OF billing_document.
SELECT * FROM vbrk INTO TABLE lt_package
  PACKAGE SIZE 10000.
  
  " Process each package
  PERFORM process_billing_package USING lt_package.
  
  " Free memory after processing
  CLEAR lt_package.
  FREE lt_package.
ENDSELECT.

" Problem 2: String concatenation in loops
" ❌ Memory leak
DATA: lv_result TYPE string.
LOOP AT lt_large_table INTO ls_entry.
  CONCATENATE lv_result ls_entry-text INTO lv_result.
ENDLOOP.

" ✅ Solution: Use string table
DATA: lt_strings TYPE TABLE OF string.
LOOP AT lt_large_table INTO ls_entry.
  APPEND ls_entry-text TO lt_strings.
ENDLOOP.
CONCATENATE LINES OF lt_strings INTO lv_result.
```

**Step 3: Implement Memory Guards**
```abap
" Memory monitoring class
CLASS cl_memory_guard DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: check_memory_threshold
      IMPORTING iv_threshold_mb TYPE i DEFAULT 500
      RAISING cx_memory_exceeded.
ENDCLASS.

CLASS cl_memory_guard IMPLEMENTATION.
  METHOD check_memory_threshold.
    " Implementation to check current memory usage
    " Raise exception if threshold exceeded
  ENDMETHOD.
ENDCLASS.

" Usage in problematic program
LOOP AT lt_data INTO ls_entry.
  " Check memory every 1000 iterations
  IF sy-tabix MOD 1000 = 0.
    TRY.
        cl_memory_guard=>check_memory_threshold( 400 ).
      CATCH cx_memory_exceeded.
        " Implement graceful exit or batch processing
        MESSAGE 'Memory limit reached. Processing paused.' TYPE 'W'.
        EXIT.
    ENDTRY.
  ENDIF.
ENDLOOP.
```

---

## Data Integration Challenges

### 🔄 **Scenario 3: Legacy System Data Migration Failure**
**Situation:** "Customer master data migration from legacy system failed for 5,000 out of 50,000 records. Business needs this fixed ASAP."

**How to Resolve:**

**Step 1: Error Analysis**
```abap
" Create error analysis report
REPORT z_migration_error_analysis.

SELECT-OPTIONS: s_run FOR zmigration_log-run_id.

START-OF-SELECTION.
  " Analyze error patterns
  SELECT error_type, COUNT(*) AS error_count
    FROM zmigration_log
    INTO TABLE @DATA(lt_error_summary)
    WHERE run_id IN @s_run
      AND status = 'E'
    GROUP BY error_type
    ORDER BY error_count DESCENDING.
  
  " Display error breakdown
  LOOP AT lt_error_summary INTO DATA(ls_error).
    WRITE: / ls_error-error_type, ls_error-error_count.
  ENDLOOP.
```

**Step 2: Implement Error Recovery Strategy**
```abap
" Error recovery framework
CLASS cl_migration_recovery DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_failed_record,
             legacy_id TYPE c LENGTH 20,
             error_msg TYPE string,
             retry_count TYPE i,
           END OF ty_failed_record.
    
    METHODS: retry_failed_records
      IMPORTING it_failed_records TYPE TABLE OF ty_failed_record.
      
  PRIVATE SECTION.
    METHODS: fix_data_quality_issues
      CHANGING cs_customer TYPE customer_data,
             
             validate_business_rules
      IMPORTING is_customer TYPE customer_data
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS cl_migration_recovery IMPLEMENTATION.
  METHOD retry_failed_records.
    LOOP AT it_failed_records INTO DATA(ls_failed).
      " Get original data
      SELECT SINGLE * FROM zlegacy_customer 
        INTO @DATA(ls_legacy)
        WHERE legacy_id = @ls_failed-legacy_id.
      
      " Apply data fixes
      DATA(ls_customer) = CORRESPONDING customer_data( ls_legacy ).
      fix_data_quality_issues( CHANGING cs_customer = ls_customer ).
      
      " Validate before retry
      IF validate_business_rules( ls_customer ) = abap_true.
        " Retry creation
        CALL FUNCTION 'BAPI_CUSTOMER_CREATEFROMDATA1'
          EXPORTING
            pi_customer = ls_customer
          TABLES
            return = DATA(lt_return).
        
        " Log results
        READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc = 0.
          " Still failed - update retry count
        ELSE.
          " Success - mark as resolved
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD fix_data_quality_issues.
    " Common data fixes
    " 1. Trim whitespace
    cs_customer-name = |{ cs_customer-name CONDENSE }|.
    
    " 2. Fix phone number format
    REPLACE ALL OCCURRENCES OF '-' IN cs_customer-phone WITH ''.
    REPLACE ALL OCCURRENCES OF '(' IN cs_customer-phone WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN cs_customer-phone WITH ''.
    
    " 3. Standardize country codes
    CASE cs_customer-country.
      WHEN 'USA' OR 'UNITED STATES'.
        cs_customer-country = 'US'.
      WHEN 'UNITED KINGDOM' OR 'UK'.
        cs_customer-country = 'GB'.
    ENDCASE.
    
    " 4. Generate customer ID if missing
    IF cs_customer-customer_id IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object = 'CUSTOMER'
        IMPORTING
          number = cs_customer-customer_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

### 🔄 **Scenario 4: Real-time Interface Synchronization Issues**
**Situation:** "E-commerce website and SAP are out of sync. Product prices on website don't match SAP. Business is losing sales."

**How to Resolve:**

**Step 1: Immediate Reconciliation**
```abap
" Emergency price sync program
REPORT z_emergency_price_sync.

PARAMETERS: p_force TYPE abap_bool AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
  " Get price differences
  SELECT s~matnr, s~kbetr AS sap_price, w~price AS web_price
    FROM a304 AS s
    LEFT JOIN zweb_prices AS w ON s~matnr = w~material
    INTO TABLE @DATA(lt_price_diff)
    WHERE s~datbi >= @sy-datum
      AND s~datab <= @sy-datum
      AND ( w~price IS NULL OR s~kbetr <> w~price ).
  
  LOOP AT lt_price_diff INTO DATA(ls_diff).
    " Send immediate price update to e-commerce
    PERFORM send_price_update USING ls_diff-matnr ls_diff-sap_price.
  ENDLOOP.
  
  MESSAGE |{ lines( lt_price_diff ) } prices synchronized| TYPE 'I'.
```

**Step 2: Implement Real-time Sync Framework**
```abap
" Real-time synchronization using change documents
CLASS cl_price_sync_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_price_change
      IMPORTING iv_material TYPE matnr
               iv_old_price TYPE kbetr
               iv_new_price TYPE kbetr.
               
  PRIVATE SECTION.
    CLASS-METHODS: send_to_ecommerce
      IMPORTING iv_material TYPE matnr
               iv_price TYPE kbetr
      RETURNING VALUE(rv_success) TYPE abap_bool,
      
                   queue_for_retry
      IMPORTING iv_material TYPE matnr
               iv_price TYPE kbetr.
ENDCLASS.

CLASS cl_price_sync_handler IMPLEMENTATION.
  METHOD handle_price_change.
    " Immediate sync attempt
    IF send_to_ecommerce( iv_material = iv_material 
                         iv_price = iv_new_price ) = abap_false.
      " If immediate sync fails, queue for retry
      queue_for_retry( iv_material = iv_material 
                      iv_price = iv_new_price ).
    ENDIF.
  ENDMETHOD.
  
  METHOD send_to_ecommerce.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_json_data   TYPE string.
    
    " Prepare JSON payload
    lv_json_data = |{ "material": "{ iv_material }", |
                && |"price": { iv_price }, |
                && |"timestamp": "{ sy-datum }{ sy-uzeit }" }|.
    
    TRY.
        " Create HTTP client
        cl_http_client=>create_by_url(
          EXPORTING url = 'https://ecommerce-api.company.com/prices'
          IMPORTING client = lo_http_client ).
        
        " Set request details
        lo_http_client->request->set_method( if_http_request=>co_request_method_put ).
        lo_http_client->request->set_content_type( 'application/json' ).
        lo_http_client->request->set_cdata( lv_json_data ).
        
        " Send request
        lo_http_client->send( ).
        lo_http_client->receive( ).
        
        " Check response
        DATA(lv_status_code) = lo_http_client->response->get_status_code( ).
        rv_success = COND #( WHEN lv_status_code = 200 THEN abap_true 
                            ELSE abap_false ).
        
      CATCH cx_http_exception.
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.
  
  METHOD queue_for_retry.
    " Queue failed sync for retry mechanism
    INSERT INTO zprice_sync_queue VALUES (
      id = cl_system_uuid=>create_uuid_x16_static( )
      material = iv_material
      price = iv_price
      retry_count = 0
      status = 'P'
      created_at = sy-datum ).
  ENDMETHOD.
ENDCLASS.

" Integration in price change process (User Exit or Enhancement)
" This goes in price maintenance transaction enhancement
ENHANCEMENT z_price_sync_enhancement.
  " Trigger sync when price changes
  IF a304-new IS NOT INITIAL.
    cl_price_sync_handler=>handle_price_change(
      iv_material = a304-new-matnr
      iv_old_price = a304-old-kbetr
      iv_new_price = a304-new-kbetr ).
  ENDIF.
ENDENHANCEMENT.
```

---

## User Experience Issues

### 👥 **Scenario 5: Transaction Timeout Issues**
**Situation:** "Users getting timeout errors when processing large purchase orders with 500+ line items."

**How to Resolve:**

**Step 1: Analyze Timeout Root Cause**
```abap
" Timeout usually occurs due to:
" 1. Long-running validations in user exits
" 2. Extensive database operations during save
" 3. Complex pricing calculations
" 4. Memory constraints

" Check transaction SM50/SM66 for long-running processes
" Monitor SM13 for update records
```

**Step 2: Implement Background Processing**
```abap
" Background processing for large POs
CLASS cl_po_background_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: process_large_po
      IMPORTING iv_po_number TYPE ebeln
      RETURNING VALUE(rv_job_id) TYPE btcjob.
      
  PRIVATE SECTION.
    METHODS: submit_background_job
      IMPORTING iv_po_number TYPE ebeln
      RETURNING VALUE(rv_job_id) TYPE btcjob.
ENDCLASS.

CLASS cl_po_background_processor IMPLEMENTATION.
  METHOD process_large_po.
    " Check PO size
    SELECT COUNT(*) FROM ekpo INTO @DATA(lv_line_count)
      WHERE ebeln = @iv_po_number.
    
    IF lv_line_count > 100.
      " Process in background
      rv_job_id = submit_background_job( iv_po_number ).
      
      MESSAGE |PO { iv_po_number } queued for background processing. Job ID: { rv_job_id }| 
        TYPE 'I'.
    ELSE.
      " Process normally
      " Standard PO processing logic
    ENDIF.
  ENDMETHOD.
  
  METHOD submit_background_job.
    DATA: lv_jobname TYPE btcjob,
          lv_jobcount TYPE btcjobcnt.
    
    " Create background job
    lv_jobname = |PO_PROCESS_{ iv_po_number }|.
    
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.
    
    " Submit program
    SUBMIT z_po_background_processor
      WITH p_ponum = iv_po_number
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.
    
    " Start job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount = lv_jobcount
        jobname = lv_jobname
        strtimmed = 'X'.
    
    rv_job_id = lv_jobcount.
  ENDMETHOD.
ENDCLASS.
```

**Step 3: Optimize Transaction Performance**
```abap
" Performance optimization in user exits
" Instead of validating all items at once, validate incrementally

ENHANCEMENT z_po_performance_optimization.
  " Cache frequently used data
  DATA: gt_material_cache TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr.
  
  " Load cache once
  IF gt_material_cache IS INITIAL.
    SELECT * FROM mara INTO TABLE gt_material_cache
      FOR ALL ENTRIES IN ekpo
      WHERE matnr = ekpo-matnr.
  ENDIF.
  
  " Use cache for lookups instead of database calls
  READ TABLE gt_material_cache INTO DATA(ls_material)
    WITH KEY matnr = ekpo-matnr.
  
  " Batch database operations
  " Instead of individual saves, collect and save in batches
ENDENHANCEMENT.
```

---

### 👥 **Scenario 6: Authorization Issues in Production**
**Situation:** "Sales team can't access customer data after recent authorization changes. Critical for month-end closing."

**How to Resolve:**

**Step 1: Quick Authorization Check**
```abap
" Emergency authorization checker
REPORT z_auth_emergency_check.

PARAMETERS: p_user TYPE syuname OBLIGATORY,
           p_tcode TYPE sytcode.

START-OF-SELECTION.
  " Check transaction authorization
  AUTHORITY-CHECK OBJECT 'S_TCODE'
    ID 'TCD' FIELD p_tcode.
  
  IF sy-subrc = 0.
    WRITE: / 'Transaction access: OK'.
  ELSE.
    WRITE: / 'Transaction access: FAILED'.
  ENDIF.
  
  " Check customer authorization
  AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
    ID 'VKORG' FIELD '1000'
    ID 'VTWEG' FIELD '10'
    ID 'SPART' FIELD '00'
    ID 'ACTVT' FIELD '03'.
  
  IF sy-subrc = 0.
    WRITE: / 'Customer data access: OK'.
  ELSE.
    WRITE: / 'Customer data access: FAILED'.
    " Show missing authorizations
    PERFORM show_missing_auth USING p_user.
  ENDIF.
```

**Step 2: Implement Dynamic Authorization**
```abap
" Dynamic authorization assignment for emergency access
CLASS cl_emergency_auth_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: grant_emergency_access
      IMPORTING iv_user TYPE syuname
               iv_duration_hours TYPE i DEFAULT 24
      RETURNING VALUE(rv_success) TYPE abap_bool.
      
  PRIVATE SECTION.
    METHODS: create_temp_role
      IMPORTING iv_user TYPE syuname
      RETURNING VALUE(rv_role) TYPE agr_name,
      
             schedule_role_removal
      IMPORTING iv_user TYPE syuname
               iv_role TYPE agr_name
               iv_hours TYPE i.
ENDCLASS.

CLASS cl_emergency_auth_manager IMPLEMENTATION.
  METHOD grant_emergency_access.
    " Create temporary role with required authorizations
    DATA(lv_temp_role) = create_temp_role( iv_user ).
    
    " Assign role to user
    CALL FUNCTION 'PRGN_ASSIGN_ROLE_TO_USER'
      EXPORTING
        username = iv_user
        rolename = lv_temp_role.
    
    " Schedule automatic removal
    schedule_role_removal( 
      iv_user = iv_user
      iv_role = lv_temp_role
      iv_hours = iv_duration_hours ).
    
    rv_success = abap_true.
  ENDMETHOD.
  
  METHOD create_temp_role.
    " Generate unique temporary role name
    rv_role = |ZTEMP_{ iv_user }_{ sy-datum }_{ sy-uzeit }|.
    
    " Copy from template emergency role
    CALL FUNCTION 'PRGN_COPY_ROLE'
      EXPORTING
        source_role = 'ZEMERGENCY_TEMPLATE'
        target_role = rv_role
        copy_texts = 'X'.
  ENDMETHOD.
  
  METHOD schedule_role_removal.
    " Create background job to remove role after specified time
    DATA: lv_start_time TYPE sy-uzeit.
    
    lv_start_time = sy-uzeit + ( iv_hours * 3600 ).
    
    SUBMIT z_remove_temp_role
      WITH p_user = iv_user
      WITH p_role = iv_role
      VIA JOB |ROLE_CLEANUP_{ iv_user }|
      AT lv_start_time
      AND RETURN.
  ENDMETHOD.
ENDCLASS.
```

---

## Integration Challenges

### 🔗 **Scenario 7: Third-party API Integration Failure**
**Situation:** "Payment gateway integration is failing randomly. Some orders are processed, others fail without clear error messages."

**How to Resolve:**

**Step 1: Implement Robust Error Handling**
```abap
" Resilient API integration framework
CLASS cl_payment_gateway_client DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_payment_response,
             success TYPE abap_bool,
             transaction_id TYPE string,
             error_code TYPE string,
             error_message TYPE string,
           END OF ty_payment_response.
    
    METHODS: process_payment
      IMPORTING iv_amount TYPE p
               iv_currency TYPE waers
               iv_card_token TYPE string
      RETURNING VALUE(rs_response) TYPE ty_payment_response.
      
  PRIVATE SECTION.
    CONSTANTS: c_max_retries TYPE i VALUE 3,
               c_retry_delay TYPE i VALUE 2.  " seconds
    
    METHODS: call_api_with_retry
      IMPORTING iv_payload TYPE string
               iv_attempt TYPE i DEFAULT 1
      RETURNING VALUE(rs_response) TYPE ty_payment_response,
      
             log_api_call
      IMPORTING iv_payload TYPE string
               is_response TYPE ty_payment_response
               iv_attempt TYPE i.
ENDCLASS.

CLASS cl_payment_gateway_client IMPLEMENTATION.
  METHOD process_payment.
    " Prepare payload
    DATA(lv_payload) = |{ "amount": { iv_amount }, |
                    && |"currency": "{ iv_currency }", |
                    && |"card_token": "{ iv_card_token }", |
                    && |"merchant_id": "12345" }|.
    
    " Call API with retry logic
    rs_response = call_api_with_retry( iv_payload = lv_payload ).
  ENDMETHOD.
  
  METHOD call_api_with_retry.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_response_json TYPE string.
    
    DO c_max_retries TIMES.
      TRY.
          " Create HTTP client with timeout
          cl_http_client=>create_by_url(
            EXPORTING 
              url = 'https://payment-gateway.com/api/charge'
            IMPORTING 
              client = lo_http_client ).
          
          " Set timeout (30 seconds)
          lo_http_client->set_timeout( timeout = 30 ).
          
          " Set headers
          lo_http_client->request->set_method( 'POST' ).
          lo_http_client->request->set_content_type( 'application/json' ).
          lo_http_client->request->set_header_field( 
            name = 'Authorization' 
            value = 'Bearer YOUR_API_KEY' ).
          
          " Set payload
          lo_http_client->request->set_cdata( iv_payload ).
          
          " Send request
          lo_http_client->send( ).
          lo_http_client->receive( ).
          
          " Parse response
          lv_response_json = lo_http_client->response->get_cdata( ).
          DATA(lv_status_code) = lo_http_client->response->get_status_code( ).
          
          IF lv_status_code = 200.
            " Success - parse JSON response
            /ui2/cl_json=>deserialize( 
              EXPORTING json = lv_response_json
              CHANGING data = rs_response ).
            
            " Log successful call
            log_api_call( 
              iv_payload = iv_payload
              is_response = rs_response
              iv_attempt = sy-index ).
            
            EXIT.  " Exit retry loop on success
            
          ELSE.
            " HTTP error - prepare for retry
            rs_response-success = abap_false.
            rs_response-error_code = |HTTP_{ lv_status_code }|.
            rs_response-error_message = lv_response_json.
          ENDIF.
          
        CATCH cx_http_exception INTO DATA(lx_http).
          " Network error - prepare for retry
          rs_response-success = abap_false.
          rs_response-error_code = 'NETWORK_ERROR'.
          rs_response-error_message = lx_http->get_text( ).
      ENDTRY.
      
      " If not the last attempt, wait before retry
      IF sy-index < c_max_retries.
        WAIT UP TO c_retry_delay SECONDS.
      ENDIF.
    ENDDO.
    
    " Log final result
    log_api_call( 
      iv_payload = iv_payload
      is_response = rs_response
      iv_attempt = iv_attempt ).
  ENDMETHOD.
  
  METHOD log_api_call.
    " Log all API calls for debugging and monitoring
    INSERT INTO zapi_call_log VALUES (
      id = cl_system_uuid=>create_uuid_x16_static( )
      api_name = 'PAYMENT_GATEWAY'
      payload = iv_payload
      response = |Success: { is_response-success }, |
              && |Code: { is_response-error_code }, |
              && |Message: { is_response-error_message }|
      attempt = iv_attempt
      timestamp = sy-datum
      user = sy-uname ).
  ENDMETHOD.
ENDCLASS.
```

**Step 2: Implement Circuit Breaker Pattern**
```abap
" Circuit breaker to prevent cascade failures
CLASS cl_circuit_breaker DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM te_circuit_state,
             closed,    " Normal operation
             open,      " Failing - reject calls
             half_open, " Testing if service recovered
           END OF ENUM te_circuit_state.
    
    METHODS: is_call_allowed RETURNING VALUE(rv_allowed) TYPE abap_bool,
             record_success,
             record_failure.
             
  PRIVATE SECTION.
    DATA: mv_state TYPE te_circuit_state VALUE closed,
          mv_failure_count TYPE i,
          mv_last_failure_time TYPE timestampl,
          mv_failure_threshold TYPE i VALUE 5,
          mv_timeout_seconds TYPE i VALUE 60.
ENDCLASS.

CLASS cl_circuit_breaker IMPLEMENTATION.
  METHOD is_call_allowed.
    CASE mv_state.
      WHEN closed.
        rv_allowed = abap_true.
        
      WHEN open.
        " Check if timeout period has passed
        GET TIME STAMP FIELD DATA(lv_current_time).
        IF lv_current_time - mv_last_failure_time > mv_timeout_seconds.
          mv_state = half_open.
          rv_allowed = abap_true.
        ELSE.
          rv_allowed = abap_false.
        ENDIF.
        
      WHEN half_open.
        rv_allowed = abap_true.
    ENDCASE.
  ENDMETHOD.
  
  METHOD record_success.
    mv_failure_count = 0.
    mv_state = closed.
  ENDMETHOD.
  
  METHOD record_failure.
    mv_failure_count = mv_failure_count + 1.
    GET TIME STAMP FIELD mv_last_failure_time.
    
    IF mv_failure_count >= mv_failure_threshold.
      mv_state = open.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

These real-time scenarios cover the most common production issues you'll face as an ABAP developer. Each scenario includes immediate troubleshooting steps, root cause analysis, and long-term prevention strategies. The solutions demonstrate enterprise-level thinking with proper error handling, monitoring, and resilience patterns.

**Interview Tip:** When discussing these scenarios, always mention:
1. **Immediate impact assessment** 
2. **Quick fixes for business continuity**
3. **Root cause analysis**
4. **Long-term prevention strategies**
5. **Monitoring and alerting improvements**