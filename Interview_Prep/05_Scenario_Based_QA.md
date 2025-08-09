# Scenario-Based Questions - Interview Q&A

## Data Migration and Integration

### ⭐⭐⭐ Q1: You need to migrate 50,000 customer records from a legacy system to SAP. How would you approach this?
**Answer:**

**Analysis Phase:**
1. **Data Mapping**: Map legacy fields to SAP customer master fields
2. **Data Quality**: Identify data cleansing requirements
3. **Volume Planning**: Design for 50K records with performance considerations
4. **Error Handling**: Plan for validation failures and rollback scenarios

**Implementation Approach:**
```abap
" Option 1: BDC Session Method (Recommended for large volumes)
REPORT z_customer_migration.

TYPES: BEGIN OF ty_legacy_customer,
         customer_id   TYPE c LENGTH 10,
         customer_name TYPE c LENGTH 35,
         address       TYPE c LENGTH 35,
         city          TYPE c LENGTH 25,
         country       TYPE c LENGTH 3,
       END OF ty_legacy_customer.

DATA: lt_legacy_data TYPE TABLE OF ty_legacy_customer,
      lt_bdcdata     TYPE TABLE OF bdcdata,
      lv_session_id  TYPE apqi-groupid.

" 1. Read legacy data in chunks
PERFORM read_legacy_data TABLES lt_legacy_data.

" 2. Create BDC session
lv_session_id = |CUSTMIG_{ sy-datum }_{ sy-uzeit }|.
CALL FUNCTION 'BDC_OPEN_GROUP'
  EXPORTING
    client = sy-mandt
    group  = lv_session_id
    user   = sy-uname
    keep   = 'X'.

" 3. Process in batches of 1000
LOOP AT lt_legacy_data INTO DATA(ls_legacy).
  " Validate and transform data
  PERFORM validate_customer_data USING ls_legacy.
  
  " Create BDC data for XD01
  PERFORM create_bdc_data USING ls_legacy TABLES lt_bdcdata.
  
  " Insert into session
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode = 'XD01'
    TABLES
      dynprotab = lt_bdcdata.
  
  " Commit every 1000 records
  IF sy-tabix MOD 1000 = 0.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    " Process session
    SUBMIT rsbdcsub WITH mappe = lv_session_id AND RETURN.
    " Open new session
    lv_session_id = |CUSTMIG_{ sy-datum }_{ sy-uzeit }_{ sy-tabix }|.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client = sy-mandt
        group  = lv_session_id
        user   = sy-uname.
  ENDIF.
ENDLOOP.

CALL FUNCTION 'BDC_CLOSE_GROUP'.

" Option 2: BAPI Method (Better error handling)
LOOP AT lt_legacy_data INTO ls_legacy.
  " Map to BAPI structure
  DATA(ls_customer_data) = VALUE bapi0002_1(
    customer     = ls_legacy-customer_id
    name         = ls_legacy-customer_name
    addr_city    = ls_legacy-city
    addr_country = ls_legacy-country ).
  
  " Call BAPI
  CALL FUNCTION 'BAPI_CUSTOMER_CREATEFROMDATA1'
    EXPORTING
      pi_customer = ls_customer_data
    IMPORTING
      po_customer = lv_new_customer
    TABLES
      return      = lt_return.
  
  " Check for errors
  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc = 0.
    " Log error and continue
    PERFORM log_error USING ls_legacy lt_return.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
    PERFORM log_success USING ls_legacy lv_new_customer.
  ENDIF.
ENDLOOP.
```

**Best Practices:**
- Use parallel processing for large volumes
- Implement comprehensive error logging
- Create rollback procedures
- Perform data validation before migration
- Test with sample data first

### ⭐⭐⭐ Q2: How would you handle a scenario where you need to synchronize data between SAP and an external e-commerce system in real-time?
**Answer:**

**Architecture Design:**
```abap
" Real-time integration using REST APIs and Change Documents

" 1. Change Document Configuration
" Table: ZCUSTOMER_SYNC
" Change document object: ZCUSTOMER
" Function group: ZCUST_CHANGE_DOC

" 2. Trigger class for real-time sync
CLASS cl_customer_sync DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: sync_customer_change
      IMPORTING iv_customer_id TYPE kunnr
               iv_change_type TYPE c  " I=Insert, U=Update, D=Delete
               RAISING cx_rest_client_exception.
ENDCLASS.

CLASS cl_customer_sync IMPLEMENTATION.
  METHOD sync_customer_change.
    DATA: lo_rest_client TYPE REF TO cl_rest_client,
          lv_url         TYPE string,
          lo_json        TYPE REF TO /ui2/cl_json.
    
    " 1. Get customer data
    SELECT SINGLE * FROM kna1 INTO @DATA(ls_customer)
      WHERE kunnr = @iv_customer_id.
    
    " 2. Transform to JSON
    DATA(lv_json_data) = /ui2/cl_json=>serialize( data = ls_customer ).
    
    " 3. Prepare REST call
    lv_url = |https://ecommerce-api.company.com/customers/{ iv_customer_id }|.
    cl_http_client=>create_by_url(
      EXPORTING url = lv_url
      IMPORTING client = DATA(lo_http_client) ).
    
    lo_rest_client = NEW cl_rest_client( io_http_client = lo_http_client ).
    
    " 4. Set headers
    lo_rest_client->if_rest_client~set_request_header(
      iv_name = 'Content-Type' 
      iv_value = 'application/json' ).
    lo_rest_client->if_rest_client~set_request_header(
      iv_name = 'Authorization'
      iv_value = 'Bearer <API_TOKEN>' ).
    
    " 5. Make appropriate REST call
    CASE iv_change_type.
      WHEN 'I'.  " Insert
        lo_rest_client->if_rest_client~post( lv_json_data ).
      WHEN 'U'.  " Update  
        lo_rest_client->if_rest_client~put( lv_json_data ).
      WHEN 'D'.  " Delete
        lo_rest_client->if_rest_client~delete( ).
    ENDCASE.
    
    " 6. Handle response
    DATA(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).
    IF lo_response->get_header_field( '~status_code' ) <> '200'.
      " Log error and potentially queue for retry
      RAISE EXCEPTION TYPE cx_rest_client_exception.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" 3. Implement in customer change exits
" Enhancement SAPMF02D - User exit for customer master changes
ENHANCEMENT z_customer_sync.
  CASE sy-tcode.
    WHEN 'XD01' OR 'XD02' OR 'XD03'.
      " Trigger sync after successful update
      TRY.
          cl_customer_sync=>sync_customer_change(
            iv_customer_id = kna1-kunnr
            iv_change_type = COND #( WHEN sy-tcode = 'XD01' THEN 'I'
                                    WHEN sy-tcode = 'XD02' THEN 'U' ) ).
        CATCH cx_rest_client_exception.
          " Queue for later processing or show warning
          MESSAGE 'Customer sync failed. Will retry later.' TYPE 'W'.
      ENDTRY.
  ENDCASE.
ENDENHANCEMENT.

" 4. Error handling and retry mechanism
CLASS cl_sync_queue DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sync_queue,
             id           TYPE guid_32,
             object_type  TYPE c LENGTH 10,
             object_id    TYPE c LENGTH 20,
             change_type  TYPE c LENGTH 1,
             retry_count  TYPE i,
             created_at   TYPE timestampl,
             status       TYPE c LENGTH 1,  " P=Pending, S=Success, F=Failed
           END OF ty_sync_queue.
    
    CLASS-METHODS: enqueue_sync IMPORTING is_sync_item TYPE ty_sync_queue,
                   process_queue.
ENDCLASS.
```

**Alternative Approaches:**
1. **Message Queuing**: Use SAP PI/PO or Cloud Integration
2. **Webhooks**: Implement inbound services for external system notifications
3. **Batch Synchronization**: Scheduled jobs for non-critical updates

### ⭐⭐⭐ Q3: A custom ABAP program is causing memory dumps in production. How do you troubleshoot and resolve this?
**Answer:**

**Troubleshooting Steps:**

**1. Immediate Analysis (ST22)**
```abap
" Check short dump analysis
" Key information to gather:
" - Error type (STORAGE_PARAMETERS_WRONG_SET, TSV_TNEW_PAGE_ALLOC_FAILED)
" - Program name and line number
" - User context and input parameters
" - System resources at time of dump
```

**2. Runtime Analysis (SAT)**
```abap
" Analyze program performance
REPORT z_memory_analysis.

" Implement memory monitoring
DATA: lv_memory_before TYPE i,
      lv_memory_after  TYPE i,
      lv_memory_used   TYPE i.

" Get initial memory consumption
CALL FUNCTION 'SYSTEM_MEMORY_INFO'
  IMPORTING
    memory_used = lv_memory_before.

" Your problematic code section
PERFORM memory_intensive_operation.

" Check memory after operation
CALL FUNCTION 'SYSTEM_MEMORY_INFO'
  IMPORTING
    memory_used = lv_memory_after.

lv_memory_used = lv_memory_after - lv_memory_before.
WRITE: / 'Memory consumed:', lv_memory_used, 'bytes'.
```

**3. Common Memory Issues and Solutions:**

```abap
" Issue 1: Large internal tables without proper management
" ❌ Problematic code
DATA: lt_huge_table TYPE TABLE OF large_structure.
SELECT * FROM huge_table INTO TABLE lt_huge_table.  " Loads millions of records

" ✅ Solution
DATA: lt_chunk TYPE TABLE OF large_structure.
SELECT * FROM huge_table INTO TABLE lt_chunk
  PACKAGE SIZE 10000.
  
  " Process chunk
  PERFORM process_chunk USING lt_chunk.
  
  " Free memory
  CLEAR lt_chunk.
  FREE lt_chunk.
ENDSELECT.

" Issue 2: String concatenation in loops
" ❌ Memory leak
DATA: lv_result TYPE string.
LOOP AT lt_large_table INTO ls_entry.
  CONCATENATE lv_result ls_entry-text INTO lv_result.
ENDLOOP.

" ✅ Efficient solution
DATA: lt_strings TYPE TABLE OF string.
LOOP AT lt_large_table INTO ls_entry.
  APPEND ls_entry-text TO lt_strings.
ENDLOOP.
CONCATENATE LINES OF lt_strings INTO lv_result.

" Issue 3: Deep nested structures
" ❌ Excessive memory allocation
TYPES: BEGIN OF ty_complex,
         header TYPE complex_header,
         items  TYPE TABLE OF complex_items,
         details TYPE TABLE OF complex_details,
       END OF ty_complex.
DATA: lt_complex TYPE TABLE OF ty_complex.

" ✅ Flat structure approach
TYPES: BEGIN OF ty_flat,
         header_field1  TYPE ...,
         header_field2  TYPE ...,
         item_info     TYPE string,  " Serialized or summarized
       END OF ty_flat.
```

**4. Implement Memory Guards:**
```abap
CLASS cl_memory_guard DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: check_memory_limit
      IMPORTING iv_threshold TYPE i DEFAULT 80  " Percentage
      RAISING cx_memory_limit_exceeded.
      
  PRIVATE SECTION.
    CLASS-DATA: cv_initial_memory TYPE i.
ENDCLASS.

CLASS cl_memory_guard IMPLEMENTATION.
  METHOD check_memory_limit.
    DATA: lv_current_memory TYPE i,
          lv_percentage     TYPE p DECIMALS 2.
    
    CALL FUNCTION 'SYSTEM_MEMORY_INFO'
      IMPORTING
        memory_used = lv_current_memory.
    
    " Calculate memory usage percentage
    lv_percentage = ( lv_current_memory / cv_initial_memory ) * 100.
    
    IF lv_percentage > iv_threshold.
      RAISE EXCEPTION TYPE cx_memory_limit_exceeded
        EXPORTING percentage = lv_percentage.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Usage in problematic program
TRY.
    LOOP AT lt_large_dataset INTO ls_entry.
      " Check memory every 1000 iterations
      IF sy-tabix MOD 1000 = 0.
        cl_memory_guard=>check_memory_limit( ).
      ENDIF.
      
      " Process entry
      PERFORM process_entry USING ls_entry.
    ENDLOOP.
    
  CATCH cx_memory_limit_exceeded.
    MESSAGE 'Process stopped due to memory constraints' TYPE 'W'.
    " Implement graceful exit or batch processing
ENDTRY.
```

## Complex Business Scenarios

### ⭐⭐⭐ Q4: Design a solution for automatic price calculation based on complex business rules involving discounts, taxes, and regional variations.
**Answer:**

```abap
" Pricing Engine Design with Strategy Pattern

" 1. Interface for pricing strategies
INTERFACE if_pricing_strategy.
  METHODS: calculate_price
    IMPORTING ir_context TYPE REF TO cl_pricing_context
    RETURNING VALUE(rv_price) TYPE p DECIMALS 2.
ENDINTERFACE.

" 2. Pricing context with all necessary data
CLASS cl_pricing_context DEFINITION.
  PUBLIC SECTION.
    DATA: customer_id     TYPE kunnr,
          material_id     TYPE matnr,
          quantity        TYPE kwmeng,
          base_price      TYPE p DECIMALS 2,
          customer_group  TYPE kdgrp,
          region          TYPE regio,
          order_date      TYPE dats,
          currency        TYPE waers.
          
    METHODS: constructor
      IMPORTING iv_customer TYPE kunnr
               iv_material TYPE matnr
               iv_quantity TYPE kwmeng.
ENDCLASS.

" 3. Concrete pricing strategies
CLASS cl_volume_discount DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_pricing_strategy.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_volume_tier,
             min_qty    TYPE kwmeng,
             max_qty    TYPE kwmeng,
             discount_pct TYPE p DECIMALS 2,
           END OF ty_volume_tier.
    DATA: mt_volume_tiers TYPE TABLE OF ty_volume_tier.
ENDCLASS.

CLASS cl_volume_discount IMPLEMENTATION.
  METHOD if_pricing_strategy~calculate_price.
    DATA(lv_discount) = VALUE p DECIMALS 2( ).
    
    " Load volume discount tiers
    SELECT * FROM zvolume_discounts INTO TABLE @DATA(lt_tiers)
      WHERE material = @ir_context->material_id.
    
    " Find applicable tier
    LOOP AT lt_tiers INTO DATA(ls_tier).
      IF ir_context->quantity BETWEEN ls_tier-min_qty AND ls_tier-max_qty.
        lv_discount = ls_tier-discount_pct.
        EXIT.
      ENDIF.
    ENDLOOP.
    
    rv_price = ir_context->base_price * ( 1 - lv_discount / 100 ).
  ENDMETHOD.
ENDCLASS.

CLASS cl_customer_discount DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_pricing_strategy.
ENDCLASS.

CLASS cl_customer_discount IMPLEMENTATION.
  METHOD if_pricing_strategy~calculate_price.
    " Get customer-specific discount
    SELECT SINGLE discount_pct FROM zcustomer_discounts 
      INTO @DATA(lv_discount)
      WHERE customer = @ir_context->customer_id
        AND material = @ir_context->material_id
        AND valid_from <= @ir_context->order_date
        AND valid_to >= @ir_context->order_date.
    
    IF sy-subrc <> 0.
      " Fallback to customer group discount
      SELECT SINGLE discount_pct FROM zcustomer_group_discounts
        INTO @lv_discount
        WHERE customer_group = @ir_context->customer_group.
    ENDIF.
    
    rv_price = ir_context->base_price * ( 1 - lv_discount / 100 ).
  ENDMETHOD.
ENDCLASS.

CLASS cl_regional_pricing DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_pricing_strategy.
ENDCLASS.

CLASS cl_regional_pricing IMPLEMENTATION.
  METHOD if_pricing_strategy~calculate_price.
    " Apply regional multiplier
    SELECT SINGLE price_factor FROM zregional_factors
      INTO @DATA(lv_factor)
      WHERE region = @ir_context->region
        AND material_group = ( SELECT SINGLE matkl FROM mara 
                              WHERE matnr = @ir_context->material_id ).
    
    IF sy-subrc = 0.
      rv_price = ir_context->base_price * lv_factor.
    ELSE.
      rv_price = ir_context->base_price.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" 4. Tax calculation strategy
CLASS cl_tax_calculator DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_pricing_strategy.
ENDCLASS.

CLASS cl_tax_calculator IMPLEMENTATION.
  METHOD if_pricing_strategy~calculate_price.
    DATA: lv_tax_rate TYPE p DECIMALS 2.
    
    " Get tax rate based on material and region
    SELECT SINGLE tax_rate FROM ztax_rates
      INTO @lv_tax_rate
      WHERE country = ( SELECT SINGLE land1 FROM t005s
                       WHERE regio = @ir_context->region )
        AND tax_code = ( SELECT SINGLE taxm1 FROM marc
                        WHERE matnr = @ir_context->material_id ).
    
    rv_price = ir_context->base_price * ( 1 + lv_tax_rate / 100 ).
  ENDMETHOD.
ENDCLASS.

" 5. Main pricing engine using Chain of Responsibility
CLASS cl_pricing_engine DEFINITION.
  PUBLIC SECTION.
    TYPES: tt_strategies TYPE TABLE OF REF TO if_pricing_strategy.
    
    METHODS: add_strategy IMPORTING ir_strategy TYPE REF TO if_pricing_strategy,
             calculate_final_price 
               IMPORTING ir_context TYPE REF TO cl_pricing_context
               RETURNING VALUE(rv_final_price) TYPE p DECIMALS 2.
               
  PRIVATE SECTION.
    DATA: mt_strategies TYPE tt_strategies.
ENDCLASS.

CLASS cl_pricing_engine IMPLEMENTATION.
  METHOD add_strategy.
    APPEND ir_strategy TO mt_strategies.
  ENDMETHOD.
  
  METHOD calculate_final_price.
    rv_final_price = ir_context->base_price.
    
    " Apply each strategy in sequence
    LOOP AT mt_strategies INTO DATA(lr_strategy).
      " Update context with current price
      ir_context->base_price = rv_final_price.
      
      " Calculate new price with current strategy
      rv_final_price = lr_strategy->calculate_price( ir_context ).
      
      " Log each step for audit trail
      INSERT INTO zprice_calculation_log VALUES (
        guid = cl_system_uuid=>create_uuid_x16_static( )
        context_id = ir_context->customer_id
        material = ir_context->material_id
        strategy = cl_abap_classdescr=>describe_by_object_ref( lr_strategy )->get_relative_name( )
        input_price = ir_context->base_price
        output_price = rv_final_price
        timestamp = sy-datum
      ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" 6. Usage example
DATA: lo_engine    TYPE REF TO cl_pricing_engine,
      lo_context   TYPE REF TO cl_pricing_context,
      lv_final_price TYPE p DECIMALS 2.

" Initialize pricing engine
lo_engine = NEW cl_pricing_engine( ).

" Add strategies in order of application
lo_engine->add_strategy( NEW cl_volume_discount( ) ).
lo_engine->add_strategy( NEW cl_customer_discount( ) ).
lo_engine->add_strategy( NEW cl_regional_pricing( ) ).
lo_engine->add_strategy( NEW cl_tax_calculator( ) ).

" Create pricing context
lo_context = NEW cl_pricing_context( 
  iv_customer = '1000001'
  iv_material = 'MAT001'
  iv_quantity = 100 ).

" Calculate final price
lv_final_price = lo_engine->calculate_final_price( lo_context ).

WRITE: / 'Final Price:', lv_final_price.
```

### ⭐⭐⭐ Q5: How would you implement a workflow approval system for purchase orders with dynamic approval hierarchy?
**Answer:**

```abap
" Dynamic Approval Workflow System

" 1. Approval hierarchy configuration
TYPES: BEGIN OF ty_approval_rule,
         rule_id      TYPE guid_32,
         company_code TYPE bukrs,
         po_type      TYPE esart,
         min_amount   TYPE p DECIMALS 2,
         max_amount   TYPE p DECIMALS 2,
         approver_level TYPE i,
         approver_role  TYPE agr_name,
         parallel_approval TYPE abap_bool,
       END OF ty_approval_rule.

" 2. Workflow instance management
CLASS cl_po_workflow DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_workflow_instance,
             workflow_id   TYPE guid_32,
             po_number     TYPE ebeln,
             current_level TYPE i,
             status        TYPE c LENGTH 1,  " P=Pending, A=Approved, R=Rejected
             created_by    TYPE syuname,
             created_at    TYPE timestampl,
           END OF ty_workflow_instance.
           
    TYPES: BEGIN OF ty_approval_step,
             step_id       TYPE guid_32,
             workflow_id   TYPE guid_32,
             approver_id   TYPE syuname,
             approval_level TYPE i,
             status        TYPE c LENGTH 1,
             approved_at   TYPE timestampl,
             comments      TYPE string,
           END OF ty_approval_step.
    
    METHODS: initiate_approval
      IMPORTING iv_po_number TYPE ebeln
      RETURNING VALUE(rv_workflow_id) TYPE guid_32
      RAISING cx_workflow_error,
      
             process_approval
      IMPORTING iv_workflow_id TYPE guid_32
               iv_approver_id TYPE syuname
               iv_decision    TYPE c  " A=Approve, R=Reject
               iv_comments    TYPE string OPTIONAL
      RAISING cx_workflow_error.
      
  PRIVATE SECTION.
    METHODS: get_approval_rules
      IMPORTING iv_po_number TYPE ebeln
      RETURNING VALUE(rt_rules) TYPE TABLE OF ty_approval_rule,
      
             get_next_approvers
      IMPORTING iv_workflow_id TYPE guid_32
               iv_level TYPE i
      RETURNING VALUE(rt_approvers) TYPE TABLE OF syuname,
      
             send_notification
      IMPORTING iv_approver TYPE syuname
               iv_po_number TYPE ebeln
               iv_action TYPE c,  " N=New, A=Approved, R=Rejected
               
             check_completion
      IMPORTING iv_workflow_id TYPE guid_32.
ENDCLASS.

CLASS cl_po_workflow IMPLEMENTATION.
  METHOD initiate_approval.
    DATA: ls_workflow TYPE ty_workflow_instance,
          lt_rules    TYPE TABLE OF ty_approval_rule,
          lt_approvers TYPE TABLE OF syuname.
    
    " Get PO details
    SELECT SINGLE * FROM ekko INTO @DATA(ls_po)
      WHERE ebeln = @iv_po_number.
    
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_workflow_error
        EXPORTING textid = cx_workflow_error=>po_not_found.
    ENDIF.
    
    " Get applicable approval rules
    lt_rules = get_approval_rules( iv_po_number ).
    
    " Create workflow instance
    ls_workflow-workflow_id = cl_system_uuid=>create_uuid_x16_static( ).
    ls_workflow-po_number = iv_po_number.
    ls_workflow-current_level = 1.
    ls_workflow-status = 'P'.
    ls_workflow-created_by = sy-uname.
    GET TIME STAMP FIELD ls_workflow-created_at.
    
    " Save workflow instance
    INSERT zworkflow_instances FROM ls_workflow.
    
    " Create initial approval steps
    LOOP AT lt_rules INTO DATA(ls_rule).
      " Get users for this approval level
      SELECT username FROM agr_users INTO TABLE @DATA(lt_users)
        WHERE agr_name = @ls_rule-approver_role.
      
      " Create approval steps for each approver
      LOOP AT lt_users INTO DATA(lv_user).
        INSERT zapproval_steps FROM VALUE #(
          step_id = cl_system_uuid=>create_uuid_x16_static( )
          workflow_id = ls_workflow-workflow_id
          approver_id = lv_user
          approval_level = ls_rule-approver_level
          status = 'P' ).
      ENDLOOP.
    ENDLOOP.
    
    " Send notifications to first level approvers
    lt_approvers = get_next_approvers( 
      iv_workflow_id = ls_workflow-workflow_id 
      iv_level = 1 ).
    
    LOOP AT lt_approvers INTO DATA(lv_approver).
      send_notification( 
        iv_approver = lv_approver
        iv_po_number = iv_po_number
        iv_action = 'N' ).
    ENDLOOP.
    
    rv_workflow_id = ls_workflow-workflow_id.
  ENDMETHOD.
  
  METHOD process_approval.
    DATA: ls_step TYPE ty_approval_step.
    
    " Update approval step
    UPDATE zapproval_steps SET
      status = @iv_decision,
      approved_at = @sy-datum,
      comments = @iv_comments
    WHERE workflow_id = @iv_workflow_id
      AND approver_id = @iv_approver_id
      AND status = 'P'.
    
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_workflow_error
        EXPORTING textid = cx_workflow_error=>invalid_approval.
    ENDIF.
    
    " Handle rejection
    IF iv_decision = 'R'.
      " Reject entire workflow
      UPDATE zworkflow_instances SET status = 'R'
        WHERE workflow_id = @iv_workflow_id.
      
      " Send rejection notification
      SELECT SINGLE po_number FROM zworkflow_instances
        INTO @DATA(lv_po_number)
        WHERE workflow_id = @iv_workflow_id.
      
      send_notification(
        iv_approver = sy-uname
        iv_po_number = lv_po_number
        iv_action = 'R' ).
      RETURN.
    ENDIF.
    
    " Check if current level is complete
    check_completion( iv_workflow_id ).
  ENDMETHOD.
  
  METHOD get_approval_rules.
    " Get PO header data
    SELECT SINGLE bukrs, bsart, netwr FROM ekko
      INTO @DATA(ls_po_header)
      WHERE ebeln = @iv_po_number.
    
    " Get applicable rules based on amount and criteria
    SELECT * FROM zapproval_rules INTO TABLE @rt_rules
      WHERE company_code = @ls_po_header-bukrs
        AND ( po_type = @ls_po_header-bsart OR po_type = '*' )
        AND min_amount <= @ls_po_header-netwr
        AND ( max_amount >= @ls_po_header-netwr OR max_amount = 0 )
      ORDER BY approver_level.
  ENDMETHOD.
  
  METHOD get_next_approvers.
    SELECT approver_id FROM zapproval_steps
      INTO TABLE @rt_approvers
      WHERE workflow_id = @iv_workflow_id
        AND approval_level = @iv_level
        AND status = 'P'.
  ENDMETHOD.
  
  METHOD send_notification.
    " Implementation would integrate with:
    " 1. SAP Business Workflow
    " 2. Email notifications
    " 3. Mobile push notifications
    " 4. Portal notifications
    
    DATA: lo_notification TYPE REF TO cl_notification_service.
    
    lo_notification = cl_notification_service=>get_instance( ).
    
    CASE iv_action.
      WHEN 'N'.
        lo_notification->send_approval_request(
          iv_recipient = iv_approver
          iv_po_number = iv_po_number ).
      WHEN 'A'.
        lo_notification->send_approval_confirmation(
          iv_recipient = iv_approver
          iv_po_number = iv_po_number ).
      WHEN 'R'.
        lo_notification->send_rejection_notification(
          iv_recipient = iv_approver
          iv_po_number = iv_po_number ).
    ENDCASE.
  ENDMETHOD.
  
  METHOD check_completion.
    " Check if all approvals for current level are complete
    SELECT COUNT(*) FROM zapproval_steps
      INTO @DATA(lv_pending_count)
      WHERE workflow_id = @iv_workflow_id
        AND approval_level = ( SELECT current_level FROM zworkflow_instances
                              WHERE workflow_id = @iv_workflow_id )
        AND status = 'P'.
    
    IF lv_pending_count = 0.
      " Current level complete, move to next level
      DATA(lv_next_level) = VALUE i( ).
      SELECT MAX( approval_level ) FROM zapproval_steps
        INTO @lv_next_level
        WHERE workflow_id = @iv_workflow_id
          AND approval_level > ( SELECT current_level FROM zworkflow_instances
                                 WHERE workflow_id = @iv_workflow_id ).
      
      IF lv_next_level > 0.
        " Move to next level
        UPDATE zworkflow_instances SET current_level = @lv_next_level
          WHERE workflow_id = @iv_workflow_id.
        
        " Send notifications to next level approvers
        DATA(lt_next_approvers) = get_next_approvers(
          iv_workflow_id = iv_workflow_id
          iv_level = lv_next_level ).
        
        LOOP AT lt_next_approvers INTO DATA(lv_approver).
          SELECT SINGLE po_number FROM zworkflow_instances
            INTO @DATA(lv_po_number)
            WHERE workflow_id = @iv_workflow_id.
          
          send_notification(
            iv_approver = lv_approver
            iv_po_number = lv_po_number
            iv_action = 'N' ).
        ENDLOOP.
      ELSE.
        " All levels approved - complete workflow
        UPDATE zworkflow_instances SET status = 'A'
          WHERE workflow_id = @iv_workflow_id.
        
        " Release PO or perform final actions
        PERFORM release_purchase_order USING iv_workflow_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" 3. Integration with standard SAP PO release strategy
FORM release_purchase_order USING iv_workflow_id TYPE guid_32.
  " Get PO number
  SELECT SINGLE po_number FROM zworkflow_instances
    INTO @DATA(lv_po_number)
    WHERE workflow_id = @iv_workflow_id.
  
  " Call standard PO release function
  CALL FUNCTION 'ME_RELEASE_PURCHASING_DOCUMENT'
    EXPORTING
      ebeln = lv_po_number
      frgke = 'X'
    EXCEPTIONS
      OTHERS = 1.
  
  IF sy-subrc = 0.
    " Send final approval notification
    " Update audit trail
    " Trigger downstream processes
  ENDIF.
ENDFORM.
```

This scenario demonstrates enterprise-level workflow design with dynamic approval routing, proper error handling, notification integration, and audit trails - all essential for real-world SAP implementations.