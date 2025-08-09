*&---------------------------------------------------------------------*
*& Class: ZCL_ORDER_PROCESSOR
*& Description: Advanced Order Processing Engine
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Business Purpose: Handle complex order processing workflows
*&---------------------------------------------------------------------*

CLASS zcl_order_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    
    " Order processing types
    TYPES: BEGIN OF ty_order_context,
             order_id        TYPE vbeln_va,
             customer_id     TYPE kunnr,
             order_type      TYPE auart,
             sales_org       TYPE vkorg,
             distribution    TYPE vtweg,
             division        TYPE spart,
             order_reason    TYPE augru,
             pricing_date    TYPE prsdt,
             delivery_date   TYPE edatu,
             currency        TYPE waers,
             total_amount    TYPE netwr_ak,
             source_system   TYPE string,
             priority        TYPE char1,
           END OF ty_order_context.
           
    TYPES: BEGIN OF ty_order_item,
             item_number     TYPE posnr_va,
             material_id     TYPE matnr,
             plant           TYPE werks_d,
             quantity        TYPE kwmeng,
             unit            TYPE vrkme,
             unit_price      TYPE netpr,
             discount_pct    TYPE abap_dec(5,2),
             tax_code        TYPE mwskz,
             delivery_date   TYPE edatu,
             item_category   TYPE pstyv,
             schedule_lines  TYPE TABLE OF ty_schedule_line,
           END OF ty_order_item.
           
    TYPES: BEGIN OF ty_schedule_line,
             schedule_number TYPE etenr,
             quantity        TYPE wmeng,
             delivery_date   TYPE edatu,
             confirmed_qty   TYPE bmeng,
           END OF ty_schedule_line.
           
    TYPES: BEGIN OF ty_processing_result,
             success         TYPE abap_bool,
             order_number    TYPE vbeln_va,
             error_messages  TYPE TABLE OF string,
             warning_messages TYPE TABLE OF string,
             processing_time TYPE p DECIMALS 3,
             steps_completed TYPE i,
           END OF ty_processing_result.
    
    " Events for order processing
    EVENTS: order_created
              EXPORTING VALUE(iv_order_id) TYPE vbeln_va
                       VALUE(is_context) TYPE ty_order_context,
                       
            order_failed
              EXPORTING VALUE(iv_order_id) TYPE vbeln_va
                       VALUE(it_errors) TYPE TABLE,
                       
            inventory_check_required
              EXPORTING VALUE(iv_material_id) TYPE matnr
                       VALUE(iv_quantity) TYPE kwmeng.
    
    " Main processing methods
    METHODS: constructor,
    
             process_order
               IMPORTING is_order_context TYPE ty_order_context
                        it_order_items TYPE TABLE OF ty_order_item
               RETURNING VALUE(rs_result) TYPE ty_processing_result
               RAISING   cx_order_processing_error,
               
             process_bulk_orders
               IMPORTING it_orders TYPE TABLE OF ty_order_context
               RETURNING VALUE(rt_results) TYPE TABLE OF ty_processing_result,
               
             validate_order
               IMPORTING is_order_context TYPE ty_order_context
                        it_order_items TYPE TABLE OF ty_order_item
               RETURNING VALUE(rt_issues) TYPE TABLE OF string
               RAISING   cx_validation_error,
               
             calculate_pricing
               IMPORTING is_order_context TYPE ty_order_context
                        it_order_items TYPE TABLE OF ty_order_item
               CHANGING  ct_order_items TYPE TABLE OF ty_order_item
               RAISING   cx_pricing_error,
               
             check_inventory_availability
               IMPORTING it_order_items TYPE TABLE OF ty_order_item
               RETURNING VALUE(rt_availability) TYPE TABLE OF ty_availability_result
               RAISING   cx_inventory_error,
               
             create_delivery_proposal
               IMPORTING iv_order_id TYPE vbeln_va
               RETURNING VALUE(rv_delivery_id) TYPE vbeln_vl
               RAISING   cx_delivery_error.

  PRIVATE SECTION.
    
    " Component dependencies
    DATA: mo_logger           TYPE REF TO zcl_application_logger,
          mo_pricing_engine   TYPE REF TO zcl_pricing_engine,
          mo_inventory_mgr    TYPE REF TO zcl_inventory_manager,
          mo_credit_mgr       TYPE REF TO zcl_credit_manager,
          mo_workflow_mgr     TYPE REF TO zcl_workflow_manager,
          mo_notification_svc TYPE REF TO zcl_notification_service.
    
    " Configuration
    DATA: mv_parallel_processing TYPE abap_bool,
          mv_auto_delivery      TYPE abap_bool,
          mv_credit_check_required TYPE abap_bool.
    
    " Processing steps enumeration
    TYPES: BEGIN OF ENUM te_processing_step,
             validation,
             credit_check,
             inventory_check,
             pricing_calculation,
             order_creation,
             delivery_proposal,
             notification,
           END OF ENUM te_processing_step.
    
    " Private methods
    METHODS: initialize_components,
    
             load_configuration,
             
             execute_processing_step
               IMPORTING iv_step TYPE te_processing_step
                        is_context TYPE ty_order_context
                        it_items TYPE TABLE OF ty_order_item
               CHANGING  cs_result TYPE ty_processing_result,
               
             perform_credit_check
               IMPORTING is_context TYPE ty_order_context
               RETURNING VALUE(rs_credit_result) TYPE ty_credit_check_result
               RAISING   cx_credit_check_error,
               
             create_sap_order
               IMPORTING is_context TYPE ty_order_context
                        it_items TYPE TABLE OF ty_order_item
               RETURNING VALUE(rv_order_number) TYPE vbeln_va
               RAISING   cx_order_creation_error,
               
             handle_order_errors
               IMPORTING it_errors TYPE TABLE OF bapiret2
               RETURNING VALUE(rt_error_messages) TYPE TABLE OF string,
               
             send_order_confirmation
               IMPORTING iv_order_id TYPE vbeln_va
                        is_context TYPE ty_order_context,
               
             log_processing_metrics
               IMPORTING is_result TYPE ty_processing_result
                        iv_start_time TYPE timestampl.

ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.

  METHOD constructor.
    initialize_components( ).
    load_configuration( ).
  ENDMETHOD.

  METHOD initialize_components.
    " Initialize all dependent services
    mo_logger = NEW zcl_application_logger( ).
    mo_pricing_engine = NEW zcl_pricing_engine( ).
    mo_inventory_mgr = NEW zcl_inventory_manager( ).
    mo_credit_mgr = NEW zcl_credit_manager( ).
    mo_workflow_mgr = NEW zcl_workflow_manager( ).
    mo_notification_svc = NEW zcl_notification_service( ).
  ENDMETHOD.

  METHOD load_configuration.
    " Load order processing configuration
    SELECT SINGLE * FROM zorder_config 
      INTO @DATA(ls_config)
      WHERE config_type = 'ORDER_PROCESSING'.
    
    IF sy-subrc = 0.
      mv_parallel_processing = ls_config-parallel_processing.
      mv_auto_delivery = ls_config-auto_delivery_creation.
      mv_credit_check_required = ls_config-credit_check_required.
    ELSE.
      " Default configuration
      mv_parallel_processing = abap_true.
      mv_auto_delivery = abap_true.
      mv_credit_check_required = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD process_order.
    " Main order processing workflow
    
    DATA: lv_start_time TYPE timestampl,
          lv_step_counter TYPE i.
    
    GET TIME STAMP FIELD lv_start_time.
    
    " Initialize result
    rs_result = VALUE #( success = abap_false steps_completed = 0 ).
    
    TRY.
        " Step 1: Validation
        execute_processing_step(
          iv_step = validation
          is_context = is_order_context
          it_items = it_order_items
          CHANGING cs_result = rs_result ).
        
        " Step 2: Credit Check (if required)
        IF mv_credit_check_required = abap_true.
          execute_processing_step(
            iv_step = credit_check
            is_context = is_order_context
            it_items = it_order_items
            CHANGING cs_result = rs_result ).
        ENDIF.
        
        " Step 3: Inventory Check
        execute_processing_step(
          iv_step = inventory_check
          is_context = is_order_context
          it_items = it_order_items
          CHANGING cs_result = rs_result ).
        
        " Step 4: Pricing Calculation
        execute_processing_step(
          iv_step = pricing_calculation
          is_context = is_order_context
          it_items = it_order_items
          CHANGING cs_result = rs_result ).
        
        " Step 5: Order Creation
        execute_processing_step(
          iv_step = order_creation
          is_context = is_order_context
          it_items = it_order_items
          CHANGING cs_result = rs_result ).
        
        " Step 6: Delivery Proposal (if auto-delivery enabled)
        IF mv_auto_delivery = abap_true AND rs_result-order_number IS NOT INITIAL.
          execute_processing_step(
            iv_step = delivery_proposal
            is_context = is_order_context
            it_items = it_order_items
            CHANGING cs_result = rs_result ).
        ENDIF.
        
        " Step 7: Notifications
        execute_processing_step(
          iv_step = notification
          is_context = is_order_context
          it_items = it_order_items
          CHANGING cs_result = rs_result ).
        
        " Mark as successful
        rs_result-success = abap_true.
        
        " Raise success event
        RAISE EVENT order_created
          EXPORTING
            iv_order_id = rs_result-order_number
            is_context = is_order_context.
        
      CATCH cx_order_processing_error INTO DATA(lx_error).
        " Handle processing errors
        APPEND lx_error->get_text( ) TO rs_result-error_messages.
        rs_result-success = abap_false.
        
        " Raise failure event
        RAISE EVENT order_failed
          EXPORTING
            iv_order_id = rs_result-order_number
            it_errors = rs_result-error_messages.
        
      CATCH cx_validation_error 
            cx_pricing_error 
            cx_inventory_error 
            cx_credit_check_error INTO DATA(lx_general_error).
        " Handle specific errors
        APPEND lx_general_error->get_text( ) TO rs_result-error_messages.
        rs_result-success = abap_false.
        
    ENDTRY.
    
    " Calculate processing time
    DATA(lv_end_time) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    rs_result-processing_time = lv_end_time - lv_start_time.
    
    " Log metrics
    log_processing_metrics( 
      is_result = rs_result
      iv_start_time = lv_start_time ).
    
  ENDMETHOD.

  METHOD execute_processing_step.
    " Execute individual processing step
    
    CASE iv_step.
      WHEN validation.
        " Validate order data
        DATA(lt_validation_issues) = validate_order( 
          is_order_context = is_context
          it_order_items = it_items ).
        
        IF lines( lt_validation_issues ) > 0.
          cs_result-error_messages = lt_validation_issues.
          RAISE EXCEPTION TYPE cx_order_processing_error
            EXPORTING
              textid = cx_order_processing_error=>validation_failed.
        ENDIF.
        
      WHEN credit_check.
        " Perform credit check
        DATA(ls_credit_result) = perform_credit_check( is_context ).
        
        IF ls_credit_result-approved = abap_false.
          APPEND |Credit check failed: { ls_credit_result-reason }| TO cs_result-error_messages.
          RAISE EXCEPTION TYPE cx_order_processing_error
            EXPORTING
              textid = cx_order_processing_error=>credit_check_failed.
        ENDIF.
        
      WHEN inventory_check.
        " Check inventory availability
        DATA(lt_availability) = check_inventory_availability( it_items ).
        
        LOOP AT lt_availability INTO DATA(ls_availability) WHERE available = abap_false.
          APPEND |Insufficient inventory for material { ls_availability-material_id }| 
            TO cs_result-warning_messages.
        ENDLOOP.
        
      WHEN pricing_calculation.
        " Calculate pricing
        DATA(lt_items_with_pricing) = it_items.
        calculate_pricing(
          is_order_context = is_context
          it_order_items = it_items
          CHANGING ct_order_items = lt_items_with_pricing ).
        
      WHEN order_creation.
        " Create SAP order
        cs_result-order_number = create_sap_order(
          is_context = is_context
          it_items = it_items ).
        
      WHEN delivery_proposal.
        " Create delivery proposal
        IF cs_result-order_number IS NOT INITIAL.
          DATA(lv_delivery_id) = create_delivery_proposal( cs_result-order_number ).
          mo_logger->log_info( |Delivery { lv_delivery_id } created for order { cs_result-order_number }| ).
        ENDIF.
        
      WHEN notification.
        " Send notifications
        send_order_confirmation(
          iv_order_id = cs_result-order_number
          is_context = is_context ).
        
    ENDCASE.
    
    " Increment step counter
    cs_result-steps_completed = cs_result-steps_completed + 1.
    
  ENDMETHOD.

  METHOD validate_order.
    " Comprehensive order validation
    
    " Validate order context
    IF is_order_context-customer_id IS INITIAL.
      APPEND 'Customer ID is required' TO rt_issues.
    ENDIF.
    
    IF is_order_context-sales_org IS INITIAL.
      APPEND 'Sales organization is required' TO rt_issues.
    ENDIF.
    
    IF is_order_context-order_type IS INITIAL.
      APPEND 'Order type is required' TO rt_issues.
    ENDIF.
    
    " Validate customer exists and is active
    SELECT SINGLE aufsd FROM kna1 
      INTO @DATA(lv_customer_block)
      WHERE kunnr = @is_order_context-customer_id.
    
    IF sy-subrc <> 0.
      APPEND |Customer { is_order_context-customer_id } does not exist| TO rt_issues.
    ELSEIF lv_customer_block IS NOT INITIAL.
      APPEND |Customer { is_order_context-customer_id } is blocked| TO rt_issues.
    ENDIF.
    
    " Validate order items
    IF lines( it_order_items ) = 0.
      APPEND 'At least one order item is required' TO rt_issues.
    ENDIF.
    
    LOOP AT it_order_items INTO DATA(ls_item).
      " Validate material
      SELECT SINGLE lvorm FROM mara 
        INTO @DATA(lv_material_deletion)
        WHERE matnr = @ls_item-material_id.
      
      IF sy-subrc <> 0.
        APPEND |Material { ls_item-material_id } does not exist| TO rt_issues.
      ELSEIF lv_material_deletion = 'X'.
        APPEND |Material { ls_item-material_id } is flagged for deletion| TO rt_issues.
      ENDIF.
      
      " Validate quantity
      IF ls_item-quantity <= 0.
        APPEND |Invalid quantity for material { ls_item-material_id }| TO rt_issues.
      ENDIF.
      
      " Validate plant
      SELECT SINGLE werks FROM marc 
        INTO @DATA(lv_check_plant)
        WHERE matnr = @ls_item-material_id
          AND werks = @ls_item-plant.
      
      IF sy-subrc <> 0.
        APPEND |Material { ls_item-material_id } not maintained for plant { ls_item-plant }| TO rt_issues.
      ENDIF.
    ENDLOOP.
    
  ENDMETHOD.

  METHOD perform_credit_check.
    " Perform credit check for customer
    
    rs_credit_result = VALUE #( approved = abap_false ).
    
    TRY.
        " Get customer credit data
        SELECT SINGLE klimk, skfor FROM knkk
          INTO @DATA(ls_credit_data)
          WHERE kunnr = @is_context-customer_id
            AND kkber = '1000'.  " Credit control area
        
        IF sy-subrc = 0.
          " Calculate current exposure
          DATA(lv_current_exposure) = mo_credit_mgr->get_customer_exposure( is_context-customer_id ).
          
          " Check if order amount would exceed credit limit
          DATA(lv_total_exposure) = lv_current_exposure + is_context-total_amount.
          
          IF lv_total_exposure <= ls_credit_data-klimk.
            rs_credit_result = VALUE #( 
              approved = abap_true
              credit_limit = ls_credit_data-klimk
              current_exposure = lv_current_exposure
              available_credit = ls_credit_data-klimk - lv_current_exposure ).
          ELSE.
            rs_credit_result = VALUE #(
              approved = abap_false
              reason = |Credit limit exceeded. Limit: { ls_credit_data-klimk }, Required: { lv_total_exposure }|
              credit_limit = ls_credit_data-klimk
              current_exposure = lv_current_exposure ).
          ENDIF.
        ELSE.
          rs_credit_result-reason = 'No credit data found for customer'.
        ENDIF.
        
      CATCH cx_credit_check_error INTO DATA(lx_error).
        rs_credit_result = VALUE #(
          approved = abap_false
          reason = lx_error->get_text( ) ).
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD create_sap_order.
    " Create SAP sales order using BAPI
    
    DATA: ls_order_header TYPE bapisdhd1,
          lt_order_items  TYPE TABLE OF bapisditm,
          lt_order_return TYPE TABLE OF bapiret2,
          lt_schedules    TYPE TABLE OF bapischdl.
    
    " Map order header
    ls_order_header = VALUE #(
      doc_type = is_context-order_type
      sales_org = is_context-sales_org
      distr_chan = is_context-distribution
      division = is_context-division
      sold_to = is_context-customer_id
      req_date_h = is_context-delivery_date
      currency = is_context-currency
      ref_doc = |ECOM-{ is_context-order_id }| ).
    
    " Map order items
    LOOP AT it_items INTO DATA(ls_item).
      APPEND VALUE #(
        itm_number = |{ sy-tabix ALPHA = IN }|
        material = |{ ls_item-material_id ALPHA = IN }|
        plant = ls_item-plant
        req_qty = ls_item-quantity
        sales_unit = ls_item-unit
        net_price = ls_item-unit_price
        currency = is_context-currency
        item_categ = ls_item-item_category
      ) TO lt_order_items.
      
      " Add schedule lines if provided
      LOOP AT ls_item-schedule_lines INTO DATA(ls_schedule).
        APPEND VALUE #(
          itm_number = |{ sy-tabix ALPHA = IN }|
          sched_line = |{ sy-tabix ALPHA = IN }|
          req_qty = ls_schedule-quantity
          req_date = ls_schedule-delivery_date
        ) TO lt_schedules.
      ENDLOOP.
    ENDLOOP.
    
    " Call BAPI to create order
    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        order_header_in = ls_order_header
      IMPORTING
        salesdocument = rv_order_number
      TABLES
        return = lt_order_return
        order_items_in = lt_order_items
        order_schedules_in = lt_schedules.
    
    " Check for errors
    READ TABLE lt_order_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    
    IF sy-subrc = 0.
      " Handle errors
      DATA(lt_error_messages) = handle_order_errors( lt_order_return ).
      
      RAISE EXCEPTION TYPE cx_order_creation_error
        EXPORTING
          textid = cx_order_creation_error=>bapi_failed
          error_messages = lt_error_messages.
    ELSE.
      " Commit the transaction
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      
      mo_logger->log_info( |Sales order { rv_order_number } created successfully| ).
    ENDIF.
    
  ENDMETHOD.

  METHOD process_bulk_orders.
    " Process multiple orders efficiently
    
    DATA: lv_processed_count TYPE i,
          lv_success_count   TYPE i,
          lv_error_count     TYPE i.
    
    mo_logger->log_info( |Starting bulk order processing for { lines( it_orders ) } orders| ).
    
    IF mv_parallel_processing = abap_true.
      " Parallel processing using tasks
      LOOP AT it_orders INTO DATA(ls_order).
        " Create processing task
        DATA(lv_task_name) = |ORDER_TASK_{ sy-tabix }|.
        
        " Process order in parallel (simplified - would use RFC)
        APPEND process_order(
          is_order_context = ls_order
          it_order_items = VALUE #( ) ) TO rt_results.
      ENDLOOP.
    ELSE.
      " Sequential processing
      LOOP AT it_orders INTO ls_order.
        TRY.
            DATA(ls_result) = process_order(
              is_order_context = ls_order
              it_order_items = VALUE #( ) ).
            
            APPEND ls_result TO rt_results.
            
            IF ls_result-success = abap_true.
              lv_success_count = lv_success_count + 1.
            ELSE.
              lv_error_count = lv_error_count + 1.
            ENDIF.
            
          CATCH cx_order_processing_error INTO DATA(lx_error).
            " Log error and continue
            mo_logger->log_error( |Order processing failed: { lx_error->get_text( ) }| ).
            lv_error_count = lv_error_count + 1.
        ENDTRY.
        
        lv_processed_count = lv_processed_count + 1.
      ENDLOOP.
    ENDIF.
    
    mo_logger->log_info( |Bulk processing completed. Processed: { lv_processed_count }, Success: { lv_success_count }, Errors: { lv_error_count }| ).
    
  ENDMETHOD.

ENDCLASS.