*&---------------------------------------------------------------------*
*& Class: ZCL_ECOMMERCE_API_HANDLER
*& Description: E-Commerce API Integration Handler
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Business Purpose: Handle all e-commerce platform integrations
*&---------------------------------------------------------------------*

CLASS zcl_ecommerce_api_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    
    " Business object types
    TYPES: BEGIN OF ty_customer_data,
             customer_id   TYPE kunnr,
             email        TYPE ad_smtpadr,
             first_name   TYPE name1_gp,
             last_name    TYPE name2_gp,
             phone        TYPE telf1,
             street       TYPE stras,
             city         TYPE ort01_gp,
             postal_code  TYPE pstlz,
             country      TYPE land1_gp,
             created_date TYPE dats,
             last_login   TYPE timestampl,
           END OF ty_customer_data.
           
    TYPES: BEGIN OF ty_product_data,
             material_id     TYPE matnr,
             sku            TYPE matnr,
             product_name   TYPE maktx,
             description    TYPE string,
             price          TYPE netpr,
             currency       TYPE waers,
             weight         TYPE brgew,
             dimensions     TYPE string,
             category       TYPE matkl,
             availability   TYPE labst,
             images         TYPE TABLE OF string,
           END OF ty_product_data.
           
    TYPES: BEGIN OF ty_order_data,
             order_id       TYPE vbeln_va,
             customer_id    TYPE kunnr,
             order_date     TYPE erdat,
             total_amount   TYPE netwr_ak,
             currency       TYPE waers,
             status         TYPE statv,
             shipping_addr  TYPE ty_customer_data,
             billing_addr   TYPE ty_customer_data,
             items          TYPE TABLE OF ty_order_item,
           END OF ty_order_data.
           
    TYPES: BEGIN OF ty_order_item,
             item_id      TYPE posnr_va,
             material_id  TYPE matnr,
             quantity     TYPE kwmeng,
             unit_price   TYPE netpr,
             total_price  TYPE netwr_ap,
             tax_amount   TYPE mwsbp,
           END OF ty_order_item.
    
    " Interface definitions
    INTERFACES: if_ecommerce_integration.
    
    " Public methods for business operations
    METHODS: constructor,
    
             " Customer operations
             sync_customer_to_ecommerce
               IMPORTING is_customer TYPE ty_customer_data
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             get_customer_from_ecommerce
               IMPORTING iv_customer_id TYPE string
               RETURNING VALUE(rs_customer) TYPE ty_customer_data
               RAISING   cx_integration_error,
               
             " Product operations  
             sync_product_to_ecommerce
               IMPORTING is_product TYPE ty_product_data
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             update_product_inventory
               IMPORTING iv_material_id TYPE matnr
                        iv_quantity TYPE menge_d
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             " Order operations
             create_order_from_ecommerce
               IMPORTING is_order_data TYPE ty_order_data
               RETURNING VALUE(rv_order_id) TYPE vbeln_va
               RAISING   cx_integration_error,
               
             update_order_status
               IMPORTING iv_order_id TYPE vbeln_va
                        iv_status TYPE statv
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             " Pricing operations
             sync_pricing_to_ecommerce
               IMPORTING iv_material_id TYPE matnr
                        iv_price_list TYPE string OPTIONAL
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             " Inventory operations
             get_real_time_inventory
               IMPORTING iv_material_id TYPE matnr
               RETURNING VALUE(rv_quantity) TYPE menge_d
               RAISING   cx_integration_error.

  PRIVATE SECTION.
    
    " Instance variables
    DATA: mo_integration_fw TYPE REF TO zcl_integration_framework,
          mo_logger        TYPE REF TO zcl_application_logger,
          mo_material_mgr  TYPE REF TO zcl_material_manager,
          mo_order_mgr     TYPE REF TO zcl_order_manager,
          mv_api_base_url  TYPE string,
          mv_api_version   TYPE string.
    
    " Private helper methods
    METHODS: initialize_components,
    
             " Data transformation methods
             transform_customer_to_json
               IMPORTING is_customer TYPE ty_customer_data
               RETURNING VALUE(rv_json) TYPE string,
               
             transform_json_to_customer
               IMPORTING iv_json TYPE string
               RETURNING VALUE(rs_customer) TYPE ty_customer_data
               RAISING   cx_transformation_error,
               
             transform_product_to_json
               IMPORTING is_product TYPE ty_product_data
               RETURNING VALUE(rv_json) TYPE string,
               
             " API endpoint builders
             build_customer_endpoint
               IMPORTING iv_customer_id TYPE string OPTIONAL
               RETURNING VALUE(rv_endpoint) TYPE string,
               
             build_product_endpoint
               IMPORTING iv_material_id TYPE matnr OPTIONAL
               RETURNING VALUE(rv_endpoint) TYPE string,
               
             build_order_endpoint
               IMPORTING iv_order_id TYPE string OPTIONAL
               RETURNING VALUE(rv_endpoint) TYPE string,
               
             " Business logic helpers
             validate_customer_data
               IMPORTING is_customer TYPE ty_customer_data
               RAISING   cx_validation_error,
               
             map_sap_order_to_ecommerce
               IMPORTING is_sap_order TYPE ty_order_data
               RETURNING VALUE(rv_json) TYPE string,
               
             process_order_response
               IMPORTING iv_response_json TYPE string
               RETURNING VALUE(rv_order_id) TYPE vbeln_va
               RAISING   cx_integration_error,
               
             handle_api_error
               IMPORTING iv_status_code TYPE i
                        iv_error_response TYPE string
               RAISING   cx_integration_error.

ENDCLASS.

CLASS zcl_ecommerce_api_handler IMPLEMENTATION.

  METHOD constructor.
    initialize_components( ).
  ENDMETHOD.

  METHOD initialize_components.
    " Initialize all dependent components
    mo_integration_fw = zcl_integration_framework=>get_instance( ).
    mo_logger = NEW zcl_application_logger( ).
    mo_material_mgr = NEW zcl_material_manager( ).
    mo_order_mgr = NEW zcl_order_manager( ).
    
    " Get configuration
    mv_api_base_url = mo_integration_fw->get_configuration( 'ECOMMERCE_BASE_URL' ).
    mv_api_version = mo_integration_fw->get_configuration( 'ECOMMERCE_API_VERSION' ).
  ENDMETHOD.

  METHOD sync_customer_to_ecommerce.
    " Synchronize customer master data to e-commerce platform
    
    DATA: ls_request  TYPE zcl_integration_framework=>ty_api_request,
          ls_response TYPE zcl_integration_framework=>ty_api_response,
          lv_json     TYPE string.
    
    TRY.
        " Validate customer data
        validate_customer_data( is_customer ).
        
        " Transform customer data to JSON
        lv_json = transform_customer_to_json( is_customer ).
        
        " Build API request
        ls_request = VALUE #(
          method   = zcl_integration_framework=>c_methods-post
          endpoint = build_customer_endpoint( )
          payload  = lv_json
          headers  = VALUE #( ( name = 'Content-Type' value = 'application/json' ) )
          timeout  = 30 ).
        
        " Call e-commerce API
        ls_response = mo_integration_fw->call_api( ls_request ).
        
        " Check response
        IF ls_response-status_code = 201 OR ls_response-status_code = 200.
          rv_success = abap_true.
          mo_logger->log_info( |Customer { is_customer-customer_id } synced successfully| ).
        ELSE.
          handle_api_error( 
            iv_status_code = ls_response-status_code
            iv_error_response = ls_response-body ).
        ENDIF.
        
      CATCH cx_validation_error INTO DATA(lx_validation).
        mo_logger->log_error( |Customer validation failed: { lx_validation->get_text( ) }| ).
        rv_success = abap_false.
        
      CATCH cx_integration_error INTO DATA(lx_integration).
        mo_logger->log_error( |Customer sync failed: { lx_integration->get_text( ) }| ).
        RAISE EXCEPTION lx_integration.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD sync_product_to_ecommerce.
    " Synchronize product data to e-commerce platform
    
    DATA: ls_request  TYPE zcl_integration_framework=>ty_api_request,
          ls_response TYPE zcl_integration_framework=>ty_api_response,
          lv_json     TYPE string.
    
    TRY.
        " Get complete product data from SAP
        DATA(ls_complete_product) = mo_material_mgr->get_complete_product_data( is_product-material_id ).
        
        " Merge with provided data
        ls_complete_product = CORRESPONDING #( BASE ( ls_complete_product ) is_product ).
        
        " Transform to JSON
        lv_json = transform_product_to_json( ls_complete_product ).
        
        " Check if product exists in e-commerce
        DATA(lv_existing_product) = check_product_exists( is_product-material_id ).
        
        " Build request for create or update
        ls_request = VALUE #(
          method   = COND #( WHEN lv_existing_product = abap_true 
                            THEN zcl_integration_framework=>c_methods-put
                            ELSE zcl_integration_framework=>c_methods-post )
          endpoint = build_product_endpoint( 
                      COND #( WHEN lv_existing_product = abap_true 
                             THEN is_product-material_id ) )
          payload  = lv_json
          headers  = VALUE #( ( name = 'Content-Type' value = 'application/json' ) )
          timeout  = 30 ).
        
        " Call API
        ls_response = mo_integration_fw->call_api( ls_request ).
        
        " Process response
        IF ls_response-status_code BETWEEN 200 AND 299.
          rv_success = abap_true.
          
          " Update sync status in SAP
          UPDATE zmaterial_sync SET 
            last_sync_date = sy-datum
            last_sync_time = sy-uzeit
            sync_status = 'S'
            WHERE material_id = is_product-material_id.
            
          mo_logger->log_info( |Product { is_product-material_id } synced successfully| ).
        ELSE.
          handle_api_error( 
            iv_status_code = ls_response-status_code
            iv_error_response = ls_response-body ).
        ENDIF.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD create_order_from_ecommerce.
    " Create SAP sales order from e-commerce order data
    
    DATA: ls_order_header TYPE bapisdhd1,
          lt_order_items  TYPE TABLE OF bapisditm,
          lt_order_return TYPE TABLE OF bapiret2,
          lv_order_number TYPE vbeln_va.
    
    TRY.
        " Map e-commerce order to SAP structures
        ls_order_header = VALUE #(
          doc_type    = 'OR'
          sales_org   = '1000'
          distr_chan  = '10'
          division    = '00'
          sold_to     = is_order_data-customer_id
          req_date_h  = is_order_data-order_date
          currency    = is_order_data-currency
          ref_doc     = is_order_data-order_id ).
        
        " Map order items
        LOOP AT is_order_data-items INTO DATA(ls_ecom_item).
          APPEND VALUE #(
            itm_number = |{ sy-tabix ALPHA = IN }|
            material   = |{ ls_ecom_item-material_id ALPHA = IN }|
            req_qty    = ls_ecom_item-quantity
            net_price  = ls_ecom_item-unit_price
            currency   = is_order_data-currency
          ) TO lt_order_items.
        ENDLOOP.
        
        " Create sales order using BAPI
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
          EXPORTING
            order_header_in = ls_order_header
          IMPORTING
            salesdocument   = lv_order_number
          TABLES
            return          = lt_order_return
            order_items_in  = lt_order_items.
        
        " Check for errors
        READ TABLE lt_order_return TRANSPORTING NO FIELDS 
          WITH KEY type = 'E'.
        
        IF sy-subrc = 0.
          " Error occurred
          DATA(lv_error_msg) = VALUE string( ).
          LOOP AT lt_order_return INTO DATA(ls_return) WHERE type = 'E'.
            lv_error_msg = |{ lv_error_msg } { ls_return-message }|.
          ENDLOOP.
          
          RAISE EXCEPTION TYPE cx_integration_error
            EXPORTING
              textid = cx_integration_error=>order_creation_failed
              error_details = lv_error_msg.
        ELSE.
          " Success - commit the transaction
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
              
          rv_order_id = lv_order_number.
          
          " Update e-commerce with SAP order number
          update_ecommerce_order_mapping( 
            iv_ecommerce_order_id = is_order_data-order_id
            iv_sap_order_id = lv_order_number ).
          
          mo_logger->log_info( |Order { lv_order_number } created from e-commerce order { is_order_data-order_id }| ).
        ENDIF.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD update_product_inventory.
    " Update product inventory in e-commerce platform
    
    DATA: ls_request  TYPE zcl_integration_framework=>ty_api_request,
          ls_response TYPE zcl_integration_framework=>ty_api_response,
          lv_json     TYPE string.
    
    " Build inventory update JSON
    lv_json = |{ "sku": "{ iv_material_id }", "quantity": { iv_quantity }, "timestamp": "{ sy-datum }{ sy-uzeit }" }|.
    
    " Build API request
    ls_request = VALUE #(
      method   = zcl_integration_framework=>c_methods-put
      endpoint = |{ build_product_endpoint( iv_material_id ) }/inventory|
      payload  = lv_json
      headers  = VALUE #( ( name = 'Content-Type' value = 'application/json' ) )
      timeout  = 15 ).
    
    TRY.
        " Call API
        ls_response = mo_integration_fw->call_api( ls_request ).
        
        IF ls_response-status_code = 200.
          rv_success = abap_true.
          
          " Log successful inventory update
          INSERT INTO zinventory_sync_log VALUES (
            material_id = iv_material_id
            sync_date = sy-datum
            sync_time = sy-uzeit
            old_quantity = 0  " Would get from previous state
            new_quantity = iv_quantity
            status = 'S'
            user_name = sy-uname ).
            
        ELSE.
          rv_success = abap_false.
          handle_api_error( 
            iv_status_code = ls_response-status_code
            iv_error_response = ls_response-body ).
        ENDIF.
        
      CATCH cx_integration_error INTO DATA(lx_error).
        mo_logger->log_error( |Inventory update failed for { iv_material_id }: { lx_error->get_text( ) }| ).
        RAISE EXCEPTION lx_error.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD sync_pricing_to_ecommerce.
    " Synchronize pricing information to e-commerce
    
    DATA: ls_request  TYPE zcl_integration_framework=>ty_api_request,
          ls_response TYPE zcl_integration_framework=>ty_api_response,
          lv_json     TYPE string,
          lt_prices   TYPE TABLE OF zpricing_data.
    
    TRY.
        " Get current pricing from SAP
        SELECT SINGLE kbetr, waers FROM a304
          INTO @DATA(ls_price)
          WHERE matnr = @iv_material_id
            AND datbi >= @sy-datum
            AND datab <= @sy-datum.
        
        IF sy-subrc = 0.
          " Build pricing JSON
          lv_json = /ui2/cl_json=>serialize( 
            data = VALUE zpricing_structure(
              material_id = iv_material_id
              price = ls_price-kbetr
              currency = ls_price-waers
              price_list = iv_price_list
              effective_date = sy-datum
              valid_until = '99991231' ) ).
          
          " Build API request
          ls_request = VALUE #(
            method   = zcl_integration_framework=>c_methods-put
            endpoint = |{ build_product_endpoint( iv_material_id ) }/pricing|
            payload  = lv_json
            headers  = VALUE #( ( name = 'Content-Type' value = 'application/json' ) )
            timeout  = 20 ).
          
          " Call API
          ls_response = mo_integration_fw->call_api( ls_request ).
          
          IF ls_response-status_code = 200.
            rv_success = abap_true.
            mo_logger->log_info( |Pricing updated for material { iv_material_id }| ).
          ELSE.
            rv_success = abap_false.
            handle_api_error( 
              iv_status_code = ls_response-status_code
              iv_error_response = ls_response-body ).
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE cx_integration_error
            EXPORTING
              textid = cx_integration_error=>pricing_not_found
              material_id = iv_material_id.
        ENDIF.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD transform_customer_to_json.
    " Transform customer data to JSON format for e-commerce API
    
    DATA: lr_json_serializer TYPE REF TO /ui2/cl_json,
          ls_ecom_customer   TYPE zecommerce_customer.
    
    " Map SAP customer structure to e-commerce format
    ls_ecom_customer = VALUE #(
      id = is_customer-customer_id
      email = is_customer-email
      first_name = is_customer-first_name
      last_name = is_customer-last_name
      phone = is_customer-phone
      default_address = VALUE #(
        address1 = is_customer-street
        city = is_customer-city
        zip = is_customer-postal_code
        country = is_customer-country )
      created_at = |{ is_customer-created_date }T00:00:00Z|
      updated_at = |{ sy-datum }T{ sy-uzeit }Z| ).
    
    " Serialize to JSON
    rv_json = /ui2/cl_json=>serialize( 
      data = ls_ecom_customer
      compress = abap_true ).
    
  ENDMETHOD.

  METHOD build_customer_endpoint.
    " Build customer API endpoint
    IF iv_customer_id IS NOT INITIAL.
      rv_endpoint = |/{ mv_api_version }/customers/{ iv_customer_id }|.
    ELSE.
      rv_endpoint = |/{ mv_api_version }/customers|.
    ENDIF.
  ENDMETHOD.

  METHOD build_product_endpoint.
    " Build product API endpoint
    IF iv_material_id IS NOT INITIAL.
      rv_endpoint = |/{ mv_api_version }/products/{ iv_material_id }|.
    ELSE.
      rv_endpoint = |/{ mv_api_version }/products|.
    ENDIF.
  ENDMETHOD.

  METHOD handle_api_error.
    " Centralized API error handling
    
    DATA: lv_error_msg TYPE string.
    
    CASE iv_status_code.
      WHEN 400.
        lv_error_msg = 'Bad Request - Invalid data format'.
      WHEN 401.
        lv_error_msg = 'Unauthorized - Authentication failed'.
      WHEN 403.
        lv_error_msg = 'Forbidden - Access denied'.
      WHEN 404.
        lv_error_msg = 'Not Found - Resource does not exist'.
      WHEN 429.
        lv_error_msg = 'Rate Limit Exceeded - Too many requests'.
      WHEN 500.
        lv_error_msg = 'Internal Server Error - E-commerce platform error'.
      WHEN OTHERS.
        lv_error_msg = |HTTP Error { iv_status_code }|.
    ENDCASE.
    
    " Log detailed error
    mo_logger->log_error( |API Error { iv_status_code }: { lv_error_msg }. Response: { iv_error_response }| ).
    
    " Raise appropriate exception
    RAISE EXCEPTION TYPE cx_integration_error
      EXPORTING
        textid = cx_integration_error=>api_error
        http_status_code = iv_status_code
        error_details = |{ lv_error_msg } - { iv_error_response }|.
    
  ENDMETHOD.

ENDCLASS.