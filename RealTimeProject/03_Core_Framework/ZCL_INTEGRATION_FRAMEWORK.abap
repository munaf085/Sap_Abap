*&---------------------------------------------------------------------*
*& Class: ZCL_INTEGRATION_FRAMEWORK
*& Description: Core Integration Framework for E-Commerce Platform
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*&---------------------------------------------------------------------*

CLASS zcl_integration_framework DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    
    " Types for integration
    TYPES: BEGIN OF ty_api_request,
             method      TYPE string,
             endpoint    TYPE string,
             headers     TYPE tihttpnvp,
             payload     TYPE string,
             timeout     TYPE i,
           END OF ty_api_request.
           
    TYPES: BEGIN OF ty_api_response,
             status_code TYPE i,
             headers     TYPE tihttpnvp,
             body        TYPE string,
             success     TYPE abap_bool,
             error_msg   TYPE string,
           END OF ty_api_response.
    
    TYPES: tt_api_log TYPE TABLE OF zapi_integration_log.
    
    " Interface definitions
    INTERFACES: if_rest_client,
                if_serializable_object.
    
    " Constants
    CONSTANTS: BEGIN OF c_methods,
                 get    TYPE string VALUE 'GET',
                 post   TYPE string VALUE 'POST',
                 put    TYPE string VALUE 'PUT',
                 delete TYPE string VALUE 'DELETE',
               END OF c_methods.
               
    CONSTANTS: BEGIN OF c_status,
                 success TYPE string VALUE 'S',
                 error   TYPE string VALUE 'E',
                 warning TYPE string VALUE 'W',
               END OF c_status.
    
    " Public methods
    CLASS-METHODS: class_constructor,
    
                   get_instance
                     RETURNING VALUE(ro_instance) TYPE REF TO zcl_integration_framework,
                   
                   call_api
                     IMPORTING is_request TYPE ty_api_request
                     RETURNING VALUE(rs_response) TYPE ty_api_response
                     RAISING   cx_integration_error,
                     
                   log_api_call
                     IMPORTING is_request  TYPE ty_api_request
                              is_response TYPE ty_api_response
                              iv_duration TYPE p DECIMALS 3,
                              
                   get_configuration
                     IMPORTING iv_config_key TYPE string
                     RETURNING VALUE(rv_config_value) TYPE string.
    
    METHODS: constructor,
    
             process_webhook
               IMPORTING iv_payload TYPE string
               RETURNING VALUE(rv_success) TYPE abap_bool
               RAISING   cx_integration_error,
               
             transform_data
               IMPORTING ir_source_data TYPE REF TO data
                        iv_transformation_type TYPE string
               RETURNING VALUE(rr_target_data) TYPE REF TO data
               RAISING   cx_transformation_error.

  PRIVATE SECTION.
    
    " Singleton instance
    CLASS-DATA: go_instance TYPE REF TO zcl_integration_framework.
    
    " Instance variables
    DATA: mv_system_id TYPE string,
          mt_config_cache TYPE TABLE OF zintegration_config,
          mo_logger TYPE REF TO zcl_application_logger,
          mo_crypto TYPE REF TO zcl_crypto_handler.
    
    " Private methods
    METHODS: initialize_system,
    
             validate_request
               IMPORTING is_request TYPE ty_api_request
               RAISING   cx_validation_error,
               
             handle_authentication
               IMPORTING ir_http_client TYPE REF TO if_http_client
               RAISING   cx_authentication_error,
               
             parse_response
               IMPORTING ir_http_client TYPE REF TO if_http_client
               RETURNING VALUE(rs_response) TYPE ty_api_response,
               
             handle_error
               IMPORTING iv_error_code TYPE string
                        iv_error_message TYPE string
               RAISING   cx_integration_error.

ENDCLASS.

CLASS zcl_integration_framework IMPLEMENTATION.

  METHOD class_constructor.
    " Initialize static components
  ENDMETHOD.

  METHOD get_instance.
    " Singleton pattern implementation
    IF go_instance IS NOT BOUND.
      go_instance = NEW zcl_integration_framework( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD constructor.
    " Initialize instance
    initialize_system( ).
    mo_logger = NEW zcl_application_logger( ).
    mo_crypto = NEW zcl_crypto_handler( ).
  ENDMETHOD.

  METHOD call_api.
    " Main API calling method with comprehensive error handling
    
    DATA: lr_http_client TYPE REF TO if_http_client,
          lv_start_time  TYPE timestampl,
          lv_end_time    TYPE timestampl,
          lv_duration    TYPE p DECIMALS 3.
    
    " Start timing
    GET TIME STAMP FIELD lv_start_time.
    
    TRY.
        " Validate request
        validate_request( is_request ).
        
        " Create HTTP client
        cl_http_client=>create_by_url(
          EXPORTING
            url = |{ get_configuration( 'BASE_URL' ) }{ is_request-endpoint }|
          IMPORTING
            client = lr_http_client ).
        
        " Set timeout
        lr_http_client->set_timeout( 
          COND #( WHEN is_request-timeout > 0 
                  THEN is_request-timeout 
                  ELSE 30 ) ).
        
        " Set method
        lr_http_client->request->set_method( is_request-method ).
        
        " Set headers
        LOOP AT is_request-headers INTO DATA(ls_header).
          lr_http_client->request->set_header_field(
            name  = ls_header-name
            value = ls_header-value ).
        ENDLOOP.
        
        " Handle authentication
        handle_authentication( lr_http_client ).
        
        " Set payload for POST/PUT
        IF is_request-method = c_methods-post OR 
           is_request-method = c_methods-put.
          lr_http_client->request->set_cdata( is_request-payload ).
        ENDIF.
        
        " Send request
        lr_http_client->send( ).
        lr_http_client->receive( ).
        
        " Parse response
        rs_response = parse_response( lr_http_client ).
        
        " Calculate duration
        GET TIME STAMP FIELD lv_end_time.
        lv_duration = lv_end_time - lv_start_time.
        
        " Log API call
        log_api_call( 
          is_request  = is_request
          is_response = rs_response
          iv_duration = lv_duration ).
        
      CATCH cx_http_exception INTO DATA(lx_http).
        handle_error( 
          iv_error_code = 'HTTP_ERROR'
          iv_error_message = lx_http->get_text( ) ).
          
      CATCH cx_validation_error INTO DATA(lx_validation).
        handle_error(
          iv_error_code = 'VALIDATION_ERROR'
          iv_error_message = lx_validation->get_text( ) ).
          
      CATCH cx_authentication_error INTO DATA(lx_auth).
        handle_error(
          iv_error_code = 'AUTH_ERROR'
          iv_error_message = lx_auth->get_text( ) ).
    ENDTRY.
    
  ENDMETHOD.

  METHOD process_webhook.
    " Process incoming webhook payloads
    
    DATA: lr_json_deserializer TYPE REF TO /ui2/cl_json,
          lr_webhook_data      TYPE REF TO data.
    
    TRY.
        " Parse JSON payload
        CREATE DATA lr_webhook_data TYPE zhook_payload.
        ASSIGN lr_webhook_data->* TO FIELD-SYMBOL(<ls_webhook>).
        
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = iv_payload
          CHANGING
            data = <ls_webhook> ).
        
        " Validate webhook signature
        IF validate_webhook_signature( iv_payload ) = abap_false.
          RAISE EXCEPTION TYPE cx_integration_error
            EXPORTING
              textid = cx_integration_error=>invalid_signature.
        ENDIF.
        
        " Process based on webhook type
        CASE <ls_webhook>-event_type.
          WHEN 'order_created'.
            process_order_webhook( <ls_webhook> ).
          WHEN 'inventory_updated'.
            process_inventory_webhook( <ls_webhook> ).
          WHEN 'customer_updated'.
            process_customer_webhook( <ls_webhook> ).
          WHEN OTHERS.
            mo_logger->log_warning( |Unknown webhook type: { <ls_webhook>-event_type }| ).
        ENDCASE.
        
        rv_success = abap_true.
        
      CATCH cx_sy_conversion_no_number
            cx_sy_move_cast_error INTO DATA(lx_conversion).
        mo_logger->log_error( |Webhook parsing error: { lx_conversion->get_text( ) }| ).
        rv_success = abap_false.
        
    ENDTRY.
    
  ENDMETHOD.

  METHOD transform_data.
    " Generic data transformation engine
    
    DATA: lr_transformer TYPE REF TO zcl_data_transformer.
    
    TRY.
        " Get appropriate transformer
        lr_transformer = zcl_transformer_factory=>get_transformer( iv_transformation_type ).
        
        " Perform transformation
        rr_target_data = lr_transformer->transform(
          ir_source_data = ir_source_data
          iv_transformation_rules = get_transformation_rules( iv_transformation_type ) ).
        
      CATCH cx_transformer_not_found INTO DATA(lx_not_found).
        RAISE EXCEPTION TYPE cx_transformation_error
          EXPORTING
            textid = cx_transformation_error=>transformer_not_found
            transformer_type = iv_transformation_type.
            
    ENDTRY.
    
  ENDMETHOD.

  METHOD validate_request.
    " Comprehensive request validation
    
    " Check required fields
    IF is_request-method IS INITIAL.
      RAISE EXCEPTION TYPE cx_validation_error
        EXPORTING
          textid = cx_validation_error=>missing_method.
    ENDIF.
    
    IF is_request-endpoint IS INITIAL.
      RAISE EXCEPTION TYPE cx_validation_error
        EXPORTING
          textid = cx_validation_error=>missing_endpoint.
    ENDIF.
    
    " Validate method
    CASE is_request-method.
      WHEN c_methods-get OR c_methods-post OR 
           c_methods-put OR c_methods-delete.
        " Valid methods
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_validation_error
          EXPORTING
            textid = cx_validation_error=>invalid_method
            method = is_request-method.
    ENDCASE.
    
  ENDMETHOD.

  METHOD handle_authentication.
    " Handle OAuth 2.0 authentication
    
    DATA: lv_access_token TYPE string,
          lv_token_type   TYPE string.
    
    " Get access token from secure storage
    lv_access_token = get_configuration( 'ACCESS_TOKEN' ).
    lv_token_type = get_configuration( 'TOKEN_TYPE' ).
    
    IF lv_access_token IS INITIAL.
      " Refresh token if needed
      refresh_access_token( ).
      lv_access_token = get_configuration( 'ACCESS_TOKEN' ).
    ENDIF.
    
    " Set authorization header
    ir_http_client->request->set_header_field(
      name  = 'Authorization'
      value = |{ lv_token_type } { lv_access_token }| ).
    
  ENDMETHOD.

  METHOD log_api_call.
    " Comprehensive API call logging
    
    DATA: ls_log_entry TYPE zapi_integration_log.
    
    ls_log_entry = VALUE #(
      log_id        = cl_system_uuid=>create_uuid_x16_static( )
      timestamp     = sy-datum
      time          = sy-uzeit
      system_id     = mv_system_id
      method        = is_request-method
      endpoint      = is_request-endpoint
      status_code   = is_response-status_code
      duration      = iv_duration
      success_flag  = is_response-success
      error_message = is_response-error_msg
      request_size  = strlen( is_request-payload )
      response_size = strlen( is_response-body )
      user_name     = sy-uname ).
    
    " Insert log entry
    INSERT zapi_integration_log FROM ls_log_entry.
    
    " Also log to application log for real-time monitoring
    IF is_response-success = abap_true.
      mo_logger->log_info( |API call successful: { is_request-method } { is_request-endpoint }| ).
    ELSE.
      mo_logger->log_error( |API call failed: { is_response-error_msg }| ).
    ENDIF.
    
  ENDMETHOD.

ENDCLASS.