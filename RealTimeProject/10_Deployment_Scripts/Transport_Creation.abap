*&---------------------------------------------------------------------*
*& Transport Creation and Deployment Scripts
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Purpose: Automated transport creation and deployment for ABAP app
*&---------------------------------------------------------------------*

REPORT z_create_ecommerce_transport.

*&---------------------------------------------------------------------*
*& Program: Z_CREATE_ECOMMERCE_TRANSPORT
*& Description: Automated transport request creation for e-commerce platform
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_object,
         pgmid    TYPE pgmid,
         object   TYPE trobjtype,
         obj_name TYPE trobj_name,
         description TYPE as4text,
       END OF ty_object.

DATA: gt_objects TYPE TABLE OF ty_object,
      gv_transport_request TYPE trkorr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_desc TYPE as4text DEFAULT 'E-Commerce Integration Platform - Development Objects',
              p_target TYPE tr_target DEFAULT 'QAS'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_tables AS CHECKBOX DEFAULT 'X',
              p_classes AS CHECKBOX DEFAULT 'X',
              p_reports AS CHECKBOX DEFAULT 'X',
              p_tcodes AS CHECKBOX DEFAULT 'X',
              p_config AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  PERFORM prepare_transport_objects.
  PERFORM create_transport_request.
  PERFORM add_objects_to_transport.
  PERFORM display_transport_info.

*&---------------------------------------------------------------------*
*& Form PREPARE_TRANSPORT_OBJECTS
*&---------------------------------------------------------------------*
FORM prepare_transport_objects.
  
  " Database Tables
  IF p_tables = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZAPI_INTEGRATION_LOG' description = 'API Integration Log Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZECOMMERCE_CONFIG' description = 'E-Commerce Configuration Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZMATERIAL_SYNC' description = 'Material Sync Status Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZCUSTOMER_SYNC' description = 'Customer Sync Status Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZORDER_MAPPING' description = 'Order System Mapping Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZINTEGRATION_QUEUE' description = 'Integration Message Queue Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZECOMMERCE_ANALYTICS' description = 'E-Commerce Analytics Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZPRICING_SYNC_LOG' description = 'Pricing Sync Log Table' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TABL' obj_name = 'ZINVENTORY_SYNC_LOG' description = 'Inventory Sync Log Table' ) TO gt_objects.
  ENDIF.
  
  " Classes
  IF p_classes = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_INTEGRATION_FRAMEWORK' description = 'Core Integration Framework Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_ECOMMERCE_API_HANDLER' description = 'E-Commerce API Handler Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_ORDER_PROCESSOR' description = 'Order Processing Engine Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_PRICING_ENGINE' description = 'Pricing Calculation Engine Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_INVENTORY_MANAGER' description = 'Inventory Management Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_CREDIT_MANAGER' description = 'Credit Check Manager Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_APPLICATION_LOGGER' description = 'Application Logging Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_NOTIFICATION_SERVICE' description = 'Notification Service Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_WORKFLOW_MANAGER' description = 'Workflow Management Class' ) TO gt_objects.
  ENDIF.
  
  " Test Classes
  IF p_classes = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_UNIT_TEST_ORDER_PROCESSOR' description = 'Unit Test for Order Processor' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_MOCK_PRICING_ENGINE' description = 'Mock Pricing Engine for Testing' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_MOCK_INVENTORY_MANAGER' description = 'Mock Inventory Manager for Testing' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'CLAS' obj_name = 'ZCL_MOCK_CREDIT_MANAGER' description = 'Mock Credit Manager for Testing' ) TO gt_objects.
  ENDIF.
  
  " Reports
  IF p_reports = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZR_ECOMMERCE_DASHBOARD' description = 'E-Commerce Analytics Dashboard' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZR_ECOMMERCE_CONFIG' description = 'E-Commerce Configuration Maintenance' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZR_SYNC_MONITOR' description = 'Synchronization Status Monitor' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZR_INTEGRATION_QUEUE' description = 'Integration Message Queue Management' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZR_INTEGRATION_TEST' description = 'Integration Testing Utility' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'Z_CREATE_ECOMMERCE_TRANSPORT' description = 'Transport Creation Utility' ) TO gt_objects.
  ENDIF.
  
  " Transaction Codes
  IF p_tcodes = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TRAN' obj_name = 'ZECOM_DASH' description = 'E-Commerce Dashboard Transaction' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TRAN' obj_name = 'ZECOM_CONFIG' description = 'E-Commerce Configuration Transaction' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TRAN' obj_name = 'ZECOM_SYNC' description = 'Synchronization Monitor Transaction' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TRAN' obj_name = 'ZECOM_QUEUE' description = 'Integration Queue Transaction' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'TRAN' obj_name = 'ZECOM_TEST' description = 'Integration Test Transaction' ) TO gt_objects.
  ENDIF.
  
  " Configuration Objects
  IF p_config = 'X'.
    APPEND VALUE #( pgmid = 'R3TR' object = 'INTF' obj_name = 'ZIF_ECOMMERCE_INTEGRATION' description = 'E-Commerce Integration Interface' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'MSAG' obj_name = 'ZECOMMERCE_MESSAGES' description = 'E-Commerce Message Class' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'DOMA' obj_name = 'ZSYNC_STATUS' description = 'Synchronization Status Domain' ) TO gt_objects.
    APPEND VALUE #( pgmid = 'R3TR' object = 'DTEL' obj_name = 'ZECOMMERCE_ID' description = 'E-Commerce ID Data Element' ) TO gt_objects.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_TRANSPORT_REQUEST
*&---------------------------------------------------------------------*
FORM create_transport_request.
  
  DATA: lv_request TYPE trkorr.
  
  " Create workbench request
  CALL FUNCTION 'TR_REQUEST_CREATE'
    EXPORTING
      iv_request_text = p_desc
      iv_request_type = 'K'  " Workbench request
      iv_target = p_target
    IMPORTING
      ev_request = lv_request
    EXCEPTIONS
      request_create_failed = 1
      OTHERS = 2.
  
  IF sy-subrc = 0.
    gv_transport_request = lv_request.
    WRITE: / 'Transport request created:', gv_transport_request.
  ELSE.
    MESSAGE 'Failed to create transport request' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_OBJECTS_TO_TRANSPORT
*&---------------------------------------------------------------------*
FORM add_objects_to_transport.
  
  DATA: lv_success_count TYPE i,
        lv_error_count TYPE i.
  
  LOOP AT gt_objects INTO DATA(ls_object).
    
    " Add object to transport
    CALL FUNCTION 'TR_OBJECT_INSERT'
      EXPORTING
        wi_korr_nr = gv_transport_request
        wi_object_key = VALUE ko100(
          pgmid = ls_object-pgmid
          object = ls_object-object
          obj_name = ls_object-obj_name )
      EXCEPTIONS
        object_insert_failed = 1
        OTHERS = 2.
    
    IF sy-subrc = 0.
      lv_success_count = lv_success_count + 1.
      WRITE: / '✅', ls_object-object, ls_object-obj_name, 'added successfully'.
    ELSE.
      lv_error_count = lv_error_count + 1.
      WRITE: / '❌', ls_object-object, ls_object-obj_name, 'failed to add'.
    ENDIF.
    
  ENDLOOP.
  
  WRITE: / 'Summary:', lv_success_count, 'objects added,', lv_error_count, 'errors'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_TRANSPORT_INFO
*&---------------------------------------------------------------------*
FORM display_transport_info.
  
  WRITE: / '=========================================='.
  WRITE: / 'TRANSPORT REQUEST SUMMARY'.
  WRITE: / '=========================================='.
  WRITE: / 'Request Number:', gv_transport_request.
  WRITE: / 'Description:', p_desc.
  WRITE: / 'Target System:', p_target.
  WRITE: / 'Total Objects:', lines( gt_objects ).
  WRITE: / '=========================================='.
  WRITE: / 'Next Steps:'.
  WRITE: / '1. Review objects in SE09/SE10'.
  WRITE: / '2. Release the request when ready'.
  WRITE: / '3. Import to target system'.
  WRITE: / '4. Perform post-deployment testing'.
  WRITE: / '=========================================='.

ENDFORM.

*&---------------------------------------------------------------------*
*& Program: Z_DEPLOYMENT_CHECKLIST
*& Description: Pre and post deployment verification
*&---------------------------------------------------------------------*

REPORT z_deployment_checklist.

TYPES: BEGIN OF ty_check_item,
         check_type    TYPE char20,
         check_name    TYPE char50,
         status        TYPE char1,
         message       TYPE string,
       END OF ty_check_item.

DATA: gt_checklist TYPE TABLE OF ty_check_item.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_pre  AS CHECKBOX DEFAULT 'X',
              p_post AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  IF p_pre = 'X'.
    PERFORM execute_pre_deployment_checks.
  ENDIF.
  
  IF p_post = 'X'.
    PERFORM execute_post_deployment_checks.
  ENDIF.
  
  PERFORM display_checklist_results.

*&---------------------------------------------------------------------*
*& Form EXECUTE_PRE_DEPLOYMENT_CHECKS
*&---------------------------------------------------------------------*
FORM execute_pre_deployment_checks.
  
  " Check 1: Verify all required tables exist
  PERFORM check_table_existence USING 'ZAPI_INTEGRATION_LOG'.
  PERFORM check_table_existence USING 'ZECOMMERCE_CONFIG'.
  PERFORM check_table_existence USING 'ZMATERIAL_SYNC'.
  PERFORM check_table_existence USING 'ZCUSTOMER_SYNC'.
  
  " Check 2: Verify all classes compile successfully
  PERFORM check_class_compilation USING 'ZCL_INTEGRATION_FRAMEWORK'.
  PERFORM check_class_compilation USING 'ZCL_ECOMMERCE_API_HANDLER'.
  PERFORM check_class_compilation USING 'ZCL_ORDER_PROCESSOR'.
  
  " Check 3: Verify transaction codes exist
  PERFORM check_transaction_code USING 'ZECOM_DASH'.
  PERFORM check_transaction_code USING 'ZECOM_CONFIG'.
  PERFORM check_transaction_code USING 'ZECOM_SYNC'.
  
  " Check 4: Verify configuration entries
  PERFORM check_configuration_setup.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXECUTE_POST_DEPLOYMENT_CHECKS
*&---------------------------------------------------------------------*
FORM execute_post_deployment_checks.
  
  " Check 1: Test API connectivity
  PERFORM test_api_connectivity.
  
  " Check 2: Verify data synchronization
  PERFORM test_data_synchronization.
  
  " Check 3: Check background job setup
  PERFORM verify_background_jobs.
  
  " Check 4: Test error handling
  PERFORM test_error_handling.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TABLE_EXISTENCE
*&---------------------------------------------------------------------*
FORM check_table_existence USING pv_table_name TYPE tabname.
  
  DATA: ls_check TYPE ty_check_item.
  
  SELECT SINGLE tabname FROM dd02l
    INTO @DATA(lv_table)
    WHERE tabname = @pv_table_name
      AND tabclass = 'TRANSP'
      AND as4local = 'A'.
  
  ls_check-check_type = 'TABLE'.
  ls_check-check_name = |Table { pv_table_name }|.
  
  IF sy-subrc = 0.
    ls_check-status = 'S'.
    ls_check-message = 'Table exists and is active'.
  ELSE.
    ls_check-status = 'E'.
    ls_check-message = 'Table missing or not active'.
  ENDIF.
  
  APPEND ls_check TO gt_checklist.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_CLASS_COMPILATION
*&---------------------------------------------------------------------*
FORM check_class_compilation USING pv_class_name TYPE seoclsname.
  
  DATA: ls_check TYPE ty_check_item.
  
  " Check if class exists and compiles
  SELECT SINGLE clsname FROM seoclass
    INTO @DATA(lv_class)
    WHERE clsname = @pv_class_name
      AND state = '1'.  " Active
  
  ls_check-check_type = 'CLASS'.
  ls_check-check_name = |Class { pv_class_name }|.
  
  IF sy-subrc = 0.
    " Try to instantiate class to check compilation
    TRY.
        DATA: lr_class TYPE REF TO object.
        CREATE OBJECT lr_class TYPE (pv_class_name).
        ls_check-status = 'S'.
        ls_check-message = 'Class exists and compiles successfully'.
      CATCH cx_sy_create_object_error.
        ls_check-status = 'W'.
        ls_check-message = 'Class exists but has compilation issues'.
    ENDTRY.
  ELSE.
    ls_check-status = 'E'.
    ls_check-message = 'Class missing or not active'.
  ENDIF.
  
  APPEND ls_check TO gt_checklist.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TEST_API_CONNECTIVITY
*&---------------------------------------------------------------------*
FORM test_api_connectivity.
  
  DATA: ls_check TYPE ty_check_item,
        lo_framework TYPE REF TO zcl_integration_framework.
  
  ls_check-check_type = 'API'.
  ls_check-check_name = 'E-Commerce API Connectivity'.
  
  TRY.
      " Test basic API connectivity
      lo_framework = zcl_integration_framework=>get_instance( ).
      
      " Perform health check call
      DATA(ls_request) = VALUE zcl_integration_framework=>ty_api_request(
        method = 'GET'
        endpoint = '/health'
        timeout = 10 ).
      
      DATA(ls_response) = lo_framework->call_api( ls_request ).
      
      IF ls_response-success = abap_true.
        ls_check-status = 'S'.
        ls_check-message = 'API connectivity successful'.
      ELSE.
        ls_check-status = 'E'.
        ls_check-message = |API call failed: { ls_response-error_msg }|.
      ENDIF.
      
    CATCH cx_integration_error INTO DATA(lx_error).
      ls_check-status = 'E'.
      ls_check-message = |API connectivity failed: { lx_error->get_text( ) }|.
  ENDTRY.
  
  APPEND ls_check TO gt_checklist.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_CHECKLIST_RESULTS
*&---------------------------------------------------------------------*
FORM display_checklist_results.
  
  DATA: gr_alv TYPE REF TO cl_salv_table.
  
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv
        CHANGING
          t_table = gt_checklist ).
      
      " Configure display
      DATA(lr_columns) = gr_alv->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Add color coding for status
      DATA(lr_column) = lr_columns->get_column( 'STATUS' ).
      lr_column->set_long_text( 'Status' ).
      
      " Set title
      DATA(lr_display) = gr_alv->get_display_settings( ).
      lr_display->set_list_header( 'Deployment Verification Checklist' ).
      
      gr_alv->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.