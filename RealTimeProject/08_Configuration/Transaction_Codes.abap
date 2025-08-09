*&---------------------------------------------------------------------*
*& Transaction Codes for E-Commerce Integration Platform
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Purpose: Complete transaction code definitions for ABAP application
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Transaction Code: ZECOM_DASH
*& Program: ZR_ECOMMERCE_DASHBOARD
*& Description: E-Commerce Integration Dashboard
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Transaction Code: ZECOM_CONFIG
*& Program: ZR_ECOMMERCE_CONFIG
*& Description: E-Commerce Configuration Maintenance
*&---------------------------------------------------------------------*

REPORT zr_ecommerce_config.

TABLES: zecommerce_config.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_type FOR zecommerce_config-config_type,
                  s_key  FOR zecommerce_config-config_key.
  PARAMETERS: p_active TYPE zecommerce_config-active AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_create AS CHECKBOX,
              p_edit   AS CHECKBOX DEFAULT 'X',
              p_delete AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

DATA: gt_config TYPE TABLE OF zecommerce_config,
      gr_alv    TYPE REF TO cl_salv_table.

START-OF-SELECTION.
  PERFORM load_configuration_data.
  PERFORM display_alv_grid.

*&---------------------------------------------------------------------*
*& Form LOAD_CONFIGURATION_DATA
*&---------------------------------------------------------------------*
FORM load_configuration_data.
  
  SELECT * FROM zecommerce_config
    INTO TABLE gt_config
    WHERE config_type IN s_type
      AND config_key IN s_key
      AND ( active = p_active OR p_active = '' ).
      
  IF sy-subrc <> 0.
    MESSAGE 'No configuration data found' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
FORM display_alv_grid.
  
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv
        CHANGING
          t_table = gt_config ).
      
      " Enable editing if edit mode is selected
      IF p_edit = 'X'.
        DATA(lr_functions) = gr_alv->get_functions( ).
        lr_functions->set_all( abap_true ).
        
        " Set up event handling for data changes
        SET HANDLER handle_data_changed FOR gr_alv->get_event( ).
      ENDIF.
      
      " Configure columns
      DATA(lr_columns) = gr_alv->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Set title
      DATA(lr_display) = gr_alv->get_display_settings( ).
      lr_display->set_list_header( 'E-Commerce Configuration Maintenance' ).
      
      gr_alv->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Transaction Code: ZECOM_SYNC
*& Program: ZR_SYNC_MONITOR
*& Description: Synchronization Status Monitor
*&---------------------------------------------------------------------*

REPORT zr_sync_monitor.

TYPES: BEGIN OF ty_sync_status,
         object_type       TYPE char10,
         object_id         TYPE char50,
         last_sync_date    TYPE dats,
         last_sync_time    TYPE tims,
         sync_status       TYPE char1,
         error_message     TYPE string,
         retry_count       TYPE int2,
       END OF ty_sync_status.

DATA: gt_sync_status TYPE TABLE OF ty_sync_status,
      gr_alv_sync    TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_cust AS CHECKBOX DEFAULT 'X',
              p_mat  AS CHECKBOX DEFAULT 'X',
              p_order AS CHECKBOX DEFAULT 'X'.
  SELECT-OPTIONS: s_date FOR sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM load_sync_status.
  PERFORM display_sync_monitor.

*&---------------------------------------------------------------------*
*& Form LOAD_SYNC_STATUS
*&---------------------------------------------------------------------*
FORM load_sync_status.
  
  " Load customer sync status
  IF p_cust = 'X'.
    SELECT 'CUSTOMER' AS object_type,
           customer_id AS object_id,
           last_sync_date,
           last_sync_time,
           sync_status,
           error_message,
           retry_count
    FROM zcustomer_sync
    WHERE last_sync_date IN s_date
    APPENDING CORRESPONDING FIELDS OF TABLE gt_sync_status.
  ENDIF.
  
  " Load material sync status
  IF p_mat = 'X'.
    SELECT 'MATERIAL' AS object_type,
           material_id AS object_id,
           last_sync_date,
           last_sync_time,
           sync_status,
           error_message,
           retry_count
    FROM zmaterial_sync
    WHERE last_sync_date IN s_date
    APPENDING CORRESPONDING FIELDS OF TABLE gt_sync_status.
  ENDIF.
  
  " Load order mapping status
  IF p_order = 'X'.
    SELECT 'ORDER' AS object_type,
           sap_order_id AS object_id,
           created_date AS last_sync_date,
           created_time AS last_sync_time,
           'S' AS sync_status,
           '' AS error_message,
           0 AS retry_count
    FROM zorder_mapping
    WHERE created_date IN s_date
    APPENDING CORRESPONDING FIELDS OF TABLE gt_sync_status.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SYNC_MONITOR
*&---------------------------------------------------------------------*
FORM display_sync_monitor.
  
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv_sync
        CHANGING
          t_table = gt_sync_status ).
      
      " Configure display
      DATA(lr_columns) = gr_alv_sync->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Add color coding for status
      DATA(lr_column) = lr_columns->get_column( 'SYNC_STATUS' ).
      lr_column->set_long_text( 'Sync Status' ).
      
      " Set title
      DATA(lr_display) = gr_alv_sync->get_display_settings( ).
      lr_display->set_list_header( 'Synchronization Status Monitor' ).
      
      gr_alv_sync->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Transaction Code: ZECOM_QUEUE
*& Program: ZR_INTEGRATION_QUEUE
*& Description: Integration Message Queue Management
*&---------------------------------------------------------------------*

REPORT zr_integration_queue.

DATA: gt_queue TYPE TABLE OF zintegration_queue,
      gr_alv_queue TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_type FOR zintegration_queue-message_type,
                  s_status FOR zintegration_queue-status,
                  s_date FOR zintegration_queue-created_at.
  PARAMETERS: p_retry AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM load_queue_data.
  PERFORM display_queue_monitor.

*&---------------------------------------------------------------------*
*& Form LOAD_QUEUE_DATA
*&---------------------------------------------------------------------*
FORM load_queue_data.
  
  SELECT * FROM zintegration_queue
    INTO TABLE gt_queue
    WHERE message_type IN s_type
      AND status IN s_status
      AND created_at IN s_date
    ORDER BY created_at DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_QUEUE_MONITOR
*&---------------------------------------------------------------------*
FORM display_queue_monitor.
  
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv_queue
        CHANGING
          t_table = gt_queue ).
      
      " Enable functions
      DATA(lr_functions) = gr_alv_queue->get_functions( ).
      lr_functions->set_all( abap_true ).
      
      " Add custom functions for retry
      IF p_retry = 'X'.
        " Add custom toolbar button for retry
        lr_functions->add_function(
          name = 'RETRY'
          icon = '@0V@'
          text = 'Retry Selected'
          tooltip = 'Retry selected failed messages' ).
      ENDIF.
      
      " Configure columns
      DATA(lr_columns) = gr_alv_queue->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Set title
      DATA(lr_display) = gr_alv_queue->get_display_settings( ).
      lr_display->set_list_header( 'Integration Message Queue' ).
      
      gr_alv_queue->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Transaction Code: ZECOM_TEST
*& Program: ZR_INTEGRATION_TEST
*& Description: Integration Testing Utility
*&---------------------------------------------------------------------*

REPORT zr_integration_test.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_cust_test AS CHECKBOX,
              p_mat_test  AS CHECKBOX,
              p_order_test AS CHECKBOX,
              p_price_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_cust_id TYPE kunnr,
              p_mat_id  TYPE matnr,
              p_order_id TYPE vbeln_va.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  PERFORM execute_integration_tests.

*&---------------------------------------------------------------------*
*& Form EXECUTE_INTEGRATION_TESTS
*&---------------------------------------------------------------------*
FORM execute_integration_tests.
  
  DATA: lo_ecommerce_handler TYPE REF TO zcl_ecommerce_api_handler,
        lv_success TYPE abap_bool.
  
  " Initialize handler
  CREATE OBJECT lo_ecommerce_handler.
  
  " Test customer synchronization
  IF p_cust_test = 'X' AND p_cust_id IS NOT INITIAL.
    WRITE: / 'Testing Customer Synchronization...'.
    
    TRY.
        " Get customer data and sync
        SELECT SINGLE * FROM kna1 INTO @DATA(ls_customer)
          WHERE kunnr = @p_cust_id.
        
        IF sy-subrc = 0.
          " Convert to API format and sync
          DATA(ls_customer_data) = VALUE zcl_ecommerce_api_handler=>ty_customer_data(
            customer_id = ls_customer-kunnr
            first_name = ls_customer-name1
            last_name = ls_customer-name2 ).
          
          lv_success = lo_ecommerce_handler->sync_customer_to_ecommerce( ls_customer_data ).
          
          IF lv_success = abap_true.
            WRITE: / '✅ Customer sync successful'.
          ELSE.
            WRITE: / '❌ Customer sync failed'.
          ENDIF.
        ELSE.
          WRITE: / '❌ Customer not found'.
        ENDIF.
        
      CATCH cx_integration_error INTO DATA(lx_error).
        WRITE: / '❌ Error:', lx_error->get_text( ).
    ENDTRY.
  ENDIF.
  
  " Test material synchronization
  IF p_mat_test = 'X' AND p_mat_id IS NOT INITIAL.
    WRITE: / 'Testing Material Synchronization...'.
    
    TRY.
        " Get material data and sync
        SELECT SINGLE m~*, t~maktx FROM mara AS m
          INNER JOIN makt AS t ON m~matnr = t~matnr
          INTO @DATA(ls_material)
          WHERE m~matnr = @p_mat_id
            AND t~spras = @sy-langu.
        
        IF sy-subrc = 0.
          DATA(ls_product_data) = VALUE zcl_ecommerce_api_handler=>ty_product_data(
            material_id = ls_material-matnr
            product_name = ls_material-maktx ).
          
          lv_success = lo_ecommerce_handler->sync_product_to_ecommerce( ls_product_data ).
          
          IF lv_success = abap_true.
            WRITE: / '✅ Material sync successful'.
          ELSE.
            WRITE: / '❌ Material sync failed'.
          ENDIF.
        ELSE.
          WRITE: / '❌ Material not found'.
        ENDIF.
        
      CATCH cx_integration_error INTO lx_error.
        WRITE: / '❌ Error:', lx_error->get_text( ).
    ENDTRY.
  ENDIF.

ENDFORM.