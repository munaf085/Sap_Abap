*&---------------------------------------------------------------------*
*& Report: ZR_ECOMMERCE_DASHBOARD
*& Description: E-Commerce Integration Analytics Dashboard
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Business Purpose: Real-time analytics and monitoring dashboard
*&---------------------------------------------------------------------*

REPORT zr_ecommerce_dashboard.

" Data declarations for dashboard
TYPES: BEGIN OF ty_order_analytics,
         order_date       TYPE dats,
         channel          TYPE string,
         order_count      TYPE i,
         total_value      TYPE netwr_ak,
         avg_order_value  TYPE p DECIMALS 2,
         currency         TYPE waers,
       END OF ty_order_analytics.

TYPES: BEGIN OF ty_integration_metrics,
         api_endpoint     TYPE string,
         success_calls    TYPE i,
         failed_calls     TYPE i,
         avg_response_time TYPE p DECIMALS 3,
         last_24h_volume  TYPE i,
         success_rate     TYPE p DECIMALS 2,
       END OF ty_integration_metrics.

TYPES: BEGIN OF ty_product_performance,
         material_id      TYPE matnr,
         material_desc    TYPE maktx,
         total_orders     TYPE i,
         total_quantity   TYPE kwmeng,
         total_revenue    TYPE netwr_ak,
         avg_price        TYPE netpr,
         last_sync_date   TYPE dats,
         sync_status      TYPE char1,
       END OF ty_product_performance.

TYPES: BEGIN OF ty_error_analysis,
         error_date       TYPE dats,
         error_category   TYPE string,
         error_count      TYPE i,
         resolution_time  TYPE p DECIMALS 2,
         impact_level     TYPE string,
       END OF ty_error_analysis.

" Internal tables for dashboard data
DATA: gt_order_analytics    TYPE TABLE OF ty_order_analytics,
      gt_integration_metrics TYPE TABLE OF ty_integration_metrics,
      gt_product_performance TYPE TABLE OF ty_product_performance,
      gt_error_analysis      TYPE TABLE OF ty_error_analysis.

" ALV objects
DATA: gr_alv_analytics      TYPE REF TO cl_salv_table,
      gr_alv_metrics        TYPE REF TO cl_salv_table,
      gr_alv_products       TYPE REF TO cl_salv_table,
      gr_alv_errors         TYPE REF TO cl_salv_table.

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_date FOR sy-datum DEFAULT sy-datum,
                  s_matnr FOR mara-matnr,
                  s_vkorg FOR vbak-vkorg DEFAULT '1000'.
  PARAMETERS: p_realtime TYPE abap_bool AS CHECKBOX DEFAULT 'X',
              p_detail   TYPE abap_bool AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_orders  TYPE abap_bool AS CHECKBOX DEFAULT 'X',
              p_integ   TYPE abap_bool AS CHECKBOX DEFAULT 'X',
              p_prods   TYPE abap_bool AS CHECKBOX DEFAULT 'X',
              p_errors  TYPE abap_bool AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

" Text symbols
SELECTION-SCREEN COMMENT /1(77) TEXT-003.

" Event handling
INITIALIZATION.
  " Set default date range to last 30 days
  s_date-low = sy-datum - 30.
  s_date-high = sy-datum.
  s_date-sign = 'I'.
  s_date-option = 'BT'.
  APPEND s_date.

AT SELECTION-SCREEN.
  " Validate selection parameters
  IF s_date[] IS INITIAL.
    MESSAGE 'Please specify date range' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  " Main processing
  PERFORM main_processing.

END-OF-SELECTION.
  " Display dashboard
  PERFORM display_dashboard.

*&---------------------------------------------------------------------*
*& Form MAIN_PROCESSING
*&---------------------------------------------------------------------*
FORM main_processing.
  
  " Show progress indicator
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Loading order analytics...'.
  
  " Load different analytics based on selection
  IF p_orders = 'X'.
    PERFORM load_order_analytics.
  ENDIF.
  
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Loading integration metrics...'.
  
  IF p_integ = 'X'.
    PERFORM load_integration_metrics.
  ENDIF.
  
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 60
      text       = 'Loading product performance...'.
  
  IF p_prods = 'X'.
    PERFORM load_product_performance.
  ENDIF.
  
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Loading error analysis...'.
  
  IF p_errors = 'X'.
    PERFORM load_error_analysis.
  ENDIF.
  
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Dashboard ready...'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_ORDER_ANALYTICS
*&---------------------------------------------------------------------*
FORM load_order_analytics.
  
  " Clear previous data
  CLEAR: gt_order_analytics.
  
  " Load order analytics using CDS view or direct SQL
  IF p_realtime = 'X'.
    " Real-time data from live tables
    SELECT 
      h~erdat AS order_date,
      CASE 
        WHEN h~ref_doc LIKE 'ECOM%' THEN 'E-Commerce'
        WHEN h~ref_doc LIKE 'POS%' THEN 'Point of Sale'
        ELSE 'Direct Sales'
      END AS channel,
      COUNT(*) AS order_count,
      SUM(h~netwr) AS total_value,
      AVG(h~netwr) AS avg_order_value,
      h~waerk AS currency
    FROM vbak AS h
    INNER JOIN vbap AS p ON h~vbeln = p~vbeln
    WHERE h~erdat IN @s_date
      AND h~vkorg IN @s_vkorg
      AND h~vbtyp = 'C'  " Sales order
    GROUP BY h~erdat, h~ref_doc, h~waerk
    INTO CORRESPONDING FIELDS OF TABLE @gt_order_analytics.
  ELSE.
    " Load from pre-aggregated analytics table
    SELECT *
    FROM zecommerce_analytics
    WHERE order_date IN @s_date
      AND sales_org IN @s_vkorg
    INTO CORRESPONDING FIELDS OF TABLE @gt_order_analytics.
  ENDIF.
  
  " Sort by date descending
  SORT gt_order_analytics BY order_date DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_INTEGRATION_METRICS
*&---------------------------------------------------------------------*
FORM load_integration_metrics.
  
  CLEAR: gt_integration_metrics.
  
  " Load API integration metrics
  SELECT 
    endpoint AS api_endpoint,
    SUM( CASE WHEN success_flag = 'X' THEN 1 ELSE 0 END ) AS success_calls,
    SUM( CASE WHEN success_flag = '' THEN 1 ELSE 0 END ) AS failed_calls,
    AVG( duration ) AS avg_response_time,
    COUNT(*) AS last_24h_volume,
    CAST( SUM( CASE WHEN success_flag = 'X' THEN 1 ELSE 0 END ) * 100.0 / COUNT(*) AS DEC(5,2) ) AS success_rate
  FROM zapi_integration_log
  WHERE timestamp >= @sy-datum
    AND timestamp IN @s_date
  GROUP BY endpoint
  ORDER BY last_24h_volume DESCENDING
  INTO CORRESPONDING FIELDS OF TABLE @gt_integration_metrics.
  
  " Calculate additional metrics
  LOOP AT gt_integration_metrics ASSIGNING FIELD-SYMBOL(<ls_metric>).
    " Add real-time status indicators
    IF <ls_metric>-success_rate < 95.
      <ls_metric>-api_endpoint = |🔴 { <ls_metric>-api_endpoint }|.
    ELSEIF <ls_metric>-success_rate < 99.
      <ls_metric>-api_endpoint = |🟡 { <ls_metric>-api_endpoint }|.
    ELSE.
      <ls_metric>-api_endpoint = |🟢 { <ls_metric>-api_endpoint }|.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_PRODUCT_PERFORMANCE
*&---------------------------------------------------------------------*
FORM load_product_performance.
  
  CLEAR: gt_product_performance.
  
  " Load product performance analytics
  SELECT 
    p~matnr AS material_id,
    t~maktx AS material_desc,
    COUNT(DISTINCT h~vbeln) AS total_orders,
    SUM(p~kwmeng) AS total_quantity,
    SUM(p~netwr) AS total_revenue,
    AVG(p~netpr) AS avg_price,
    s~last_sync_date,
    s~sync_status
  FROM vbap AS p
  INNER JOIN vbak AS h ON p~vbeln = h~vbeln
  INNER JOIN makt AS t ON p~matnr = t~matnr
  LEFT OUTER JOIN zmaterial_sync AS s ON p~matnr = s~material_id
  WHERE h~erdat IN @s_date
    AND h~vkorg IN @s_vkorg
    AND p~matnr IN @s_matnr
    AND t~spras = @sy-langu
  GROUP BY p~matnr, t~maktx, s~last_sync_date, s~sync_status
  ORDER BY total_revenue DESCENDING
  INTO CORRESPONDING FIELDS OF TABLE @gt_product_performance.
  
  " Add performance indicators
  LOOP AT gt_product_performance ASSIGNING FIELD-SYMBOL(<ls_product>).
    " Color coding based on sync status
    CASE <ls_product>-sync_status.
      WHEN 'S'.  " Success
        <ls_product>-material_desc = |✅ { <ls_product>-material_desc }|.
      WHEN 'E'.  " Error
        <ls_product>-material_desc = |❌ { <ls_product>-material_desc }|.
      WHEN 'W'.  " Warning
        <ls_product>-material_desc = |⚠️ { <ls_product>-material_desc }|.
      WHEN OTHERS.
        <ls_product>-material_desc = |❓ { <ls_product>-material_desc }|.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_ERROR_ANALYSIS
*&---------------------------------------------------------------------*
FORM load_error_analysis.
  
  CLEAR: gt_error_analysis.
  
  " Load error analysis data
  SELECT 
    timestamp AS error_date,
    CASE 
      WHEN error_message LIKE '%AUTH%' THEN 'Authentication'
      WHEN error_message LIKE '%TIMEOUT%' THEN 'Timeout'
      WHEN error_message LIKE '%CONNECTION%' THEN 'Connection'
      WHEN error_message LIKE '%VALIDATION%' THEN 'Data Validation'
      ELSE 'Other'
    END AS error_category,
    COUNT(*) AS error_count,
    AVG( CAST( resolution_time AS DEC(10,2) ) ) AS resolution_time,
    CASE 
      WHEN COUNT(*) > 100 THEN 'Critical'
      WHEN COUNT(*) > 50 THEN 'High'
      WHEN COUNT(*) > 10 THEN 'Medium'
      ELSE 'Low'
    END AS impact_level
  FROM zapi_integration_log
  WHERE timestamp IN @s_date
    AND success_flag = ''
  GROUP BY timestamp, 
    CASE 
      WHEN error_message LIKE '%AUTH%' THEN 'Authentication'
      WHEN error_message LIKE '%TIMEOUT%' THEN 'Timeout'
      WHEN error_message LIKE '%CONNECTION%' THEN 'Connection'
      WHEN error_message LIKE '%VALIDATION%' THEN 'Data Validation'
      ELSE 'Other'
    END
  ORDER BY error_count DESCENDING
  INTO CORRESPONDING FIELDS OF TABLE @gt_error_analysis.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DASHBOARD
*&---------------------------------------------------------------------*
FORM display_dashboard.
  
  DATA: lr_container TYPE REF TO cl_gui_custom_container,
        lr_splitter  TYPE REF TO cl_gui_splitter_container.
  
  " Create main container and splitter for dashboard layout
  CREATE OBJECT lr_container
    EXPORTING
      container_name = 'DASHBOARD_CONTAINER'.
  
  CREATE OBJECT lr_splitter
    EXPORTING
      parent  = lr_container
      rows    = 2
      columns = 2.
  
  " Display different sections based on selection
  IF p_orders = 'X' AND gt_order_analytics IS NOT INITIAL.
    PERFORM display_order_analytics USING lr_splitter.
  ENDIF.
  
  IF p_integ = 'X' AND gt_integration_metrics IS NOT INITIAL.
    PERFORM display_integration_metrics USING lr_splitter.
  ENDIF.
  
  IF p_prods = 'X' AND gt_product_performance IS NOT INITIAL.
    PERFORM display_product_performance USING lr_splitter.
  ENDIF.
  
  IF p_errors = 'X' AND gt_error_analysis IS NOT INITIAL.
    PERFORM display_error_analysis USING lr_splitter.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ORDER_ANALYTICS
*&---------------------------------------------------------------------*
FORM display_order_analytics USING pr_splitter TYPE REF TO cl_gui_splitter_container.
  
  DATA: lr_container TYPE REF TO cl_gui_container.
  
  " Get container for order analytics
  lr_container = pr_splitter->get_container( row = 1 column = 1 ).
  
  TRY.
      " Create ALV for order analytics
      cl_salv_table=>factory(
        EXPORTING
          r_container = lr_container
        IMPORTING
          r_salv_table = gr_alv_analytics
        CHANGING
          t_table = gt_order_analytics ).
      
      " Configure ALV
      DATA(lr_columns) = gr_alv_analytics->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Set column properties
      DATA(lr_column) = lr_columns->get_column( 'ORDER_DATE' ).
      lr_column->set_long_text( 'Order Date' ).
      lr_column->set_medium_text( 'Date' ).
      
      lr_column = lr_columns->get_column( 'TOTAL_VALUE' ).
      lr_column->set_long_text( 'Total Value' ).
      lr_column->set_medium_text( 'Value' ).
      
      " Add totals
      DATA(lr_aggregations) = gr_alv_analytics->get_aggregations( ).
      lr_aggregations->add_aggregation( 
        columnname = 'ORDER_COUNT'
        aggregation = if_salv_c_aggregation=>total ).
      lr_aggregations->add_aggregation( 
        columnname = 'TOTAL_VALUE'
        aggregation = if_salv_c_aggregation=>total ).
      
      " Enable functions
      DATA(lr_functions) = gr_alv_analytics->get_functions( ).
      lr_functions->set_all( abap_true ).
      
      " Set title
      DATA(lr_display) = gr_alv_analytics->get_display_settings( ).
      lr_display->set_list_header( 'Order Analytics Dashboard' ).
      
      " Display
      gr_alv_analytics->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_INTEGRATION_METRICS
*&---------------------------------------------------------------------*
FORM display_integration_metrics USING pr_splitter TYPE REF TO cl_gui_splitter_container.
  
  DATA: lr_container TYPE REF TO cl_gui_container.
  
  lr_container = pr_splitter->get_container( row = 1 column = 2 ).
  
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = lr_container
        IMPORTING
          r_salv_table = gr_alv_metrics
        CHANGING
          t_table = gt_integration_metrics ).
      
      " Configure columns
      DATA(lr_columns) = gr_alv_metrics->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Add conditional formatting for success rate
      DATA(lr_column) = lr_columns->get_column( 'SUCCESS_RATE' ).
      lr_column->set_long_text( 'Success Rate %' ).
      
      " Set title
      DATA(lr_display) = gr_alv_metrics->get_display_settings( ).
      lr_display->set_list_header( 'Integration Metrics' ).
      
      gr_alv_metrics->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_PRODUCT_PERFORMANCE
*&---------------------------------------------------------------------*
FORM display_product_performance USING pr_splitter TYPE REF TO cl_gui_splitter_container.
  
  DATA: lr_container TYPE REF TO cl_gui_container.
  
  lr_container = pr_splitter->get_container( row = 2 column = 1 ).
  
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = lr_container
        IMPORTING
          r_salv_table = gr_alv_products
        CHANGING
          t_table = gt_product_performance ).
      
      " Configure ALV
      DATA(lr_columns) = gr_alv_products->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Hide sync status column and show in description
      lr_columns->get_column( 'SYNC_STATUS' )->set_visible( abap_false ).
      
      " Set title
      DATA(lr_display) = gr_alv_products->get_display_settings( ).
      lr_display->set_list_header( 'Product Performance' ).
      
      gr_alv_products->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ERROR_ANALYSIS
*&---------------------------------------------------------------------*
FORM display_error_analysis USING pr_splitter TYPE REF TO cl_gui_splitter_container.
  
  DATA: lr_container TYPE REF TO cl_gui_container.
  
  lr_container = pr_splitter->get_container( row = 2 column = 2 ).
  
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = lr_container
        IMPORTING
          r_salv_table = gr_alv_errors
        CHANGING
          t_table = gt_error_analysis ).
      
      " Configure ALV
      DATA(lr_columns) = gr_alv_errors->get_columns( ).
      lr_columns->set_optimize( abap_true ).
      
      " Set title
      DATA(lr_display) = gr_alv_errors->get_display_settings( ).
      lr_display->set_list_header( 'Error Analysis' ).
      
      gr_alv_errors->display( ).
      
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

" Additional utility forms for enhanced functionality

*&---------------------------------------------------------------------*
*& Form EXPORT_TO_EXCEL
*&---------------------------------------------------------------------*
FORM export_to_excel.
  " Export dashboard data to Excel for further analysis
  
  DATA: lr_excel TYPE REF TO zcl_excel_writer.
  
  CREATE OBJECT lr_excel.
  
  " Add different sheets for each data set
  IF gt_order_analytics IS NOT INITIAL.
    lr_excel->add_worksheet( 
      iv_sheet_name = 'Order Analytics'
      it_data = gt_order_analytics ).
  ENDIF.
  
  IF gt_integration_metrics IS NOT INITIAL.
    lr_excel->add_worksheet( 
      iv_sheet_name = 'Integration Metrics'
      it_data = gt_integration_metrics ).
  ENDIF.
  
  " Save Excel file
  lr_excel->save_file( 'ECommerce_Dashboard_Export.xlsx' ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCHEDULE_AUTOMATIC_REFRESH
*&---------------------------------------------------------------------*
FORM schedule_automatic_refresh.
  " Schedule background job for automatic dashboard refresh
  
  DATA: lv_jobname TYPE btcjob,
        lv_jobcount TYPE btcjobcnt.
  
  " Create background job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname = 'ECOMMERCE_DASHBOARD_REFRESH'
    IMPORTING
      jobcount = lv_jobcount.
  
  " Submit this report as background job
  SUBMIT zr_ecommerce_dashboard
    WITH p_realtime = 'X'
    WITH p_orders = 'X'
    WITH p_integ = 'X'
    VIA JOB 'ECOMMERCE_DASHBOARD_REFRESH' NUMBER lv_jobcount
    AND RETURN.
  
  " Schedule job to run every hour
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount = lv_jobcount
      jobname = 'ECOMMERCE_DASHBOARD_REFRESH'
      strtimmed = 'X'
      periodic_id = 'HOURLY'.

ENDFORM.