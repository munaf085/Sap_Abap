# Module 7: Reports Development

## 🎯 **Complete Guide to ABAP Reports**

**Learn reporting from basics to enterprise-level patterns - No reporting experience required!**

Master ABAP reports from fundamental concepts to sophisticated analytics dashboards used in enterprise SAP environments.

---

## 📖 **Table of Contents**
1. [🌟 Reports Fundamentals - What & Why](#-reports-fundamentals---what--why)
2. [📝 Your First Classical Report](#-your-first-classical-report)
3. [🔧 Selection Screens & Parameters](#-selection-screens--parameters)
4. [🔄 Interactive Reports & Drill-Down](#-interactive-reports--drill-down)
5. [📊 ALV Framework - Modern Lists](#-alv-framework---modern-lists)
6. [⚡ Report Performance Optimization](#-report-performance-optimization)
7. [🎨 Dynamic Report Generation](#-dynamic-report-generation)
8. [🚀 Enterprise Reporting Patterns](#-enterprise-reporting-patterns)

---

## 🌟 **Reports Fundamentals - What & Why**

### **What are ABAP Reports?**

An **ABAP Report** is like a **data summary tool** that extracts information from the database and presents it in a readable format. Think of it as asking SAP: "Show me all customers who bought products last month, and organize the information nicely."

#### **Real-World Analogy: Restaurant Order Summary**
```abap
" Manual Process (Without Reports):
" - Go through each receipt individually
" - Write down details on paper
" - Calculate totals manually
" - Very time-consuming!

" ABAP Report Process:
" - Tell computer what you want to see
" - System automatically finds all data
" - Presents organized summary
" - Instant results!
```

### **Why Do We Need Reports?**

#### **Benefits:**
- 📊 **Data Analysis** - Turn raw data into meaningful information
- 📈 **Business Intelligence** - Support decision making
- 📋 **Documentation** - Create printable summaries
- 🔍 **Data Exploration** - Find patterns and trends
- 📱 **User-Friendly** - Non-technical users can get information

#### **Types of Information Reports Provide:**
```abap
" Business Examples:
" - Monthly sales summary by region
" - Customer payment status report
" - Inventory levels by warehouse
" - Employee attendance records
" - Financial account balances
" - Product performance analysis
```

### **Types of ABAP Reports - Simple Explanation**

| **Report Type** | **What It Does** | **Real-World Example** |
|-----------------|------------------|------------------------|
| **Classical Report** | Shows data in simple list | Phone book listing |
| **Interactive Report** | Click for more details | Web page with links |
| **ALV Report** | Modern grid with features | Excel-like spreadsheet |
| **Form Report** | Formatted documents | Invoice or receipt |
| **Analytical Report** | Charts and graphs | Sales dashboard |

---

## 📝 **Your First Classical Report**

### **Creating a Simple Report**

Let's create a basic student report step by step.

#### **Step 1: Basic Report Structure**
```abap
*&---------------------------------------------------------------------*
*& Report Z_STUDENT_LIST
*& Purpose: Display list of students
*& Author: Learning ABAP
*&---------------------------------------------------------------------*

REPORT z_student_list.

" This is where our report starts
START-OF-SELECTION.
  WRITE: 'My First ABAP Report!'.
  WRITE: / 'This will show student information.'.
```

#### **Step 2: Add Data Selection**
```abap
REPORT z_student_report.

" Define what data we want to work with
TYPES: BEGIN OF ty_student,
         id     TYPE c LENGTH 8,
         name   TYPE c LENGTH 30,
         grade  TYPE c LENGTH 2,
         score  TYPE i,
       END OF ty_student.

DATA: lt_students TYPE TABLE OF ty_student,
      ls_student  TYPE ty_student.

" Get data (in real life, this would come from database)
START-OF-SELECTION.
  " Add sample students
  ls_student-id = 'STU001'.
  ls_student-name = 'John Smith'.
  ls_student-grade = 'A'.
  ls_student-score = 95.
  APPEND ls_student TO lt_students.
  
  ls_student-id = 'STU002'.
  ls_student-name = 'Mary Johnson'.
  ls_student-grade = 'B'.
  ls_student-score = 87.
  APPEND ls_student TO lt_students.
  
  ls_student-id = 'STU003'.
  ls_student-name = 'Bob Wilson'.
  ls_student-grade = 'A'.
  ls_student-score = 92.
  APPEND ls_student TO lt_students.
```

#### **Step 3: Display the Data**
```abap
" Display the report
END-OF-SELECTION.
  WRITE: / 'STUDENT REPORT',
         / '=============='.
  WRITE: /.
  
  " Column headers
  WRITE: / 'ID      Name                     Grade Score'.
  WRITE: / '------- ------------------------ ----- -----'.
  
  " Display each student
  LOOP AT lt_students INTO ls_student.
    WRITE: / ls_student-id,
             ls_student-name,
             ls_student-grade,
             ls_student-score.
  ENDLOOP.
  
  WRITE: /, 
         / |Total Students: { lines( lt_students ) }|.
```

### **Understanding Report Events**

Reports have different **events** - points where you can add your code:

#### **Report Events Explained**
```abap
" INITIALIZATION - Runs first, set default values
INITIALIZATION.
  WRITE: / 'Setting up report...'.

" AT SELECTION-SCREEN - Validates user input  
AT SELECTION-SCREEN.
  " Check if user entered valid data
  
" START-OF-SELECTION - Main processing begins
START-OF-SELECTION.
  WRITE: / 'Starting report processing...'.
  " Get data from database
  " Process calculations
  
" END-OF-SELECTION - Display results
END-OF-SELECTION.
  WRITE: / 'Displaying results...'.
  " Show the formatted output
  
" TOP-OF-PAGE - Header on each page
TOP-OF-PAGE.
  WRITE: / 'Company Name - Student Report'.
  WRITE: / sy-datum.  " Current date
```

---

## 🔧 **Selection Screens & Parameters**

### **Adding User Input**

Selection screens let users specify what data they want to see - like filters in Excel.

#### **Basic Parameters**
```abap
REPORT z_student_report_with_input.

" User can enter student ID to search for
PARAMETERS: p_stuid TYPE c LENGTH 8.

" User can choose grade to filter by
PARAMETERS: p_grade TYPE c LENGTH 2.

" User can check a box for detailed output
PARAMETERS: p_detail TYPE abap_bool AS CHECKBOX.

START-OF-SELECTION.
  IF p_stuid IS NOT INITIAL.
    WRITE: / |Searching for student: { p_stuid }|.
  ENDIF.
  
  IF p_grade IS NOT INITIAL.
    WRITE: / |Filtering by grade: { p_grade }|.
  ENDIF.
  
  IF p_detail = abap_true.
    WRITE: / 'Detailed output requested'.
  ENDIF.
```

#### **Select-Options for Ranges**
```abap
" Let user enter range of dates or IDs
SELECT-OPTIONS: s_stuid FOR ls_student-id,     " Range of student IDs
                s_score FOR ls_student-score.  " Range of scores

START-OF-SELECTION.
  " Use the ranges in database selection
  LOOP AT lt_students INTO ls_student
    WHERE id IN s_stuid        " Student ID in range
    AND   score IN s_score.    " Score in range
    
    " Display matching students
    WRITE: / |{ ls_student-id }: { ls_student-name } - { ls_student-score }|.
  ENDLOOP.
```

#### **Organized Selection Screen**
```abap
REPORT z_advanced_student_report.

" Group related fields together
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS: p_clss TYPE c LENGTH 10 DEFAULT 'CS101'.
  SELECT-OPTIONS: s_stuid FOR ls_student-id.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS: p_detail AS CHECKBOX DEFAULT 'X',
              p_export AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

" Text elements (SE32 - Text Elements)
" TEXT-001: 'Student Selection'
" TEXT-002: 'Output Options'
```

### **Real-World Example: Sales Report with Parameters**
```abap
REPORT z_sales_report.

" Define data structure
TYPES: BEGIN OF ty_sales,
         sales_date TYPE dats,
         customer   TYPE c LENGTH 20,
         product    TYPE c LENGTH 20,
         amount     TYPE p DECIMALS 2,
         region     TYPE c LENGTH 10,
       END OF ty_sales.

DATA: lt_sales TYPE TABLE OF ty_sales,
      ls_sales TYPE ty_sales.

" Selection screen for user input
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS: s_date FOR ls_sales-sales_date OBLIGATORY.
  PARAMETERS: p_region TYPE c LENGTH 10.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS: p_total AS CHECKBOX DEFAULT 'X',
              p_detail AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  " Sample data (in real life, SELECT from database)
  lt_sales = VALUE #(
    ( sales_date = '20231001' customer = 'ABC Corp' product = 'Laptop' amount = '1200.00' region = 'North' )
    ( sales_date = '20231002' customer = 'XYZ Inc' product = 'Mouse' amount = '25.00' region = 'South' )
    ( sales_date = '20231003' customer = 'DEF Ltd' product = 'Monitor' amount = '300.00' region = 'North' )
  ).

END-OF-SELECTION.
  " Header
  WRITE: / 'SALES REPORT',
         / '============'.
  WRITE: / |Period: { s_date-low } to { s_date-high }|.
  IF p_region IS NOT INITIAL.
    WRITE: / |Region: { p_region }|.
  ENDIF.
  WRITE: /.

  " Data
  DATA: lv_total TYPE p DECIMALS 2.
  
  LOOP AT lt_sales INTO ls_sales
    WHERE sales_date IN s_date
    AND   ( p_region IS INITIAL OR region = p_region ).
    
    IF p_detail = abap_true.
      WRITE: / |{ ls_sales-sales_date } { ls_sales-customer } { ls_sales-product } ${ ls_sales-amount }|.
    ENDIF.
    
    lv_total = lv_total + ls_sales-amount.
  ENDLOOP.
  
  IF p_total = abap_true.
    WRITE: /, / |Total Sales: ${ lv_total }|.
  ENDIF.
```

---

## 1. Classical Reports Advanced

### Complex Report Architecture

#### **Hierarchical Report Structure**
```abap
*&---------------------------------------------------------------------*
*& Report Z_ENTERPRISE_FINANCIAL_ANALYSIS
*&---------------------------------------------------------------------*
*& Purpose: Multi-dimensional financial analysis with drill-down
*& Author: Expert Developer
*& Architecture: Modular design with separation of concerns
*&---------------------------------------------------------------------*

REPORT z_enterprise_financial_analysis.

" ===== TYPE DEFINITIONS =====
TYPES: BEGIN OF ty_financial_data,
         company_code    TYPE bukrs,
         profit_center   TYPE prctr,
         cost_center     TYPE kostl,
         fiscal_year     TYPE gjahr,
         fiscal_period   TYPE monat,
         account         TYPE saknr,
         account_text    TYPE txt50_skat,
         debit_amount    TYPE dmbtr,
         credit_amount   TYPE dmbtr,
         net_amount      TYPE dmbtr,
         currency        TYPE waers,
         exchange_rate   TYPE ukurs,
         base_amount     TYPE dmbtr,
       END OF ty_financial_data,
       
       tt_financial_data TYPE TABLE OF ty_financial_data,
       
       BEGIN OF ty_summary,
         dimension       TYPE string,
         description     TYPE string,
         total_debit     TYPE dmbtr,
         total_credit    TYPE dmbtr,
         net_position    TYPE dmbtr,
         percentage      TYPE p DECIMALS 2,
       END OF ty_summary,
       
       tt_summary TYPE TABLE OF ty_summary.

" ===== DATA DECLARATIONS =====
DATA: gt_financial_data TYPE tt_financial_data,
      gt_summary_cc     TYPE tt_summary,  " Cost Center Summary
      gt_summary_pc     TYPE tt_summary,  " Profit Center Summary
      gt_summary_acc    TYPE tt_summary,  " Account Summary
      go_report_engine  TYPE REF TO zcl_report_engine,
      go_data_processor TYPE REF TO zcl_financial_processor.

" ===== SELECTION SCREEN =====
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY MEMORY ID buk,
            p_gjahr TYPE gjahr OBLIGATORY DEFAULT sy-datum(4),
            p_curr  TYPE waers DEFAULT 'USD'.
            
SELECT-OPTIONS: s_monat FOR sy-datum+4(2) DEFAULT '01' TO '12',
                s_kostl FOR acdoca-kostl,
                s_prctr FOR acdoca-prctr,
                s_saknr FOR acdoca-racct.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_detail AS CHECKBOX DEFAULT 'X',
            p_summ   AS CHECKBOX DEFAULT 'X',
            p_graph  AS CHECKBOX,
            p_export AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

" ===== EVENTS =====
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  PERFORM validate_selection_screen.

START-OF-SELECTION.
  PERFORM main_processing.

END-OF-SELECTION.
  PERFORM output_generation.

" ===== IMPLEMENTATIONS =====

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  " Initialize report framework
  CREATE OBJECT go_report_engine
    EXPORTING
      iv_report_id = 'Z_FIN_ANALYSIS'
      iv_version   = '2.0'.
      
  CREATE OBJECT go_data_processor.
  
  " Set default values based on user profile
  PERFORM set_user_defaults.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAIN_PROCESSING
*&---------------------------------------------------------------------*
FORM main_processing.
  DATA: lv_start_time TYPE timestamp,
        lv_end_time   TYPE timestamp.
        
  GET TIME STAMP FIELD lv_start_time.
  
  " Performance monitoring
  go_report_engine->start_performance_monitoring( ).
  
  " Data extraction with optimization
  PERFORM extract_financial_data.
  
  " Data processing and calculations
  PERFORM process_financial_data.
  
  " Generate summaries
  IF p_summ = 'X'.
    PERFORM generate_summaries.
  ENDIF.
  
  GET TIME STAMP FIELD lv_end_time.
  
  " Log performance metrics
  go_report_engine->log_performance( 
    iv_start_time = lv_start_time
    iv_end_time = lv_end_time
    iv_records_processed = lines( gt_financial_data )
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXTRACT_FINANCIAL_DATA
*&---------------------------------------------------------------------*
FORM extract_financial_data.
  " Optimized data extraction with parallel processing
  DATA: lt_ranges TYPE ztt_parallel_ranges,
        lo_parallel_extractor TYPE REF TO zcl_parallel_extractor.
        
  " Create parallel processing ranges
  CREATE OBJECT lo_parallel_extractor.
  
  lt_ranges = lo_parallel_extractor->create_date_ranges( 
    iv_from_date = |{ p_gjahr }{ s_monat-low }01|
    iv_to_date   = |{ p_gjahr }{ s_monat-high }31|
    iv_chunks    = 4
  ).
  
  " Extract data in parallel chunks
  LOOP AT lt_ranges INTO DATA(ls_range).
    lo_parallel_extractor->extract_chunk_async( 
      is_range = ls_range
      iv_company_code = p_bukrs
      it_cost_centers = s_kostl[]
      it_profit_centers = s_prctr[]
      it_accounts = s_saknr[]
    ).
  ENDLOOP.
  
  " Wait for all extractions to complete
  lo_parallel_extractor->wait_for_completion( ).
  
  " Merge results
  gt_financial_data = lo_parallel_extractor->get_merged_results( ).
  
  " Apply currency conversion if needed
  IF p_curr <> 'USD'.  " Assuming USD is base currency
    PERFORM convert_currency.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_FINANCIAL_DATA
*&---------------------------------------------------------------------*
FORM process_financial_data.
  " Advanced data processing with business logic
  
  " Calculate derived fields
  LOOP AT gt_financial_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    " Net amount calculation
    <ls_data>-net_amount = <ls_data>-debit_amount - <ls_data>-credit_amount.
    
    " Base currency conversion
    <ls_data>-base_amount = <ls_data>-net_amount * <ls_data>-exchange_rate.
    
    " Enrich with master data
    go_data_processor->enrich_account_text( CHANGING cs_data = <ls_data> ).
  ENDLOOP.
  
  " Apply business rules
  go_data_processor->apply_business_rules( CHANGING ct_data = gt_financial_data ).
  
  " Data quality checks
  go_data_processor->validate_data_quality( 
    EXPORTING it_data = gt_financial_data
    IMPORTING et_issues = DATA(lt_quality_issues)
  ).
  
  " Report data quality issues
  IF lines( lt_quality_issues ) > 0.
    go_report_engine->log_data_quality_issues( lt_quality_issues ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GENERATE_SUMMARIES
*&---------------------------------------------------------------------*
FORM generate_summaries.
  " Multi-dimensional aggregation
  
  " Cost Center Summary
  go_data_processor->aggregate_by_cost_center( 
    EXPORTING it_source = gt_financial_data
    IMPORTING et_summary = gt_summary_cc
  ).
  
  " Profit Center Summary
  go_data_processor->aggregate_by_profit_center( 
    EXPORTING it_source = gt_financial_data
    IMPORTING et_summary = gt_summary_pc
  ).
  
  " Account Summary
  go_data_processor->aggregate_by_account( 
    EXPORTING it_source = gt_financial_data
    IMPORTING et_summary = gt_summary_acc
  ).
  
  " Calculate percentages and ratios
  PERFORM calculate_analytics.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form OUTPUT_GENERATION
*&---------------------------------------------------------------------*
FORM output_generation.
  " Multi-format output generation
  
  IF p_detail = 'X'.
    " Detailed ALV report
    PERFORM display_detailed_alv.
  ENDIF.
  
  IF p_summ = 'X'.
    " Summary dashboard
    PERFORM display_summary_dashboard.
  ENDIF.
  
  IF p_graph = 'X'.
    " Graphical analysis
    PERFORM display_charts.
  ENDIF.
  
  IF p_export = 'X'.
    " Export to Excel with formatting
    PERFORM export_to_excel.
  ENDIF.
ENDFORM.
```

#### **Advanced Selection Screen with Variants**
```abap
" Enhanced selection screen with dynamic options
SELECTION-SCREEN BEGIN OF TABBED BLOCK mytab FOR 8 LINES.

" Tab 1: Basic Parameters
SELECTION-SCREEN TAB (20) tab1 USER-COMMAND ucomm1 DEFAULT SCREEN 1001.
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK sel1 WITH FRAME TITLE TEXT-s01.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY MEMORY ID buk 
              MATCHCODE OBJECT h_t001,
            p_gjahr TYPE gjahr OBLIGATORY DEFAULT sy-datum(4)
              MATCHCODE OBJECT h_t009b.
SELECT-OPTIONS: s_monat FOR sy-datum+4(2) DEFAULT '01' TO '12' NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK sel1.
SELECTION-SCREEN END OF SCREEN 1001.

" Tab 2: Advanced Filters
SELECTION-SCREEN TAB (20) tab2 USER-COMMAND ucomm2 DEFAULT SCREEN 1002.
SELECTION-SCREEN BEGIN OF SCREEN 1002 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK sel2 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS: s_kostl FOR acdoca-kostl MATCHCODE OBJECT h_csks,
                s_prctr FOR acdoca-prctr MATCHCODE OBJECT h_cepc,
                s_saknr FOR acdoca-racct MATCHCODE OBJECT h_ska1,
                s_hkont FOR acdoca-hkont NO INTERVALS.
PARAMETERS: p_only_posted AS CHECKBOX DEFAULT 'X',
            p_include_stat AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK sel2.
SELECTION-SCREEN END OF SCREEN 1002.

" Tab 3: Output Options
SELECTION-SCREEN TAB (20) tab3 USER-COMMAND ucomm3 DEFAULT SCREEN 1003.
SELECTION-SCREEN BEGIN OF SCREEN 1003 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK sel3 WITH FRAME TITLE TEXT-s03.
PARAMETERS: p_layout TYPE disvariant-variant MATCHCODE OBJECT h_alv_variant,
            p_max_rows TYPE i DEFAULT 50000,
            p_curr TYPE waers DEFAULT 'USD' MATCHCODE OBJECT h_tcurc.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-out FOR FIELD p_alv.
PARAMETERS: p_alv RADIOBUTTON GROUP out DEFAULT 'X',
            p_excel RADIOBUTTON GROUP out,
            p_pdf RADIOBUTTON GROUP out.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sel3.
SELECTION-SCREEN END OF SCREEN 1003.

SELECTION-SCREEN END OF BLOCK mytab.

" Event handling for tabbed selection screen
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'UCOMM1'.
      mytab-activetab = 'TAB1'.
      mytab-dynnr = '1001'.
    WHEN 'UCOMM2'.
      mytab-activetab = 'TAB2'.
      mytab-dynnr = '1002'.
    WHEN 'UCOMM3'.
      mytab-activetab = 'TAB3'.
      mytab-dynnr = '1003'.
  ENDCASE.
```

---

## 2. Interactive Reports & Drill-Down

### Advanced Interactive Reporting

#### **Multi-Level Drill-Down Framework**
```abap
CLASS zcl_interactive_report DEFINITION.
  PUBLIC SECTION.
    METHODS: display_report
               IMPORTING iv_level TYPE i DEFAULT 1
                         it_filter TYPE ztt_report_filter OPTIONAL,
                         
             handle_user_command
               IMPORTING iv_ucomm TYPE sy-ucomm
                         iv_selfield TYPE slis_selfield.
                         
  PRIVATE SECTION.
    DATA: mt_level1_data TYPE ztt_summary_level1,
          mt_level2_data TYPE ztt_detail_level2,
          mt_level3_data TYPE ztt_transaction_level3,
          mv_current_level TYPE i,
          ms_current_filter TYPE zreport_filter.
          
    METHODS: build_level1_display,
             build_level2_display
               IMPORTING is_level1_key TYPE zkey_level1,
             build_level3_display  
               IMPORTING is_level2_key TYPE zkey_level2,
               
             setup_interactive_events
               IMPORTING io_alv TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS zcl_interactive_report IMPLEMENTATION.
  METHOD display_report.
    mv_current_level = iv_level.
    
    CASE mv_current_level.
      WHEN 1.
        build_level1_display( ).
      WHEN 2.
        " Drill-down to level 2 triggered by user interaction
        " Filter data based on level 1 selection
      WHEN 3.
        " Drill-down to level 3 (transaction details)
    ENDCASE.
  ENDMETHOD.
  
  METHOD build_level1_display.
    " Summary level display
    DATA: lo_alv TYPE REF TO cl_salv_table.
    
    " Create ALV instance
    cl_salv_table=>factory( 
      IMPORTING r_salv_table = lo_alv
      CHANGING t_table = mt_level1_data
    ).
    
    " Setup columns
    DATA(lo_columns) = lo_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
    
    " Enable hotspots for drill-down
    DATA(lo_column) = lo_columns->get_column( 'PROFIT_CENTER' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    
    " Setup interactive events
    setup_interactive_events( lo_alv ).
    
    " Display
    lo_alv->display( ).
  ENDMETHOD.
  
  METHOD handle_user_command.
    CASE iv_ucomm.
      WHEN '&IC1'.  " Double-click or hotspot
        CASE iv_selfield-fieldname.
          WHEN 'PROFIT_CENTER'.
            " Drill down to profit center details
            DATA(ls_level1_key) = VALUE zkey_level1( 
              profit_center = iv_selfield-value
            ).
            build_level2_display( ls_level1_key ).
            
          WHEN 'COST_CENTER'.
            " Drill down to cost center details
            
          WHEN 'ACCOUNT'.
            " Drill down to account details
            
        ENDCASE.
        
      WHEN 'BACK'.
        " Navigate back to previous level
        mv_current_level -= 1.
        display_report( mv_current_level ).
        
      WHEN 'EXPORT'.
        " Export current view to Excel
        
    ENDCASE.
  ENDMETHOD.
  
  METHOD setup_interactive_events.
    " Setup event handling for user interactions
    DATA(lo_events) = io_alv->get_event( ).
    
    SET HANDLER me->handle_user_command FOR lo_events.
    
    " Enable standard functions
    DATA(lo_functions) = io_alv->get_functions( ).
    lo_functions->set_all( abap_true ).
    
    " Add custom buttons
    lo_functions->add_function( 
      name = 'DRILL_DOWN'
      icon = icon_drill_down
      text = 'Drill Down'
      tooltip = 'Drill down to next level'
      position = if_salv_c_function_position=>right_of_salv_functions
    ).
  ENDMETHOD.
ENDCLASS.
```

#### **Context-Sensitive Navigation**
```abap
" Dynamic context menu based on data content
CLASS zcl_context_menu_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: build_context_menu
               IMPORTING is_row_data TYPE any
                         iv_column TYPE string
               RETURNING VALUE(ro_context_menu) TYPE REF TO cl_ctmenu,
               
             handle_context_selection
               IMPORTING iv_fcode TYPE sy-ucomm
                         is_row_data TYPE any.
                         
  PRIVATE SECTION.
    METHODS: add_navigation_options
               IMPORTING io_menu TYPE REF TO cl_ctmenu
                         is_data TYPE any,
                         
             add_action_options
               IMPORTING io_menu TYPE REF TO cl_ctmenu
                         is_data TYPE any.
ENDCLASS.

CLASS zcl_context_menu_handler IMPLEMENTATION.
  METHOD build_context_menu.
    CREATE OBJECT ro_context_menu.
    
    " Add standard options
    ro_context_menu->add_function( 
      fcode = 'DETAIL'
      text = 'Show Details'
    ).
    
    " Add navigation options based on data type
    add_navigation_options( 
      io_menu = ro_context_menu
      is_data = is_row_data
    ).
    
    " Add action options based on user authorization
    add_action_options( 
      io_menu = ro_context_menu
      is_data = is_row_data
    ).
    
    ro_context_menu->add_separator( ).
    
    " Add export options
    ro_context_menu->add_function( 
      fcode = 'EXPORT_ROW'
      text = 'Export Row'
    ).
  ENDMETHOD.
  
  METHOD add_navigation_options.
    " Dynamic navigation based on data content
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lt_components TYPE cl_abap_structdescr=>component_table.
          
    lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( is_data ).
    lt_components = lo_struct_descr->get_components( ).
    
    " Check for customer-related fields
    READ TABLE lt_components TRANSPORTING NO FIELDS
      WITH KEY name = 'CUSTOMER_ID'.
    IF sy-subrc = 0.
      io_menu->add_function( 
        fcode = 'SHOW_CUSTOMER'
        text = 'Customer Master'
      ).
    ENDIF.
    
    " Check for material-related fields
    READ TABLE lt_components TRANSPORTING NO FIELDS
      WITH KEY name = 'MATERIAL'.
    IF sy-subrc = 0.
      io_menu->add_function( 
        fcode = 'SHOW_MATERIAL'
        text = 'Material Master'
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## 3. ALV Framework Mastery

### Advanced ALV Customization

#### **Enterprise ALV Factory**
```abap
CLASS zcl_enterprise_alv_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: create_advanced_alv
                     IMPORTING ir_data TYPE REF TO data
                               is_layout TYPE zalv_layout_config
                     RETURNING VALUE(ro_alv) TYPE REF TO cl_salv_table,
                     
                   apply_corporate_theme
                     IMPORTING io_alv TYPE REF TO cl_salv_table,
                     
                   setup_enterprise_functions
                     IMPORTING io_alv TYPE REF TO cl_salv_table
                               it_custom_functions TYPE ztt_custom_functions.
                               
  PRIVATE SECTION.
    CLASS-METHODS: setup_columns
                     IMPORTING io_alv TYPE REF TO cl_salv_table
                               is_layout TYPE zalv_layout_config,
                               
                   setup_aggregations
                     IMPORTING io_alv TYPE REF TO cl_salv_table
                               it_aggregations TYPE ztt_alv_aggregations,
                               
                   setup_conditional_formatting
                     IMPORTING io_alv TYPE REF TO cl_salv_table
                               it_conditions TYPE ztt_format_conditions.
ENDCLASS.

CLASS zcl_enterprise_alv_factory IMPLEMENTATION.
  METHOD create_advanced_alv.
    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
    ASSIGN ir_data->* TO <lt_data>.
    
    " Create SALV instance
    cl_salv_table=>factory( 
      IMPORTING r_salv_table = ro_alv
      CHANGING t_table = <lt_data>
    ).
    
    " Apply layout configuration
    setup_columns( io_alv = ro_alv is_layout = is_layout ).
    
    " Setup aggregations if specified
    IF lines( is_layout-aggregations ) > 0.
      setup_aggregations( 
        io_alv = ro_alv
        it_aggregations = is_layout-aggregations
      ).
    ENDIF.
    
    " Setup conditional formatting
    IF lines( is_layout-format_conditions ) > 0.
      setup_conditional_formatting( 
        io_alv = ro_alv
        it_conditions = is_layout-format_conditions
      ).
    ENDIF.
    
    " Apply corporate theme
    apply_corporate_theme( ro_alv ).
  ENDMETHOD.
  
  METHOD apply_corporate_theme.
    " Apply company-specific styling
    DATA(lo_display) = io_alv->get_display_settings( ).
    
    " Set corporate colors and fonts
    lo_display->set_striped_pattern( abap_true ).
    lo_display->set_list_header( 'Enterprise Financial Report' ).
    
    " Setup zebra pattern with corporate colors
    DATA(lo_layout) = io_alv->get_layout( ).
    lo_layout->set_key( value = 'ENTERPRISE_LAYOUT' ).
    lo_layout->set_default( abap_true ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ENDMETHOD.
  
  METHOD setup_columns.
    DATA(lo_columns) = io_alv->get_columns( ).
    
    " Optimize all columns
    lo_columns->set_optimize( abap_true ).
    
    " Configure specific columns based on layout
    LOOP AT is_layout-column_config INTO DATA(ls_column_config).
      TRY.
          DATA(lo_column) = lo_columns->get_column( ls_column_config-fieldname ).
          
          " Set column properties
          lo_column->set_short_text( ls_column_config-short_text ).
          lo_column->set_medium_text( ls_column_config-medium_text ).
          lo_column->set_long_text( ls_column_config-long_text ).
          
          " Set data type specific formatting
          CASE ls_column_config-data_type.
            WHEN 'CURR'.
              lo_column->set_currency_column( ls_column_config-currency_field ).
            WHEN 'QUAN'.
              lo_column->set_quantity_column( ls_column_config-unit_field ).
            WHEN 'DATS'.
              lo_column->set_edit_mask( '__/__/____' ).
          ENDCASE.
          
          " Set column width and alignment
          IF ls_column_config-output_length > 0.
            lo_column->set_output_length( ls_column_config-output_length ).
          ENDIF.
          
          " Enable/disable features
          IF ls_column_config-hotspot = abap_true.
            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
          ENDIF.
          
          IF ls_column_config-key = abap_true.
            lo_column->set_key( abap_true ).
          ENDIF.
          
        CATCH cx_salv_not_found.
          " Column not found - log warning
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD setup_aggregations.
    DATA(lo_aggregations) = io_alv->get_aggregations( ).
    
    LOOP AT it_aggregations INTO DATA(ls_aggregation).
      TRY.
          CASE ls_aggregation-aggregation_type.
            WHEN 'SUM'.
              lo_aggregations->add_aggregation( 
                columnname = ls_aggregation-column_name
                aggregation = if_salv_c_aggregation=>total
              ).
            WHEN 'AVG'.
              lo_aggregations->add_aggregation( 
                columnname = ls_aggregation-column_name
                aggregation = if_salv_c_aggregation=>average
              ).
            WHEN 'COUNT'.
              lo_aggregations->add_aggregation( 
                columnname = ls_aggregation-column_name
                aggregation = if_salv_c_aggregation=>count
              ).
          ENDCASE.
          
        CATCH cx_salv_data_error.
          " Invalid aggregation - log error
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD setup_conditional_formatting.
    DATA(lo_functional_settings) = io_alv->get_functional_settings( ).
    DATA(lo_sorts) = io_alv->get_sorts( ).
    
    " Implement conditional formatting based on business rules
    LOOP AT it_conditions INTO DATA(ls_condition).
      " Example: Highlight negative values in red
      IF ls_condition-condition_type = 'NEGATIVE_VALUES'.
        " Setup conditional formatting for negative amounts
        " This would require custom implementation
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

#### **Dynamic Column Generation**
```abap
CLASS zcl_dynamic_alv_builder DEFINITION.
  PUBLIC SECTION.
    METHODS: build_dynamic_alv
               IMPORTING it_field_catalog TYPE ztt_field_catalog
                         ir_data TYPE REF TO data
               RETURNING VALUE(ro_alv) TYPE REF TO cl_salv_table.
               
  PRIVATE SECTION.
    METHODS: create_dynamic_structure
               IMPORTING it_field_catalog TYPE ztt_field_catalog
               RETURNING VALUE(rr_struct_type) TYPE REF TO cl_abap_structdescr,
               
             populate_dynamic_data
               IMPORTING ir_source TYPE REF TO data
                         it_field_mapping TYPE ztt_field_mapping
               CHANGING cr_target TYPE REF TO data.
ENDCLASS.

CLASS zcl_dynamic_alv_builder IMPLEMENTATION.
  METHOD build_dynamic_alv.
    " Create dynamic structure based on field catalog
    DATA(lr_struct_type) = create_dynamic_structure( it_field_catalog ).
    
    " Create dynamic internal table
    DATA(lr_table_type) = cl_abap_tabledescr=>create( lr_struct_type ).
    CREATE DATA ir_data TYPE HANDLE lr_table_type.
    
    " Populate data
    populate_dynamic_data( 
      ir_source = ir_data
      it_field_mapping = VALUE #( )  " Define mapping rules
      CHANGING cr_target = ir_data
    ).
    
    " Create ALV
    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
    ASSIGN ir_data->* TO <lt_data>.
    
    cl_salv_table=>factory( 
      IMPORTING r_salv_table = ro_alv
      CHANGING t_table = <lt_data>
    ).
    
    " Apply field catalog settings
    DATA(lo_columns) = ro_alv->get_columns( ).
    
    LOOP AT it_field_catalog INTO DATA(ls_field).
      TRY.
          DATA(lo_column) = lo_columns->get_column( ls_field-fieldname ).
          
          lo_column->set_short_text( ls_field-scrtext_s ).
          lo_column->set_medium_text( ls_field-scrtext_m ).
          lo_column->set_long_text( ls_field-scrtext_l ).
          
          IF ls_field-datatype = 'CURR'.
            lo_column->set_currency_column( ls_field-cfieldname ).
          ENDIF.
          
        CATCH cx_salv_not_found.
          " Handle missing column
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD create_dynamic_structure.
    DATA: lt_components TYPE cl_abap_structdescr=>component_table.
    
    LOOP AT it_field_catalog INTO DATA(ls_field).
      " Create component for each field
      DATA(ls_component) = VALUE abap_componentdescr( 
        name = ls_field-fieldname
        type = SWITCH #( ls_field-datatype
                        WHEN 'CHAR' THEN cl_abap_elemdescr=>get_c( ls_field-outputlen )
                        WHEN 'NUMC' THEN cl_abap_elemdescr=>get_n( ls_field-outputlen )
                        WHEN 'DATS' THEN cl_abap_elemdescr=>get_d( )
                        WHEN 'TIMS' THEN cl_abap_elemdescr=>get_t( )
                        WHEN 'INT4' THEN cl_abap_elemdescr=>get_i( )
                        WHEN 'CURR' THEN cl_abap_elemdescr=>get_p( p_length = 15 p_decimals = 2 )
                        ELSE cl_abap_elemdescr=>get_string( ) )
      ).
      
      APPEND ls_component TO lt_components.
    ENDLOOP.
    
    rr_struct_type = cl_abap_structdescr=>create( lt_components ).
  ENDMETHOD.
ENDCLASS.
```

This expert-level reports module continues with advanced ALV features, performance optimization, and enterprise patterns. The content builds on previous modules and demonstrates sophisticated reporting techniques used in large-scale SAP implementations.

---

**Next Module**: [Module 8: Forms Development](Module_08_Forms_Development.md)