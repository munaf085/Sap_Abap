# Module 9: Dialog Programming - Expert Interface Development

## 🎯 Master Advanced User Interface Development
From classical screens to modern Web Dynpro and Fiori integration patterns used in enterprise SAP applications.

---

## 📖 Table of Contents
1. [Screen Architecture & Flow Logic](#screen-architecture--flow-logic)
2. [Advanced Screen Controls](#advanced-screen-controls)
3. [Table Controls & ALV Integration](#table-controls--alv-integration)
4. [Dynamic Screen Programming](#dynamic-screen-programming)
5. [Web Dynpro ABAP](#web-dynpro-abap)
6. [Fiori Integration](#fiori-integration)
7. [Performance & User Experience](#performance--user-experience)
8. [Enterprise UI Patterns](#enterprise-ui-patterns)

---

## 1. Screen Architecture & Flow Logic

### Advanced Screen Design Patterns

#### **Enterprise Module Pool Architecture**
```abap
*&---------------------------------------------------------------------*
*& Module Pool: ZSAP_CUSTOMER_MANAGEMENT
*& Purpose: Enterprise customer management with advanced UI patterns
*& Architecture: MVC pattern with separation of concerns
*&---------------------------------------------------------------------*

PROGRAM zsap_customer_management.

" ===== TYPE DEFINITIONS =====
TYPES: BEGIN OF ty_customer_data,
         customer_id    TYPE kunnr,
         name          TYPE name1_gp,
         street        TYPE stras,
         city          TYPE ort01,
         country       TYPE land1,
         phone         TYPE telf1,
         email         TYPE ad_smtpadr,
         credit_limit  TYPE dmbtr,
         currency      TYPE waers,
         status        TYPE zstatus,
         created_by    TYPE syuname,
         created_at    TYPE dats,
         changed_by    TYPE syuname,
         changed_at    TYPE dats,
       END OF ty_customer_data,
       
       BEGIN OF ty_screen_control,
         mode          TYPE zscreen_mode,  " DISPLAY, CHANGE, CREATE
         editable      TYPE abap_bool,
         first_time    TYPE abap_bool,
         save_required TYPE abap_bool,
         authority_ok  TYPE abap_bool,
       END OF ty_screen_control.

" ===== GLOBAL DATA =====
DATA: gs_customer_data   TYPE ty_customer_data,
      gs_customer_backup TYPE ty_customer_data,
      gs_screen_control  TYPE ty_screen_control,
      go_customer_model  TYPE REF TO zcl_customer_model,
      go_ui_controller   TYPE REF TO zcl_ui_controller,
      go_validator       TYPE REF TO zcl_data_validator.

" ===== SCREEN DECLARATIONS =====
" Screen 0100: Customer Overview (Search and List)
" Screen 0200: Customer Details (Display/Edit)
" Screen 0300: Address Management
" Screen 0400: Contact Information
" Screen 0500: Financial Details

" ===== INITIALIZATION =====
INITIALIZATION.
  " Initialize controllers and models
  CREATE OBJECT: go_customer_model,
                 go_ui_controller,
                 go_validator.
                 
  " Set initial screen control
  gs_screen_control = VALUE #( 
    mode = 'DISPLAY'
    editable = abap_false
    first_time = abap_true
    save_required = abap_false
  ).

*&---------------------------------------------------------------------*
*& PBO Modules - Process Before Output
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  " Customer Overview Screen
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
  
  " Initialize search screen
  go_ui_controller->setup_search_screen( ).
  
  " Load recent customers for quick access
  go_ui_controller->load_recent_customers( ).
ENDMODULE.

MODULE status_0200 OUTPUT.
  " Customer Details Screen
  CASE gs_screen_control-mode.
    WHEN 'DISPLAY'.
      SET PF-STATUS 'STATUS_0200_DISPLAY'.
    WHEN 'CHANGE'.
      SET PF-STATUS 'STATUS_0200_CHANGE'.
    WHEN 'CREATE'.
      SET PF-STATUS 'STATUS_0200_CREATE'.
  ENDCASE.
  
  SET TITLEBAR 'TITLE_0200' WITH gs_customer_data-customer_id.
  
  " Setup screen fields based on mode
  go_ui_controller->setup_detail_screen( 
    is_customer_data = gs_customer_data
    is_screen_control = gs_screen_control
  ).
ENDMODULE.

MODULE modify_screen_0200 OUTPUT.
  " Dynamic screen modification
  LOOP AT SCREEN.
    " Apply field-level security
    go_ui_controller->apply_field_security( 
      EXPORTING iv_field_name = screen-name
      CHANGING cs_screen = screen
    ).
    
    " Apply business logic for field modifications
    CASE screen-name.
      WHEN 'GS_CUSTOMER_DATA-CUSTOMER_ID'.
        IF gs_screen_control-mode = 'CREATE'.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        
      WHEN 'GS_CUSTOMER_DATA-CREDIT_LIMIT'.
        " Only financial users can modify credit limit
        IF go_ui_controller->check_financial_authority( ) = abap_false.
          screen-input = 0.
        ENDIF.
        
      WHEN 'GS_CUSTOMER_DATA-STATUS'.
        " Status can only be changed by managers
        IF go_ui_controller->check_manager_authority( ) = abap_false.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

*&---------------------------------------------------------------------*
*& PAI Modules - Process After Input
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  " Handle user commands on overview screen
  CASE sy-ucomm.
    WHEN 'SEARCH'.
      PERFORM search_customers.
      
    WHEN 'CREATE'.
      gs_screen_control-mode = 'CREATE'.
      CLEAR gs_customer_data.
      CALL SCREEN 0200.
      
    WHEN 'DISPLAY' OR 'CHANGE'.
      " Get selected customer from list
      PERFORM get_selected_customer.
      gs_screen_control-mode = sy-ucomm.
      CALL SCREEN 0200.
      
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

MODULE user_command_0200 INPUT.
  " Handle user commands on detail screen
  DATA: lv_answer TYPE c.
  
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_customer_data.
      
    WHEN 'EDIT'.
      " Switch to edit mode
      gs_customer_backup = gs_customer_data.
      gs_screen_control-mode = 'CHANGE'.
      gs_screen_control-editable = abap_true.
      
    WHEN 'CANCEL'.
      " Cancel changes and restore backup
      IF gs_screen_control-save_required = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar = 'Confirm Cancel'
            text_question = 'Discard unsaved changes?'
          IMPORTING
            answer = lv_answer.
            
        IF lv_answer = '1'.
          gs_customer_data = gs_customer_backup.
          gs_screen_control-save_required = abap_false.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
      
      SET SCREEN 0100.
      
    WHEN 'BACK'.
      IF gs_screen_control-save_required = abap_true.
        PERFORM prompt_save_changes.
      ENDIF.
      SET SCREEN 0100.
      
    WHEN 'DELETE'.
      PERFORM delete_customer.
      
    WHEN 'COPY'.
      PERFORM copy_customer.
  ENDCASE.
ENDMODULE.

MODULE validate_input_0200 INPUT.
  " Real-time field validation
  CASE sy-ucomm.
    WHEN 'SAVE' OR 'ENTER'.
      " Comprehensive validation before save
      DATA(lt_errors) = go_validator->validate_customer_data( gs_customer_data ).
      
      IF lines( lt_errors ) > 0.
        " Display validation errors
        go_ui_controller->display_validation_errors( lt_errors ).
        MESSAGE 'Please correct the errors before saving' TYPE 'E'.
      ENDIF.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Business Logic Implementation
*&---------------------------------------------------------------------*

FORM search_customers.
  DATA: lt_customers TYPE ztt_customer_search_results,
        ls_search_criteria TYPE zcustomer_search_criteria.
        
  " Get search criteria from screen
  go_ui_controller->get_search_criteria( 
    IMPORTING es_criteria = ls_search_criteria
  ).
  
  " Perform search with optimization
  go_customer_model->search_customers( 
    EXPORTING is_criteria = ls_search_criteria
    IMPORTING et_results = lt_customers
  ).
  
  " Display results in ALV
  go_ui_controller->display_search_results( lt_customers ).
ENDFORM.

FORM save_customer_data.
  DATA: lx_exception TYPE REF TO zcx_customer_save_error.
  
  " Final validation
  DATA(lt_errors) = go_validator->validate_customer_data( gs_customer_data ).
  IF lines( lt_errors ) > 0.
    go_ui_controller->display_validation_errors( lt_errors ).
    RETURN.
  ENDIF.
  
  " Save with transaction control
  TRY.
      go_customer_model->save_customer( 
        EXPORTING is_customer_data = gs_customer_data
                  iv_mode = gs_screen_control-mode
        IMPORTING ev_customer_id = gs_customer_data-customer_id
      ).
      
      " Update screen control
      gs_screen_control-save_required = abap_false.
      gs_screen_control-mode = 'DISPLAY'.
      
      MESSAGE 'Customer saved successfully' TYPE 'S'.
      
    CATCH zcx_customer_save_error INTO lx_exception.
      MESSAGE lx_exception->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.
```

#### **Advanced Flow Logic Patterns**
```abap
*&---------------------------------------------------------------------*
*& Flow Logic: Screen 0200 - Customer Details
*&---------------------------------------------------------------------*

PROCESS BEFORE OUTPUT.
  MODULE status_0200.
  MODULE modify_screen_0200.
  
  " Conditional modules based on screen mode
  IF gs_screen_control-mode = 'DISPLAY'.
    MODULE prepare_display_mode.
  ELSEIF gs_screen_control-mode = 'CHANGE'.
    MODULE prepare_change_mode.
  ELSEIF gs_screen_control-mode = 'CREATE'.
    MODULE prepare_create_mode.
  ENDIF.
  
  " Load dependent data
  MODULE load_dropdown_values.
  MODULE load_related_data.

PROCESS AFTER INPUT.
  " Field-level validation on specific fields
  FIELD gs_customer_data-customer_id MODULE validate_customer_id ON INPUT.
  FIELD gs_customer_data-email MODULE validate_email ON INPUT.
  FIELD gs_customer_data-credit_limit MODULE validate_credit_limit ON INPUT.
  
  " Chain validation for related fields
  CHAIN.
    FIELD: gs_customer_data-street,
           gs_customer_data-city,
           gs_customer_data-country.
    MODULE validate_address ON CHAIN-INPUT.
  ENDCHAIN.
  
  " Main command processing
  MODULE user_command_0200.
  
  " Save point for transaction control
  MODULE set_save_point ON EXIT-COMMAND.

*&---------------------------------------------------------------------*
*& Advanced Validation Modules
*&---------------------------------------------------------------------*

MODULE validate_customer_id INPUT.
  " Real-time customer ID validation
  IF gs_customer_data-customer_id IS NOT INITIAL.
    " Check format
    IF go_validator->validate_customer_id_format( gs_customer_data-customer_id ) = abap_false.
      MESSAGE 'Invalid customer ID format' TYPE 'E'.
    ENDIF.
    
    " Check for duplicates (only in create mode)
    IF gs_screen_control-mode = 'CREATE'.
      IF go_customer_model->customer_exists( gs_customer_data-customer_id ) = abap_true.
        MESSAGE 'Customer ID already exists' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.

MODULE validate_email INPUT.
  " Email format validation
  IF gs_customer_data-email IS NOT INITIAL.
    IF go_validator->validate_email_format( gs_customer_data-email ) = abap_false.
      MESSAGE 'Invalid email format' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.

MODULE validate_credit_limit INPUT.
  " Business logic validation for credit limit
  IF gs_customer_data-credit_limit > 0.
    " Check authorization for high credit limits
    IF gs_customer_data-credit_limit > 100000.
      IF go_ui_controller->check_high_credit_authority( ) = abap_false.
        MESSAGE 'Insufficient authorization for high credit limits' TYPE 'E'.
      ENDIF.
    ENDIF.
    
    " Validate currency is specified
    IF gs_customer_data-currency IS INITIAL.
      MESSAGE 'Currency must be specified for credit limit' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.

MODULE validate_address INPUT.
  " Comprehensive address validation
  DATA: ls_address TYPE zaddress_structure.
  
  ls_address = VALUE #( 
    street = gs_customer_data-street
    city = gs_customer_data-city
    country = gs_customer_data-country
  ).
  
  " Use external service for address validation
  IF go_validator->validate_address( ls_address ) = abap_false.
    MESSAGE 'Invalid address information' TYPE 'W'.
  ENDIF.
ENDMODULE.
```

---

## 2. Advanced Screen Controls

### Sophisticated UI Components

#### **Custom Control Integration**
```abap
*&---------------------------------------------------------------------*
*& Custom Control: Advanced Customer Search
*&---------------------------------------------------------------------*

CLASS zcl_custom_search_control DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING io_parent_container TYPE REF TO cl_gui_container,
               
             set_search_criteria
               IMPORTING it_criteria TYPE ztt_search_criteria,
               
             get_selected_customers
               RETURNING VALUE(rt_customers) TYPE ztt_customer_results,
               
             refresh_display,
             
             free.
             
  PRIVATE SECTION.
    DATA: mo_splitter           TYPE REF TO cl_gui_splitter_container,
          mo_search_container   TYPE REF TO cl_gui_container,
          mo_results_container  TYPE REF TO cl_gui_container,
          mo_search_panel       TYPE REF TO zcl_search_panel,
          mo_results_alv        TYPE REF TO cl_gui_alv_grid,
          mt_search_results     TYPE ztt_customer_results.
          
    METHODS: create_search_panel,
             create_results_grid,
             handle_search_triggered
               FOR EVENT search_triggered OF zcl_search_panel
               IMPORTING sender.
ENDCLASS.

CLASS zcl_custom_search_control IMPLEMENTATION.
  METHOD constructor.
    " Create splitter container
    CREATE OBJECT mo_splitter
      EXPORTING
        parent = io_parent_container
        rows = 2
        columns = 1.
        
    " Get sub-containers
    mo_search_container = mo_splitter->get_container( row = 1 column = 1 ).
    mo_results_container = mo_splitter->get_container( row = 2 column = 1 ).
    
    " Set splitter proportions
    mo_splitter->set_row_height( id = 1 height = 30 ).  " 30% for search
    mo_splitter->set_row_height( id = 2 height = 70 ).  " 70% for results
    
    " Create child controls
    create_search_panel( ).
    create_results_grid( ).
    
    " Set up event handling
    SET HANDLER handle_search_triggered FOR mo_search_panel.
  ENDMETHOD.
  
  METHOD create_search_panel.
    " Create custom search panel with advanced features
    CREATE OBJECT mo_search_panel
      EXPORTING
        io_parent = mo_search_container.
        
    " Configure search panel
    mo_search_panel->add_search_field( 
      iv_field_name = 'CUSTOMER_ID'
      iv_label = 'Customer ID'
      iv_type = 'INPUT'
      iv_length = 10
    ).
    
    mo_search_panel->add_search_field( 
      iv_field_name = 'NAME'
      iv_label = 'Customer Name'
      iv_type = 'INPUT'
      iv_length = 35
    ).
    
    mo_search_panel->add_search_field( 
      iv_field_name = 'COUNTRY'
      iv_label = 'Country'
      iv_type = 'DROPDOWN'
      it_values = get_country_values( )
    ).
    
    mo_search_panel->add_search_field( 
      iv_field_name = 'STATUS'
      iv_label = 'Status'
      iv_type = 'CHECKBOX_GROUP'
      it_values = get_status_values( )
    ).
    
    " Add advanced search options
    mo_search_panel->add_advanced_options( 
      iv_fuzzy_search = abap_true
      iv_case_sensitive = abap_false
      iv_wildcard_support = abap_true
    ).
  ENDMETHOD.
  
  METHOD create_results_grid.
    " Create ALV grid for results display
    CREATE OBJECT mo_results_alv
      EXPORTING
        i_parent = mo_results_container.
        
    " Set up field catalog
    DATA(lt_fieldcat) = build_results_fieldcat( ).
    
    " Configure ALV layout
    DATA(ls_layout) = VALUE lvc_s_layo( 
      zebra = 'X'
      grid_title = 'Search Results'
      smalltitle = 'X'
      cwidth_opt = 'X'
      sel_mode = 'A'  " Multiple selection
    ).
    
    " Set ALV data
    mo_results_alv->set_table_for_first_display( 
      EXPORTING
        is_layout = ls_layout
        it_fieldcatalog = lt_fieldcat
      CHANGING
        it_outtab = mt_search_results
    ).
    
    " Enable additional ALV features
    mo_results_alv->set_ready_for_input( 0 ).
    mo_results_alv->set_toolbar_interactive( ).
    
    " Set up context menu
    DATA(lo_menu) = NEW cl_ctmenu( ).
    lo_menu->add_function( 
      fcode = 'DISPLAY_DETAIL'
      text = 'Display Details'
    ).
    lo_menu->add_function( 
      fcode = 'EDIT_CUSTOMER'
      text = 'Edit Customer'
    ).
    
    mo_results_alv->set_table_context_menu( lo_menu ).
  ENDMETHOD.
  
  METHOD handle_search_triggered.
    " Handle search event from search panel
    DATA(ls_criteria) = sender->get_search_criteria( ).
    
    " Perform search
    DATA(lo_search_engine) = NEW zcl_customer_search_engine( ).
    
    mt_search_results = lo_search_engine->search( 
      is_criteria = ls_criteria
      iv_max_results = 1000
    ).
    
    " Refresh results display
    mo_results_alv->refresh_table_display( ).
    
    " Update status
    MESSAGE |{ lines( mt_search_results ) } customers found| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.
```

#### **Advanced Table Control**
```abap
*&---------------------------------------------------------------------*
*& Enhanced Table Control with Business Logic
*&---------------------------------------------------------------------*

MODULE handle_table_control_0300 INPUT.
  " Advanced table control for customer contacts
  DATA: lv_tc_lines TYPE i,
        lv_current_line TYPE i.
        
  " Get table control info
  DESCRIBE TABLE gt_contacts LINES lv_tc_lines.
  
  " Handle table control events
  CASE sy-ucomm.
    WHEN 'TC_CONTACTS_INSERT'.
      " Insert new line with defaults
      lv_current_line = tc_contacts-current_line.
      
      INSERT VALUE zcustomer_contact( 
        customer_id = gs_customer_data-customer_id
        contact_type = 'EMAIL'
        created_by = sy-uname
        created_at = sy-datum
      ) INTO gt_contacts INDEX lv_current_line.
      
      " Refresh table control
      tc_contacts-lines = lines( gt_contacts ).
      
    WHEN 'TC_CONTACTS_DELETE'.
      " Delete selected lines with confirmation
      LOOP AT gt_contacts INTO DATA(ls_contact) WHERE selected = 'X'.
        DELETE gt_contacts INDEX sy-tabix.
      ENDLOOP.
      
      tc_contacts-lines = lines( gt_contacts ).
      
    WHEN 'TC_CONTACTS_COPY'.
      " Copy selected line
      READ TABLE gt_contacts INTO ls_contact INDEX tc_contacts-current_line.
      IF sy-subrc = 0.
        CLEAR ls_contact-contact_id.  " Generate new ID
        ls_contact-created_at = sy-datum.
        ls_contact-created_by = sy-uname.
        APPEND ls_contact TO gt_contacts.
        tc_contacts-lines = lines( gt_contacts ).
      ENDIF.
      
    WHEN 'TC_CONTACTS_VALIDATE'.
      " Validate all table entries
      LOOP AT gt_contacts ASSIGNING FIELD-SYMBOL(<ls_contact>).
        " Validate email format
        IF <ls_contact>-contact_type = 'EMAIL'.
          IF go_validator->validate_email_format( <ls_contact>-contact_value ) = abap_false.
            MESSAGE |Invalid email in line { sy-tabix }| TYPE 'E'.
          ENDIF.
        ENDIF.
        
        " Validate phone format
        IF <ls_contact>-contact_type = 'PHONE'.
          IF go_validator->validate_phone_format( <ls_contact>-contact_value ) = abap_false.
            MESSAGE |Invalid phone number in line { sy-tabix }| TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.

MODULE modify_table_control_0300 OUTPUT.
  " Dynamic modification of table control fields
  LOOP AT gt_contacts INTO DATA(ls_contact) 
    WITH CONTROL tc_contacts 
    CURSOR tc_contacts-current_line.
    
    MODULE modify_contact_line.
  ENDLOOP.
ENDMODULE.

MODULE modify_contact_line OUTPUT.
  " Field-level modifications within table control
  DATA: lv_line_index TYPE i.
  
  lv_line_index = sy-stepl + tc_contacts-top_line - 1.
  
  " Read current line data
  READ TABLE gt_contacts INTO DATA(ls_current_contact) INDEX lv_line_index.
  
  " Apply conditional logic
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GT_CONTACTS-CONTACT_VALUE'.
        " Make field mandatory based on contact type
        IF ls_current_contact-contact_type IS NOT INITIAL.
          screen-required = 2.
        ENDIF.
        
        " Apply input help based on type
        CASE ls_current_contact-contact_type.
          WHEN 'EMAIL'.
            screen-input_help = 'EMAIL_F4'.
          WHEN 'PHONE'.
            screen-input_help = 'PHONE_F4'.
        ENDCASE.
        
      WHEN 'GT_CONTACTS-PRIMARY_CONTACT'.
        " Only one primary contact allowed
        IF ls_current_contact-primary_contact = 'X'.
          " Disable primary flag for other contacts
          " (This would require additional logic)
        ENDIF.
    ENDCASE.
    
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
```

---

## 3. Table Controls & ALV Integration

### Enterprise ALV Integration in Screens

#### **Embedded ALV with Business Logic**
```abap
*&---------------------------------------------------------------------*
*& ALV Integration in Dialog Programming
*&---------------------------------------------------------------------*

CLASS zcl_embedded_alv_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING io_container TYPE REF TO cl_gui_container,
               
             display_customer_orders
               IMPORTING iv_customer_id TYPE kunnr,
               
             handle_user_command
               FOR EVENT user_command OF cl_gui_alv_grid
               IMPORTING e_ucomm sender,
               
             handle_data_changed
               FOR EVENT data_changed OF cl_gui_alv_grid
               IMPORTING er_data_changed sender,
               
             handle_hotspot_click
               FOR EVENT hotspot_click OF cl_gui_alv_grid
               IMPORTING e_row_id e_column_id sender.
               
  PRIVATE SECTION.
    DATA: mo_alv_grid      TYPE REF TO cl_gui_alv_grid,
          mt_orders        TYPE ztt_customer_orders,
          mt_fieldcat      TYPE lvc_t_fcat,
          ms_layout        TYPE lvc_s_layo,
          mv_customer_id   TYPE kunnr.
          
    METHODS: build_fieldcat,
             setup_layout,
             setup_events,
             add_custom_buttons,
             validate_changed_data
               IMPORTING ir_data_changed TYPE REF TO cl_alv_changed_data_protocol
               RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS zcl_embedded_alv_manager IMPLEMENTATION.
  METHOD constructor.
    " Create ALV grid
    CREATE OBJECT mo_alv_grid
      EXPORTING
        i_parent = io_container.
        
    " Set up ALV configuration
    build_fieldcat( ).
    setup_layout( ).
    setup_events( ).
    add_custom_buttons( ).
  ENDMETHOD.
  
  METHOD display_customer_orders.
    mv_customer_id = iv_customer_id.
    
    " Load order data
    SELECT * FROM zcustomer_orders
      INTO CORRESPONDING FIELDS OF TABLE mt_orders
      WHERE customer_id = iv_customer_id
      ORDER BY order_date DESCENDING.
      
    " Display in ALV
    mo_alv_grid->set_table_for_first_display( 
      EXPORTING
        is_layout = ms_layout
        it_fieldcatalog = mt_fieldcat
      CHANGING
        it_outtab = mt_orders
    ).
    
    " Enable editing for specific fields
    mo_alv_grid->set_ready_for_input( 1 ).
  ENDMETHOD.
  
  METHOD build_fieldcat.
    " Build comprehensive field catalog
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZCUSTOMER_ORDER'
      CHANGING
        ct_fieldcat = mt_fieldcat.
        
    " Customize field properties
    LOOP AT mt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
      CASE <ls_fieldcat>-fieldname.
        WHEN 'ORDER_ID'.
          <ls_fieldcat>-hotspot = 'X'.
          <ls_fieldcat>-emphasize = 'C110'.  " Highlight
          
        WHEN 'ORDER_STATUS'.
          <ls_fieldcat>-edit = 'X'.
          <ls_fieldcat>-drdn_hndl = get_status_dropdown_handle( ).
          
        WHEN 'ORDER_AMOUNT'.
          <ls_fieldcat>-cfieldname = 'CURRENCY'.
          <ls_fieldcat>-emphasize = 'C510'.
          
        WHEN 'DELIVERY_DATE'.
          <ls_fieldcat>-edit = 'X'.
          
        WHEN 'PRIORITY'.
          <ls_fieldcat>-edit = 'X'.
          <ls_fieldcat>-checkbox = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD setup_layout.
    ms_layout = VALUE #( 
      zebra = 'X'
      grid_title = 'Customer Orders'
      sel_mode = 'D'    " Multiple selection
      cwidth_opt = 'X'
      stylefname = 'CELL_STYLE'
      ctab_fname = 'CELL_COLOR'
    ).
  ENDMETHOD.
  
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'CREATE_ORDER'.
        " Create new order
        CALL SCREEN 0400 STARTING AT 10 10.
        
      WHEN 'COPY_ORDER'.
        " Copy selected order
        DATA(lt_selected) = get_selected_rows( ).
        IF lines( lt_selected ) = 1.
          PERFORM copy_order USING lt_selected[ 1 ].
        ENDIF.
        
      WHEN 'DELETE_ORDER'.
        " Delete selected orders with confirmation
        PERFORM delete_selected_orders.
        
      WHEN 'EXPORT_EXCEL'.
        " Export to Excel
        mo_alv_grid->export_to_excel( ).
        
      WHEN 'REFRESH'.
        " Refresh data
        display_customer_orders( mv_customer_id ).
    ENDCASE.
  ENDMETHOD.
  
  METHOD handle_data_changed.
    " Validate and process data changes
    IF validate_changed_data( er_data_changed ) = abap_true.
      " Apply changes to internal table
      er_data_changed->apply_changes( ).
      
      " Save changes to database
      PERFORM save_order_changes.
      
      MESSAGE 'Changes saved successfully' TYPE 'S'.
    ELSE.
      " Reject changes
      MESSAGE 'Invalid data - changes rejected' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  
  METHOD validate_changed_data.
    rv_valid = abap_true.
    
    " Validate each changed cell
    LOOP AT ir_data_changed->mt_good_cells INTO DATA(ls_cell).
      CASE ls_cell-fieldname.
        WHEN 'ORDER_STATUS'.
          " Validate status transition
          IF validate_status_change( ls_cell ) = abap_false.
            rv_valid = abap_false.
            ir_data_changed->add_message( 
              i_msgty = 'E'
              i_msgid = 'ZCUSTOMER'
              i_msgno = '001'
              i_msgv1 = 'Invalid status transition'
              i_fieldname = ls_cell-fieldname
              i_row_id = ls_cell-row_id
            ).
          ENDIF.
          
        WHEN 'DELIVERY_DATE'.
          " Validate delivery date
          IF ls_cell-value < sy-datum.
            rv_valid = abap_false.
            ir_data_changed->add_message( 
              i_msgty = 'E'
              i_msgid = 'ZCUSTOMER'
              i_msgno = '002'
              i_msgv1 = 'Delivery date cannot be in the past'
              i_fieldname = ls_cell-fieldname
              i_row_id = ls_cell-row_id
            ).
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

---

## 4. Dynamic Screen Programming

### Runtime Screen Generation

#### **Dynamic Screen Builder**
```abap
*&---------------------------------------------------------------------*
*& Dynamic Screen Generation Framework
*&---------------------------------------------------------------------*

CLASS zcl_dynamic_screen_builder DEFINITION.
  PUBLIC SECTION.
    METHODS: build_screen_dynamically
               IMPORTING it_field_definitions TYPE ztt_screen_fields
                         is_screen_properties TYPE zscreen_properties
               RETURNING VALUE(rv_screen_number) TYPE sydynnr,
               
             generate_flow_logic
               IMPORTING iv_screen_number TYPE sydynnr
                         it_field_definitions TYPE ztt_screen_fields
               RETURNING VALUE(rv_flow_logic) TYPE string,
               
             create_screen_at_runtime
               IMPORTING iv_program_name TYPE progname
                         iv_screen_number TYPE sydynnr
                         is_screen_definition TYPE zscreen_definition.
                         
  PRIVATE SECTION.
    METHODS: calculate_field_positions
               IMPORTING it_fields TYPE ztt_screen_fields
               RETURNING VALUE(rt_positions) TYPE ztt_field_positions,
               
             generate_screen_elements
               IMPORTING it_fields TYPE ztt_screen_fields
                         it_positions TYPE ztt_field_positions
               RETURNING VALUE(rt_elements) TYPE ztt_screen_elements,
               
             apply_screen_logic
               IMPORTING iv_screen_number TYPE sydynnr
                         it_business_rules TYPE ztt_screen_rules.
ENDCLASS.

CLASS zcl_dynamic_screen_builder IMPLEMENTATION.
  METHOD build_screen_dynamically.
    DATA: ls_screen_attr TYPE zscreen_attributes,
          lt_elements    TYPE ztt_screen_elements,
          lt_positions   TYPE ztt_field_positions.
          
    " Generate unique screen number
    rv_screen_number = get_next_screen_number( ).
    
    " Calculate optimal field positions
    lt_positions = calculate_field_positions( it_field_definitions ).
    
    " Generate screen elements
    lt_elements = generate_screen_elements( 
      it_fields = it_field_definitions
      it_positions = lt_positions
    ).
    
    " Create screen attributes
    ls_screen_attr = VALUE #( 
      screen_number = rv_screen_number
      screen_type = is_screen_properties-screen_type
      next_screen = is_screen_properties-next_screen
      screen_group1 = is_screen_properties-screen_group1
      screen_group2 = is_screen_properties-screen_group2
      screen_group3 = is_screen_properties-screen_group3
    ).
    
    " Generate screen in repository
    CALL FUNCTION 'RS_SCREEN_CREATE'
      EXPORTING
        program = sy-repid
        screen = rv_screen_number
        screen_attr = ls_screen_attr
        elements = lt_elements
      EXCEPTIONS
        object_not_found = 1
        permission_failure = 2
        OTHERS = 3.
        
    IF sy-subrc <> 0.
      MESSAGE 'Error creating dynamic screen' TYPE 'E'.
    ENDIF.
    
    " Generate and assign flow logic
    DATA(lv_flow_logic) = generate_flow_logic( 
      iv_screen_number = rv_screen_number
      it_field_definitions = it_field_definitions
    ).
    
    " Set flow logic
    CALL FUNCTION 'RS_SCREEN_SET_FLOW_LOGIC'
      EXPORTING
        program = sy-repid
        screen = rv_screen_number
        flow_logic = lv_flow_logic.
  ENDMETHOD.
  
  METHOD calculate_field_positions.
    " Intelligent field positioning algorithm
    DATA: lv_current_row TYPE i VALUE 1,
          lv_current_col TYPE i VALUE 2,
          lv_max_col     TYPE i VALUE 80,
          lv_row_height  TYPE i VALUE 2.
          
    LOOP AT it_fields INTO DATA(ls_field).
      " Check if field fits in current row
      IF lv_current_col + ls_field-length > lv_max_col.
        " Move to next row
        lv_current_row += lv_row_height.
        lv_current_col = 2.
      ENDIF.
      
      " Assign position
      APPEND VALUE #( 
        field_name = ls_field-field_name
        row = lv_current_row
        column = lv_current_col
        length = ls_field-length
        height = 1
      ) TO rt_positions.
      
      " Update position for next field
      lv_current_col += ls_field-length + 2.  " Add spacing
    ENDLOOP.
  ENDMETHOD.
  
  METHOD generate_screen_elements.
    " Convert field definitions to screen elements
    LOOP AT it_fields INTO DATA(ls_field).
      " Get position for this field
      READ TABLE it_positions INTO DATA(ls_position)
        WITH KEY field_name = ls_field-field_name.
        
      " Create label element
      APPEND VALUE #( 
        type = 'TEXT'
        name = |LBL_{ ls_field-field_name }|
        row = ls_position-row
        column = ls_position-column - 20
        length = 18
        height = 1
        text = ls_field-field_label
      ) TO rt_elements.
      
      " Create field element
      DATA(ls_element) = VALUE zscreen_element( 
        type = ls_field-field_type
        name = ls_field-field_name
        row = ls_position-row
        column = ls_position-column
        length = ls_position-length
        height = ls_position-height
        datatype = ls_field-data_type
        outputlen = ls_field-output_length
      ).
      
      " Apply field-specific properties
      CASE ls_field-field_type.
        WHEN 'INPUT'.
          ls_element-input = 'X'.
          IF ls_field-mandatory = 'X'.
            ls_element-required = 'X'.
          ENDIF.
          
        WHEN 'OUTPUT'.
          ls_element-output = 'X'.
          
        WHEN 'DROPDOWN'.
          ls_element-input = 'X'.
          ls_element-listbox = 'X'.
          
        WHEN 'CHECKBOX'.
          ls_element-input = 'X'.
          ls_element-as_checkbox = 'X'.
          
        WHEN 'RADIOBUTTON'.
          ls_element-input = 'X'.
          ls_element-as_radiobutton = 'X'.
          ls_element-group1 = ls_field-radio_group.
      ENDCASE.
      
      APPEND ls_element TO rt_elements.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD generate_flow_logic.
    " Generate appropriate flow logic
    rv_flow_logic = |PROCESS BEFORE OUTPUT.\n|
                 && |  MODULE status_{ iv_screen_number }.\n|
                 && |  MODULE modify_screen_{ iv_screen_number }.\n\n|
                 && |PROCESS AFTER INPUT.\n|.
                 
    " Add field validations
    LOOP AT it_field_definitions INTO DATA(ls_field) WHERE validation_required = 'X'.
      rv_flow_logic = rv_flow_logic 
                   && |  FIELD { ls_field-field_name } MODULE validate_{ ls_field-field_name } ON INPUT.\n|.
    ENDLOOP.
    
    rv_flow_logic = rv_flow_logic 
                 && |  MODULE user_command_{ iv_screen_number }.\n|.
  ENDMETHOD.
ENDCLASS.
```

This comprehensive dialog programming module covers advanced screen development techniques used in enterprise SAP applications. The content progresses from basic screen concepts to sophisticated dynamic UI generation and modern integration patterns.

---

**Next Module**: [Module 10: BDC & Data Migration](Module_10_BDC_Data_Migration.md)