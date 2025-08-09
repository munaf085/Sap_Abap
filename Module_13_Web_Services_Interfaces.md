# Module 13: Web Services & Interfaces

## 🎯 **Complete Guide to SAP Integration**

**Learn integration from basics to enterprise-level patterns - No integration experience required!**

Master SAP integration from fundamental concepts to advanced web services and enterprise integration patterns used in modern SAP landscapes.

---

## 📖 **Table of Contents**
1. [🌟 Integration Fundamentals - What & Why](#-integration-fundamentals---what--why)
2. [🌐 Web Services Basics - Your First API](#-web-services-basics---your-first-api)
3. [📡 REST APIs - Modern Communication](#-rest-apis---modern-communication)
4. [📊 OData Services - SAP's Standard](#-odata-services---saps-standard)
5. [🔧 RFC & BAPI - SAP Internal Communication](#-rfc--bapi---sap-internal-communication)
6. [📨 IDocs - Document Exchange](#-idocs---document-exchange)
7. [🔒 Security & Authentication](#-security--authentication)
8. [🚀 Enterprise Integration Patterns](#-enterprise-integration-patterns)

---

## 🌟 **Integration Fundamentals - What & Why**

### **What is System Integration?**

**System Integration** is like **building bridges** between different computer systems so they can talk to each other and share information. Think of it as creating a translator that helps different people who speak different languages communicate.

#### **Real-World Analogy: Restaurant Communication**
```abap
" Without Integration (Manual Process):
" - Customer calls restaurant to place order
" - Staff writes order on paper
" - Manually tells kitchen
" - Manually updates inventory
" - Manually processes payment
" - Very slow and error-prone!

" With Integration (Automated Process):
" - Customer orders online
" - System automatically sends order to kitchen
" - System automatically updates inventory
" - System automatically processes payment
" - All systems work together seamlessly!
```

### **Why Do We Need Integration?**

#### **Business Scenarios:**
- 🏢 **Multiple Systems** - Every company has many different software systems
- 📊 **Data Sharing** - Systems need to share information
- 🔄 **Process Automation** - Reduce manual work
- 📱 **Modern Applications** - Mobile apps need data from SAP
- 🌍 **Partner Communication** - Exchange data with suppliers/customers

#### **SAP Integration Examples:**
```abap
" Common Integration Scenarios:

" 1. E-Commerce → SAP
"    Online store → Create sales order in SAP
"    
" 2. Mobile App → SAP
"    Employee app → Check leave balance from SAP HR
"    
" 3. SAP → Bank
"    SAP Finance → Send payment file to bank
"    
" 4. SAP → SAP
"    SAP HR → SAP Payroll (data exchange)
"    
" 5. External → SAP
"    CRM system → Customer data to SAP
```

### **Types of Integration - Simple Explanation**

| **Integration Type** | **What It Does** | **Real-World Example** |
|---------------------|------------------|------------------------|
| **Web Service** | Programs talk over internet | Online shopping API |
| **API** | Structured way to request data | Weather app getting forecast |
| **File Transfer** | Exchange files between systems | Email with attachments |
| **Database Connection** | Direct database access | Shared customer database |
| **Message Queue** | Asynchronous communication | Text message system |

### **Communication Methods**

#### **Synchronous vs Asynchronous**
```abap
" Synchronous (Real-time):
" - Ask question → Wait for answer → Continue
" - Like phone call
" - Example: Check customer credit limit before order

CALL FUNCTION 'CHECK_CREDIT_LIMIT'
  EXPORTING
    customer_id = '12345'
  IMPORTING
    credit_status = lv_status.

IF lv_status = 'APPROVED'.
  " Continue with order
ENDIF.

" Asynchronous (Background):
" - Send message → Continue working → Get answer later
" - Like email
" - Example: Send order confirmation email

CALL FUNCTION 'SEND_ORDER_CONFIRMATION' 
  IN BACKGROUND TASK
  EXPORTING
    order_number = '98765'.
    
" Don't wait for email to be sent, continue processing
```

---

## 🌐 **Web Services Basics - Your First API**

### **What are Web Services?**

**Web Services** are like **standardized messengers** that carry information between different computer systems over the internet using common protocols.

#### **Your First Simple Web Service**

Let's create a basic web service to get customer information:

```abap
" Step 1: Create a function module that can be called remotely
FUNCTION z_get_customer_info.
*"----------------------------------------------------------------------
*" Remote-Enabled Function Module
*" Purpose: Get customer basic information
*"----------------------------------------------------------------------
*"IMPORTING
*"  CUSTOMER_ID TYPE KUNNR
*"EXPORTING
*"  CUSTOMER_NAME TYPE NAME1_GP
*"  CUSTOMER_CITY TYPE ORT01_GP
*"  CREDIT_LIMIT TYPE KLIMK
*"EXCEPTIONS
*"  CUSTOMER_NOT_FOUND

  " Get customer data from database
  SELECT SINGLE name1, ort01
    FROM kna1
    INTO (customer_name, customer_city)
    WHERE kunnr = customer_id.
    
  IF sy-subrc <> 0.
    RAISE customer_not_found.
  ENDIF.
  
  " Get credit limit
  SELECT SINGLE klimk
    FROM knb1
    INTO credit_limit
    WHERE kunnr = customer_id.

ENDFUNCTION.
```

#### **Making It Available as Web Service**

```abap
" Step 2: Create web service using SE80
" 1. Go to SE80 (Object Navigator)
" 2. Right-click on function module Z_GET_CUSTOMER_INFO
" 3. Create → Web Service → Create Web Service
" 4. Choose service name: Z_CUSTOMER_SERVICE
" 5. System generates WSDL (Web Service Description)

" Step 3: Test the web service
" 1. Go to WSADMIN (Web Service Administration)
" 2. Find your service: Z_CUSTOMER_SERVICE
" 3. Test with sample data
```

### **Consuming Web Services from External Systems**

Now other systems can call your SAP web service:

```abap
" Example: JavaScript calling SAP web service
// External web application calling SAP
const soapEnvelope = `
  <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
    <soapenv:Body>
      <z_get_customer_info>
        <customer_id>0000012345</customer_id>
      </z_get_customer_info>
    </soapenv:Body>
  </soapenv:Envelope>
`;

fetch('http://sapserver:8000/sap/bc/soap/rfc', {
  method: 'POST',
  headers: { 'Content-Type': 'text/xml' },
  body: soapEnvelope
})
.then(response => response.text())
.then(data => {
  // Process customer information
  console.log('Customer data received:', data);
});
```

### **Real-World Example: Order Status API**

```abap
" Business Requirement: 
" "Customer wants to check order status from company website"

FUNCTION z_get_order_status.
*"----------------------------------------------------------------------
*" Get sales order status and details
*"----------------------------------------------------------------------
*"IMPORTING
*"  ORDER_NUMBER TYPE VBELN_VA
*"EXPORTING
*"  ORDER_STATUS TYPE CHAR20
*"  ORDER_VALUE TYPE NETWR_AP
*"  DELIVERY_DATE TYPE EDATU_VBRK
*"  TRACKING_NUMBER TYPE CHAR35
*"EXCEPTIONS
*"  ORDER_NOT_FOUND

  DATA: ls_header TYPE vbak,
        lt_items TYPE TABLE OF vbap,
        ls_delivery TYPE likp.

  " Get order header
  SELECT SINGLE * FROM vbak INTO ls_header
    WHERE vbeln = order_number.
    
  IF sy-subrc <> 0.
    RAISE order_not_found.
  ENDIF.
  
  " Determine order status
  CASE ls_header-gbstk.
    WHEN 'A'.
      order_status = 'Processing'.
    WHEN 'B'.
      order_status = 'Partially Processed'.
    WHEN 'C'.
      order_status = 'Completely Processed'.
    WHEN OTHERS.
      order_status = 'Unknown'.
  ENDCASE.
  
  " Calculate total order value
  SELECT SUM( netwr ) FROM vbap INTO order_value
    WHERE vbeln = order_number.
    
  " Get delivery information
  SELECT SINGLE wadat_ist, bolnr
    FROM likp INTO (delivery_date, tracking_number)
    WHERE vbeln = order_number.

ENDFUNCTION.
```

#### **API Documentation**

```abap
" API Documentation for z_get_order_status:
" 
" Endpoint: /sap/bc/soap/rfc/Z_GET_ORDER_STATUS
" Method: POST
" Content-Type: text/xml
" 
" Input Parameters:
" - ORDER_NUMBER (required): Sales order number (e.g., "4500000123")
" 
" Output Parameters:
" - ORDER_STATUS: Current status of the order
" - ORDER_VALUE: Total monetary value of the order
" - DELIVERY_DATE: Expected or actual delivery date
" - TRACKING_NUMBER: Shipping tracking number (if available)
" 
" Error Handling:
" - ORDER_NOT_FOUND: Returns when order number doesn't exist
" 
" Example Usage:
" Input: ORDER_NUMBER = "4500000123"
" Output: 
"   ORDER_STATUS = "Shipped"
"   ORDER_VALUE = 1250.00
"   DELIVERY_DATE = 20231025
"   TRACKING_NUMBER = "1Z999AA1234567890"
```

---

## 1. Web Service Architecture

### Enterprise Integration Framework

#### **Comprehensive Web Service Framework**
```abap
*&---------------------------------------------------------------------*
*& Enterprise Web Service Framework
*& Purpose: Unified framework for all web service types
*& Architecture: Service-oriented with proper separation of concerns
*&---------------------------------------------------------------------*

" ===== CORE INTERFACES =====

" Base web service interface
INTERFACE zif_web_service.
  TYPES: BEGIN OF ty_service_metadata,
           service_name    TYPE string,
           service_version TYPE string,
           endpoint_url    TYPE string,
           auth_required   TYPE abap_bool,
           rate_limit      TYPE i,
         END OF ty_service_metadata.
         
  METHODS: get_metadata 
             RETURNING VALUE(rs_metadata) TYPE ty_service_metadata,
           handle_request
             IMPORTING ir_request TYPE REF TO data
             RETURNING VALUE(rr_response) TYPE REF TO data
             RAISING zcx_service_error.
ENDINTERFACE.

" HTTP request handler interface
INTERFACE zif_http_handler.
  METHODS: handle_get
             IMPORTING io_request TYPE REF TO if_http_request
             CHANGING co_response TYPE REF TO if_http_response
             RAISING zcx_http_error,
             
           handle_post
             IMPORTING io_request TYPE REF TO if_http_request
             CHANGING co_response TYPE REF TO if_http_response
             RAISING zcx_http_error,
             
           handle_put
             IMPORTING io_request TYPE REF TO if_http_request
             CHANGING co_response TYPE REF TO if_http_response
             RAISING zcx_http_error,
             
           handle_delete
             IMPORTING io_request TYPE REF TO if_http_request
             CHANGING co_response TYPE REF TO if_http_response
             RAISING zcx_http_error.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& REST API Handler Framework
*&---------------------------------------------------------------------*

CLASS zcl_rest_api_handler DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_http_extension,
                zif_http_handler.
                
    METHODS: constructor
               IMPORTING io_service_registry TYPE REF TO zif_service_registry OPTIONAL.
               
  PRIVATE SECTION.
    DATA: mo_service_registry TYPE REF TO zif_service_registry,
          mo_auth_manager     TYPE REF TO zif_auth_manager,
          mo_rate_limiter     TYPE REF TO zif_rate_limiter,
          mo_logger          TYPE REF TO zif_api_logger.
          
    METHODS: parse_request_path
               IMPORTING iv_path TYPE string
               RETURNING VALUE(rs_parsed) TYPE zapi_path_info,
               
             authenticate_request
               IMPORTING io_request TYPE REF TO if_http_request
               RETURNING VALUE(rs_auth_result) TYPE zauth_result,
               
             check_rate_limit
               IMPORTING iv_client_id TYPE string
               RETURNING VALUE(rv_allowed) TYPE abap_bool,
               
             log_api_call
               IMPORTING iv_method TYPE string
                         iv_path TYPE string
                         iv_status_code TYPE i
                         iv_response_time TYPE i.
ENDCLASS.

CLASS zcl_rest_api_handler IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    DATA: lv_method TYPE string,
          lv_path   TYPE string,
          lv_start_time TYPE timestamp,
          lv_end_time   TYPE timestamp.
          
    GET TIME STAMP FIELD lv_start_time.
    
    TRY.
        lv_method = server->request->get_method( ).
        lv_path = server->request->get_header_field( '~path' ).
        
        " Parse request path for routing
        DATA(ls_path_info) = parse_request_path( lv_path ).
        
        " Authenticate request
        DATA(ls_auth_result) = authenticate_request( server->request ).
        IF ls_auth_result-authenticated = abap_false.
          server->response->set_status( code = 401 reason = 'Unauthorized' ).
          RETURN.
        ENDIF.
        
        " Check rate limiting
        IF check_rate_limit( ls_auth_result-client_id ) = abap_false.
          server->response->set_status( code = 429 reason = 'Too Many Requests' ).
          RETURN.
        ENDIF.
        
        " Route to appropriate handler
        CASE lv_method.
          WHEN 'GET'.
            zif_http_handler~handle_get( 
              io_request = server->request
              CHANGING co_response = server->response
            ).
          WHEN 'POST'.
            zif_http_handler~handle_post( 
              io_request = server->request
              CHANGING co_response = server->response
            ).
          WHEN 'PUT'.
            zif_http_handler~handle_put( 
              io_request = server->request
              CHANGING co_response = server->response
            ).
          WHEN 'DELETE'.
            zif_http_handler~handle_delete( 
              io_request = server->request
              CHANGING co_response = server->response
            ).
          WHEN OTHERS.
            server->response->set_status( code = 405 reason = 'Method Not Allowed' ).
        ENDCASE.
        
      CATCH zcx_http_error INTO DATA(lx_http_error).
        server->response->set_status( 
          code = lx_http_error->get_http_code( )
          reason = lx_http_error->get_text( )
        ).
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        
    ENDTRY.
    
    GET TIME STAMP FIELD lv_end_time.
    
    " Log API call
    log_api_call( 
      iv_method = lv_method
      iv_path = lv_path
      iv_status_code = server->response->get_status( )-code
      iv_response_time = lv_end_time - lv_start_time
    ).
  ENDMETHOD.
  
  METHOD zif_http_handler~handle_get.
    " Generic GET handler with content negotiation
    DATA: lv_accept_type TYPE string,
          lr_response_data TYPE REF TO data.
          
    lv_accept_type = io_request->get_header_field( 'Accept' ).
    
    " Parse path for resource identification
    DATA(ls_path_info) = parse_request_path( io_request->get_header_field( '~path' ) ).
    
    " Get service based on resource
    DATA(lo_service) = mo_service_registry->get_service( ls_path_info-service_name ).
    
    IF lo_service IS BOUND.
      " Prepare request data
      CREATE DATA lr_response_data TYPE zapi_request.
      FIELD-SYMBOLS: <ls_request> TYPE zapi_request.
      ASSIGN lr_response_data->* TO <ls_request>.
      
      <ls_request>-method = 'GET'.
      <ls_request>-resource_id = ls_path_info-resource_id.
      <ls_request>-query_parameters = get_query_parameters( io_request ).
      
      " Call service
      DATA(lr_service_response) = lo_service->handle_request( lr_response_data ).
      
      " Format response based on Accept header
      CASE lv_accept_type.
        WHEN 'application/json'.
          DATA(lv_json_response) = format_json_response( lr_service_response ).
          co_response->set_cdata( lv_json_response ).
          co_response->set_header_field( name = 'Content-Type' value = 'application/json' ).
          
        WHEN 'application/xml'.
          DATA(lv_xml_response) = format_xml_response( lr_service_response ).
          co_response->set_cdata( lv_xml_response ).
          co_response->set_header_field( name = 'Content-Type' value = 'application/xml' ).
          
        WHEN OTHERS.
          " Default to JSON
          lv_json_response = format_json_response( lr_service_response ).
          co_response->set_cdata( lv_json_response ).
          co_response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      ENDCASE.
      
      co_response->set_status( code = 200 reason = 'OK' ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_http_error
        EXPORTING http_code = 404 message = 'Service not found'.
    ENDIF.
  ENDMETHOD.
  
  METHOD zif_http_handler~handle_post.
    " Generic POST handler for resource creation
    DATA: lr_request_data TYPE REF TO data,
          lv_content_type TYPE string.
          
    lv_content_type = io_request->get_header_field( 'Content-Type' ).
    
    " Parse request body based on content type
    CASE lv_content_type.
      WHEN 'application/json'.
        lr_request_data = parse_json_request( io_request->get_cdata( ) ).
      WHEN 'application/xml'.
        lr_request_data = parse_xml_request( io_request->get_cdata( ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_http_error
          EXPORTING http_code = 415 message = 'Unsupported Media Type'.
    ENDCASE.
    
    " Process through service
    DATA(ls_path_info) = parse_request_path( io_request->get_header_field( '~path' ) ).
    DATA(lo_service) = mo_service_registry->get_service( ls_path_info-service_name ).
    
    IF lo_service IS BOUND.
      DATA(lr_response) = lo_service->handle_request( lr_request_data ).
      
      " Return created resource
      DATA(lv_json_response) = format_json_response( lr_response ).
      co_response->set_cdata( lv_json_response ).
      co_response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      co_response->set_status( code = 201 reason = 'Created' ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_http_error
        EXPORTING http_code = 404 message = 'Service not found'.
    ENDIF.
  ENDMETHOD.
  
  METHODS: get_query_parameters
             IMPORTING io_request TYPE REF TO if_http_request
             RETURNING VALUE(rt_parameters) TYPE ztt_query_parameters,
             
           format_json_response
             IMPORTING ir_data TYPE REF TO data
             RETURNING VALUE(rv_json) TYPE string,
             
           format_xml_response
             IMPORTING ir_data TYPE REF TO data
             RETURNING VALUE(rv_xml) TYPE string,
             
           parse_json_request
             IMPORTING iv_json TYPE string
             RETURNING VALUE(rr_data) TYPE REF TO data,
             
           parse_xml_request
             IMPORTING iv_xml TYPE string
             RETURNING VALUE(rr_data) TYPE REF TO data.
ENDCLASS.
```

---

## 2. RESTful API Development

### Professional REST API Implementation

#### **Customer Management REST API**
```abap
*&---------------------------------------------------------------------*
*& Customer Management REST API
*& Endpoints: /api/v1/customers
*& Methods: GET, POST, PUT, DELETE
*&---------------------------------------------------------------------*

CLASS zcl_customer_rest_service DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_web_service.
    
    " Resource-specific methods
    METHODS: get_customers
               IMPORTING it_filters TYPE ztt_api_filters
                         is_pagination TYPE zapi_pagination
               RETURNING VALUE(rs_response) TYPE zcustomer_collection_response,
               
             get_customer_by_id
               IMPORTING iv_customer_id TYPE string
               RETURNING VALUE(rs_response) TYPE zcustomer_response,
               
             create_customer
               IMPORTING is_customer_data TYPE zcustomer_create_request
               RETURNING VALUE(rs_response) TYPE zcustomer_response,
               
             update_customer
               IMPORTING iv_customer_id TYPE string
                         is_customer_data TYPE zcustomer_update_request
               RETURNING VALUE(rs_response) TYPE zcustomer_response,
               
             delete_customer
               IMPORTING iv_customer_id TYPE string
               RETURNING VALUE(rs_response) TYPE zapi_delete_response.
               
  PRIVATE SECTION.
    DATA: mo_customer_repository TYPE REF TO zif_customer_repository,
          mo_validator          TYPE REF TO zif_customer_validator,
          mo_transformer        TYPE REF TO zif_data_transformer.
          
    METHODS: validate_create_request
               IMPORTING is_request TYPE zcustomer_create_request
               RAISING zcx_validation_error,
               
             validate_update_request
               IMPORTING is_request TYPE zcustomer_update_request
               RAISING zcx_validation_error,
               
             transform_to_dto
               IMPORTING io_customer TYPE REF TO zcl_customer
               RETURNING VALUE(rs_dto) TYPE zcustomer_dto,
               
             apply_filters
               IMPORTING it_customers TYPE ztt_customers
                         it_filters TYPE ztt_api_filters
               RETURNING VALUE(rt_filtered) TYPE ztt_customers,
               
             apply_pagination
               IMPORTING it_customers TYPE ztt_customers
                         is_pagination TYPE zapi_pagination
               RETURNING VALUE(rs_paginated) TYPE zcustomer_collection_response.
ENDCLASS.

CLASS zcl_customer_rest_service IMPLEMENTATION.
  METHOD zif_web_service~get_metadata.
    rs_metadata = VALUE #( 
      service_name = 'Customer Management API'
      service_version = 'v1.0'
      endpoint_url = '/api/v1/customers'
      auth_required = abap_true
      rate_limit = 1000
    ).
  ENDMETHOD.
  
  METHOD zif_web_service~handle_request.
    " Main request dispatcher
    FIELD-SYMBOLS: <ls_request> TYPE zapi_request.
    ASSIGN ir_request->* TO <ls_request>.
    
    CASE <ls_request>-method.
      WHEN 'GET'.
        IF <ls_request>-resource_id IS NOT INITIAL.
          " Get single customer
          DATA(ls_customer_response) = get_customer_by_id( <ls_request>-resource_id ).
          CREATE DATA rr_response TYPE zcustomer_response.
          rr_response->* = ls_customer_response.
        ELSE.
          " Get customer collection
          DATA(ls_collection_response) = get_customers( 
            it_filters = <ls_request>-filters
            is_pagination = <ls_request>-pagination
          ).
          CREATE DATA rr_response TYPE zcustomer_collection_response.
          rr_response->* = ls_collection_response.
        ENDIF.
        
      WHEN 'POST'.
        " Create new customer
        FIELD-SYMBOLS: <ls_create_request> TYPE zcustomer_create_request.
        ASSIGN <ls_request>-body->* TO <ls_create_request>.
        
        ls_customer_response = create_customer( <ls_create_request> ).
        CREATE DATA rr_response TYPE zcustomer_response.
        rr_response->* = ls_customer_response.
        
      WHEN 'PUT'.
        " Update existing customer
        FIELD-SYMBOLS: <ls_update_request> TYPE zcustomer_update_request.
        ASSIGN <ls_request>-body->* TO <ls_update_request>.
        
        ls_customer_response = update_customer( 
          iv_customer_id = <ls_request>-resource_id
          is_customer_data = <ls_update_request>
        ).
        CREATE DATA rr_response TYPE zcustomer_response.
        rr_response->* = ls_customer_response.
        
      WHEN 'DELETE'.
        " Delete customer
        DATA(ls_delete_response) = delete_customer( <ls_request>-resource_id ).
        CREATE DATA rr_response TYPE zapi_delete_response.
        rr_response->* = ls_delete_response.
        
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_service_error
          EXPORTING message = |Unsupported method: { <ls_request>-method }|.
    ENDCASE.
  ENDMETHOD.
  
  METHOD get_customers.
    " Get all customers with filtering and pagination
    DATA(lt_all_customers) = mo_customer_repository->find_all( ).
    
    " Apply filters
    DATA(lt_filtered_customers) = apply_filters( 
      it_customers = lt_all_customers
      it_filters = it_filters
    ).
    
    " Apply pagination
    rs_response = apply_pagination( 
      it_customers = lt_filtered_customers
      is_pagination = is_pagination
    ).
    
    " Transform to DTOs
    LOOP AT rs_response-data ASSIGNING FIELD-SYMBOL(<ls_customer>).
      <ls_customer> = transform_to_dto( <ls_customer>-customer_ref ).
    ENDLOOP.
    
    " Set response metadata
    rs_response-metadata = VALUE #( 
      total_count = lines( lt_filtered_customers )
      page_number = is_pagination-page_number
      page_size = is_pagination-page_size
      has_next_page = COND #( WHEN lines( rs_response-data ) = is_pagination-page_size THEN abap_true ELSE abap_false )
    ).
  ENDMETHOD.
  
  METHOD get_customer_by_id.
    " Get single customer by ID
    DATA(lo_customer) = mo_customer_repository->find_by_id( iv_customer_id ).
    
    IF lo_customer IS BOUND.
      rs_response = VALUE #( 
        success = abap_true
        data = transform_to_dto( CAST zcl_customer( lo_customer ) )
        message = 'Customer retrieved successfully'
      ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_service_error
        EXPORTING 
          message = |Customer not found: { iv_customer_id }|
          http_code = 404.
    ENDIF.
  ENDMETHOD.
  
  METHOD create_customer.
    " Validate request
    validate_create_request( is_customer_data ).
    
    " Create customer object
    DATA(lo_customer) = NEW zcl_customer( 
      iv_name = is_customer_data-name
    ).
    
    " Set additional properties
    lo_customer->with_address( is_customer_data-address ).
    lo_customer->with_contact_info( is_customer_data-contact_info ).
    
    " Save to repository
    DATA(lv_saved) = mo_customer_repository->save( lo_customer ).
    
    IF lv_saved = abap_true.
      rs_response = VALUE #( 
        success = abap_true
        data = transform_to_dto( lo_customer )
        message = 'Customer created successfully'
      ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_service_error
        EXPORTING message = 'Failed to create customer'.
    ENDIF.
  ENDMETHOD.
  
  METHOD update_customer.
    " Get existing customer
    DATA(lo_customer) = CAST zcl_customer( mo_customer_repository->find_by_id( iv_customer_id ) ).
    
    IF lo_customer IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_service_error
        EXPORTING 
          message = |Customer not found: { iv_customer_id }|
          http_code = 404.
    ENDIF.
    
    " Validate update request
    validate_update_request( is_customer_data ).
    
    " Apply updates
    IF is_customer_data-name IS NOT INITIAL.
      lo_customer->with_name( is_customer_data-name ).
    ENDIF.
    
    IF is_customer_data-address IS NOT INITIAL.
      lo_customer->change_address( is_customer_data-address ).
    ENDIF.
    
    IF is_customer_data-contact_info IS NOT INITIAL.
      lo_customer->update_contact_info( is_customer_data-contact_info ).
    ENDIF.
    
    " Save changes
    DATA(lv_saved) = mo_customer_repository->save( lo_customer ).
    
    IF lv_saved = abap_true.
      rs_response = VALUE #( 
        success = abap_true
        data = transform_to_dto( lo_customer )
        message = 'Customer updated successfully'
      ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_service_error
        EXPORTING message = 'Failed to update customer'.
    ENDIF.
  ENDMETHOD.
  
  METHOD apply_filters.
    rt_filtered = it_customers.
    
    " Apply each filter
    LOOP AT it_filters INTO DATA(ls_filter).
      CASE ls_filter-field_name.
        WHEN 'NAME'.
          DELETE rt_filtered WHERE name NS ls_filter-filter_value.
        WHEN 'COUNTRY'.
          DELETE rt_filtered WHERE address-country <> ls_filter-filter_value.
        WHEN 'STATUS'.
          DELETE rt_filtered WHERE status <> ls_filter-filter_value.
        WHEN 'CREATED_FROM'.
          DELETE rt_filtered WHERE created_at < ls_filter-filter_value.
        WHEN 'CREATED_TO'.
          DELETE rt_filtered WHERE created_at > ls_filter-filter_value.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD apply_pagination.
    DATA: lv_start_index TYPE i,
          lv_end_index   TYPE i.
          
    " Calculate pagination indices
    lv_start_index = ( is_pagination-page_number - 1 ) * is_pagination-page_size + 1.
    lv_end_index = lv_start_index + is_pagination-page_size - 1.
    
    " Extract page data
    LOOP AT it_customers INTO DATA(ls_customer) FROM lv_start_index TO lv_end_index.
      APPEND VALUE #( customer_ref = ls_customer ) TO rs_paginated-data.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& API Response Models
*&---------------------------------------------------------------------*

" Customer DTO for API responses
TYPES: BEGIN OF zcustomer_dto,
         id           TYPE string,
         name         TYPE string,
         email        TYPE string,
         phone        TYPE string,
         address      TYPE zcl_customer=>ty_address,
         risk_score   TYPE i,
         created_at   TYPE timestamp,
         updated_at   TYPE timestamp,
         _links       TYPE zapi_links,
       END OF zcustomer_dto,
       
       " Collection response with metadata
       BEGIN OF zcustomer_collection_response,
         data         TYPE TABLE OF zcustomer_dto,
         metadata     TYPE zapi_pagination_metadata,
         _links       TYPE zapi_collection_links,
       END OF zcustomer_collection_response,
       
       " Single resource response
       BEGIN OF zcustomer_response,
         success      TYPE abap_bool,
         data         TYPE zcustomer_dto,
         message      TYPE string,
         errors       TYPE ztt_api_errors,
       END OF zcustomer_response.
```

---

## 3. OData Services

### Advanced OData Implementation

#### **OData Service Provider**
```abap
*&---------------------------------------------------------------------*
*& OData Service Implementation
*& Service: ZCUSTOMER_SRV
*& Purpose: Customer management via OData protocol
*&---------------------------------------------------------------------*

CLASS zcl_customer_odata_dpc_ext DEFINITION
  INHERITING FROM zcl_customer_odata_dpc
  FINAL.
  
  PUBLIC SECTION.
    " Redefine standard methods
    METHODS: customerset_get_entities REDEFINITION,
             customerset_get_entity REDEFINITION,
             customerset_create_entity REDEFINITION,
             customerset_update_entity REDEFINITION,
             customerset_delete_entity REDEFINITION.
             
  PRIVATE SECTION.
    DATA: mo_customer_service TYPE REF TO zcl_customer_rest_service,
          mo_odata_converter  TYPE REF TO zcl_odata_converter.
          
    METHODS: handle_odata_exception
               IMPORTING ix_exception TYPE REF TO cx_root,
               
             apply_odata_query_options
               IMPORTING io_tech_request_context TYPE REF TO /iwbep/cl_cp_request_context
               RETURNING VALUE(rs_query_options) TYPE zodata_query_options,
               
             convert_filter_to_abap
               IMPORTING iv_filter TYPE string
               RETURNING VALUE(rt_filters) TYPE ztt_api_filters.
ENDCLASS.

CLASS zcl_customer_odata_dpc_ext IMPLEMENTATION.
  METHOD customerset_get_entities.
    TRY.
        " Extract query options
        DATA(ls_query_options) = apply_odata_query_options( io_tech_request_context ).
        
        " Convert to internal format
        DATA(lt_filters) = convert_filter_to_abap( ls_query_options-filter ).
        DATA(ls_pagination) = VALUE zapi_pagination( 
          page_number = 1
          page_size = COND #( WHEN ls_query_options-top > 0 THEN ls_query_options-top ELSE 50 )
        ).
        
        " Get data from business service
        DATA(ls_collection_response) = mo_customer_service->get_customers( 
          it_filters = lt_filters
          is_pagination = ls_pagination
        ).
        
        " Convert to OData format
        LOOP AT ls_collection_response-data INTO DATA(ls_customer_dto).
          DATA(ls_odata_entity) = mo_odata_converter->convert_dto_to_odata( ls_customer_dto ).
          APPEND ls_odata_entity TO et_entityset.
        ENDLOOP.
        
        " Handle $inlinecount
        IF ls_query_options-inlinecount = '$inlinecount=allpages'.
          es_response_context-count = ls_collection_response-metadata-total_count.
        ENDIF.
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        handle_odata_exception( lx_service_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD customerset_get_entity.
    TRY.
        " Extract key values
        io_tech_request_context->get_converted_keys( 
          IMPORTING et_key_tab = DATA(lt_keys)
        ).
        
        READ TABLE lt_keys INTO DATA(ls_key) WITH KEY name = 'ID'.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE /iwbep/cx_cp_request_context
            EXPORTING message_text = 'Customer ID is required'.
        ENDIF.
        
        " Get single customer
        DATA(ls_response) = mo_customer_service->get_customer_by_id( ls_key-value ).
        
        " Convert to OData entity
        er_entity = mo_odata_converter->convert_dto_to_odata( ls_response-data ).
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        handle_odata_exception( lx_service_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD customerset_create_entity.
    TRY.
        " Convert OData entity to DTO
        DATA(ls_create_request) = mo_odata_converter->convert_odata_to_create_dto( er_entity ).
        
        " Create via business service
        DATA(ls_response) = mo_customer_service->create_customer( ls_create_request ).
        
        " Convert response back to OData format
        er_entity = mo_odata_converter->convert_dto_to_odata( ls_response-data ).
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        handle_odata_exception( lx_service_error ).
      CATCH zcx_validation_error INTO DATA(lx_validation_error).
        handle_odata_exception( lx_validation_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD customerset_update_entity.
    TRY.
        " Extract key for update
        io_tech_request_context->get_converted_keys( 
          IMPORTING et_key_tab = DATA(lt_keys)
        ).
        
        READ TABLE lt_keys INTO DATA(ls_key) WITH KEY name = 'ID'.
        
        " Convert OData entity to update DTO
        DATA(ls_update_request) = mo_odata_converter->convert_odata_to_update_dto( er_entity ).
        
        " Update via business service
        DATA(ls_response) = mo_customer_service->update_customer( 
          iv_customer_id = ls_key-value
          is_customer_data = ls_update_request
        ).
        
        " Convert response back to OData format
        er_entity = mo_odata_converter->convert_dto_to_odata( ls_response-data ).
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        handle_odata_exception( lx_service_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD customerset_delete_entity.
    TRY.
        " Extract key for deletion
        io_tech_request_context->get_converted_keys( 
          IMPORTING et_key_tab = DATA(lt_keys)
        ).
        
        READ TABLE lt_keys INTO DATA(ls_key) WITH KEY name = 'ID'.
        
        " Delete via business service
        DATA(ls_response) = mo_customer_service->delete_customer( ls_key-value ).
        
        IF ls_response-success = abap_false.
          RAISE EXCEPTION TYPE zcx_service_error
            EXPORTING message = ls_response-message.
        ENDIF.
        
      CATCH zcx_service_error INTO DATA(lx_service_error).
        handle_odata_exception( lx_service_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD apply_odata_query_options.
    " Extract standard OData query options
    DATA(lo_query_options) = io_tech_request_context->get_query_options( ).
    
    rs_query_options = VALUE #( 
      filter = lo_query_options->get_filter( )
      orderby = lo_query_options->get_orderby( )
      skip = lo_query_options->get_skip( )
      top = lo_query_options->get_top( )
      inlinecount = lo_query_options->get_inlinecount( )
      select = lo_query_options->get_select( )
      expand = lo_query_options->get_expand( )
    ).
  ENDMETHOD.
  
  METHOD convert_filter_to_abap.
    " Parse OData $filter expression to internal filter format
    " This is a simplified implementation - real-world would need full OData filter parser
    
    IF iv_filter CS 'name eq'.
      " Extract name filter: name eq 'value'
      FIND REGEX 'name eq ''(.*)''' IN iv_filter SUBMATCHES DATA(lv_name_value).
      IF sy-subrc = 0.
        APPEND VALUE #( field_name = 'NAME' operator = 'EQ' filter_value = lv_name_value ) TO rt_filters.
      ENDIF.
    ENDIF.
    
    IF iv_filter CS 'country eq'.
      " Extract country filter
      FIND REGEX 'country eq ''(.*)''' IN iv_filter SUBMATCHES DATA(lv_country_value).
      IF sy-subrc = 0.
        APPEND VALUE #( field_name = 'COUNTRY' operator = 'EQ' filter_value = lv_country_value ) TO rt_filters.
      ENDIF.
    ENDIF.
    
    " Add more filter parsing as needed
  ENDMETHOD.
  
  METHOD handle_odata_exception.
    " Convert internal exceptions to OData errors
    DATA: lo_message_container TYPE REF TO /iwbep/if_cp_message_container.
    
    lo_message_container = /iwbep/cl_cp_message_container=>create_container( ).
    
    CASE TYPE OF ix_exception.
      WHEN TYPE zcx_service_error.
        DATA(lx_service_error) = CAST zcx_service_error( ix_exception ).
        lo_message_container->add_message( 
          iv_msg_type = 'E'
          iv_msg_text = lx_service_error->get_text( )
          iv_error_category = /iwbep/cx_cp_request_context=>gcs_error_category-business_error
        ).
        
      WHEN TYPE zcx_validation_error.
        DATA(lx_validation_error) = CAST zcx_validation_error( ix_exception ).
        lo_message_container->add_message( 
          iv_msg_type = 'E'
          iv_msg_text = lx_validation_error->get_text( )
          iv_error_category = /iwbep/cx_cp_request_context=>gcs_error_category-business_error
        ).
        
      WHEN OTHERS.
        lo_message_container->add_message( 
          iv_msg_type = 'E'
          iv_msg_text = 'Internal server error'
          iv_error_category = /iwbep/cx_cp_request_context=>gcs_error_category-system_error
        ).
    ENDCASE.
    
    " Raise OData exception
    RAISE EXCEPTION TYPE /iwbep/cx_cp_request_context
      EXPORTING message_container = lo_message_container.
  ENDMETHOD.
ENDCLASS.
```

This comprehensive web services and interfaces module demonstrates enterprise-level integration patterns, proper REST API design, OData service implementation, and modern SAP integration techniques used in large-scale SAP landscapes.

---

**Next Module**: [Module 14: Performance Optimization](Module_14_Performance_Optimization.md)