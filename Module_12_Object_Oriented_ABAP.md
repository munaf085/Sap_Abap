# Module 12: Object-Oriented ABAP (OO-ABAP)

## 🎯 **Complete Guide to Object-Oriented Programming in ABAP**

**Learn OOP from basics to enterprise-level patterns - No prior OOP knowledge required!**

This module teaches you everything about Object-Oriented Programming in ABAP, starting from fundamental concepts and progressing to advanced enterprise patterns.

---

## 📖 **Table of Contents**
1. [🌟 OOP Fundamentals - What & Why](#-oop-fundamentals---what--why)
2. [🏗️ Classes & Objects - Building Blocks](#️-classes--objects---building-blocks)
3. [🔧 Methods & Attributes - Core Components](#-methods--attributes---core-components)
4. [🎭 Inheritance - Code Reusability](#-inheritance---code-reusability)
5. [🔌 Interfaces - Contracts & Standards](#-interfaces---contracts--standards)
6. [🛡️ Encapsulation - Data Protection](#️-encapsulation---data-protection)
7. [🔄 Polymorphism - Flexible Design](#-polymorphism---flexible-design)
8. [⚠️ Exception Handling - Error Management](#️-exception-handling---error-management)
9. [🏭 Design Patterns - Best Practices](#-design-patterns---best-practices)
10. [🚀 Advanced Enterprise Patterns](#-advanced-enterprise-patterns)

---

## 🌟 **OOP Fundamentals - What & Why**

### **What is Object-Oriented Programming?**

Object-Oriented Programming (OOP) is a programming approach that organizes code around **objects** rather than functions. Think of it like building with LEGO blocks - each block (object) has specific properties and can perform certain actions.

#### **Real-World Analogy: Car Factory**
```abap
" Traditional Programming (Procedural):
" - build_engine()
" - build_wheels()
" - assemble_car()
" - paint_car()

" Object-Oriented Programming:
" - Car object with properties: color, engine, wheels
" - Car object with methods: start(), stop(), accelerate()
" - Different car types: Sedan, SUV, Truck (inheritance)
```

### **Why Use OOP in ABAP?**

#### **Benefits:**
- 🔄 **Reusability** - Write once, use many times
- 🛡️ **Maintainability** - Changes in one place affect whole system
- 🎯 **Modularity** - Break complex problems into smaller parts
- 🔒 **Security** - Control access to data and methods
- 🚀 **Scalability** - Easy to extend and modify

#### **ABAP Without OOP vs With OOP:**
```abap
" ❌ Traditional ABAP (Procedural)
REPORT z_calculate_salary.

DATA: gv_employee_id TYPE string,
      gv_basic_salary TYPE p DECIMALS 2,
      gv_bonus TYPE p DECIMALS 2,
      gv_total_salary TYPE p DECIMALS 2.

PERFORM get_employee_data USING gv_employee_id
                         CHANGING gv_basic_salary gv_bonus.
PERFORM calculate_total USING gv_basic_salary gv_bonus
                       CHANGING gv_total_salary.
PERFORM display_result USING gv_employee_id gv_total_salary.

" ✅ Object-Oriented ABAP
REPORT z_calculate_salary_oop.

DATA(lo_employee) = NEW zcl_employee( iv_id = 'EMP001' ).
lo_employee->calculate_salary( ).
lo_employee->display_salary( ).
```

### **Core OOP Concepts - Simple Explanation**

| **Concept** | **Simple Definition** | **Real-World Example** |
|-------------|----------------------|------------------------|
| **Class** | Blueprint/Template | Car blueprint |
| **Object** | Instance of a class | Actual car built from blueprint |
| **Method** | Actions object can do | start_engine(), brake() |
| **Attribute** | Properties of object | color, model, year |
| **Inheritance** | Child inherits from parent | Sports car inherits from Car |
| **Interface** | Contract/Agreement | All vehicles must start() and stop() |

---

## 🏗️ **Classes & Objects - Building Blocks**

### **Understanding Classes**

A **Class** is like a blueprint or template. It defines what properties (attributes) and actions (methods) objects will have.

#### **Class Structure in ABAP**
```abap
" Class Definition - The Blueprint
CLASS zcl_student DEFINITION.
  
  PUBLIC SECTION.
    " What others can see and use
    METHODS: constructor IMPORTING iv_name TYPE string
                                   iv_age TYPE i,
             get_name RETURNING VALUE(rv_name) TYPE string,
             get_age RETURNING VALUE(rv_age) TYPE i,
             study IMPORTING iv_subject TYPE string,
             display_info.
             
  PRIVATE SECTION.
    " Internal data - hidden from outside
    DATA: mv_name TYPE string,
          mv_age TYPE i,
          mv_subjects_studied TYPE i.
          
ENDCLASS.

" Class Implementation - The Actual Code
CLASS zcl_student IMPLEMENTATION.

  METHOD constructor.
    " Initialize object when created
    mv_name = iv_name.
    mv_age = iv_age.
    mv_subjects_studied = 0.
    
    WRITE: |Student created: { mv_name }, Age: { mv_age }|.
  ENDMETHOD.

  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.

  METHOD get_age.
    rv_age = mv_age.
  ENDMETHOD.

  METHOD study.
    mv_subjects_studied = mv_subjects_studied + 1.
    WRITE: |{ mv_name } is studying { iv_subject }|.
  ENDMETHOD.

  METHOD display_info.
    WRITE: / |Student: { mv_name }|,
           / |Age: { mv_age }|,
           / |Subjects Studied: { mv_subjects_studied }|.
  ENDMETHOD.

ENDCLASS.
```

### **Creating and Using Objects**

#### **Step-by-Step Object Creation**
```abap
" Step 1: Declare object reference
DATA: lo_student1 TYPE REF TO zcl_student,
      lo_student2 TYPE REF TO zcl_student.

" Step 2: Create objects (instances)
lo_student1 = NEW zcl_student( iv_name = 'John' iv_age = 20 ).
lo_student2 = NEW zcl_student( iv_name = 'Mary' iv_age = 22 ).

" Step 3: Use objects (call methods)
lo_student1->study( 'Mathematics' ).
lo_student1->study( 'Physics' ).
lo_student2->study( 'Chemistry' ).

" Step 4: Display information
lo_student1->display_info( ).
lo_student2->display_info( ).

" Output:
" Student created: John, Age: 20
" Student created: Mary, Age: 22
" John is studying Mathematics
" John is studying Physics
" Mary is studying Chemistry
" Student: John
" Age: 20
" Subjects Studied: 2
" Student: Mary
" Age: 22
" Subjects Studied: 1
```

### **Class vs Object - Clear Distinction**

```abap
" 🏗️ CLASS = Blueprint (Definition)
CLASS zcl_car DEFINITION.
  PUBLIC SECTION.
    METHODS: start_engine,
             stop_engine,
             accelerate.
  PRIVATE SECTION.
    DATA: mv_engine_running TYPE abap_bool.
ENDCLASS.

" 🚗 OBJECTS = Actual Cars (Instances)
DATA: lo_toyota TYPE REF TO zcl_car,    " Toyota car
      lo_honda TYPE REF TO zcl_car,     " Honda car
      lo_bmw TYPE REF TO zcl_car.       " BMW car

" Each object is independent
lo_toyota = NEW zcl_car( ).
lo_honda = NEW zcl_car( ).
lo_bmw = NEW zcl_car( ).

" Each car can be in different states
lo_toyota->start_engine( ).  " Toyota running
" Honda and BMW are still off
```

---

## 1. Advanced OOP Architecture

### Enterprise Class Design

#### **Comprehensive OOP Framework**
```abap
*&---------------------------------------------------------------------*
*& Enterprise OOP Framework
*& Purpose: Demonstrate advanced OOP patterns and architecture
*& Design: Clean Architecture with dependency injection
*&---------------------------------------------------------------------*

" ===== FOUNDATIONAL INTERFACES =====

" Base entity interface for domain objects
INTERFACE zif_entity.
  METHODS: get_id RETURNING VALUE(rv_id) TYPE string,
           set_id IMPORTING iv_id TYPE string,
           is_valid RETURNING VALUE(rv_valid) TYPE abap_bool,
           to_string RETURNING VALUE(rv_string) TYPE string.
ENDINTERFACE.

" Repository pattern interface
INTERFACE zif_repository.
  METHODS: find_by_id 
             IMPORTING iv_id TYPE string
             RETURNING VALUE(ro_entity) TYPE REF TO zif_entity,
           find_all
             RETURNING VALUE(rt_entities) TYPE ztt_entities,
           save
             IMPORTING io_entity TYPE REF TO zif_entity
             RETURNING VALUE(rv_success) TYPE abap_bool,
           delete
             IMPORTING iv_id TYPE string
             RETURNING VALUE(rv_success) TYPE abap_bool.
ENDINTERFACE.

" Service layer interface
INTERFACE zif_service.
  METHODS: execute
             IMPORTING it_parameters TYPE ztt_service_parameters
             RETURNING VALUE(rs_result) TYPE zservice_result.
ENDINTERFACE.

" Event publisher interface
INTERFACE zif_event_publisher.
  METHODS: publish
             IMPORTING io_event TYPE REF TO zif_domain_event.
ENDINTERFACE.

" Domain event interface
INTERFACE zif_domain_event.
  METHODS: get_event_type RETURNING VALUE(rv_type) TYPE string,
           get_timestamp RETURNING VALUE(rv_timestamp) TYPE timestamp,
           get_data RETURNING VALUE(rr_data) TYPE REF TO data.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Advanced Customer Domain Model
*&---------------------------------------------------------------------*

CLASS zcl_customer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_entity.
    
    " Value objects
    TYPES: BEGIN OF ty_address,
             street      TYPE string,
             city        TYPE string,
             postal_code TYPE string,
             country     TYPE land1,
           END OF ty_address,
           
           BEGIN OF ty_contact_info,
             email       TYPE ad_smtpadr,
             phone       TYPE telf1,
             mobile      TYPE telf1,
           END OF ty_contact_info.
    
    " Constructor with builder pattern support
    METHODS: constructor
               IMPORTING iv_id TYPE string OPTIONAL
                         iv_name TYPE string OPTIONAL,
                         
             " Fluent interface methods
             with_name
               IMPORTING iv_name TYPE string
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_customer,
               
             with_address
               IMPORTING is_address TYPE ty_address
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_customer,
               
             with_contact_info
               IMPORTING is_contact TYPE ty_contact_info
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_customer,
               
             " Business methods
             change_address
               IMPORTING is_new_address TYPE ty_address
               RAISING zcx_invalid_address,
               
             update_contact_info
               IMPORTING is_contact TYPE ty_contact_info
               RAISING zcx_invalid_contact,
               
             calculate_risk_score
               RETURNING VALUE(rv_score) TYPE i,
               
             " Query methods
             get_name RETURNING VALUE(rv_name) TYPE string,
             get_address RETURNING VALUE(rs_address) TYPE ty_address,
             get_contact_info RETURNING VALUE(rs_contact) TYPE ty_contact_info,
             is_active RETURNING VALUE(rv_active) TYPE abap_bool.
             
  PROTECTED SECTION.
    " Domain logic validation
    METHODS: validate_address
               IMPORTING is_address TYPE ty_address
               RAISING zcx_invalid_address,
               
             validate_contact_info
               IMPORTING is_contact TYPE ty_contact_info
               RAISING zcx_invalid_contact,
               
             raise_domain_event
               IMPORTING io_event TYPE REF TO zif_domain_event.
               
  PRIVATE SECTION.
    DATA: mv_id           TYPE string,
          mv_name         TYPE string,
          ms_address      TYPE ty_address,
          ms_contact_info TYPE ty_contact_info,
          mv_active       TYPE abap_bool,
          mv_created_at   TYPE timestamp,
          mv_updated_at   TYPE timestamp,
          mo_event_publisher TYPE REF TO zif_event_publisher.
ENDCLASS.

CLASS zcl_customer IMPLEMENTATION.
  METHOD constructor.
    mv_id = COND #( WHEN iv_id IS NOT INITIAL THEN iv_id ELSE cl_system_uuid=>create_uuid_x16_static( ) ).
    mv_name = iv_name.
    mv_active = abap_true.
    GET TIME STAMP FIELD mv_created_at.
    mv_updated_at = mv_created_at.
    
    " Inject event publisher (could be done via DI container)
    mo_event_publisher = zcl_event_publisher=>get_instance( ).
  ENDMETHOD.
  
  METHOD with_name.
    mv_name = iv_name.
    ro_customer = me.
  ENDMETHOD.
  
  METHOD with_address.
    validate_address( is_address ).
    ms_address = is_address.
    ro_customer = me.
  ENDMETHOD.
  
  METHOD with_contact_info.
    validate_contact_info( is_contact ).
    ms_contact_info = is_contact.
    ro_customer = me.
  ENDMETHOD.
  
  METHOD change_address.
    validate_address( is_new_address ).
    
    DATA(ls_old_address) = ms_address.
    ms_address = is_new_address.
    GET TIME STAMP FIELD mv_updated_at.
    
    " Raise domain event
    DATA(lo_event) = NEW zcl_customer_address_changed_event( 
      iv_customer_id = mv_id
      is_old_address = ls_old_address
      is_new_address = is_new_address
    ).
    
    raise_domain_event( lo_event ).
  ENDMETHOD.
  
  METHOD calculate_risk_score.
    " Complex risk calculation logic
    rv_score = 50.  " Base score
    
    " Country risk factor
    CASE ms_address-country.
      WHEN 'US' OR 'DE' OR 'GB'.
        rv_score -= 10.
      WHEN 'IN' OR 'CN' OR 'BR'.
        rv_score += 5.
      WHEN OTHERS.
        rv_score += 15.
    ENDCASE.
    
    " Contact completeness factor
    IF ms_contact_info-email IS NOT INITIAL.
      rv_score -= 5.
    ENDIF.
    
    IF ms_contact_info-phone IS NOT INITIAL.
      rv_score -= 5.
    ENDIF.
    
    " Ensure bounds
    rv_score = COND #( WHEN rv_score < 0 THEN 0 
                      WHEN rv_score > 100 THEN 100 
                      ELSE rv_score ).
  ENDMETHOD.
  
  METHOD zif_entity~get_id.
    rv_id = mv_id.
  ENDMETHOD.
  
  METHOD zif_entity~set_id.
    mv_id = iv_id.
  ENDMETHOD.
  
  METHOD zif_entity~is_valid.
    rv_valid = COND #( WHEN mv_name IS NOT INITIAL AND mv_active = abap_true THEN abap_true ELSE abap_false ).
  ENDMETHOD.
  
  METHOD zif_entity~to_string.
    rv_string = |Customer: { mv_name } (ID: { mv_id })|.
  ENDMETHOD.
  
  METHOD validate_address.
    IF is_address-street IS INITIAL OR 
       is_address-city IS INITIAL OR 
       is_address-country IS INITIAL.
      RAISE EXCEPTION TYPE zcx_invalid_address
        EXPORTING message = 'Address must have street, city, and country'.
    ENDIF.
    
    " Validate country code
    SELECT SINGLE land1 FROM t005
      INTO @DATA(lv_country)
      WHERE land1 = @is_address-country.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_invalid_address
        EXPORTING message = 'Invalid country code'.
    ENDIF.
  ENDMETHOD.
  
  METHOD validate_contact_info.
    " Email validation
    IF is_contact-email IS NOT INITIAL.
      FIND REGEX '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$' IN is_contact-email.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_invalid_contact
          EXPORTING message = 'Invalid email format'.
      ENDIF.
    ENDIF.
    
    " Phone validation
    IF is_contact-phone IS NOT INITIAL.
      FIND REGEX '^\+?[1-9]\d{1,14}$' IN is_contact-phone.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_invalid_contact
          EXPORTING message = 'Invalid phone format'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  
  METHOD raise_domain_event.
    IF mo_event_publisher IS BOUND.
      mo_event_publisher->publish( io_event ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Repository Implementation
*&---------------------------------------------------------------------*

CLASS zcl_customer_repository DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_repository.
    
    METHODS: constructor
               IMPORTING io_db_adapter TYPE REF TO zif_database_adapter OPTIONAL.
               
  PRIVATE SECTION.
    DATA: mo_db_adapter TYPE REF TO zif_database_adapter,
          mt_cache      TYPE ztt_customer_cache.
          
    METHODS: load_from_database
               IMPORTING iv_id TYPE string
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_customer,
               
             save_to_database
               IMPORTING io_customer TYPE REF TO zcl_customer
               RETURNING VALUE(rv_success) TYPE abap_bool,
               
             update_cache
               IMPORTING io_customer TYPE REF TO zcl_customer.
ENDCLASS.

CLASS zcl_customer_repository IMPLEMENTATION.
  METHOD constructor.
    mo_db_adapter = COND #( WHEN io_db_adapter IS BOUND 
                           THEN io_db_adapter 
                           ELSE NEW zcl_sap_database_adapter( ) ).
  ENDMETHOD.
  
  METHOD zif_repository~find_by_id.
    " Check cache first
    READ TABLE mt_cache INTO DATA(ls_cache_entry)
      WITH KEY customer_id = iv_id.
      
    IF sy-subrc = 0 AND ls_cache_entry-cached_at > sy-timestamp - 300. " 5 min cache
      ro_entity ?= ls_cache_entry-customer_ref.
      RETURN.
    ENDIF.
    
    " Load from database
    ro_entity ?= load_from_database( iv_id ).
    
    " Update cache
    IF ro_entity IS BOUND.
      update_cache( CAST zcl_customer( ro_entity ) ).
    ENDIF.
  ENDMETHOD.
  
  METHOD zif_repository~save.
    DATA(lo_customer) = CAST zcl_customer( io_entity ).
    
    rv_success = save_to_database( lo_customer ).
    
    IF rv_success = abap_true.
      update_cache( lo_customer ).
    ENDIF.
  ENDMETHOD.
  
  METHOD load_from_database.
    " Database access through adapter pattern
    DATA(lt_customer_data) = mo_db_adapter->select_customer( iv_id ).
    
    IF lines( lt_customer_data ) > 0.
      DATA(ls_data) = lt_customer_data[ 1 ].
      
      " Reconstruct customer object
      ro_customer = NEW zcl_customer( 
        iv_id = ls_data-customer_id
        iv_name = ls_data-name
      ).
      
      " Set additional properties
      ro_customer->with_address( VALUE #( 
        street = ls_data-street
        city = ls_data-city
        postal_code = ls_data-postal_code
        country = ls_data-country
      ) ).
      
      ro_customer->with_contact_info( VALUE #( 
        email = ls_data-email
        phone = ls_data-phone
        mobile = ls_data-mobile
      ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## 2. Design Patterns Implementation

### Enterprise Design Patterns

#### **Factory & Abstract Factory Patterns**
```abap
*&---------------------------------------------------------------------*
*& Factory Pattern Implementation
*&---------------------------------------------------------------------*

" Abstract factory for creating business objects
INTERFACE zif_business_object_factory.
  METHODS: create_customer
             IMPORTING iv_type TYPE string
             RETURNING VALUE(ro_customer) TYPE REF TO zif_entity,
             
           create_order
             IMPORTING iv_type TYPE string
             RETURNING VALUE(ro_order) TYPE REF TO zif_entity,
             
           create_product
             IMPORTING iv_type TYPE string
             RETURNING VALUE(ro_product) TYPE REF TO zif_entity.
ENDINTERFACE.

" Concrete factory for retail business
CLASS zcl_retail_object_factory DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_business_object_factory.
    
  PRIVATE SECTION.
    METHODS: create_retail_customer
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_retail_customer,
             create_corporate_customer
               RETURNING VALUE(ro_customer) TYPE REF TO zcl_corporate_customer.
ENDCLASS.

CLASS zcl_retail_object_factory IMPLEMENTATION.
  METHOD zif_business_object_factory~create_customer.
    CASE iv_type.
      WHEN 'RETAIL'.
        ro_customer ?= create_retail_customer( ).
      WHEN 'CORPORATE'.
        ro_customer ?= create_corporate_customer( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_unsupported_type
          EXPORTING type = iv_type.
    ENDCASE.
  ENDMETHOD.
  
  METHOD create_retail_customer.
    ro_customer = NEW zcl_retail_customer( ).
  ENDMETHOD.
  
  METHOD create_corporate_customer.
    ro_customer = NEW zcl_corporate_customer( ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Strategy Pattern Implementation
*&---------------------------------------------------------------------*

" Strategy interface for pricing
INTERFACE zif_pricing_strategy.
  METHODS: calculate_price
             IMPORTING iv_base_price TYPE dmbtr
                       is_context TYPE zprice_context
             RETURNING VALUE(rv_final_price) TYPE dmbtr.
ENDINTERFACE.

" Concrete strategies
CLASS zcl_standard_pricing DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_pricing_strategy.
ENDCLASS.

CLASS zcl_standard_pricing IMPLEMENTATION.
  METHOD zif_pricing_strategy~calculate_price.
    rv_final_price = iv_base_price.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_volume_discount_pricing DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_pricing_strategy.
    
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_discount_tiers,
                 tier1_quantity TYPE i VALUE 10,
                 tier1_discount TYPE p DECIMALS 2 VALUE '0.05',
                 tier2_quantity TYPE i VALUE 50,
                 tier2_discount TYPE p DECIMALS 2 VALUE '0.10',
                 tier3_quantity TYPE i VALUE 100,
                 tier3_discount TYPE p DECIMALS 2 VALUE '0.15',
               END OF c_discount_tiers.
ENDCLASS.

CLASS zcl_volume_discount_pricing IMPLEMENTATION.
  METHOD zif_pricing_strategy~calculate_price.
    DATA: lv_discount TYPE p DECIMALS 2.
    
    " Determine discount based on quantity
    IF is_context-quantity >= c_discount_tiers-tier3_quantity.
      lv_discount = c_discount_tiers-tier3_discount.
    ELSEIF is_context-quantity >= c_discount_tiers-tier2_quantity.
      lv_discount = c_discount_tiers-tier2_discount.
    ELSEIF is_context-quantity >= c_discount_tiers-tier1_quantity.
      lv_discount = c_discount_tiers-tier1_discount.
    ENDIF.
    
    rv_final_price = iv_base_price * ( 1 - lv_discount ).
  ENDMETHOD.
ENDCLASS.

" Context class using strategy
CLASS zcl_price_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING io_strategy TYPE REF TO zif_pricing_strategy,
               
             set_strategy
               IMPORTING io_strategy TYPE REF TO zif_pricing_strategy,
               
             calculate
               IMPORTING iv_base_price TYPE dmbtr
                         is_context TYPE zprice_context
               RETURNING VALUE(rv_price) TYPE dmbtr.
               
  PRIVATE SECTION.
    DATA: mo_pricing_strategy TYPE REF TO zif_pricing_strategy.
ENDCLASS.

CLASS zcl_price_calculator IMPLEMENTATION.
  METHOD constructor.
    mo_pricing_strategy = io_strategy.
  ENDMETHOD.
  
  METHOD set_strategy.
    mo_pricing_strategy = io_strategy.
  ENDMETHOD.
  
  METHOD calculate.
    IF mo_pricing_strategy IS BOUND.
      rv_price = mo_pricing_strategy->calculate_price( 
        iv_base_price = iv_base_price
        is_context = is_context
      ).
    ELSE.
      rv_price = iv_base_price.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Observer Pattern Implementation
*&---------------------------------------------------------------------*

" Observer interface
INTERFACE zif_observer.
  METHODS: update
             IMPORTING io_subject TYPE REF TO zif_observable
                       iv_event_type TYPE string
                       ir_event_data TYPE REF TO data.
ENDINTERFACE.

" Observable interface
INTERFACE zif_observable.
  METHODS: attach_observer
             IMPORTING io_observer TYPE REF TO zif_observer,
           detach_observer
             IMPORTING io_observer TYPE REF TO zif_observer,
           notify_observers
             IMPORTING iv_event_type TYPE string
                       ir_event_data TYPE REF TO data OPTIONAL.
ENDINTERFACE.

" Concrete observable
CLASS zcl_order DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_observable,
                zif_entity.
                
    METHODS: constructor
               IMPORTING iv_order_id TYPE string,
               
             add_item
               IMPORTING is_item TYPE zorder_item,
               
             change_status
               IMPORTING iv_new_status TYPE zorder_status,
               
             get_status
               RETURNING VALUE(rv_status) TYPE zorder_status.
               
  PRIVATE SECTION.
    DATA: mv_order_id   TYPE string,
          mv_status     TYPE zorder_status,
          mt_items      TYPE ztt_order_items,
          mt_observers  TYPE ztt_observers.
ENDCLASS.

CLASS zcl_order IMPLEMENTATION.
  METHOD constructor.
    mv_order_id = iv_order_id.
    mv_status = 'NEW'.
  ENDMETHOD.
  
  METHOD zif_observable~attach_observer.
    APPEND io_observer TO mt_observers.
  ENDMETHOD.
  
  METHOD zif_observable~detach_observer.
    DELETE mt_observers WHERE table_line = io_observer.
  ENDMETHOD.
  
  METHOD zif_observable~notify_observers.
    LOOP AT mt_observers INTO DATA(lo_observer).
      lo_observer->update( 
        io_subject = me
        iv_event_type = iv_event_type
        ir_event_data = ir_event_data
      ).
    ENDLOOP.
  ENDMETHOD.
  
  METHOD change_status.
    DATA(lv_old_status) = mv_status.
    mv_status = iv_new_status.
    
    " Create event data
    DATA(ls_status_change) = VALUE zstatus_change_event( 
      order_id = mv_order_id
      old_status = lv_old_status
      new_status = iv_new_status
      timestamp = sy-datum
    ).
    
    CREATE DATA DATA(lr_event_data) TYPE zstatus_change_event.
    lr_event_data->* = ls_status_change.
    
    " Notify observers
    zif_observable~notify_observers( 
      iv_event_type = 'STATUS_CHANGED'
      ir_event_data = lr_event_data
    ).
  ENDMETHOD.
ENDCLASS.

" Concrete observers
CLASS zcl_inventory_observer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_observer.
ENDCLASS.

CLASS zcl_inventory_observer IMPLEMENTATION.
  METHOD zif_observer~update.
    CASE iv_event_type.
      WHEN 'STATUS_CHANGED'.
        " Handle inventory updates based on order status
        FIELD-SYMBOLS: <ls_event> TYPE zstatus_change_event.
        ASSIGN ir_event_data->* TO <ls_event>.
        
        CASE <ls_event>-new_status.
          WHEN 'CONFIRMED'.
            " Reserve inventory
          WHEN 'SHIPPED'.
            " Reduce inventory
          WHEN 'CANCELLED'.
            " Release inventory
        ENDCASE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_notification_observer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_observer.
ENDCLASS.

CLASS zcl_notification_observer IMPLEMENTATION.
  METHOD zif_observer~update.
    CASE iv_event_type.
      WHEN 'STATUS_CHANGED'.
        " Send notifications
        FIELD-SYMBOLS: <ls_event> TYPE zstatus_change_event.
        ASSIGN ir_event_data->* TO <ls_event>.
        
        " Send email notification
        DATA(lo_email_service) = zcl_email_service=>get_instance( ).
        lo_email_service->send_order_status_notification( <ls_event> ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

---

## 3. Interfaces & Abstract Classes

### Advanced Interface Design

#### **Complex Interface Hierarchies**
```abap
*&---------------------------------------------------------------------*
*& Advanced Interface Architecture
*&---------------------------------------------------------------------*

" Base behavior interface
INTERFACE zif_auditable.
  METHODS: get_created_by RETURNING VALUE(rv_user) TYPE syuname,
           get_created_at RETURNING VALUE(rv_timestamp) TYPE timestamp,
           get_modified_by RETURNING VALUE(rv_user) TYPE syuname,
           get_modified_at RETURNING VALUE(rv_timestamp) TYPE timestamp.
ENDINTERFACE.

" Persistence behavior
INTERFACE zif_persistable.
  METHODS: save RAISING zcx_persistence_error,
           delete RAISING zcx_persistence_error,
           is_dirty RETURNING VALUE(rv_dirty) TYPE abap_bool.
ENDINTERFACE.

" Validation behavior
INTERFACE zif_validatable.
  METHODS: validate RETURNING VALUE(rt_errors) TYPE ztt_validation_errors,
           is_valid RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDINTERFACE.

" Composite interface for business entities
INTERFACE zif_business_entity.
  INTERFACES: zif_entity,
              zif_auditable,
              zif_persistable,
              zif_validatable.
              
  " Additional business entity methods
  METHODS: get_business_key RETURNING VALUE(rv_key) TYPE string,
           is_active RETURNING VALUE(rv_active) TYPE abap_bool,
           archive RAISING zcx_business_rule_violation.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Abstract Base Class
*&---------------------------------------------------------------------*

CLASS zcl_abstract_business_entity DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES: zif_business_entity ABSTRACT METHODS save delete validate.
    
    " Template method pattern
    METHODS: process_business_transaction FINAL
               IMPORTING is_transaction_data TYPE any
               RAISING zcx_business_error.
               
  PROTECTED SECTION.
    " Template method hooks (to be implemented by subclasses)
    METHODS: before_transaction_processing ABSTRACT
               IMPORTING is_transaction_data TYPE any
               RAISING zcx_business_error,
               
             execute_transaction_logic ABSTRACT
               IMPORTING is_transaction_data TYPE any
               RAISING zcx_business_error,
               
             after_transaction_processing ABSTRACT
               IMPORTING is_transaction_data TYPE any
               RAISING zcx_business_error,
               
             " Common utility methods
             log_business_event
               IMPORTING iv_event_type TYPE string
                         ir_event_data TYPE REF TO data,
                         
             check_authorization
               IMPORTING iv_activity TYPE string
               RAISING zcx_authorization_error.
               
  PRIVATE SECTION.
    DATA: mv_entity_id    TYPE string,
          mv_created_by   TYPE syuname,
          mv_created_at   TYPE timestamp,
          mv_modified_by  TYPE syuname,
          mv_modified_at  TYPE timestamp,
          mv_is_dirty     TYPE abap_bool.
ENDCLASS.

CLASS zcl_abstract_business_entity IMPLEMENTATION.
  METHOD process_business_transaction.
    " Template method implementation
    TRY.
        " Pre-processing hook
        before_transaction_processing( is_transaction_data ).
        
        " Core business logic hook
        execute_transaction_logic( is_transaction_data ).
        
        " Post-processing hook
        after_transaction_processing( is_transaction_data ).
        
        " Mark as modified
        mv_modified_by = sy-uname.
        GET TIME STAMP FIELD mv_modified_at.
        mv_is_dirty = abap_true.
        
      CATCH zcx_business_error INTO DATA(lx_business_error).
        " Log error and re-raise
        log_business_event( 
          iv_event_type = 'TRANSACTION_ERROR'
          ir_event_data = REF #( lx_business_error->get_text( ) )
        ).
        RAISE EXCEPTION lx_business_error.
    ENDTRY.
  ENDMETHOD.
  
  METHOD zif_auditable~get_created_by.
    rv_user = mv_created_by.
  ENDMETHOD.
  
  METHOD zif_auditable~get_created_at.
    rv_timestamp = mv_created_at.
  ENDMETHOD.
  
  METHOD zif_auditable~get_modified_by.
    rv_user = mv_modified_by.
  ENDMETHOD.
  
  METHOD zif_auditable~get_modified_at.
    rv_timestamp = mv_modified_at.
  ENDMETHOD.
  
  METHOD zif_persistable~is_dirty.
    rv_dirty = mv_is_dirty.
  ENDMETHOD.
  
  METHOD zif_entity~get_id.
    rv_id = mv_entity_id.
  ENDMETHOD.
  
  METHOD zif_entity~set_id.
    mv_entity_id = iv_id.
  ENDMETHOD.
  
  METHOD log_business_event.
    " Centralized business event logging
    DATA(lo_logger) = zcl_business_logger=>get_instance( ).
    lo_logger->log_event( 
      iv_entity_id = mv_entity_id
      iv_event_type = iv_event_type
      ir_event_data = ir_event_data
    ).
  ENDMETHOD.
  
  METHOD check_authorization.
    " Centralized authorization check
    AUTHORITY-CHECK OBJECT 'ZBUSINESS'
      ID 'ENTITY' FIELD mv_entity_id
      ID 'ACTVT' FIELD iv_activity.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_authorization_error
        EXPORTING activity = iv_activity entity_id = mv_entity_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete Implementation
*&---------------------------------------------------------------------*

CLASS zcl_sales_order DEFINITION INHERITING FROM zcl_abstract_business_entity.
  PUBLIC SECTION.
    " Constructor
    METHODS: constructor
               IMPORTING iv_order_id TYPE string
                         iv_customer_id TYPE string.
                         
    " Business methods
    METHODS: add_line_item
               IMPORTING is_item TYPE zsales_order_item
               RAISING zcx_business_error,
               
             calculate_total
               RETURNING VALUE(rv_total) TYPE dmbtr,
               
             submit_for_approval
               RAISING zcx_business_error.
               
  PROTECTED SECTION.
    " Template method implementations
    METHODS: before_transaction_processing REDEFINITION,
             execute_transaction_logic REDEFINITION,
             after_transaction_processing REDEFINITION.
             
  PRIVATE SECTION.
    DATA: mv_order_id     TYPE string,
          mv_customer_id  TYPE string,
          mt_line_items   TYPE ztt_sales_order_items,
          mv_status       TYPE zsales_order_status,
          mv_total_amount TYPE dmbtr.
          
    METHODS: validate_line_item
               IMPORTING is_item TYPE zsales_order_item
               RAISING zcx_invalid_item,
               
             recalculate_totals.
ENDCLASS.

CLASS zcl_sales_order IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_order_id = iv_order_id.
    mv_customer_id = iv_customer_id.
    mv_status = 'NEW'.
    zif_entity~set_id( iv_order_id ).
  ENDMETHOD.
  
  METHOD before_transaction_processing.
    " Pre-transaction validation
    check_authorization( 'CHANGE' ).
    
    " Validate business state
    IF mv_status = 'APPROVED' OR mv_status = 'CLOSED'.
      RAISE EXCEPTION TYPE zcx_business_error
        EXPORTING message = 'Cannot modify approved or closed orders'.
    ENDIF.
  ENDMETHOD.
  
  METHOD execute_transaction_logic.
    " Core business logic is delegated to specific methods
    " This is called from the template method
  ENDMETHOD.
  
  METHOD after_transaction_processing.
    " Post-transaction processing
    recalculate_totals( ).
    
    " Trigger business events
    log_business_event( 
      iv_event_type = 'ORDER_MODIFIED'
      ir_event_data = REF #( mv_order_id )
    ).
  ENDMETHOD.
  
  METHOD add_line_item.
    " Use template method for transaction processing
    process_business_transaction( is_item ).
    
    " Actual line item addition logic
    validate_line_item( is_item ).
    APPEND is_item TO mt_line_items.
  ENDMETHOD.
  
  METHOD zif_business_entity~save.
    " Implementation of save logic
    DATA(lo_repository) = NEW zcl_sales_order_repository( ).
    
    TRY.
        lo_repository->save( me ).
        mv_is_dirty = abap_false.
        
      CATCH zcx_repository_error INTO DATA(lx_repo_error).
        RAISE EXCEPTION TYPE zcx_persistence_error
          EXPORTING previous = lx_repo_error.
    ENDTRY.
  ENDMETHOD.
  
  METHOD zif_business_entity~validate.
    " Comprehensive validation
    IF mv_customer_id IS INITIAL.
      APPEND VALUE #( 
        field = 'CUSTOMER_ID'
        message = 'Customer ID is required'
        severity = 'E'
      ) TO rt_errors.
    ENDIF.
    
    IF lines( mt_line_items ) = 0.
      APPEND VALUE #( 
        field = 'LINE_ITEMS'
        message = 'At least one line item is required'
        severity = 'E'
      ) TO rt_errors.
    ENDIF.
    
    " Validate each line item
    LOOP AT mt_line_items INTO DATA(ls_item).
      TRY.
          validate_line_item( ls_item ).
        CATCH zcx_invalid_item INTO DATA(lx_invalid_item).
          APPEND VALUE #( 
            field = |LINE_ITEM_{ sy-tabix }|
            message = lx_invalid_item->get_text( )
            severity = 'E'
          ) TO rt_errors.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

This advanced Object-Oriented ABAP module demonstrates enterprise-level OOP patterns, sophisticated class hierarchies, and design patterns commonly used in large-scale SAP applications. The examples show proper abstraction, encapsulation, and polymorphism principles applied to real business scenarios.

---

**Next Module**: [Module 13: Web Services & Interfaces](Module_13_Web_Services_Interfaces.md)