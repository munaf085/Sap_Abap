# Object-Oriented ABAP - Interview Q&A

## Classes and Objects

### ⭐⭐ Q1: What is Object-Oriented Programming and its benefits in ABAP?
**Answer:** Object-Oriented Programming (OOP) is a programming paradigm based on objects that contain data (attributes) and code (methods).

**Benefits in ABAP:**
- **Encapsulation**: Data hiding and controlled access
- **Inheritance**: Code reusability through parent-child relationships
- **Polymorphism**: Same interface, different implementations
- **Abstraction**: Simplified interfaces hiding complex implementations
- **Modularity**: Better code organization and maintenance

**OOP vs Procedural:**
```abap
" Procedural approach
PERFORM calculate_tax USING lv_amount CHANGING lv_tax.

" Object-oriented approach
DATA(lo_calculator) = NEW cl_tax_calculator( ).
DATA(lv_tax) = lo_calculator->calculate_tax( lv_amount ).
```

### ⭐⭐ Q2: How do you define a class in ABAP?
**Answer:**
```abap
" Global class (SE80/ADT) - Class CL_CALCULATOR
CLASS cl_calculator DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_result,
             value  TYPE p DECIMALS 2,
             status TYPE string,
           END OF ty_result.
    
    METHODS: constructor IMPORTING iv_precision TYPE i DEFAULT 2,
             add IMPORTING iv_num1 TYPE p
                          iv_num2 TYPE p
                 RETURNING VALUE(rv_result) TYPE p,
             get_precision RETURNING VALUE(rv_precision) TYPE i.
             
  PROTECTED SECTION.
    DATA: mv_precision TYPE i.
    
  PRIVATE SECTION.
    METHODS: validate_input IMPORTING iv_number TYPE p
                           RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS cl_calculator IMPLEMENTATION.
  METHOD constructor.
    mv_precision = iv_precision.
  ENDMETHOD.
  
  METHOD add.
    IF validate_input( iv_num1 ) = abap_true AND 
       validate_input( iv_num2 ) = abap_true.
      rv_result = iv_num1 + iv_num2.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_precision.
    rv_precision = mv_precision.
  ENDMETHOD.
  
  METHOD validate_input.
    rv_valid = COND #( WHEN iv_number IS NOT INITIAL THEN abap_true
                      ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.
```

### ⭐⭐ Q3: Explain visibility sections in ABAP classes.
**Answer:**
```abap
CLASS cl_example DEFINITION.
  PUBLIC SECTION.
    " Accessible from anywhere
    DATA: mv_public_var TYPE string.
    METHODS: public_method.
    
  PROTECTED SECTION.
    " Accessible within class and subclasses
    DATA: mv_protected_var TYPE string.
    METHODS: protected_method.
    
  PRIVATE SECTION.
    " Accessible only within the same class
    DATA: mv_private_var TYPE string.
    METHODS: private_method.
ENDCLASS.

" Usage
DATA(lo_obj) = NEW cl_example( ).
lo_obj->public_method( ).        " ✓ Allowed
lo_obj->mv_public_var = 'test'.  " ✓ Allowed
" lo_obj->protected_method( ).   " ✗ Not allowed
" lo_obj->private_method( ).     " ✗ Not allowed
```

### ⭐⭐ Q4: What are static and instance components?
**Answer:**
```abap
CLASS cl_counter DEFINITION.
  PUBLIC SECTION.
    " Static components - belong to class, shared by all instances
    CLASS-DATA: cv_total_instances TYPE i.
    CLASS-METHODS: get_total_instances RETURNING VALUE(rv_count) TYPE i,
                   class_constructor.
    
    " Instance components - belong to specific object instance
    DATA: mv_instance_id TYPE i.
    METHODS: constructor,
             get_instance_id RETURNING VALUE(rv_id) TYPE i.
ENDCLASS.

CLASS cl_counter IMPLEMENTATION.
  METHOD class_constructor.
    " Called once when class is first accessed
    cv_total_instances = 0.
  ENDMETHOD.
  
  METHOD constructor.
    " Called for each new instance
    cv_total_instances = cv_total_instances + 1.
    mv_instance_id = cv_total_instances.
  ENDMETHOD.
  
  METHOD get_total_instances.
    rv_count = cv_total_instances.
  ENDMETHOD.
  
  METHOD get_instance_id.
    rv_id = mv_instance_id.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_counter1) = NEW cl_counter( ).  " cv_total_instances = 1
DATA(lo_counter2) = NEW cl_counter( ).  " cv_total_instances = 2

WRITE: / 'Total instances:', cl_counter=>get_total_instances( ). " 2
WRITE: / 'Instance 1 ID:', lo_counter1->get_instance_id( ).     " 1
WRITE: / 'Instance 2 ID:', lo_counter2->get_instance_id( ).     " 2
```

## Inheritance

### ⭐⭐ Q5: How does inheritance work in ABAP?
**Answer:** Inheritance allows a class to inherit attributes and methods from another class.

```abap
" Parent class
CLASS cl_vehicle DEFINITION.
  PUBLIC SECTION.
    DATA: mv_brand TYPE string,
          mv_model TYPE string.
    METHODS: constructor IMPORTING iv_brand TYPE string
                                  iv_model TYPE string,
             start_engine,
             get_info RETURNING VALUE(rv_info) TYPE string.
  PROTECTED SECTION.
    DATA: mv_engine_status TYPE string.
ENDCLASS.

CLASS cl_vehicle IMPLEMENTATION.
  METHOD constructor.
    mv_brand = iv_brand.
    mv_model = iv_model.
    mv_engine_status = 'OFF'.
  ENDMETHOD.
  
  METHOD start_engine.
    mv_engine_status = 'ON'.
    MESSAGE |Engine started for { mv_brand } { mv_model }| TYPE 'I'.
  ENDMETHOD.
  
  METHOD get_info.
    rv_info = |{ mv_brand } { mv_model } - Engine: { mv_engine_status }|.
  ENDMETHOD.
ENDCLASS.

" Child class
CLASS cl_car DEFINITION INHERITING FROM cl_vehicle.
  PUBLIC SECTION.
    DATA: mv_doors TYPE i.
    METHODS: constructor IMPORTING iv_brand TYPE string
                                  iv_model TYPE string
                                  iv_doors TYPE i,
             open_trunk.
    " Override parent method
    METHODS: get_info REDEFINITION.
ENDCLASS.

CLASS cl_car IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_brand = iv_brand iv_model = iv_model ).
    mv_doors = iv_doors.
  ENDMETHOD.
  
  METHOD open_trunk.
    MESSAGE 'Trunk opened' TYPE 'I'.
  ENDMETHOD.
  
  METHOD get_info.
    DATA(lv_parent_info) = super->get_info( ).
    rv_info = |{ lv_parent_info } - Doors: { mv_doors }|.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_car) = NEW cl_car( iv_brand = 'Toyota' 
                          iv_model = 'Camry' 
                          iv_doors = 4 ).
lo_car->start_engine( ).  " Inherited method
lo_car->open_trunk( ).    " Own method
WRITE: / lo_car->get_info( ). " Redefined method
```

### ⭐⭐⭐ Q6: What is method redefinition vs method overloading?
**Answer:**

**Method Redefinition**: Changing the implementation of an inherited method
```abap
CLASS cl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS: display.
ENDCLASS.

CLASS cl_child DEFINITION INHERITING FROM cl_parent.
  PUBLIC SECTION.
    " Redefining parent method - same signature
    METHODS: display REDEFINITION.
ENDCLASS.

CLASS cl_child IMPLEMENTATION.
  METHOD display.
    " New implementation
    WRITE: 'Child display method'.
  ENDMETHOD.
ENDCLASS.
```

**Method Overloading**: Multiple methods with same name but different signatures (Not directly supported in ABAP, use optional parameters)
```abap
CLASS cl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS: add IMPORTING iv_num1 TYPE i
                          iv_num2 TYPE i OPTIONAL
                          iv_num3 TYPE i OPTIONAL
                 RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS cl_calculator IMPLEMENTATION.
  METHOD add.
    rv_result = iv_num1.
    IF iv_num2 IS SUPPLIED.
      rv_result = rv_result + iv_num2.
    ENDIF.
    IF iv_num3 IS SUPPLIED.
      rv_result = rv_result + iv_num3.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_calc) = NEW cl_calculator( ).
DATA(lv_result1) = lo_calc->add( iv_num1 = 5 ).
DATA(lv_result2) = lo_calc->add( iv_num1 = 5 iv_num2 = 3 ).
DATA(lv_result3) = lo_calc->add( iv_num1 = 5 iv_num2 = 3 iv_num3 = 2 ).
```

## Interfaces

### ⭐⭐ Q7: What are interfaces and why use them?
**Answer:** Interfaces define a contract that implementing classes must follow. They contain only method declarations, no implementation.

**Benefits:**
- Multiple inheritance (class can implement multiple interfaces)
- Loose coupling between components
- Standardized method signatures
- Better testability and maintainability

```abap
" Interface definition
INTERFACE if_drawable.
  METHODS: draw,
           get_coordinates RETURNING VALUE(rv_coords) TYPE string.
ENDINTERFACE.

INTERFACE if_movable.
  METHODS: move IMPORTING iv_x TYPE i iv_y TYPE i,
           get_position RETURNING VALUE(rv_position) TYPE string.
ENDINTERFACE.

" Class implementing multiple interfaces
CLASS cl_shape DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_drawable,
                if_movable.
  PRIVATE SECTION.
    DATA: mv_x TYPE i,
          mv_y TYPE i.
ENDCLASS.

CLASS cl_shape IMPLEMENTATION.
  METHOD if_drawable~draw.
    WRITE: / 'Drawing shape at', mv_x, mv_y.
  ENDMETHOD.
  
  METHOD if_drawable~get_coordinates.
    rv_coords = |X: { mv_x }, Y: { mv_y }|.
  ENDMETHOD.
  
  METHOD if_movable~move.
    mv_x = iv_x.
    mv_y = iv_y.
  ENDMETHOD.
  
  METHOD if_movable~get_position.
    rv_position = |Position: { mv_x }, { mv_y }|.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_shape) = NEW cl_shape( ).
lo_shape->if_movable~move( iv_x = 10 iv_y = 20 ).
lo_shape->if_drawable~draw( ).

" Interface reference
DATA: lr_drawable TYPE REF TO if_drawable.
lr_drawable = lo_shape.
lr_drawable->draw( ).
```

### ⭐⭐⭐ Q8: Explain abstract classes and when to use them.
**Answer:** Abstract classes cannot be instantiated directly and may contain abstract methods that must be implemented by subclasses.

```abap
" Abstract class
CLASS cl_abstract_animal DEFINITION ABSTRACT.
  PUBLIC SECTION.
    DATA: mv_name TYPE string.
    METHODS: constructor IMPORTING iv_name TYPE string,
             " Concrete method
             eat,
             " Abstract method - must be implemented by subclasses
             make_sound ABSTRACT.
ENDCLASS.

CLASS cl_abstract_animal IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  
  METHOD eat.
    WRITE: / mv_name, 'is eating'.
  ENDMETHOD.
ENDCLASS.

" Concrete subclass
CLASS cl_dog DEFINITION INHERITING FROM cl_abstract_animal.
  PUBLIC SECTION.
    METHODS: make_sound REDEFINITION.
ENDCLASS.

CLASS cl_dog IMPLEMENTATION.
  METHOD make_sound.
    WRITE: / mv_name, 'says Woof!'.
  ENDMETHOD.
ENDCLASS.

" Usage
" DATA(lo_animal) = NEW cl_abstract_animal( ). " ✗ Error - cannot instantiate
DATA(lo_dog) = NEW cl_dog( iv_name = 'Buddy' ).
lo_dog->eat( ).
lo_dog->make_sound( ).

" Use cases for abstract classes:
" 1. Common functionality with some methods that must be specialized
" 2. Enforcing implementation of critical methods
" 3. Template method pattern
```

## Polymorphism and Design Patterns

### ⭐⭐⭐ Q9: Demonstrate polymorphism in ABAP.
**Answer:** Polymorphism allows objects of different types to be treated uniformly through a common interface.

```abap
" Common interface
INTERFACE if_payment_processor.
  METHODS: process_payment IMPORTING iv_amount TYPE p
                          RETURNING VALUE(rv_success) TYPE abap_bool,
           get_fee_percentage RETURNING VALUE(rv_fee) TYPE p.
ENDINTERFACE.

" Different payment implementations
CLASS cl_credit_card DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_payment_processor.
ENDCLASS.

CLASS cl_credit_card IMPLEMENTATION.
  METHOD if_payment_processor~process_payment.
    WRITE: / 'Processing credit card payment:', iv_amount.
    rv_success = abap_true.
  ENDMETHOD.
  
  METHOD if_payment_processor~get_fee_percentage.
    rv_fee = '2.5'.
  ENDMETHOD.
ENDCLASS.

CLASS cl_paypal DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_payment_processor.
ENDCLASS.

CLASS cl_paypal IMPLEMENTATION.
  METHOD if_payment_processor~process_payment.
    WRITE: / 'Processing PayPal payment:', iv_amount.
    rv_success = abap_true.
  ENDMETHOD.
  
  METHOD if_payment_processor~get_fee_percentage.
    rv_fee = '3.0'.
  ENDMETHOD.
ENDCLASS.

" Payment manager using polymorphism
CLASS cl_payment_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: process_order IMPORTING ir_processor TYPE REF TO if_payment_processor
                                   iv_amount TYPE p.
ENDCLASS.

CLASS cl_payment_manager IMPLEMENTATION.
  METHOD process_order.
    DATA(lv_fee) = ir_processor->get_fee_percentage( ).
    DATA(lv_total) = iv_amount + ( iv_amount * lv_fee / 100 ).
    
    IF ir_processor->process_payment( lv_total ) = abap_true.
      WRITE: / 'Order processed successfully. Total:', lv_total.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Usage - Same interface, different behaviors
DATA: lo_manager TYPE REF TO cl_payment_manager,
      lo_cc      TYPE REF TO cl_credit_card,
      lo_paypal  TYPE REF TO cl_paypal.

lo_manager = NEW #( ).
lo_cc = NEW #( ).
lo_paypal = NEW #( ).

" Polymorphic behavior
lo_manager->process_order( ir_processor = lo_cc iv_amount = 100 ).
lo_manager->process_order( ir_processor = lo_paypal iv_amount = 100 ).
```

### ⭐⭐⭐ Q10: Implement the Singleton design pattern in ABAP.
**Answer:**
```abap
CLASS cl_singleton DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO cl_singleton.
    METHODS: get_config_value IMPORTING iv_key TYPE string
                             RETURNING VALUE(rv_value) TYPE string,
             set_config_value IMPORTING iv_key TYPE string
                                       iv_value TYPE string.
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO cl_singleton.
    DATA: mt_config TYPE TABLE OF zconfig_table.
    METHODS: constructor.
ENDCLASS.

CLASS cl_singleton IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW cl_singleton( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.
  
  METHOD constructor.
    " Private constructor - load configuration
    SELECT * FROM zconfig_table INTO TABLE mt_config.
  ENDMETHOD.
  
  METHOD get_config_value.
    READ TABLE mt_config INTO DATA(ls_config) WITH KEY key = iv_key.
    IF sy-subrc = 0.
      rv_value = ls_config-value.
    ENDIF.
  ENDMETHOD.
  
  METHOD set_config_value.
    READ TABLE mt_config ASSIGNING FIELD-SYMBOL(<ls_config>) WITH KEY key = iv_key.
    IF sy-subrc = 0.
      <ls_config>-value = iv_value.
    ELSE.
      APPEND VALUE #( key = iv_key value = iv_value ) TO mt_config.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_config1) = cl_singleton=>get_instance( ).
DATA(lo_config2) = cl_singleton=>get_instance( ).

" lo_config1 and lo_config2 point to the same instance
lo_config1->set_config_value( iv_key = 'APP_NAME' iv_value = 'MyApp' ).
DATA(lv_value) = lo_config2->get_config_value( iv_key = 'APP_NAME' ).
WRITE: / lv_value. " Outputs: MyApp
```

### ⭐⭐⭐ Q11: Implement the Factory design pattern.
**Answer:**
```abap
" Product interface
INTERFACE if_document.
  METHODS: create_content RETURNING VALUE(rv_content) TYPE string,
           save_document IMPORTING iv_path TYPE string.
ENDINTERFACE.

" Concrete products
CLASS cl_pdf_document DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_document.
ENDCLASS.

CLASS cl_pdf_document IMPLEMENTATION.
  METHOD if_document~create_content.
    rv_content = 'PDF document content'.
  ENDMETHOD.
  
  METHOD if_document~save_document.
    WRITE: / 'Saving PDF to:', iv_path.
  ENDMETHOD.
ENDCLASS.

CLASS cl_word_document DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_document.
ENDCLASS.

CLASS cl_word_document IMPLEMENTATION.
  METHOD if_document~create_content.
    rv_content = 'Word document content'.
  ENDMETHOD.
  
  METHOD if_document~save_document.
    WRITE: / 'Saving Word document to:', iv_path.
  ENDMETHOD.
ENDCLASS.

" Factory class
CLASS cl_document_factory DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM te_document_type,
             pdf,
             word,
             excel,
           END OF ENUM te_document_type.
           
    CLASS-METHODS: create_document 
      IMPORTING iv_type TYPE te_document_type
      RETURNING VALUE(ro_document) TYPE REF TO if_document
      RAISING cx_invalid_parameter.
ENDCLASS.

CLASS cl_document_factory IMPLEMENTATION.
  METHOD create_document.
    CASE iv_type.
      WHEN pdf.
        ro_document = NEW cl_pdf_document( ).
      WHEN word.
        ro_document = NEW cl_word_document( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_invalid_parameter.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

" Usage
TRY.
    DATA(lo_doc) = cl_document_factory=>create_document( cl_document_factory=>pdf ).
    DATA(lv_content) = lo_doc->create_content( ).
    lo_doc->save_document( '/tmp/document.pdf' ).
  CATCH cx_invalid_parameter.
    MESSAGE 'Invalid document type' TYPE 'E'.
ENDTRY.
```

## Exception Handling

### ⭐⭐ Q12: How do you handle exceptions in object-oriented ABAP?
**Answer:**
```abap
" Custom exception class
CLASS cx_calculation_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF divide_by_zero,
                 msgid TYPE symsgid VALUE 'ZCALC',
                 msgno TYPE symsgno VALUE '001',
                 attr1 TYPE scx_attrname VALUE 'DIVIDEND',
               END OF divide_by_zero.
    DATA: dividend TYPE i.
    METHODS: constructor IMPORTING textid TYPE sotr_conc OPTIONAL
                                  previous TYPE REF TO cx_root OPTIONAL
                                  dividend TYPE i OPTIONAL.
ENDCLASS.

CLASS cx_calculation_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    me->dividend = dividend.
  ENDMETHOD.
ENDCLASS.

" Class using exceptions
CLASS cl_advanced_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS: divide IMPORTING iv_dividend TYPE i
                             iv_divisor TYPE i
                    RETURNING VALUE(rv_result) TYPE p
                    RAISING cx_calculation_error.
ENDCLASS.

CLASS cl_advanced_calculator IMPLEMENTATION.
  METHOD divide.
    IF iv_divisor = 0.
      RAISE EXCEPTION TYPE cx_calculation_error
        EXPORTING
          textid = cx_calculation_error=>divide_by_zero
          dividend = iv_dividend.
    ENDIF.
    rv_result = iv_dividend / iv_divisor.
  ENDMETHOD.
ENDCLASS.

" Exception handling
DATA(lo_calc) = NEW cl_advanced_calculator( ).

TRY.
    DATA(lv_result) = lo_calc->divide( iv_dividend = 10 iv_divisor = 0 ).
  CATCH cx_calculation_error INTO DATA(lx_error).
    WRITE: / 'Error:', lx_error->get_text( ).
    WRITE: / 'Dividend was:', lx_error->dividend.
ENDTRY.
```

This comprehensive Object-Oriented ABAP Q&A covers the essential OOP concepts that are crucial for advanced ABAP interviews. The examples demonstrate practical implementations that show deep understanding of OOP principles.