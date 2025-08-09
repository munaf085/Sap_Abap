# Module 12: Object-Oriented ABAP (OO-ABAP)

## 🎯 **Complete Guide to Object-Oriented Programming in ABAP**

**Learn OOP from basics to enterprise-level patterns - No prior OOP knowledge required!**

This module is structured as a progressive learning journey, broken down into digestible submodules. Each submodule builds upon the previous one, with plenty of hands-on examples.

---

## 📖 **Module Structure - Learning Path**

### **Phase 1: Foundation (Start Here)**
- **[12.1 OOP Fundamentals](#121-oop-fundamentals)** - What is OOP and why use it?
- **[12.2 Your First Class](#122-your-first-class)** - Creating your very first object
- **[12.3 Understanding Objects](#123-understanding-objects)** - Objects vs Classes explained simply

### **Phase 2: Core Concepts** 
- **[12.4 Methods & Attributes](#124-methods--attributes)** - The building blocks of classes
- **[12.5 Constructor Methods](#125-constructor-methods)** - Initializing objects properly
- **[12.6 Visibility & Encapsulation](#126-visibility--encapsulation)** - Protecting your data

### **Phase 3: Advanced OOP**
- **[12.7 Inheritance](#127-inheritance)** - Building on existing classes
- **[12.8 Interfaces](#128-interfaces)** - Contracts and standards
- **[12.9 Polymorphism](#129-polymorphism)** - One interface, multiple implementations

### **Phase 4: Professional Development**
- **[12.10 Exception Handling](#1210-exception-handling)** - Error management in OOP
- **[12.11 Design Patterns](#1211-design-patterns)** - Industry best practices
- **[12.12 Enterprise Applications](#1212-enterprise-applications)** - Real-world scenarios

---

## 🎯 **Learning Objectives**

**By the end of this module, you'll be able to:**
- ✅ Understand what OOP is and why it's useful
- ✅ Create and use classes and objects confidently
- ✅ Apply inheritance to build efficient code hierarchies
- ✅ Use interfaces to create flexible, maintainable code
- ✅ Handle errors professionally using exception classes
- ✅ Apply common design patterns in real projects
- ✅ Build enterprise-level ABAP applications using OOP principles

---

## 12.1 OOP Fundamentals

### 🤔 **What is Object-Oriented Programming? (Explained Simply)**

Think of Object-Oriented Programming (OOP) like organizing a **digital office**:

#### **Traditional Programming = Messy Office**
```
📁 All files scattered on desk
📋 No organization system
🔍 Hard to find anything
😵 Everything mixed together
⏰ Takes forever to get work done
```

#### **Object-Oriented Programming = Organized Office**
```
🗃️ Files organized in labeled folders
📋 Each folder has specific purpose
🔍 Easy to find what you need
😊 Everything has its place
⚡ Get work done efficiently
```

### **Real-World Example: Bank Account**

Let's understand OOP by thinking about a bank account:

#### **What is a Bank Account?** (This is a CLASS)
- It's a **template** that defines what all bank accounts have
- **Properties**: Account number, balance, owner name
- **Actions**: Deposit money, withdraw money, check balance

#### **Your Personal Account** (This is an OBJECT)
- **Your account number**: 123456789
- **Your balance**: $1,500
- **Your name**: John Smith
- **You can**: Deposit, withdraw, check balance

```abap
" In ABAP, this looks like:

" CLASS = The template/blueprint
CLASS zcl_bank_account DEFINITION.
  " What every bank account has
  PRIVATE SECTION.
    DATA: account_number TYPE string,
          balance TYPE p DECIMALS 2,
          owner_name TYPE string.
  
  PUBLIC SECTION.
    " What every bank account can do
    METHODS: deposit IMPORTING amount TYPE p DECIMALS 2,
             withdraw IMPORTING amount TYPE p DECIMALS 2,
             get_balance RETURNING VALUE(rv_balance) TYPE p DECIMALS 2.
ENDCLASS.

" OBJECT = Your actual account
DATA(lo_my_account) = NEW zcl_bank_account( ).
lo_my_account->deposit( 1500 ).  " Put $1500 in your account
lo_my_account->withdraw( 200 ).  " Take out $200
```

---

## 12.2 Your First Class

### 🏗️ **Let's Build Your First Class - Step by Step!**

We'll create a simple `Person` class that represents a person with a name and age.

#### **Step 1: Understanding the Structure**
Every ABAP class has two parts:
1. **DEFINITION** - What the class can have and do
2. **IMPLEMENTATION** - How it actually works

#### **Step 2: Create the Definition**

```abap
*&---------------------------------------------------------------------*
*& Your First ABAP Class: Person
*&---------------------------------------------------------------------*

CLASS zcl_person DEFINITION.
  
  PUBLIC SECTION.
    " What others can see and use
    METHODS: 
      " Constructor - runs when person is created
      constructor IMPORTING iv_name TYPE string
                           iv_age TYPE i,
      
      " Actions the person can do
      introduce,  " Person introduces themselves
      have_birthday,  " Person gets older
      
      " Information others can get
      get_name RETURNING VALUE(rv_name) TYPE string,
      get_age RETURNING VALUE(rv_age) TYPE i.
      
  PRIVATE SECTION.
    " Internal information - hidden from outside
    DATA: mv_name TYPE string,  " Person's name
          mv_age TYPE i.        " Person's age
          
ENDCLASS.
```

#### **Step 3: Create the Implementation**

```abap
CLASS zcl_person IMPLEMENTATION.

  METHOD constructor.
    " This runs when a new person is created
    mv_name = iv_name.
    mv_age = iv_age.
    
    WRITE: |Creating person: { mv_name }, Age: { mv_age }|.
  ENDMETHOD.

  METHOD introduce.
    " Person introduces themselves
    WRITE: |Hello! My name is { mv_name } and I am { mv_age } years old.|.
  ENDMETHOD.

  METHOD have_birthday.
    " Person gets one year older
    mv_age = mv_age + 1.
    WRITE: |🎉 Happy Birthday { mv_name }! You are now { mv_age } years old.|.
  ENDMETHOD.

  METHOD get_name.
    " Return the person's name
    rv_name = mv_name.
  ENDMETHOD.

  METHOD get_age.
    " Return the person's age
    rv_age = mv_age.
  ENDMETHOD.

ENDCLASS.
```

#### **Step 4: Using Your Class**

```abap
REPORT z_test_person.

" Include the class definition and implementation here
" (or create it in SE80 as a class)

START-OF-SELECTION.

  " Create people (objects)
  DATA(lo_john) = NEW zcl_person( iv_name = 'John' iv_age = 25 ).
  DATA(lo_mary) = NEW zcl_person( iv_name = 'Mary' iv_age = 30 ).
  
  " Make them introduce themselves
  lo_john->introduce( ).
  lo_mary->introduce( ).
  
  " It's John's birthday!
  lo_john->have_birthday( ).
  
  " Check their ages
  WRITE: / |John is now { lo_john->get_age( ) } years old|.
  WRITE: / |Mary is still { lo_mary->get_age( ) } years old|.
```

#### **What You'll See:**
```
Creating person: John, Age: 25
Creating person: Mary, Age: 30
Hello! My name is John and I am 25 years old.
Hello! My name is Mary and I am 30 years old.
🎉 Happy Birthday John! You are now 26 years old.
John is now 26 years old
Mary is still 30 years old
```

### **🎯 Key Points to Remember:**
- **CLASS** = Blueprint (like a cookie cutter)
- **OBJECT** = Instance (like an actual cookie)
- **METHODS** = What objects can do (actions)
- **ATTRIBUTES** = What objects have (properties)
- **PUBLIC** = Others can see and use
- **PRIVATE** = Hidden inside the class

---

## 12.3 Understanding Objects

### 🧠 **Objects vs Classes - Crystal Clear Explanation**

#### **The Cookie Analogy**
```
🍪 COOKIE CUTTER (Class)
├── Shape: Round
├── Size: 3 inches
├── Instructions: "Cut dough into cookies"
└── One cutter can make many cookies

🍪🍪🍪 ACTUAL COOKIES (Objects)
├── Cookie #1: Chocolate chip
├── Cookie #2: Sugar cookie  
├── Cookie #3: Oatmeal raisin
└── Each cookie is unique but made from same cutter
```

#### **In ABAP Terms:**
```abap
" CLASS = The cookie cutter (template)
CLASS zcl_cookie DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_flavor TYPE string,
             get_flavor RETURNING VALUE(rv_flavor) TYPE string.
  PRIVATE SECTION.
    DATA: mv_flavor TYPE string.
ENDCLASS.

" OBJECTS = Individual cookies
DATA(lo_chocolate_cookie) = NEW zcl_cookie( 'Chocolate Chip' ).
DATA(lo_sugar_cookie) = NEW zcl_cookie( 'Sugar' ).
DATA(lo_oatmeal_cookie) = NEW zcl_cookie( 'Oatmeal Raisin' ).

" Each object is independent
WRITE: lo_chocolate_cookie->get_flavor( ).  " Shows: Chocolate Chip
WRITE: lo_sugar_cookie->get_flavor( ).      " Shows: Sugar
```

### **Why This Matters in Business**

#### **Real Business Example: Employee Management**

Instead of having scattered employee data:
```abap
" ❌ Traditional approach - messy!
DATA: emp1_name TYPE string VALUE 'John',
      emp1_salary TYPE p VALUE 50000,
      emp1_dept TYPE string VALUE 'IT',
      emp2_name TYPE string VALUE 'Mary',
      emp2_salary TYPE p VALUE 60000,
      emp2_dept TYPE string VALUE 'Finance'.
      " ... gets messy with more employees!
```

Use objects for clean organization:
```abap
" ✅ Object-oriented approach - clean!
DATA(lo_john) = NEW zcl_employee( iv_name = 'John' 
                                  iv_salary = 50000 
                                  iv_dept = 'IT' ).
                                  
DATA(lo_mary) = NEW zcl_employee( iv_name = 'Mary' 
                                  iv_salary = 60000 
                                  iv_dept = 'Finance' ).

" Easy to work with
lo_john->give_raise( 5000 ).
lo_mary->transfer_department( 'Management' ).
```

---

## 12.4 Methods & Attributes

### 🔧 **Understanding Methods - The Actions Objects Can Do**

Think of methods as **skills** that objects have. Just like people have different skills (cooking, driving, singing), objects have different methods they can perform.

#### **Types of Methods**

| **Method Type** | **Purpose** | **Real-World Example** |
|----------------|-------------|------------------------|
| **Constructor** | Initialize object when created | Birth certificate creation |
| **Getter** | Get information from object | Asking someone their name |
| **Setter** | Change object's information | Updating your address |
| **Action** | Make object do something | Asking someone to dance |

#### **Simple Calculator Class Example**

```abap
CLASS zcl_calculator DEFINITION.
  
  PUBLIC SECTION.
    " Constructor method
    METHODS: constructor,
    
    " Action methods
    METHODS: add IMPORTING iv_num1 TYPE i 
                          iv_num2 TYPE i
                 RETURNING VALUE(rv_result) TYPE i,
                 
             subtract IMPORTING iv_num1 TYPE i 
                               iv_num2 TYPE i
                      RETURNING VALUE(rv_result) TYPE i,
                      
             get_last_result RETURNING VALUE(rv_result) TYPE i.
             
  PRIVATE SECTION.
    DATA: mv_last_result TYPE i.
    
ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.

  METHOD constructor.
    mv_last_result = 0.
    WRITE: |Calculator ready to use!|.
  ENDMETHOD.

  METHOD add.
    rv_result = iv_num1 + iv_num2.
    mv_last_result = rv_result.
    WRITE: |{ iv_num1 } + { iv_num2 } = { rv_result }|.
  ENDMETHOD.

  METHOD subtract.
    rv_result = iv_num1 - iv_num2.
    mv_last_result = rv_result.
    WRITE: |{ iv_num1 } - { iv_num2 } = { rv_result }|.
  ENDMETHOD.

  METHOD get_last_result.
    rv_result = mv_last_result.
  ENDMETHOD.

ENDCLASS.
```

#### **Using the Calculator**

```abap
REPORT z_test_calculator.

START-OF-SELECTION.

  " Create calculator object
  DATA(lo_calc) = NEW zcl_calculator( ).
  
  " Use the calculator
  DATA(lv_result1) = lo_calc->add( iv_num1 = 10 iv_num2 = 5 ).
  DATA(lv_result2) = lo_calc->subtract( iv_num1 = 20 iv_num2 = 8 ).
  
  " Get last result
  WRITE: / |Last calculation result: { lo_calc->get_last_result( ) }|.
```

### 🏷️ **Understanding Attributes - The Properties Objects Have**

Attributes are like **characteristics** of objects. Think of them as the information that makes each object unique.

#### **Types of Attributes**

| **Attribute Type** | **Visibility** | **Description** | **Example** |
|-------------------|----------------|-----------------|-------------|
| **Instance Attributes** | Private/Public | Each object has its own copy | Person's name, age |
| **Static Attributes** | Private/Public | Shared by all objects of the class | Company name, tax rate |
| **Constants** | Public | Never change | PI = 3.14159 |

#### **Student Class with Different Attribute Types**

```abap
CLASS zcl_student DEFINITION.
  
  PUBLIC SECTION.
    " Constructor
    METHODS: constructor IMPORTING iv_name TYPE string
                                  iv_grade TYPE i,
                                  
    " Public methods
    METHODS: get_info RETURNING VALUE(rv_info) TYPE string,
             study IMPORTING iv_hours TYPE i,
             get_total_students RETURNING VALUE(rv_count) TYPE i.
             
    " Public constant
    CONSTANTS: c_passing_grade TYPE i VALUE 60.
    
  PRIVATE SECTION.
    " Instance attributes (each student has their own)
    DATA: mv_name TYPE string,
          mv_grade TYPE i,
          mv_study_hours TYPE i.
          
    " Static attribute (shared by all students)
    CLASS-DATA: gv_total_students TYPE i.
    
ENDCLASS.

CLASS zcl_student IMPLEMENTATION.

  METHOD constructor.
    mv_name = iv_name.
    mv_grade = iv_grade.
    mv_study_hours = 0.
    
    " Increment total students count
    gv_total_students = gv_total_students + 1.
    
    WRITE: |Student { mv_name } enrolled. Total students: { gv_total_students }|.
  ENDMETHOD.

  METHOD get_info.
    rv_info = |Name: { mv_name }, Grade: { mv_grade }, Study Hours: { mv_study_hours }|.
  ENDMETHOD.

  METHOD study.
    mv_study_hours = mv_study_hours + iv_hours.
    WRITE: |{ mv_name } studied for { iv_hours } hours. Total: { mv_study_hours }|.
  ENDMETHOD.

  METHOD get_total_students.
    rv_count = gv_total_students.
  ENDMETHOD.

ENDCLASS.
```

#### **Testing the Student Class**

```abap
START-OF-SELECTION.

  " Create students
  DATA(lo_john) = NEW zcl_student( iv_name = 'John' iv_grade = 85 ).
  DATA(lo_mary) = NEW zcl_student( iv_name = 'Mary' iv_grade = 92 ).
  DATA(lo_bob) = NEW zcl_student( iv_name = 'Bob' iv_grade = 78 ).
  
  " Students study different amounts
  lo_john->study( 3 ).
  lo_mary->study( 5 ).
  lo_bob->study( 2 ).
  
  " Display information
  WRITE: / lo_john->get_info( ).
  WRITE: / lo_mary->get_info( ).
  WRITE: / lo_bob->get_info( ).
  
  " Check passing grade constant
  WRITE: / |Passing grade is: { zcl_student=>c_passing_grade }|.
  
  " Check total students
  WRITE: / |Total students enrolled: { lo_john->get_total_students( ) }|.
```

---

## 12.5 Constructor Methods

### 🏗️ **Constructors - Setting Up Your Objects Properly**

A constructor is like **setting up a new employee** on their first day of work. You give them their name tag, desk, computer, and tell them the basic rules.

#### **Why Do We Need Constructors?**

```abap
" ❌ Without constructor - objects start empty
DATA(lo_person) = NEW zcl_person( ).
" Now lo_person has no name, no age - basically useless!

" ✅ With constructor - objects start ready to use
DATA(lo_person) = NEW zcl_person( iv_name = 'John' iv_age = 25 ).
" Now lo_person is fully set up and ready to work!
```

#### **Simple Bank Account with Constructor**

```abap
CLASS zcl_bank_account DEFINITION.
  
  PUBLIC SECTION.
    " Constructor with required information
    METHODS: constructor IMPORTING iv_account_number TYPE string
                                  iv_owner_name TYPE string
                                  iv_initial_balance TYPE p DECIMALS 2 DEFAULT 0,
                                  
             " Account operations
             deposit IMPORTING iv_amount TYPE p DECIMALS 2,
             withdraw IMPORTING iv_amount TYPE p DECIMALS 2
                     RETURNING VALUE(rv_success) TYPE abap_bool,
             get_balance RETURNING VALUE(rv_balance) TYPE p DECIMALS 2,
             get_account_info RETURNING VALUE(rv_info) TYPE string.
             
  PRIVATE SECTION.
    DATA: mv_account_number TYPE string,
          mv_owner_name TYPE string,
          mv_balance TYPE p DECIMALS 2,
          mv_account_created TYPE dats.
          
ENDCLASS.

CLASS zcl_bank_account IMPLEMENTATION.

  METHOD constructor.
    " Set up the account
    mv_account_number = iv_account_number.
    mv_owner_name = iv_owner_name.
    mv_balance = iv_initial_balance.
    mv_account_created = sy-datum.
    
    " Welcome message
    WRITE: |🏦 Account { mv_account_number } created for { mv_owner_name }|.
    WRITE: |   Initial balance: ${ mv_balance }|.
    WRITE: |   Created on: { mv_account_created }|.
  ENDMETHOD.

  METHOD deposit.
    mv_balance = mv_balance + iv_amount.
    WRITE: |💰 Deposited ${ iv_amount }. New balance: ${ mv_balance }|.
  ENDMETHOD.

  METHOD withdraw.
    IF iv_amount <= mv_balance.
      mv_balance = mv_balance - iv_amount.
      rv_success = abap_true.
      WRITE: |💸 Withdrew ${ iv_amount }. New balance: ${ mv_balance }|.
    ELSE.
      rv_success = abap_false.
      WRITE: |❌ Insufficient funds! Cannot withdraw ${ iv_amount }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_balance.
    rv_balance = mv_balance.
  ENDMETHOD.

  METHOD get_account_info.
    rv_info = |Account: { mv_account_number }, Owner: { mv_owner_name }, Balance: ${ mv_balance }|.
  ENDMETHOD.

ENDCLASS.
```

#### **Using the Bank Account**

```abap
START-OF-SELECTION.

  " Create accounts with different initial balances
  DATA(lo_john_account) = NEW zcl_bank_account( 
    iv_account_number = '12345'
    iv_owner_name = 'John Smith'
    iv_initial_balance = '1000.00' 
  ).
  
  DATA(lo_mary_account) = NEW zcl_bank_account( 
    iv_account_number = '67890'
    iv_owner_name = 'Mary Johnson'
    " No initial balance specified - defaults to 0
  ).
  
  " Test operations
  lo_john_account->deposit( '500.00' ).
  lo_john_account->withdraw( '200.00' ).
  
  lo_mary_account->deposit( '300.00' ).
  lo_mary_account->withdraw( '400.00' ).  " This should fail
  
  " Display final information
  WRITE: / lo_john_account->get_account_info( ).
  WRITE: / lo_mary_account->get_account_info( ).
```

### **🎯 Constructor Best Practices**

#### **1. Always Validate Input**
```abap
METHOD constructor.
  " Validate account number
  IF iv_account_number IS INITIAL.
    RAISE EXCEPTION TYPE zcx_invalid_input
      EXPORTING message = 'Account number cannot be empty'.
  ENDIF.
  
  " Validate initial balance
  IF iv_initial_balance < 0.
    RAISE EXCEPTION TYPE zcx_invalid_input
      EXPORTING message = 'Initial balance cannot be negative'.
  ENDIF.
  
  " If validation passes, set up the object
  mv_account_number = iv_account_number.
  mv_balance = iv_initial_balance.
ENDMETHOD.
```

#### **2. Use Default Parameters**
```abap
" Good: Provide sensible defaults
METHODS: constructor IMPORTING iv_name TYPE string
                              iv_age TYPE i DEFAULT 18
                              iv_country TYPE string DEFAULT 'USA'.
```

#### **3. Initialize All Attributes**
```abap
METHOD constructor.
  " Initialize everything, even if not provided
  mv_name = iv_name.
  mv_age = iv_age.
  mv_created_date = sy-datum.
  mv_last_login = '00000000'.  " Initialize to empty date
  mv_is_active = abap_true.    " Default to active
ENDMETHOD.
```

---

## 12.10 Exception Handling

### ⚠️ **Handling Errors Like a Pro**

Exception handling in OOP is like having a **safety net** when things go wrong. Instead of your program crashing, you handle problems gracefully.

#### **Simple Exception Example**

```abap
" Custom exception class
CLASS zcx_bank_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_message TYPE string.
  PRIVATE SECTION.
    DATA: mv_message TYPE string.
ENDCLASS.

CLASS zcx_bank_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_message = iv_message.
  ENDMETHOD.
ENDCLASS.

" Bank account with proper error handling
CLASS zcl_safe_bank_account DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_balance TYPE p DECIMALS 2,
             withdraw IMPORTING iv_amount TYPE p DECIMALS 2
                     RAISING zcx_bank_error.
  PRIVATE SECTION.
    DATA: mv_balance TYPE p DECIMALS 2.
ENDCLASS.

CLASS zcl_safe_bank_account IMPLEMENTATION.
  METHOD constructor.
    mv_balance = iv_balance.
  ENDMETHOD.

  METHOD withdraw.
    IF iv_amount <= 0.
      RAISE EXCEPTION TYPE zcx_bank_error
        EXPORTING iv_message = 'Cannot withdraw negative or zero amount'.
    ENDIF.
    
    IF iv_amount > mv_balance.
      RAISE EXCEPTION TYPE zcx_bank_error
        EXPORTING iv_message = 'Insufficient funds'.
    ENDIF.
    
    mv_balance = mv_balance - iv_amount.
    WRITE: |✅ Withdrew ${ iv_amount }. Balance: ${ mv_balance }|.
  ENDMETHOD.
ENDCLASS.

" Using exception handling
START-OF-SELECTION.
  DATA(lo_account) = NEW zcl_safe_bank_account( 1000 ).
  
  TRY.
    lo_account->withdraw( 500 ).   " This works
    lo_account->withdraw( 600 ).   " This fails - handled gracefully
    
  CATCH zcx_bank_error INTO DATA(lo_error).
    WRITE: |❌ Error: { lo_error->get_text( ) }|.
  ENDTRY.
```

---

## 12.11 Design Patterns

### 🏭 **Common Solutions to Common Problems**

Design patterns are like **recipes** - proven solutions to problems that happen often in programming.

#### **Singleton Pattern - One and Only One**

```abap
" Ensures only one instance of a class exists
CLASS zcl_application_logger DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_logger) TYPE REF TO zcl_application_logger.
    METHODS: log_message IMPORTING iv_message TYPE string.
    
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO zcl_application_logger.
    METHODS: constructor.
ENDCLASS.

CLASS zcl_application_logger IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW zcl_application_logger( ).
    ENDIF.
    ro_logger = go_instance.
  ENDMETHOD.

  METHOD constructor.
    WRITE: |📝 Logger initialized|.
  ENDMETHOD.

  METHOD log_message.
    WRITE: |📝 LOG: { sy-datum } { sy-uzeit } - { iv_message }|.
  ENDMETHOD.
ENDCLASS.

" Usage
DATA(lo_logger1) = zcl_application_logger=>get_instance( ).
DATA(lo_logger2) = zcl_application_logger=>get_instance( ).
" lo_logger1 and lo_logger2 are the SAME object!
```

---

## 12.12 Enterprise Applications

### 🏢 **Real-World Business Applications**

Here's how you'd use OOP in actual SAP business scenarios:

#### **Sales Order Processing System**

```abap
" Sales order with full OOP design
CLASS zcl_sales_order DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_customer_id TYPE string,
             add_item IMPORTING iv_material TYPE string
                               iv_quantity TYPE i
                               iv_price TYPE p DECIMALS 2,
             calculate_total RETURNING VALUE(rv_total) TYPE p DECIMALS 2,
             process_order RETURNING VALUE(rv_success) TYPE abap_bool.
             
  PRIVATE SECTION.
    DATA: mv_customer_id TYPE string,
          mt_items TYPE TABLE OF zorder_item,
          mv_status TYPE string.
ENDCLASS.

" This demonstrates enterprise-level OOP patterns:
" - Encapsulation (private data)
" - Clear interfaces (public methods)  
" - Business logic separation
" - Error handling
" - Maintainable code structure
```

### **🎯 Congratulations! You've Completed OOP in ABAP!**

You now understand:
- ✅ What OOP is and why it's useful
- ✅ How to create classes and objects
- ✅ Methods, attributes, and constructors
- ✅ Encapsulation and data protection
- ✅ Inheritance for code reuse
- ✅ Interfaces for flexible design
- ✅ Polymorphism for powerful solutions
- ✅ Exception handling for robust code
- ✅ Design patterns for professional development
- ✅ Enterprise application patterns

You're now ready to build professional, maintainable ABAP applications using object-oriented principles!

---

## 12.Advanced Enterprise Examples

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