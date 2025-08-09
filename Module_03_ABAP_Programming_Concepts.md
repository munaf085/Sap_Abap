# Module 3: ABAP Programming Concepts

## 🎯 Learning Objectives
By the end of this module, you will:
- Master ABAP data types and variable declarations
- Understand control structures and program flow
- Work with operators and expressions
- Handle strings, dates, and numbers effectively
- Create robust programs with proper error handling

---

## 📖 Table of Contents
1. [Data Types and Declarations](#data-types-and-declarations)
2. [Variables and Constants](#variables-and-constants)
3. [Control Structures](#control-structures)
4. [Operators and Expressions](#operators-and-expressions)
5. [String Operations](#string-operations)
6. [Date and Time Handling](#date-and-time-handling)
7. [Error Handling](#error-handling)
8. [Practice Exercises](#practice-exercises)

---

## 1. Data Types and Declarations

### ABAP Data Types Hierarchy

#### **Elementary Types (Built-in)**
| Type | Length | Description | Example |
|------|--------|-------------|---------|
| **C** | 1-65535 | Character/Text | 'Hello' |
| **N** | 1-65535 | Numeric string | '12345' |
| **D** | 8 | Date (YYYYMMDD) | '20240115' |
| **T** | 6 | Time (HHMMSS) | '143000' |
| **I** | 4 | Integer | 42 |
| **P** | 1-16 | Packed decimal | 123.45 |
| **F** | 8 | Floating point | 3.14159 |
| **X** | 1-65535 | Hexadecimal | 'A1B2' |
| **STRING** | Variable | Dynamic string | 'Dynamic text' |
| **XSTRING** | Variable | Dynamic hex | Binary data |

#### **Complete Types (DDIC-based)**
```abap
" Using Data Dictionary elements
DATA: customer_id TYPE kunnr,
      amount      TYPE dmbtr,
      currency    TYPE waers.
```

#### **Reference Types**
```abap
" Object references
DATA: lo_customer TYPE REF TO zcl_customer.

" Data references
DATA: lr_data TYPE REF TO data.
```

### Type Declaration Syntax

#### **DATA Statement**
```abap
" Basic declaration
DATA: lv_name TYPE string,
      lv_age  TYPE i.

" With initial values
DATA: lv_count TYPE i VALUE 0,
      lv_text  TYPE string VALUE 'Initial'.

" Multiple declarations
DATA: BEGIN OF ls_customer,
        id   TYPE kunnr,
        name TYPE string,
        city TYPE string,
      END OF ls_customer.
```

#### **TYPES Statement**
```abap
" Define custom types
TYPES: ty_customer_id TYPE kunnr,
       ty_amount      TYPE p DECIMALS 2.

" Structure type
TYPES: BEGIN OF ty_address,
         street  TYPE string,
         city    TYPE string,
         country TYPE string,
       END OF ty_address.

" Table type
TYPES: tt_customers TYPE TABLE OF zcustomer.
```

---

## 2. Variables and Constants

### Variable Naming Conventions

#### **Prefixes for Variables**
| Prefix | Description | Example |
|--------|-------------|---------|
| **lv_** | Local Variable | lv_counter |
| **gv_** | Global Variable | gv_company_code |
| **ls_** | Local Structure | ls_customer |
| **gs_** | Global Structure | gs_user_info |
| **lt_** | Local Table | lt_orders |
| **gt_** | Global Table | gt_materials |
| **lr_** | Local Reference | lr_data_ref |
| **gr_** | Global Reference | gr_object_ref |

#### **Examples**
```abap
" Variables
DATA: lv_customer_name TYPE string,
      lv_order_amount  TYPE p DECIMALS 2,
      lv_process_date  TYPE dats.

" Structures
DATA: ls_customer TYPE zcustomer_str,
      ls_address  TYPE zaddress_str.

" Internal tables
DATA: lt_customers TYPE TABLE OF zcustomer,
      lt_orders    TYPE TABLE OF zorder.
```

### Constants

#### **Defining Constants**
```abap
" Simple constants
CONSTANTS: c_max_items TYPE i VALUE 100,
           c_company   TYPE string VALUE 'ACME Corp',
           c_version   TYPE string VALUE '1.0.0'.

" Complex constants
CONSTANTS: BEGIN OF c_status,
             active   TYPE c VALUE 'A',
             inactive TYPE c VALUE 'I',
             pending  TYPE c VALUE 'P',
           END OF c_status.
```

### Field Symbols

#### **Dynamic Data Access**
```abap
" Field symbol declaration
FIELD-SYMBOLS: <lfs_any>      TYPE any,
               <lfs_customer> TYPE zcustomer.

" Assignment and usage
ASSIGN lv_variable TO <lfs_any>.
IF <lfs_any> IS ASSIGNED.
  WRITE: <lfs_any>.
ENDIF.
```

---

## 3. Control Structures

### Conditional Statements

#### **IF-ELSEIF-ELSE**
```abap
DATA: lv_score TYPE i VALUE 85.

IF lv_score >= 90.
  WRITE: 'Grade: A'.
ELSEIF lv_score >= 80.
  WRITE: 'Grade: B'.
ELSEIF lv_score >= 70.
  WRITE: 'Grade: C'.
ELSE.
  WRITE: 'Grade: F'.
ENDIF.
```

#### **CASE Statement**
```abap
DATA: lv_day TYPE i VALUE 3.

CASE lv_day.
  WHEN 1.
    WRITE: 'Monday'.
  WHEN 2.
    WRITE: 'Tuesday'.
  WHEN 3.
    WRITE: 'Wednesday'.
  WHEN 4 OR 5.
    WRITE: 'Thursday or Friday'.
  WHEN OTHERS.
    WRITE: 'Weekend'.
ENDCASE.
```

### Loop Structures

#### **DO Loop**
```abap
DATA: lv_counter TYPE i.

DO 5 TIMES.
  lv_counter = lv_counter + 1.
  WRITE: / 'Iteration:', lv_counter.
ENDDO.
```

#### **WHILE Loop**
```abap
DATA: lv_number TYPE i VALUE 1.

WHILE lv_number <= 10.
  WRITE: / 'Number:', lv_number.
  lv_number = lv_number + 1.
ENDWHILE.
```

#### **LOOP AT (Internal Tables)**
```abap
DATA: lt_numbers TYPE TABLE OF i,
      lv_number  TYPE i.

" Populate table
APPEND: 10 TO lt_numbers,
        20 TO lt_numbers,
        30 TO lt_numbers.

" Loop through table
LOOP AT lt_numbers INTO lv_number.
  WRITE: / 'Value:', lv_number.
ENDLOOP.
```

### Control Flow Statements

#### **EXIT and CONTINUE**
```abap
DO 10 TIMES.
  IF sy-index = 5.
    CONTINUE.  " Skip iteration 5
  ENDIF.
  
  IF sy-index = 8.
    EXIT.      " Exit loop at 8
  ENDIF.
  
  WRITE: / 'Index:', sy-index.
ENDDO.
```

#### **CHECK Statement**
```abap
LOOP AT lt_customers INTO ls_customer.
  CHECK ls_customer-status = 'A'.  " Process only active customers
  
  " Process customer
  WRITE: / ls_customer-name.
ENDLOOP.
```

---

## 4. Operators and Expressions

### Arithmetic Operators

#### **Basic Operations**
```abap
DATA: lv_a TYPE i VALUE 10,
      lv_b TYPE i VALUE 3,
      lv_result TYPE p DECIMALS 2.

" Addition
lv_result = lv_a + lv_b.     " Result: 13

" Subtraction
lv_result = lv_a - lv_b.     " Result: 7

" Multiplication
lv_result = lv_a * lv_b.     " Result: 30

" Division
lv_result = lv_a / lv_b.     " Result: 3.33

" Modulo
lv_result = lv_a MOD lv_b.   " Result: 1

" Power
lv_result = lv_a ** 2.       " Result: 100
```

#### **Mathematical Functions**
```abap
DATA: lv_value TYPE f VALUE '3.14159',
      lv_result TYPE f.

lv_result = abs( -5 ).        " Absolute value: 5
lv_result = sqrt( 16 ).       " Square root: 4
lv_result = sin( lv_value ).  " Sine
lv_result = cos( lv_value ).  " Cosine
lv_result = exp( 2 ).         " Exponential
lv_result = log( 10 ).        " Natural logarithm
```

### Comparison Operators

#### **Relational Operators**
```abap
IF lv_a = lv_b.       " Equal to
IF lv_a <> lv_b.      " Not equal to
IF lv_a > lv_b.       " Greater than
IF lv_a < lv_b.       " Less than
IF lv_a >= lv_b.      " Greater than or equal
IF lv_a <= lv_b.      " Less than or equal
```

#### **String Comparisons**
```abap
DATA: lv_text1 TYPE string VALUE 'Hello',
      lv_text2 TYPE string VALUE 'hello'.

IF lv_text1 = lv_text2.      " Case-sensitive
IF lv_text1 EQ lv_text2.     " Same as =

" Pattern matching
IF lv_text1 CP 'Hel*'.       " Contains pattern
IF lv_text1 CS 'ell'.        " Contains substring
IF lv_text1 NS 'xyz'.        " Does not contain
```

### Logical Operators

#### **Boolean Operations**
```abap
DATA: lv_condition1 TYPE abap_bool VALUE abap_true,
      lv_condition2 TYPE abap_bool VALUE abap_false.

" AND operation
IF lv_condition1 = abap_true AND lv_condition2 = abap_true.
  " Both conditions true
ENDIF.

" OR operation
IF lv_condition1 = abap_true OR lv_condition2 = abap_true.
  " At least one condition true
ENDIF.

" NOT operation
IF NOT lv_condition1 = abap_true.
  " Condition is false
ENDIF.
```

---

## 5. String Operations

### String Declaration and Assignment

#### **Static vs Dynamic Strings**
```abap
" Static string (fixed length)
DATA: lv_static_text TYPE c LENGTH 20 VALUE 'Fixed Length'.

" Dynamic string (variable length)
DATA: lv_dynamic_text TYPE string VALUE 'Variable Length'.
```

### String Manipulation Functions

#### **Basic Operations**
```abap
DATA: lv_text     TYPE string VALUE 'Hello World',
      lv_result   TYPE string,
      lv_length   TYPE i,
      lv_position TYPE i.

" Length
lv_length = strlen( lv_text ).          " Result: 11

" Uppercase/Lowercase
lv_result = to_upper( lv_text ).        " 'HELLO WORLD'
lv_result = to_lower( lv_text ).        " 'hello world'

" Find position
FIND 'World' IN lv_text MATCH OFFSET lv_position.  " Position: 6

" Replace
REPLACE 'World' IN lv_text WITH 'ABAP'. " 'Hello ABAP'

" Substring
lv_result = lv_text+0(5).               " 'Hello'
lv_result = lv_text+6(*).               " 'World'
```

#### **Advanced String Operations**
```abap
DATA: lv_source TYPE string VALUE '  Hello, World!  ',
      lv_target TYPE string.

" Trim whitespace
lv_target = |{ lv_source CASE = (cl_abap_format=>c_raw) WIDTH = 20 ALIGN = LEFT PAD = '0' }|.

" Concatenate strings
CONCATENATE 'Hello' 'World' INTO lv_target SEPARATED BY ' '.

" Split string
DATA: lt_parts TYPE TABLE OF string.
SPLIT lv_source AT ',' INTO TABLE lt_parts.

" String templates (ABAP 7.4+)
lv_target = |Hello { sy-uname }, today is { sy-datum DATE = USER }|.
```

### Regular Expressions

#### **Pattern Matching**
```abap
DATA: lv_text TYPE string VALUE 'Phone: +1-555-123-4567',
      lv_phone TYPE string.

" Extract phone number
FIND REGEX '\+?\d{1,3}-?\d{3}-?\d{3}-?\d{4}' 
     IN lv_text 
     SUBMATCHES lv_phone.

" Validate email format
DATA: lv_email TYPE string VALUE 'user@example.com',
      lv_valid TYPE abap_bool.

FIND REGEX '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$' 
     IN lv_email.
     
IF sy-subrc = 0.
  lv_valid = abap_true.
ENDIF.
```

---

## 6. Date and Time Handling

### Date Operations

#### **Date Declarations**
```abap
" Date variables
DATA: lv_date       TYPE dats,
      lv_time       TYPE tims,
      lv_timestamp  TYPE timestampl.

" Get current date/time
lv_date = sy-datum.
lv_time = sy-uzeit.
GET TIME STAMP FIELD lv_timestamp.
```

#### **Date Calculations**
```abap
DATA: lv_start_date TYPE dats VALUE '20240101',
      lv_end_date   TYPE dats,
      lv_days       TYPE i.

" Add days to date
lv_end_date = lv_start_date + 30.

" Calculate difference
lv_days = lv_end_date - lv_start_date.

" Date validation
CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
  EXPORTING
    date    = lv_start_date
  EXCEPTIONS
    OTHERS = 1.
    
IF sy-subrc <> 0.
  MESSAGE 'Invalid date' TYPE 'E'.
ENDIF.
```

#### **Advanced Date Functions**
```abap
" Get first/last day of month
CALL FUNCTION 'FIRST_DAY_OF_MONTH'
  EXPORTING
    i_date = sy-datum
  IMPORTING
    e_date = lv_first_day.

" Calculate workdays
CALL FUNCTION 'WORKDAYS_BETWEEN_TWO_DATES'
  EXPORTING
    i_date_from = lv_start_date
    i_date_to   = lv_end_date
  IMPORTING
    e_days      = lv_workdays.
```

### Time Zone Handling

#### **Convert Between Time Zones**
```abap
DATA: lv_utc_timestamp TYPE timestampl,
      lv_local_date    TYPE dats,
      lv_local_time    TYPE tims.

CONVERT TIME STAMP lv_utc_timestamp 
        TIME ZONE 'EST' 
        INTO DATE lv_local_date 
             TIME lv_local_time.
```

---

## 7. Error Handling

### Exception Handling

#### **TRY-CATCH-CLEANUP**
```abap
DATA: lv_dividend TYPE i VALUE 10,
      lv_divisor  TYPE i VALUE 0,
      lv_result   TYPE p DECIMALS 2.

TRY.
    lv_result = lv_dividend / lv_divisor.
    WRITE: 'Result:', lv_result.
    
  CATCH cx_sy_zerodivide INTO DATA(lx_zero_div).
    WRITE: 'Error: Division by zero -', lx_zero_div->get_text( ).
    
  CATCH cx_sy_arithmetic_error INTO DATA(lx_arith).
    WRITE: 'Arithmetic error:', lx_arith->get_text( ).
    
  CLEANUP.
    " Cleanup code here
    WRITE: 'Cleanup performed'.
ENDTRY.
```

#### **Custom Exceptions**
```abap
" Define custom exception class
CLASS zcx_custom_error DEFINITION
  INHERITING FROM cx_static_check.
  
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING message TYPE string.
    DATA: message TYPE string.
ENDCLASS.

" Use custom exception
TRY.
    IF lv_value < 0.
      RAISE EXCEPTION TYPE zcx_custom_error
        EXPORTING message = 'Negative values not allowed'.
    ENDIF.
    
  CATCH zcx_custom_error INTO DATA(lx_custom).
    WRITE: lx_custom->message.
ENDTRY.
```

### Assertion and Debugging

#### **ASSERT Statement**
```abap
DATA: lv_value TYPE i VALUE 5.

" Assert condition (raises exception if false)
ASSERT lv_value > 0.

" Conditional assertion
ASSERT ID zassert_group CONDITION lv_value <> 0.
```

#### **Debugging Aids**
```abap
" Set breakpoint
BREAK-POINT.

" Conditional breakpoint
IF lv_debug_mode = abap_true.
  BREAK-POINT.
ENDIF.

" Write to debug log
MESSAGE i001(z_custom) WITH 'Debug message' lv_value.
```

---

## 8. Practice Exercises

### Exercise 1: Data Type Exploration

#### **Objective**: Practice with different ABAP data types.

```abap
REPORT z_data_types_exercise.

DATA: lv_char    TYPE c LENGTH 10 VALUE 'ABAP',
      lv_number  TYPE i VALUE 42,
      lv_decimal TYPE p DECIMALS 2 VALUE '123.45',
      lv_date    TYPE d VALUE '20240115',
      lv_time    TYPE t VALUE '143000',
      lv_string  TYPE string VALUE 'Dynamic String'.

START-OF-SELECTION.
  WRITE: / 'Data Types Demonstration'.
  WRITE: / '========================='.
  WRITE: / 'Character:', lv_char.
  WRITE: / 'Integer:', lv_number.
  WRITE: / 'Decimal:', lv_decimal.
  WRITE: / 'Date:', lv_date DD/MM/YYYY.
  WRITE: / 'Time:', lv_time USING EDIT MASK '__:__:__'.
  WRITE: / 'String:', lv_string.
```

### Exercise 2: Control Structures

#### **Objective**: Create a program using various control structures.

```abap
REPORT z_control_structures.

DATA: lt_numbers TYPE TABLE OF i,
      lv_number  TYPE i,
      lv_sum     TYPE i,
      lv_count   TYPE i.

START-OF-SELECTION.
  " Populate table with numbers 1-10
  DO 10 TIMES.
    APPEND sy-index TO lt_numbers.
  ENDDO.
  
  " Process numbers
  LOOP AT lt_numbers INTO lv_number.
    " Skip even numbers
    IF lv_number MOD 2 = 0.
      CONTINUE.
    ENDIF.
    
    " Exit if greater than 7
    IF lv_number > 7.
      EXIT.
    ENDIF.
    
    lv_sum = lv_sum + lv_number.
    lv_count = lv_count + 1.
    
    WRITE: / 'Processing odd number:', lv_number.
  ENDLOOP.
  
  WRITE: / 'Sum of processed numbers:', lv_sum.
  WRITE: / 'Count of processed numbers:', lv_count.
```

### Exercise 3: String Manipulation

#### **Objective**: Practice string operations and formatting.

```abap
REPORT z_string_exercise.

DATA: lv_first_name TYPE string VALUE 'John',
      lv_last_name  TYPE string VALUE 'Doe',
      lv_full_name  TYPE string,
      lv_email      TYPE string,
      lv_length     TYPE i.

START-OF-SELECTION.
  " Concatenate names
  lv_full_name = |{ lv_first_name } { lv_last_name }|.
  
  " Create email
  lv_email = |{ to_lower( lv_first_name ) }.{ to_lower( lv_last_name ) }@company.com|.
  
  " Get length
  lv_length = strlen( lv_full_name ).
  
  " Display results
  WRITE: / 'String Manipulation Results'.
  WRITE: / '============================'.
  WRITE: / 'Full Name:', lv_full_name.
  WRITE: / 'Email:', lv_email.
  WRITE: / 'Name Length:', lv_length.
  WRITE: / 'Uppercase:', to_upper( lv_full_name ).
```

---

## 📝 Key Takeaways

1. **ABAP offers rich data types** - elementary, complete, and reference types
2. **Naming conventions** improve code readability and maintenance
3. **Control structures** provide flexible program flow management
4. **String operations** are powerful with built-in functions and templates
5. **Date/time handling** requires specific functions for accurate calculations
6. **Exception handling** with TRY-CATCH ensures robust applications

---

## 🔗 What's Next?

In **Module 4**, we'll explore:
- ABAP Dictionary (DDIC) objects
- Database table design
- Views and data elements
- Domain and search helps

---

**Previous Module**: [Module 2: ABAP Workbench & Development Environment](Module_02_ABAP_Workbench.md)  
**Next Module**: [Module 4: Data Dictionary (DDIC)](Module_04_Data_Dictionary.md)