# ABAP Fundamentals - Interview Q&A

## Basic Concepts and Syntax

### ⭐ Q1: What is ABAP and what does it stand for?
**Answer:** ABAP stands for "Advanced Business Application Programming." It is a high-level programming language created by SAP for developing applications on the SAP platform. ABAP is used to customize and extend SAP software, create reports, develop interfaces, and build business applications.

### ⭐ Q2: What are the key characteristics of ABAP?
**Answer:** 
- **Event-driven programming**: ABAP programs are based on events
- **Interpreted language**: Code is interpreted at runtime
- **SQL integration**: Built-in SQL support for database operations
- **Platform independence**: Runs on various operating systems
- **Strong typing**: Strict data type checking
- **Built-in database access**: Direct integration with SAP database

### ⭐ Q3: Explain the basic structure of an ABAP program.
**Answer:**
```abap
REPORT program_name.

" Global declarations
DATA: variable_name TYPE data_type.

" Event blocks
INITIALIZATION.
  " Code executed at program start

START-OF-SELECTION.
  " Main processing logic

END-OF-SELECTION.
  " Code executed after main processing
```

### ⭐ Q4: What are the different types of ABAP programs?
**Answer:**
1. **Reports (Type 1)**: For data analysis and output
2. **Module Pool (Type M)**: For transaction-based applications
3. **Function Groups (Type F)**: Container for function modules
4. **Class Pool (Type K)**: Container for global classes
5. **Interface Pool (Type J)**: Container for global interfaces
6. **Subroutine Pool (Type S)**: Container for external subroutines

## Data Types and Variables

### ⭐ Q5: What are the elementary data types in ABAP?
**Answer:**
```abap
" Character types
DATA: lv_char   TYPE c LENGTH 10,
      lv_string TYPE string,
      lv_numc   TYPE n LENGTH 8.

" Numeric types  
DATA: lv_int    TYPE i,
      lv_packed TYPE p DECIMALS 2,
      lv_float  TYPE f.

" Date and time
DATA: lv_date   TYPE d,
      lv_time   TYPE t.

" Other types
DATA: lv_hex    TYPE x LENGTH 4,
      lv_xstring TYPE xstring.
```

### ⭐⭐ Q6: What is the difference between DATA and CONSTANTS?
**Answer:**
```abap
" DATA - Variable, can be changed
DATA: lv_counter TYPE i VALUE 0.
lv_counter = lv_counter + 1. " Valid

" CONSTANTS - Cannot be changed after declaration
CONSTANTS: lc_max_records TYPE i VALUE 1000.
lc_max_records = 500. " Syntax error!

" PARAMETERS - Input fields on selection screen
PARAMETERS: p_input TYPE string.

" SELECT-OPTIONS - Range tables for selection screens
SELECT-OPTIONS: s_date FOR sy-datum.
```

### ⭐⭐ Q7: Explain field symbols and reference variables.
**Answer:**
```abap
" Field symbols - Like pointers, point to existing data
FIELD-SYMBOLS: <fs_any> TYPE any,
               <fs_table> TYPE STANDARD TABLE.

DATA: lv_text TYPE string VALUE 'Hello'.
ASSIGN lv_text TO <fs_any>.
<fs_any> = 'World'. " Changes lv_text to 'World'

" Reference variables - Object references
DATA: lr_ref TYPE REF TO data,
      lr_obj TYPE REF TO object.

CREATE DATA lr_ref TYPE string.
ASSIGN lr_ref->* TO <fs_any>.
```

## Control Structures

### ⭐ Q8: What are the different loop types in ABAP?
**Answer:**
```abap
" DO loop - Execute n times
DO 5 TIMES.
  WRITE: / 'Iteration:', sy-index.
ENDDO.

" WHILE loop - Condition-based
WHILE lv_counter < 10.
  lv_counter = lv_counter + 1.
ENDWHILE.

" LOOP for internal tables
LOOP AT lt_table INTO ls_structure.
  " Process each entry
ENDLOOP.

" LOOP with field symbols (better performance)
LOOP AT lt_table ASSIGNING <fs_structure>.
  " Direct access without copying
ENDLOOP.
```

### ⭐⭐ Q9: Explain conditional statements in ABAP.
**Answer:**
```abap
" IF-ELSEIF-ELSE
IF lv_grade >= 90.
  lv_result = 'A'.
ELSEIF lv_grade >= 80.
  lv_result = 'B'.
ELSEIF lv_grade >= 70.
  lv_result = 'C'.
ELSE.
  lv_result = 'F'.
ENDIF.

" CASE statement
CASE lv_status.
  WHEN 'A' OR 'B'.
    MESSAGE 'Active status' TYPE 'I'.
  WHEN 'C'.
    MESSAGE 'Inactive status' TYPE 'W'.
  WHEN OTHERS.
    MESSAGE 'Unknown status' TYPE 'E'.
ENDCASE.

" CHECK statement - Continue if condition is true
CHECK lv_amount > 0.

" EXIT - Leave current processing block
EXIT. 

" CONTINUE - Skip to next iteration
CONTINUE.
```

## Modularization

### ⭐⭐ Q10: What are the different modularization techniques in ABAP?
**Answer:**
1. **Subroutines (FORM routines)**:
```abap
PERFORM calculate_tax USING lv_amount CHANGING lv_tax.

FORM calculate_tax USING p_amount TYPE p
                   CHANGING p_tax TYPE p.
  p_tax = p_amount * '0.15'.
ENDFORM.
```

2. **Function Modules**:
```abap
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = lv_input
  IMPORTING
    output = lv_output.
```

3. **Methods (Object-Oriented)**:
```abap
DATA: lo_obj TYPE REF TO cl_my_class.
lo_obj->method_name( 
  EXPORTING iv_param = lv_value
  IMPORTING ev_result = lv_result ).
```

### ⭐⭐ Q11: What is the difference between USING and CHANGING parameters?
**Answer:**
```abap
FORM example_form USING    p_input  TYPE string     " Input only
                  CHANGING p_output TYPE string.    " Input/Output
  
  " p_input - Value passed in, cannot be modified
  " p_output - Value passed in and can be modified
  
  p_output = |Modified: { p_input }|.
ENDFORM.

" Call the form
DATA: lv_text TYPE string VALUE 'Original'.
PERFORM example_form USING 'Input' CHANGING lv_text.
" lv_text now contains 'Modified: Input'
```

## String Operations

### ⭐ Q12: How do you perform string operations in ABAP?
**Answer:**
```abap
DATA: lv_string1 TYPE string VALUE 'Hello',
      lv_string2 TYPE string VALUE 'World',
      lv_result  TYPE string,
      lv_length  TYPE i.

" Concatenation
CONCATENATE lv_string1 lv_string2 INTO lv_result 
            SEPARATED BY space.

" Modern syntax (7.40+)
lv_result = |{ lv_string1 } { lv_string2 }|.

" String functions
lv_length = strlen( lv_string1 ).

" String operations
SPLIT lv_result AT space INTO lv_string1 lv_string2.
REPLACE 'Hello' WITH 'Hi' INTO lv_result.
TRANSLATE lv_result TO UPPER CASE.

" Find and replace
FIND 'World' IN lv_result.
IF sy-subrc = 0.
  MESSAGE 'Found!' TYPE 'I'.
ENDIF.
```

## Error Handling

### ⭐⭐ Q13: How do you handle exceptions in ABAP?
**Answer:**
```abap
" Classical exception handling
CALL FUNCTION 'SOME_FUNCTION'
  EXPORTING
    input = lv_input
  IMPORTING  
    output = lv_output
  EXCEPTIONS
    error_occurred = 1
    invalid_input  = 2
    OTHERS        = 3.

CASE sy-subrc.
  WHEN 0.
    " Success
  WHEN 1.
    MESSAGE 'Error occurred' TYPE 'E'.
  WHEN 2.
    MESSAGE 'Invalid input' TYPE 'E'.
  WHEN OTHERS.
    MESSAGE 'Unknown error' TYPE 'E'.
ENDCASE.

" Modern exception handling (Class-based)
TRY.
    DATA(lv_result) = cl_some_class=>static_method( lv_input ).
  CATCH cx_some_exception INTO DATA(lx_exception).
    MESSAGE lx_exception->get_text( ) TYPE 'E'.
  CATCH cx_sy_conversion_error.
    MESSAGE 'Conversion error' TYPE 'E'.
ENDTRY.
```

## System Fields

### ⭐ Q14: What are important system fields in ABAP?
**Answer:**
```abap
" Important system fields (sy structure)
sy-subrc     " Return code of operations (0 = success)
sy-tabix     " Current line index in loops
sy-index     " Current iteration in DO loops
sy-datum     " Current system date
sy-uzeit     " Current system time
sy-uname     " Current username
sy-mandt     " Current client
sy-langu     " Current language
sy-tcode     " Current transaction code
sy-cprog     " Current program name
sy-repid     " Current report name

" Usage examples
IF sy-subrc = 0.
  MESSAGE |Success! Current user: { sy-uname }| TYPE 'I'.
ENDIF.

WRITE: / 'Today is:', sy-datum,
       / 'Current time:', sy-uzeit.
```

### ⭐⭐ Q15: What is the difference between sy-tabix and sy-index?
**Answer:**
```abap
" sy-index - Used in DO loops
DO 5 TIMES.
  WRITE: / 'DO iteration:', sy-index. " 1, 2, 3, 4, 5
ENDDO.

" sy-tabix - Current line number in table loops
LOOP AT lt_table INTO ls_entry.
  WRITE: / 'Table line:', sy-tabix. " Current row number
  IF sy-tabix = 3.
    EXIT. " Exit after 3rd record
  ENDIF.
ENDLOOP.

" sy-tabix is also set by table operations
READ TABLE lt_table INTO ls_entry INDEX 5.
WRITE: / 'Read line:', sy-tabix. " 5 if successful
```

## Memory Management

### ⭐⭐ Q16: Explain ABAP memory and SAP memory.
**Answer:**
```abap
" ABAP Memory - Session specific, accessible within same session
" Export to ABAP memory
EXPORT parameter_1 = lv_value1
       parameter_2 = lv_value2
       TO MEMORY ID 'MY_ID'.

" Import from ABAP memory  
IMPORT parameter_1 = lv_value1
       parameter_2 = lv_value2
       FROM MEMORY ID 'MY_ID'.

" SAP Memory - User specific, accessible across sessions
" Set parameter
SET PARAMETER ID 'MAT' FIELD lv_material.

" Get parameter
GET PARAMETER ID 'MAT' FIELD lv_material.

" Free memory
FREE MEMORY ID 'MY_ID'.
```

This concludes the ABAP Fundamentals Q&A section. These questions cover the essential concepts that form the foundation of ABAP programming and are commonly asked in entry to intermediate level interviews.