# Module 3: ABAP Programming Concepts - From Beginner to Confident

## 🎯 **What You'll Learn (In Simple Terms)**

**Building on Module 1 & 2!** Now that you know what SAP and ABAP are, let's learn the building blocks of programming. Think of this as learning the "vocabulary and grammar" of the ABAP language.

**By the end, you'll understand:**
- How to store different types of information (like names, numbers, dates)
- How to make decisions in your programs (if this, then do that)
- How to repeat actions (loops)
- How to work with text and numbers
- How to handle mistakes gracefully

**🌟 Don't worry - we'll explain everything with real-world examples!**

---

## 📖 **Table of Contents**
1. [📦 Data Types - Different Boxes for Different Things](#-data-types---different-boxes-for-different-things)
2. [📋 Variables - Storing Information](#-variables---storing-information)
3. [🔄 Making Decisions - If This, Then That](#-making-decisions---if-this-then-that)
4. [🔁 Repeating Actions - Loops](#-repeating-actions---loops)
5. [📝 Working with Text](#-working-with-text)
6. [📅 Working with Dates and Time](#-working-with-dates-and-time)
7. [🛡️ Handling Mistakes](#️-handling-mistakes)
8. [🎯 Practice Time](#-practice-time)

---

## 📦 **Data Types - Different Boxes for Different Things**

### **Think of Data Types Like Storage Boxes**

Imagine you're organizing your room. You use different containers for different things:
- 📚 **Bookshelf** for books
- 👔 **Closet** for clothes  
- 💎 **Jewelry box** for valuables
- 🧮 **Calculator** for numbers

**ABAP data types work the same way** - different "containers" for different kinds of information!

### **The Most Important Data Types (Explained Simply)**

#### **📝 Text Data (For Words and Sentences)**

**1. Character (C) - Like a Text Box**
```abap
" For storing names, addresses, descriptions
DATA: customer_name TYPE c LENGTH 30 VALUE 'John Smith'.
DATA: city          TYPE c LENGTH 20 VALUE 'New York'.
DATA: country       TYPE c LENGTH 15 VALUE 'USA'.

" Think of LENGTH like deciding how big your text box is
" LENGTH 30 = Can hold up to 30 letters/characters
```

**Real-World Example:**
```abap
DATA: first_name TYPE c LENGTH 20 VALUE 'Alice'.
DATA: last_name  TYPE c LENGTH 20 VALUE 'Johnson'.

WRITE: 'Customer:', first_name, last_name.
" Output: Customer: Alice Johnson
```

**2. String - Like a Rubber Text Box (Grows as Needed)**
```abap
" For text that can be any length
DATA: long_description TYPE string.
long_description = 'This is a very long description that can be as long as I want it to be without worrying about running out of space!'.

WRITE: long_description.
```

#### **🔢 Number Data (For Math and Calculations)**

**1. Integer (I) - Whole Numbers Only**
```abap
" For counting things (no decimal points)
DATA: age           TYPE i VALUE 25.
DATA: quantity      TYPE i VALUE 100.
DATA: total_items   TYPE i VALUE 5.

" Perfect for: ages, quantities, counts
WRITE: 'Age:', age.
WRITE: / 'Items:', total_items.
```

**2. Packed Decimal (P) - For Money and Precise Numbers**
```abap
" For money, prices, percentages (with decimal points)
DATA: price        TYPE p DECIMALS 2 VALUE '199.99'.
DATA: tax_rate     TYPE p DECIMALS 3 VALUE '8.375'.
DATA: total_amount TYPE p DECIMALS 2.

total_amount = price + ( price * tax_rate / 100 ).

WRITE: 'Price: $', price.
WRITE: / 'Total with tax: $', total_amount.
```

**Why DECIMALS 2?** Because money usually has 2 decimal places ($19.99)
**Why DECIMALS 3?** Because tax rates can be 8.375%

#### **📅 Date and Time Data**

**1. Date (DATS) - For Storing Dates**
```abap
" Always in format YYYYMMDD (Year-Month-Day)
DATA: birth_date    TYPE dats VALUE '19900515'.  " May 15, 1990
DATA: order_date    TYPE dats VALUE '20241215'.  " December 15, 2024
DATA: today         TYPE dats.

today = sy-datum.  " Get today's date

WRITE: 'Born on:', birth_date.
WRITE: / 'Today is:', today.
```

**2. Time (TIMS) - For Storing Time**
```abap
" Always in format HHMMSS (Hour-Minute-Second)
DATA: meeting_time  TYPE tims VALUE '143000'.  " 2:30:00 PM
DATA: current_time  TYPE tims.

current_time = sy-uzeit.  " Get current time

WRITE: 'Meeting at:', meeting_time.
WRITE: / 'Current time:', current_time.
```

### **Simple Data Type Comparison**

| **What You Want to Store** | **Use This Type** | **Example** |
|----------------------------|------------------|-------------|
| 👤 Person's name | `c LENGTH 30` | 'John Smith' |
| 📄 Long text/description | `string` | 'Very long text...' |
| 👶 Person's age | `i` | 25 |
| 💰 Price or money | `p DECIMALS 2` | 199.99 |
| 📅 Birthday | `dats` | '19900515' |
| ⏰ Appointment time | `tims` | '143000' |
| ✅ True/False choice | `abap_bool` | abap_true |

### **How to Choose the Right Data Type**

#### **Ask Yourself These Questions:**

1. **"What am I storing?"**
   - Names/text → Use `c` or `string`
   - Whole numbers → Use `i`
   - Money/decimals → Use `p DECIMALS 2`
   - Dates → Use `dats`
   - Times → Use `tims`

2. **"How long can it be?"**
   - Short text (name) → `c LENGTH 30`
   - Medium text (address) → `c LENGTH 100`
   - Very long text → `string`

3. **"Does it need decimal points?"**
   - No decimals (age, quantity) → `i`
   - With decimals (price, percentage) → `p DECIMALS 2`

### **Real-World Example: Student Information**

```abap
*&---------------------------------------------------------------------*
*& Example: Complete student information with right data types
*&---------------------------------------------------------------------*

" Student personal info
DATA: student_id    TYPE c LENGTH 8  VALUE 'STU00001'.
DATA: first_name    TYPE c LENGTH 20 VALUE 'Emma'.
DATA: last_name     TYPE c LENGTH 20 VALUE 'Wilson'.
DATA: birth_date    TYPE dats        VALUE '20000312'.  " March 12, 2000

" Academic info
DATA: enrollment_date TYPE dats      VALUE '20220901'.  " Sept 1, 2022
DATA: current_year    TYPE i         VALUE 3.           " 3rd year
DATA: gpa            TYPE p DECIMALS 2 VALUE '3.75'.    " 3.75 GPA

" Financial info
DATA: tuition_fee    TYPE p DECIMALS 2 VALUE '15000.00'.
DATA: scholarship    TYPE p DECIMALS 2 VALUE '5000.00'.
DATA: balance_due    TYPE p DECIMALS 2.

" Contact info
DATA: email          TYPE string     VALUE 'emma.wilson@university.edu'.
DATA: phone          TYPE c LENGTH 15 VALUE '+1-555-123-4567'.

" Calculate balance
balance_due = tuition_fee - scholarship.

" Display information
WRITE: / 'STUDENT INFORMATION'.
WRITE: / '=================='.
WRITE: / 'ID:', student_id.
WRITE: / 'Name:', first_name, last_name.
WRITE: / 'Born:', birth_date.
WRITE: / 'Year:', current_year.
WRITE: / 'GPA:', gpa.
WRITE: / 'Balance Due: $', balance_due.
WRITE: / 'Email:', email.
```

### **Common Beginner Mistakes (And How to Fix Them)**

#### **❌ Mistake 1: Wrong data type for numbers**
```abap
" WRONG - Using text for numbers
DATA: age TYPE c LENGTH 2 VALUE '25'.
DATA: new_age TYPE c LENGTH 2.
new_age = age + 1.  " This won't work correctly!
```

**✅ Fix: Use integer for whole numbers**
```abap
" CORRECT - Using integer for numbers
DATA: age TYPE i VALUE 25.
DATA: new_age TYPE i.
new_age = age + 1.  " Now this works: 26
```

#### **❌ Mistake 2: Text too long for container**
```abap
" WRONG - Text longer than LENGTH
DATA: name TYPE c LENGTH 10 VALUE 'Alexander Hamilton'.  " Too long!
```

**✅ Fix: Make container bigger or use string**
```abap
" CORRECT - Bigger container
DATA: name TYPE c LENGTH 30 VALUE 'Alexander Hamilton'.  " Fits!

" OR use string (grows automatically)
DATA: name TYPE string VALUE 'Alexander Hamilton'.  " Always fits!
```

#### **❌ Mistake 3: Wrong date format**
```abap
" WRONG - Wrong date format
DATA: birth_date TYPE dats VALUE '05/15/1990'.  " Wrong format!
```

**✅ Fix: Use YYYYMMDD format**
```abap
" CORRECT - ABAP date format
DATA: birth_date TYPE dats VALUE '19900515'.  " Correct!
```

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

#### **🐛 Complete ABAP Debugging Arsenal**

##### **1. Breakpoint Types**

**Hard-Coded Breakpoints**
```abap
" Unconditional breakpoint
BREAK-POINT.

" Conditional breakpoint
IF lv_debug_flag = 'X'.
  BREAK-POINT.
ENDIF.

" Breakpoint with user check
BREAK sy-uname.  " Only breaks for specific user
```

**External Breakpoints (SE80/ADT)**
- **Set in Editor**: Double-click line number
- **Session Breakpoints**: Valid for current session only
- **User Breakpoints**: Valid for specific user
- **HTTP Breakpoints**: For web-based debugging

##### **2. Debugging Commands & Shortcuts**

| **Key** | **Action** | **Description** |
|---------|------------|-----------------|
| **F5** | Single Step | Execute current line |
| **F6** | Execute | Execute current statement block |
| **F7** | Return | Return from subroutine/method |
| **F8** | Continue | Continue to next breakpoint |
| **Shift+F12** | Terminate | Stop debugging session |

##### **3. Debug Techniques by Scenario**

**🔍 Variable Inspection**
```abap
" In debugger, use these techniques:
" 1. Variables tab - see all variables in scope
" 2. Watchpoints - monitor specific variable changes
" 3. Change variable values during debug session

" Example: Inspect internal table
DATA: lt_customers TYPE TABLE OF kna1.
SELECT * FROM kna1 INTO TABLE lt_customers UP TO 10 ROWS.

BREAK-POINT.  " Check lt_customers content here
```

**🔍 SQL Statement Debugging**
```abap
" Debug database operations
SELECT single * FROM mara INTO DATA(ls_material)
  WHERE matnr = 'TEST123'.

BREAK-POINT.  " Check ls_material and sy-subrc

" Use SQL Trace (ST05) for detailed DB analysis:
" 1. Go to ST05
" 2. Turn on SQL trace
" 3. Execute your program
" 4. Turn off trace and analyze
```

**🔍 Exception Debugging**
```abap
TRY.
    DATA(lv_result) = 10 / 0.
  CATCH cx_sy_zerodivide INTO DATA(lx_error).
    BREAK-POINT.  " Debug exception details
    WRITE: lx_error->get_text( ).
ENDTRY.
```

##### **4. Advanced Debugging Features**

**Watchpoints (Variable Monitoring)**
```abap
" Set watchpoint in debugger:
" 1. Go to Breakpoints tab
" 2. Click Watchpoint
" 3. Enter variable name and condition
" Example: gv_counter > 100
```

**Conditional Breakpoints**
```abap
" Method 1: In code
IF sy-uname = 'DEVELOPER' AND lv_test_mode = 'X'.
  BREAK-POINT.
ENDIF.

" Method 2: In debugger
" Set condition directly on breakpoint in debugger UI
```

##### **5. Professional Debugging Tools**

**Runtime Analysis (SAT)**
```abap
" Transaction: SAT
" Measure performance while debugging
" 1. Go to SAT
" 2. Enter program name
" 3. Execute with measurement
" 4. Analyze runtime statistics
```

**SQL Trace (ST05)**
```abap
" Analyze database calls
" 1. ST05 → Turn on SQL trace
" 2. Execute program
" 3. Turn off trace
" 4. Display trace → Analyze DB performance
```

##### **6. Debug Production Issues Safely**

**Safe Production Debugging**
```abap
" Method 1: Use logging instead of breakpoints
" Create log entries for investigation

" Method 2: Conditional debugging
IF sy-mandt = '100' AND sy-uname = 'DEVELOPER'.
  BREAK-POINT.
ENDIF.

" Method 3: Use application log
MESSAGE i001(z_custom) WITH 'Debug info:' lv_variable.
```

##### **7. Best Practices**

**DO's:**
- ✅ **Use meaningful variable names** for easier debugging
- ✅ **Remove breakpoints** before transport
- ✅ **Use conditional breakpoints** to avoid unnecessary stops
- ✅ **Check sy-subrc** after database operations
- ✅ **Use logging** for production debugging

**DON'Ts:**
- ❌ **Don't leave BREAK-POINT** in production code
- ❌ **Don't debug in production** without proper authorization
- ❌ **Don't modify data** during debugging sessions
- ❌ **Don't debug without understanding** the business process

##### **8. Quick Debug Commands Reference**

| **Scenario** | **Command/Action** |
|-------------|-------------------|
| **Start Debug** | `/h` in command field |
| **Set Breakpoint** | `BREAK-POINT.` in code |
| **External Breakpoint** | Double-click line in SE80 |
| **Step Into** | F5 |
| **Step Over** | F6 |
| **Continue** | F8 |
| **Change Variable** | Double-click variable in debugger |
| **SQL Trace** | ST05 |
| **Performance** | SAT |

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

## 📚 **Continue Your Learning Journey**

| **Previous Module** | **Next Module** |
|---|---|
| [Module 2: ABAP Workbench & Development Environment](Module_02_ABAP_Workbench.md) | [Module 4: Data Dictionary (DDIC)](Module_04_Data_Dictionary.md) |

**Additional Resources**: [📚 Comprehensive Resource Hub](Additional_Resources.md) - Access all documentation, tools, and learning materials in one place.

**🐛 Debugging Reference**: [Complete ABAP Debugging Guide](ABAP_Debugging_Complete_Guide.md) - Master professional debugging techniques and troubleshooting strategies.