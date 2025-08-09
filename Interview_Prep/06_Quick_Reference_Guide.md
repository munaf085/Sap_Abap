# Quick Reference Guide - ABAP Interview Essentials

## 🔤 Common ABAP Syntax Quick Reference

### Data Declaration
```abap
" Basic data types
DATA: lv_char   TYPE c LENGTH 10,
      lv_string TYPE string,
      lv_int    TYPE i,
      lv_packed TYPE p DECIMALS 2,
      lv_date   TYPE d,
      lv_time   TYPE t.

" Constants and parameters
CONSTANTS: lc_max TYPE i VALUE 100.
PARAMETERS: p_input TYPE string.
SELECT-OPTIONS: s_date FOR sy-datum.

" Internal tables
DATA: lt_table TYPE TABLE OF mara,
      ls_structure TYPE mara.

" Field symbols and references
FIELD-SYMBOLS: <fs_any> TYPE any.
DATA: lr_ref TYPE REF TO data.
```

### Control Structures
```abap
" IF-ELSE
IF condition.
  " code
ELSEIF condition.
  " code  
ELSE.
  " code
ENDIF.

" CASE
CASE variable.
  WHEN 'A'.
    " code
  WHEN 'B' OR 'C'.
    " code
  WHEN OTHERS.
    " code
ENDCASE.

" Loops
DO 5 TIMES.
ENDDO.

WHILE condition.
ENDWHILE.

LOOP AT lt_table INTO ls_structure.
ENDLOOP.

LOOP AT lt_table ASSIGNING <fs_structure>.
ENDLOOP.
```

## 📊 Database Operations Quick Reference

### SELECT Statements
```abap
" Basic SELECT
SELECT * FROM mara INTO TABLE lt_mara.
SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr = '123'.

" SELECT with specific fields
SELECT matnr, mtart FROM mara INTO TABLE lt_mara.

" JOIN
SELECT m~matnr, t~maktx
  FROM mara AS m
  INNER JOIN makt AS t ON m~matnr = t~matnr
  INTO TABLE lt_result.

" FOR ALL ENTRIES
IF lt_mara IS NOT INITIAL.
  SELECT matnr, maktx FROM makt
    INTO TABLE lt_makt
    FOR ALL ENTRIES IN lt_mara
    WHERE matnr = lt_mara-matnr.
ENDIF.

" Aggregation
SELECT mtart, COUNT(*) FROM mara 
  INTO TABLE lt_count 
  GROUP BY mtart.
```

### Data Modification
```abap
" INSERT
INSERT mara FROM ls_mara.
INSERT mara FROM TABLE lt_mara.

" UPDATE  
UPDATE mara SET mtart = 'FERT' WHERE matnr = '123'.
UPDATE mara FROM ls_mara.

" MODIFY (INSERT or UPDATE)
MODIFY mara FROM ls_mara.

" DELETE
DELETE FROM mara WHERE matnr = '123'.
DELETE mara FROM TABLE lt_mara.
```

## 🏗️ Object-Oriented Quick Reference

### Class Definition
```abap
CLASS cl_example DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_param TYPE string,
             public_method RETURNING VALUE(rv_result) TYPE string.
    DATA: mv_public TYPE string.
    
  PROTECTED SECTION.
    DATA: mv_protected TYPE string.
    
  PRIVATE SECTION.
    DATA: mv_private TYPE string.
    METHODS: private_method.
ENDCLASS.

CLASS cl_example IMPLEMENTATION.
  METHOD constructor.
    mv_public = iv_param.
  ENDMETHOD.
  
  METHOD public_method.
    rv_result = mv_public.
  ENDMETHOD.
ENDCLASS.
```

### Inheritance
```abap
CLASS cl_child DEFINITION INHERITING FROM cl_parent.
  PUBLIC SECTION.
    METHODS: parent_method REDEFINITION,
             new_method.
ENDCLASS.

CLASS cl_child IMPLEMENTATION.
  METHOD parent_method.
    super->parent_method( ).  " Call parent method
    " Additional logic
  ENDMETHOD.
ENDCLASS.
```

### Interfaces
```abap
INTERFACE if_example.
  METHODS: interface_method 
    IMPORTING iv_param TYPE string
    RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.

CLASS cl_implementation DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_example.
ENDCLASS.

CLASS cl_implementation IMPLEMENTATION.
  METHOD if_example~interface_method.
    rv_result = iv_param.
  ENDMETHOD.
ENDCLASS.
```

## 📈 Performance Best Practices

### SQL Optimization
```abap
" ✅ Good practices
" Use specific WHERE clauses
SELECT * FROM mara WHERE mtart = 'FERT'.

" Select only needed fields
SELECT matnr, mtart FROM mara INTO TABLE lt_mara.

" Use JOINs instead of FOR ALL ENTRIES when possible
SELECT m~matnr, t~maktx FROM mara AS m
  INNER JOIN makt AS t ON m~matnr = t~matnr
  INTO TABLE lt_result.

" ❌ Avoid
" SELECT without WHERE clause
SELECT * FROM mara INTO TABLE lt_mara.

" Nested SELECT in loops
LOOP AT lt_orders INTO ls_order.
  SELECT SINGLE kunnr FROM vbpa INTO lv_customer
    WHERE vbeln = ls_order-vbeln.
ENDLOOP.
```

### Internal Table Best Practices
```abap
" Use appropriate table types
DATA: lt_standard TYPE STANDARD TABLE OF mara,
      lt_sorted   TYPE SORTED TABLE OF mara WITH UNIQUE KEY matnr,
      lt_hashed   TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr.

" Use field symbols for large tables
LOOP AT lt_large_table ASSIGNING <fs_entry>.
  <fs_entry>-field = 'NEW_VALUE'.
ENDLOOP.

" Use BINARY SEARCH for sorted tables
READ TABLE lt_sorted INTO ls_entry 
  WITH KEY matnr = '123' BINARY SEARCH.
```

## 🎯 Common Interview Questions - Quick Answers

### Q: Difference between APPEND and INSERT?
**A:** 
- APPEND: Adds entry at end of table (only for standard tables)
- INSERT: Adds entry at specific index or maintains sorting

### Q: When to use HASHED vs SORTED tables?
**A:**
- HASHED: When you need fast key-based access and don't need sorting
- SORTED: When you need both fast access and maintain sort order

### Q: What is the difference between TYPES and DATA?
**A:**
- TYPES: Creates data type definition (template)
- DATA: Creates actual data object using the type

### Q: Explain sy-subrc values
**A:**
- 0: Operation successful
- 4: Warning (e.g., record not found in READ TABLE)
- 8: Error (e.g., conversion error)
- Other values: Specific function module return codes

### Q: What is the purpose of COMMIT WORK?
**A:**
- Saves all database changes made in current LUW (Logical Unit of Work)
- Releases database locks
- Triggers COMMIT events in change documents

## 🔧 Debugging Quick Tips

### Common Debugging Commands
```abap
" Set breakpoints
BREAK-POINT.
BREAK <username>.

" Watch variables
" Use /h in command field to start debugger

" Debugging tools in ADT/Eclipse:
" F5 - Step into
" F6 - Step over  
" F7 - Step return
" F8 - Continue
```

### Error Handling
```abap
" Check return codes
IF sy-subrc <> 0.
  " Handle error
ENDIF.

" Exception handling
TRY.
    " risky code
  CATCH cx_exception INTO DATA(lx_error).
    MESSAGE lx_error->get_text( ) TYPE 'E'.
ENDTRY.
```

## 📋 System Fields Reference

| Field | Description | Example |
|-------|-------------|---------|
| sy-subrc | Return code | 0 = success |
| sy-tabix | Table index | Current row in LOOP |
| sy-index | Loop counter | Current DO iteration |
| sy-datum | Current date | 20240101 |
| sy-uzeit | Current time | 143022 |
| sy-uname | Username | DEVELOPER |
| sy-mandt | Client | 100 |
| sy-tcode | Transaction | VA01 |
| sy-cprog | Current program | Z_MY_REPORT |

## 🎪 Function Module vs Method Calls

### Function Module
```abap
CALL FUNCTION 'FUNCTION_NAME'
  EXPORTING
    input_param = lv_input
  IMPORTING
    output_param = lv_output
  TABLES
    table_param = lt_table
  EXCEPTIONS
    error = 1
    OTHERS = 2.

IF sy-subrc <> 0.
  " Handle error
ENDIF.
```

### Method Call
```abap
DATA(lo_object) = NEW cl_class( ).

" Static method
DATA(lv_result) = cl_class=>static_method( iv_param = lv_input ).

" Instance method  
lo_object->instance_method(
  EXPORTING iv_input = lv_input
  IMPORTING ev_output = lv_output ).
```

## 💡 Modern ABAP Syntax (7.40+)

### Inline Declarations
```abap
" Old syntax
DATA: lv_result TYPE string.
lv_result = some_method( ).

" New syntax
DATA(lv_result) = some_method( ).

" Value assignments
DATA(lt_table) = VALUE string_table( ( 'Line 1' ) ( 'Line 2' ) ).

" FOR loops in value statements
DATA(lt_squares) = VALUE int4_table( 
  FOR i = 1 THEN i + 1 WHILE i <= 10 ( i * i ) ).
```

### String Templates
```abap
" Old syntax
CONCATENATE 'Hello' lv_name 'from' lv_system INTO lv_message
  SEPARATED BY space.

" New syntax
DATA(lv_message) = |Hello { lv_name } from { lv_system }|.
```

This quick reference guide provides essential ABAP syntax and concepts that are commonly tested in interviews. Keep this handy during your preparation and review!