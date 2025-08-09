# Technical Concepts - Interview Q&A

## Data Dictionary (DDIC)

### ⭐ Q1: What is Data Dictionary and its purpose?
**Answer:** The Data Dictionary (DDIC) is a central repository in SAP that stores definitions of all data used in the system. It maintains metadata about:
- Database tables and views
- Data elements and domains
- Search helps and lock objects
- Table types and structures

**Purpose:**
- Ensures data consistency across the system
- Provides data reusability
- Maintains data integrity
- Supports documentation and metadata management

### ⭐⭐ Q2: Explain the relationship between Domain, Data Element, and Table Field.
**Answer:**
```
Domain → Data Element → Table Field
   ↓         ↓           ↓
Technical   Business   Database
attributes  semantics   field
```

**Domain**: Technical attributes (data type, length, conversion routines)
```abap
" Example Domain: ZMAT_NUM
Data Type: CHAR
Length: 18
Conversion Exit: MATN1
```

**Data Element**: Business semantics (field labels, documentation)
```abap
" Example Data Element: ZMATNR
Domain: ZMAT_NUM
Short text: Material Number
Medium text: Material Number
Long text: Material Number
```

**Table Field**: Database field using the data element
```abap
" Table ZMARA field MATNR uses data element ZMATNR
```

### ⭐⭐ Q3: What are the different types of tables in SAP?
**Answer:**
1. **Transparent Tables**: 
   - One-to-one relationship with database table
   - Most commonly used
   - Example: MARA, VBAK

2. **Pooled Tables**:
   - Multiple tables stored in one table pool
   - Used for system tables
   - Example: T100 (messages)

3. **Cluster Tables**:
   - Related tables grouped together
   - Share same primary key
   - Example: BSEG, BKPF

```abap
" Accessing different table types
SELECT * FROM mara INTO TABLE lt_mara.     " Transparent
SELECT * FROM t100 INTO TABLE lt_messages. " Pooled  
SELECT * FROM bseg INTO TABLE lt_items.    " Cluster
```

### ⭐⭐ Q4: What are Search Helps and their types?
**Answer:** Search Helps provide value assistance for input fields.

**Types:**
1. **Elementary Search Help**: Basic search help with one search method
2. **Collective Search Help**: Combination of multiple elementary search helps

```abap
" Elementary Search Help example
Search Help: ZSH_MATERIAL
Search Method: MARA
Selection Method: Dialog with value restriction
Dialog Type: Display values immediately

" Collective Search Help example  
Search Help: ZSH_CUSTOMER
Included Elementary Search Helps:
- ZSH_CUSTOMER_BY_NUMBER
- ZSH_CUSTOMER_BY_NAME
- ZSH_CUSTOMER_BY_CITY
```

### ⭐⭐⭐ Q5: Explain Table Maintenance and its generation.
**Answer:** Table Maintenance allows users to maintain table data through SM30.

**Steps to generate:**
1. Set table maintenance allowed in table definition
2. Generate maintenance dialog (SE80 → Utilities → Table Maintenance Generator)
3. Specify maintenance type (one-step/two-step)
4. Add maintenance screens and authorization groups

```abap
" Table maintenance events
FORM maintenance_events.
  " Before save
  " After save  
  " Before delete
  " After delete
ENDFORM.
```

## Internal Tables

### ⭐ Q6: What are Internal Tables and their types?
**Answer:** Internal Tables are temporary data objects that exist only during program execution.

**Types:**
```abap
" Standard Table - Can have duplicate entries
DATA: lt_standard TYPE STANDARD TABLE OF mara.

" Sorted Table - Automatically sorted, unique/non-unique keys
DATA: lt_sorted TYPE SORTED TABLE OF mara 
      WITH UNIQUE KEY matnr.

" Hashed Table - Hash algorithm access, must have unique key
DATA: lt_hashed TYPE HASHED TABLE OF mara
      WITH UNIQUE KEY matnr.

" Index Table - Standard and Sorted tables
" Key Table - Sorted and Hashed tables
```

### ⭐⭐ Q7: Explain different ways to declare Internal Tables.
**Answer:**
```abap
" Method 1: Using standard table types
DATA: lt_mara TYPE TABLE OF mara,
      ls_mara TYPE mara.

" Method 2: Using TYPES
TYPES: BEGIN OF ty_material,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         maktx TYPE makt-maktx,
       END OF ty_material.
DATA: lt_material TYPE TABLE OF ty_material.

" Method 3: Using ranges
RANGES: r_matnr FOR mara-matnr.

" Method 4: Using LIKE
DATA: lt_like TYPE TABLE OF mara,
      ls_like LIKE LINE OF lt_like.

" Method 5: Modern syntax (7.40+)
DATA(lt_modern) = VALUE mara_t( ).
```

### ⭐⭐ Q8: What are the different ways to read from Internal Tables?
**Answer:**
```abap
DATA: lt_mara TYPE TABLE OF mara,
      ls_mara TYPE mara.

" 1. READ TABLE with index
READ TABLE lt_mara INTO ls_mara INDEX 1.

" 2. READ TABLE with key
READ TABLE lt_mara INTO ls_mara WITH KEY matnr = 'MAT001'.

" 3. READ TABLE with work area
READ TABLE lt_mara INTO DATA(ls_work) WITH KEY matnr = 'MAT001'.

" 4. READ TABLE with field symbol
READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>) 
           WITH KEY matnr = 'MAT001'.

" 5. READ TABLE binary search (for sorted tables)
READ TABLE lt_mara INTO ls_mara WITH KEY matnr = 'MAT001'
           BINARY SEARCH.

" 6. LOOP with condition
LOOP AT lt_mara INTO ls_mara WHERE mtart = 'FERT'.
  " Process only finished goods
ENDLOOP.

" Check success
IF sy-subrc = 0.
  " Record found
ENDIF.
```

### ⭐⭐⭐ Q9: Explain Internal Table operations and performance considerations.
**Answer:**
```abap
" APPEND - Add single entry (only for standard tables)
APPEND ls_mara TO lt_mara.

" INSERT - Add entry at specific position
INSERT ls_mara INTO lt_mara INDEX 1.

" COLLECT - Add or sum up entries
COLLECT ls_mara INTO lt_mara.

" MODIFY - Update existing entry
MODIFY lt_mara FROM ls_mara INDEX 1.
MODIFY lt_mara FROM ls_mara TRANSPORTING mtart WHERE matnr = 'MAT001'.

" DELETE - Remove entries
DELETE lt_mara INDEX 1.
DELETE lt_mara WHERE mtart = 'ROH'.

" SORT - Sort table
SORT lt_mara BY matnr ASCENDING mtart DESCENDING.

" Performance Tips:
" 1. Use HASHED tables for key-based access
" 2. Use SORTED tables for sorted data
" 3. Use field symbols to avoid copying
" 4. Use BINARY SEARCH for large sorted tables
" 5. Avoid nested loops on large tables
```

## Database Operations

### ⭐ Q10: What are the different types of SELECT statements?
**Answer:**
```abap
" 1. SELECT SINGLE - Get one record
SELECT SINGLE matnr, mtart FROM mara 
  INTO (lv_matnr, lv_mtart)
  WHERE matnr = 'MAT001'.

" 2. SELECT * - Get all fields
SELECT * FROM mara INTO TABLE lt_mara
  WHERE mtart = 'FERT'.

" 3. SELECT with specific fields
SELECT matnr, mtart, maktx 
  FROM mara AS a
  INNER JOIN makt AS b ON a~matnr = b~matnr
  INTO TABLE lt_result
  WHERE a~mtart = 'FERT'
    AND b~spras = sy-langu.

" 4. SELECT with aggregation
SELECT mtart, COUNT(*) AS count
  FROM mara
  INTO TABLE lt_count
  GROUP BY mtart.

" 5. SELECT with FOR ALL ENTRIES
SELECT matnr, maktx FROM makt
  INTO TABLE lt_makt
  FOR ALL ENTRIES IN lt_mara
  WHERE matnr = lt_mara-matnr
    AND spras = sy-langu.
```

### ⭐⭐ Q11: Explain database locks and their types.
**Answer:**
**Database Locks** prevent multiple users from changing the same data simultaneously.

**Types:**
1. **Shared Lock (S)**: Multiple users can read, no one can write
2. **Exclusive Lock (X)**: Only one user can read/write
3. **Optimistic Lock**: Check for changes before update
4. **Pessimistic Lock**: Lock immediately when accessed

```abap
" Enqueue/Dequeue functions
CALL FUNCTION 'ENQUEUE_EZLOCK_MATERIAL'
  EXPORTING
    matnr = lv_matnr
  EXCEPTIONS
    foreign_lock = 1
    system_failure = 2.

IF sy-subrc = 0.
  " Process data
  " ...
  
  " Release lock
  CALL FUNCTION 'DEQUEUE_EZLOCK_MATERIAL'
    EXPORTING
      matnr = lv_matnr.
ENDIF.
```

### ⭐⭐⭐ Q12: What is FOR ALL ENTRIES and its considerations?
**Answer:** FOR ALL ENTRIES is used to select data based on entries in an internal table.

```abap
" Example
SELECT matnr, maktx FROM makt
  INTO TABLE lt_makt
  FOR ALL ENTRIES IN lt_mara
  WHERE matnr = lt_mara-matnr
    AND spras = sy-langu.
```

**Considerations:**
1. **Check if driver table is not empty**:
```abap
IF lt_mara IS NOT INITIAL.
  SELECT matnr, maktx FROM makt
    INTO TABLE lt_makt
    FOR ALL ENTRIES IN lt_mara
    WHERE matnr = lt_mara-matnr.
ENDIF.
```

2. **Remove duplicates** from driver table
3. **Performance**: Can be slower than joins for large datasets
4. **Memory**: Loads all driver table entries into memory

## Reports Development

### ⭐ Q13: What are the different types of reports in ABAP?
**Answer:**
1. **Classical Reports**: Simple list output
2. **Interactive Reports**: User can interact with output
3. **ALV Reports**: SAP List Viewer with built-in functions

```abap
" Classical Report
REPORT z_classical_report.
START-OF-SELECTION.
  SELECT * FROM mara INTO TABLE DATA(lt_mara).
  LOOP AT lt_mara INTO DATA(ls_mara).
    WRITE: / ls_mara-matnr, ls_mara-mtart.
  ENDLOOP.

" Interactive Report
REPORT z_interactive_report.
AT LINE-SELECTION.
  " Handle line selection
  
AT USER-COMMAND.
  " Handle user commands

" ALV Report  
REPORT z_alv_report.
DATA: lr_alv TYPE REF TO cl_salv_table.
cl_salv_table=>factory( 
  IMPORTING r_salv_table = lr_alv
  CHANGING t_table = lt_mara ).
lr_alv->display( ).
```

### ⭐⭐ Q14: Explain selection screens and their elements.
**Answer:**
```abap
REPORT z_selection_screen.

" Parameters - Single input fields
PARAMETERS: p_matnr TYPE mara-matnr OBLIGATORY,
           p_mtart TYPE mara-mtart DEFAULT 'FERT',
           p_date  TYPE sy-datum DEFAULT sy-datum.

" Select-options - Range tables
SELECT-OPTIONS: s_matnr FOR mara-matnr,
               s_erdat FOR mara-erdat.

" Selection screen events
INITIALIZATION.
  " Set default values
  p_date = sy-datum.

AT SELECTION-SCREEN.
  " Validate inputs
  IF p_matnr IS INITIAL.
    MESSAGE 'Material number is required' TYPE 'E'.
  ENDIF.

AT SELECTION-SCREEN ON p_matnr.
  " Validate specific field
  SELECT SINGLE matnr FROM mara INTO @DATA(lv_check)
    WHERE matnr = @p_matnr.
  IF sy-subrc <> 0.
    MESSAGE 'Material does not exist' TYPE 'E'.
  ENDIF.
```

### ⭐⭐⭐ Q15: What is ALV and its advantages?
**Answer:** **ALV (ABAP List Viewer)** is SAP's standard tool for displaying data in a formatted, interactive list.

**Advantages:**
- Built-in sorting, filtering, and totaling
- Excel export functionality
- Column hiding/showing
- Print and email options
- Standard look and feel

**Types:**
1. **SALV (Simple ALV)**: Newer, object-oriented
2. **REUSE ALV**: Older, function-based

```abap
" SALV Example
DATA: lr_alv TYPE REF TO cl_salv_table,
      lr_columns TYPE REF TO cl_salv_columns_table,
      lr_column TYPE REF TO cl_salv_column_table.

cl_salv_table=>factory(
  IMPORTING r_salv_table = lr_alv
  CHANGING t_table = lt_data ).

" Get columns object
lr_columns = lr_alv->get_columns( ).
lr_columns->set_optimize( abap_true ).

" Modify specific column
lr_column ?= lr_columns->get_column( 'MATNR' ).
lr_column->set_long_text( 'Material Number' ).

" Display
lr_alv->display( ).
```

## Forms and BDC

### ⭐⭐ Q16: What is BDC and its methods?
**Answer:** **BDC (Batch Data Communication)** is used to transfer data from external systems to SAP.

**Methods:**
1. **Call Transaction**: Direct transaction call
2. **Session Method**: Batch session processing
3. **Direct Input**: Direct database update

```abap
" Call Transaction Method
DATA: lt_bdcdata TYPE TABLE OF bdcdata.

PERFORM bdc_screen USING 'SAPMM01' '0060'.
PERFORM bdc_field  USING 'RMMG1-MATNR' lv_matnr.
PERFORM bdc_field  USING 'RMMG1-MTART' lv_mtart.

CALL TRANSACTION 'MM01' 
  USING lt_bdcdata
  MODE 'A'
  UPDATE 'S'
  MESSAGES INTO lt_messages.

" Session Method
CALL FUNCTION 'BDC_OPEN_GROUP'
  EXPORTING
    client = sy-mandt
    group  = 'ZMAT_UPLOAD'
    user   = sy-uname.

CALL FUNCTION 'BDC_INSERT'
  EXPORTING
    tcode = 'MM01'
  TABLES
    dynprotab = lt_bdcdata.

CALL FUNCTION 'BDC_CLOSE_GROUP'.
```

### ⭐⭐ Q17: What are the different modes in Call Transaction?
**Answer:**
```abap
" Mode A - Display screens (All screens)
CALL TRANSACTION 'MM01' USING lt_bdcdata MODE 'A'.

" Mode E - Display only Error screens  
CALL TRANSACTION 'MM01' USING lt_bdcdata MODE 'E'.

" Mode N - No display (Background)
CALL TRANSACTION 'MM01' USING lt_bdcdata MODE 'N'.

" Update modes
" S - Synchronous update (wait for completion)
" A - Asynchronous update (don't wait)
" L - Local update (in same work process)
CALL TRANSACTION 'MM01' 
  USING lt_bdcdata 
  MODE 'N' 
  UPDATE 'S'.
```

This covers the essential technical concepts that are commonly tested in ABAP interviews. The questions progress from basic understanding to advanced implementation scenarios.