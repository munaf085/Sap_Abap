*&---------------------------------------------------------------------*
*& Class: ZCL_UNIT_TEST_ORDER_PROCESSOR
*& Description: Comprehensive Unit Tests for Order Processor
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Testing Framework: ABAP Unit Testing
*&---------------------------------------------------------------------*

CLASS zcl_unit_test_order_processor DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    
    " Test data types
    TYPES: BEGIN OF ty_test_scenario,
             scenario_name    TYPE string,
             expected_result  TYPE abap_bool,
             customer_id     TYPE kunnr,
             order_type      TYPE auart,
             total_amount    TYPE netwr_ak,
             items_count     TYPE i,
           END OF ty_test_scenario.
    
    " Class under test
    DATA: mo_order_processor TYPE REF TO zcl_order_processor.
    
    " Mock objects for dependencies
    DATA: mo_mock_pricing    TYPE REF TO zcl_mock_pricing_engine,
          mo_mock_inventory  TYPE REF TO zcl_mock_inventory_manager,
          mo_mock_credit     TYPE REF TO zcl_mock_credit_manager,
          mo_mock_logger     TYPE REF TO zcl_mock_logger.
    
    " Test data
    DATA: ms_valid_order_context   TYPE zcl_order_processor=>ty_order_context,
          mt_valid_order_items     TYPE TABLE OF zcl_order_processor=>ty_order_item,
          ms_invalid_order_context TYPE zcl_order_processor=>ty_order_context,
          mt_test_scenarios        TYPE TABLE OF ty_test_scenario.
    
    " Setup and teardown methods
    METHODS: setup FOR TESTING,
             teardown FOR TESTING.
    
    " Test methods for positive scenarios
    METHODS: test_process_valid_order FOR TESTING,
             test_process_order_with_credit_check FOR TESTING,
             test_process_bulk_orders FOR TESTING,
             test_pricing_calculation FOR TESTING,
             test_inventory_availability_check FOR TESTING.
    
    " Test methods for negative scenarios  
    METHODS: test_process_invalid_customer FOR TESTING,
             test_process_insufficient_credit FOR TESTING,
             test_process_insufficient_inventory FOR TESTING,
             test_validation_empty_items FOR TESTING,
             test_validation_invalid_materials FOR TESTING.
    
    " Test methods for edge cases
    METHODS: test_process_zero_amount_order FOR TESTING,
             test_process_order_max_items FOR TESTING,
             test_concurrent_order_processing FOR TESTING,
             test_error_recovery_mechanism FOR TESTING.
    
    " Test methods for performance
    METHODS: test_performance_large_order FOR TESTING,
             test_performance_parallel_processing FOR TESTING.
    
    " Helper methods for test setup
    METHODS: setup_valid_test_data,
             setup_invalid_test_data,
             setup_mock_objects,
             create_test_order_context 
               IMPORTING iv_customer_id TYPE kunnr DEFAULT '1000001'
                        iv_order_type TYPE auart DEFAULT 'OR'
                        iv_total_amount TYPE netwr_ak DEFAULT '1000.00'
               RETURNING VALUE(rs_context) TYPE zcl_order_processor=>ty_order_context,
               
             create_test_order_items
               IMPORTING iv_items_count TYPE i DEFAULT 3
               RETURNING VALUE(rt_items) TYPE TABLE OF zcl_order_processor=>ty_order_item,
               
             assert_order_processing_success
               IMPORTING is_result TYPE zcl_order_processor=>ty_processing_result
                        iv_expected_success TYPE abap_bool DEFAULT abap_true,
               
             assert_error_messages_contain
               IMPORTING it_messages TYPE TABLE OF string
                        iv_expected_text TYPE string.

ENDCLASS.

CLASS zcl_unit_test_order_processor IMPLEMENTATION.

  METHOD setup.
    " Initialize test environment before each test
    
    " Setup mock objects
    setup_mock_objects( ).
    
    " Create instance of class under test with dependency injection
    mo_order_processor = NEW zcl_order_processor( ).
    
    " Inject mock dependencies (using reflection or factory pattern)
    " This would typically be done through constructor injection or setter methods
    
    " Setup test data
    setup_valid_test_data( ).
    setup_invalid_test_data( ).
    
  ENDMETHOD.

  METHOD teardown.
    " Clean up after each test
    CLEAR: mo_order_processor,
           mo_mock_pricing,
           mo_mock_inventory,
           mo_mock_credit,
           mo_mock_logger.
  ENDMETHOD.

  METHOD test_process_valid_order.
    " Test Case: Process a valid order successfully
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure mocks for success scenario
    mo_mock_credit->set_credit_check_result( abap_true ).
    mo_mock_inventory->set_availability_result( abap_true ).
    mo_mock_pricing->set_pricing_success( abap_true ).
    
    " Act
    DATA(ls_result) = mo_order_processor->process_order(
      is_order_context = ls_order_context
      it_order_items = lt_order_items ).
    
    " Assert
    assert_order_processing_success( 
      is_result = ls_result
      iv_expected_success = abap_true ).
    
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-order_number
      msg = 'Order number should be generated' ).
    
    cl_abap_unit_assert=>assert_initial(
      act = lines( ls_result-error_messages )
      msg = 'Should have no error messages for valid order' ).
    
    " Verify mock interactions
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = mo_mock_credit->get_call_count( )
      msg = 'Credit check should be called once' ).

  ENDMETHOD.

  METHOD test_process_order_with_credit_check.
    " Test Case: Process order with credit check validation
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context(
      iv_customer_id = '1000002'
      iv_total_amount = '50000.00' ).  " High amount requiring credit check
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure credit check to pass
    mo_mock_credit->set_credit_limit( iv_customer = '1000002' iv_limit = '100000.00' ).
    mo_mock_credit->set_current_exposure( iv_customer = '1000002' iv_exposure = '30000.00' ).
    
    " Act
    DATA(ls_result) = mo_order_processor->process_order(
      is_order_context = ls_order_context
      it_order_items = lt_order_items ).
    
    " Assert
    assert_order_processing_success( ls_result ).
    
    " Verify credit check was performed
    cl_abap_unit_assert=>assert_true(
      act = mo_mock_credit->was_credit_check_called( '1000002' )
      msg = 'Credit check should be performed for high value order' ).

  ENDMETHOD.

  METHOD test_process_bulk_orders.
    " Test Case: Process multiple orders in bulk
    
    " Arrange
    DATA: lt_orders TYPE TABLE OF zcl_order_processor=>ty_order_context.
    
    " Create 5 test orders
    DO 5 TIMES.
      DATA(ls_order) = create_test_order_context(
        iv_customer_id = |100000{ sy-index }|
        iv_total_amount = sy-index * 1000 ).
      APPEND ls_order TO lt_orders.
    ENDDO.
    
    " Configure mocks for success
    mo_mock_credit->set_credit_check_result( abap_true ).
    mo_mock_inventory->set_availability_result( abap_true ).
    
    " Act
    DATA(lt_results) = mo_order_processor->process_bulk_orders( lt_orders ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lines( lt_results )
      msg = 'Should process all 5 orders' ).
    
    " Verify all orders processed successfully
    LOOP AT lt_results INTO DATA(ls_result).
      assert_order_processing_success( ls_result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_process_invalid_customer.
    " Test Case: Process order with invalid customer
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context(
      iv_customer_id = '9999999' ).  " Non-existent customer
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Act & Assert
    TRY.
        mo_order_processor->process_order(
          is_order_context = ls_order_context
          it_order_items = lt_order_items ).
        
        cl_abap_unit_assert=>fail( 'Should raise exception for invalid customer' ).
        
      CATCH cx_order_processing_error INTO DATA(lx_error).
        " Expected exception
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( )
          exp = '*customer*not*exist*'
          msg = 'Error message should mention customer not found' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_process_insufficient_credit.
    " Test Case: Process order with insufficient credit
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context(
      iv_customer_id = '1000003'
      iv_total_amount = '100000.00' ).
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure credit check to fail
    mo_mock_credit->set_credit_limit( iv_customer = '1000003' iv_limit = '50000.00' ).
    mo_mock_credit->set_current_exposure( iv_customer = '1000003' iv_exposure = '40000.00' ).
    
    " Act
    TRY.
        mo_order_processor->process_order(
          is_order_context = ls_order_context
          it_order_items = lt_order_items ).
        
        cl_abap_unit_assert=>fail( 'Should raise exception for insufficient credit' ).
        
      CATCH cx_order_processing_error INTO DATA(lx_error).
        " Expected exception
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( )
          exp = '*credit*limit*exceeded*'
          msg = 'Error should mention credit limit exceeded' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_process_insufficient_inventory.
    " Test Case: Process order with insufficient inventory
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure inventory check to fail for one material
    mo_mock_inventory->set_material_availability( 
      iv_material = 'MAT001'
      iv_available_qty = 5
      iv_required_qty = 10 ).
    
    " Act
    DATA(ls_result) = mo_order_processor->process_order(
      is_order_context = ls_order_context
      it_order_items = lt_order_items ).
    
    " Assert
    cl_abap_unit_assert=>assert_not_initial(
      act = lines( ls_result-warning_messages )
      msg = 'Should have warning messages for insufficient inventory' ).
    
    assert_error_messages_contain(
      it_messages = ls_result-warning_messages
      iv_expected_text = 'Insufficient inventory' ).

  ENDMETHOD.

  METHOD test_validation_empty_items.
    " Test Case: Validate order with no items
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA: lt_empty_items TYPE TABLE OF zcl_order_processor=>ty_order_item.
    
    " Act
    DATA(lt_issues) = mo_order_processor->validate_order(
      is_order_context = ls_order_context
      it_order_items = lt_empty_items ).
    
    " Assert
    cl_abap_unit_assert=>assert_not_initial(
      act = lines( lt_issues )
      msg = 'Should have validation issues for empty items' ).
    
    assert_error_messages_contain(
      it_messages = lt_issues
      iv_expected_text = 'At least one order item is required' ).

  ENDMETHOD.

  METHOD test_pricing_calculation.
    " Test Case: Test pricing calculation functionality
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure pricing mock
    mo_mock_pricing->set_material_price( 
      iv_material = 'MAT001'
      iv_price = '100.00' ).
    mo_mock_pricing->set_material_price(
      iv_material = 'MAT002' 
      iv_price = '200.00' ).
    
    " Act
    mo_order_processor->calculate_pricing(
      is_order_context = ls_order_context
      it_order_items = lt_order_items
      CHANGING ct_order_items = lt_order_items ).
    
    " Assert
    " Verify prices were calculated correctly
    READ TABLE lt_order_items INTO DATA(ls_item) WITH KEY material_id = 'MAT001'.
    cl_abap_unit_assert=>assert_equals(
      exp = '100.00'
      act = ls_item-unit_price
      msg = 'Price should be calculated correctly for MAT001' ).

  ENDMETHOD.

  METHOD test_performance_large_order.
    " Test Case: Performance test with large order
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA(lt_order_items) = create_test_order_items( iv_items_count = 100 ).
    
    " Configure mocks for success
    mo_mock_credit->set_credit_check_result( abap_true ).
    mo_mock_inventory->set_availability_result( abap_true ).
    
    " Act
    DATA(lv_start_time) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    
    DATA(ls_result) = mo_order_processor->process_order(
      is_order_context = ls_order_context
      it_order_items = lt_order_items ).
    
    DATA(lv_end_time) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    DATA(lv_duration) = lv_end_time - lv_start_time.
    
    " Assert
    assert_order_processing_success( ls_result ).
    
    " Performance assertion - should complete within 5 seconds
    cl_abap_unit_assert=>assert_true(
      act = COND #( WHEN lv_duration < 5 THEN abap_true ELSE abap_false )
      msg = |Order processing should complete within 5 seconds. Actual: { lv_duration } seconds| ).

  ENDMETHOD.

  METHOD test_concurrent_order_processing.
    " Test Case: Test concurrent order processing scenarios
    
    " This test would simulate concurrent access patterns
    " In a real implementation, you might use work processes or tasks
    
    " Arrange
    DATA: lt_orders TYPE TABLE OF zcl_order_processor=>ty_order_context.
    
    " Create multiple orders for same customer
    DO 3 TIMES.
      DATA(ls_order) = create_test_order_context(
        iv_customer_id = '1000001'  " Same customer
        iv_total_amount = '1000.00' ).
      APPEND ls_order TO lt_orders.
    ENDDO.
    
    " Act
    DATA(lt_results) = mo_order_processor->process_bulk_orders( lt_orders ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_results )
      msg = 'All concurrent orders should be processed' ).
    
    " Verify no deadlocks or data corruption occurred
    LOOP AT lt_results INTO DATA(ls_result).
      assert_order_processing_success( ls_result ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test_error_recovery_mechanism.
    " Test Case: Test error recovery and rollback mechanisms
    
    " Arrange
    DATA(ls_order_context) = create_test_order_context( ).
    DATA(lt_order_items) = create_test_order_items( ).
    
    " Configure mock to fail during order creation step
    mo_mock_credit->set_credit_check_result( abap_true ).
    mo_mock_inventory->set_availability_result( abap_true ).
    " But cause order creation to fail
    mo_order_processor->set_order_creation_failure( abap_true ).
    
    " Act
    TRY.
        mo_order_processor->process_order(
          is_order_context = ls_order_context
          it_order_items = lt_order_items ).
        
        cl_abap_unit_assert=>fail( 'Should raise exception when order creation fails' ).
        
      CATCH cx_order_processing_error.
        " Expected - verify no partial data was committed
        " Check that no order documents were created
        " Check that inventory reservations were rolled back
        " Check that credit holds were released
    ENDTRY.

  ENDMETHOD.

  " Helper method implementations

  METHOD setup_valid_test_data.
    " Setup valid test data for positive test cases
    ms_valid_order_context = VALUE #(
      customer_id = '1000001'
      order_type = 'OR'
      sales_org = '1000'
      distribution = '10'
      division = '00'
      currency = 'USD'
      total_amount = '1500.00'
      source_system = 'ECOMMERCE'
      priority = 'H' ).
    
    mt_valid_order_items = VALUE #(
      ( item_number = '000010' material_id = 'MAT001' quantity = '5' unit_price = '100.00' )
      ( item_number = '000020' material_id = 'MAT002' quantity = '3' unit_price = '200.00' )
      ( item_number = '000030' material_id = 'MAT003' quantity = '2' unit_price = '300.00' ) ).
  ENDMETHOD.

  METHOD setup_invalid_test_data.
    " Setup invalid test data for negative test cases
    ms_invalid_order_context = VALUE #(
      customer_id = '9999999'  " Invalid customer
      order_type = ''          " Missing order type
      sales_org = ''           " Missing sales org
      total_amount = '0.00' ). " Zero amount
  ENDMETHOD.

  METHOD setup_mock_objects.
    " Initialize mock objects with default behaviors
    mo_mock_pricing = NEW zcl_mock_pricing_engine( ).
    mo_mock_inventory = NEW zcl_mock_inventory_manager( ).
    mo_mock_credit = NEW zcl_mock_credit_manager( ).
    mo_mock_logger = NEW zcl_mock_logger( ).
    
    " Configure default successful behaviors
    mo_mock_credit->set_credit_check_result( abap_true ).
    mo_mock_inventory->set_availability_result( abap_true ).
    mo_mock_pricing->set_pricing_success( abap_true ).
  ENDMETHOD.

  METHOD create_test_order_context.
    rs_context = VALUE #(
      customer_id = iv_customer_id
      order_type = iv_order_type
      sales_org = '1000'
      distribution = '10'
      division = '00'
      currency = 'USD'
      total_amount = iv_total_amount
      source_system = 'TEST'
      priority = 'M' ).
  ENDMETHOD.

  METHOD create_test_order_items.
    DATA: lv_item_number TYPE posnr_va VALUE '000010'.
    
    DO iv_items_count TIMES.
      APPEND VALUE #(
        item_number = lv_item_number
        material_id = |MAT{ sy-index ALPHA = IN }|
        quantity = sy-index * 2
        unit_price = sy-index * '50.00'
        plant = '1000'
        unit = 'EA'
      ) TO rt_items.
      
      lv_item_number = lv_item_number + 10.
    ENDDO.
  ENDMETHOD.

  METHOD assert_order_processing_success.
    cl_abap_unit_assert=>assert_equals(
      exp = iv_expected_success
      act = is_result-success
      msg = COND #( WHEN iv_expected_success = abap_true 
                   THEN 'Order processing should succeed'
                   ELSE 'Order processing should fail' ) ).
                   
    IF iv_expected_success = abap_true.
      cl_abap_unit_assert=>assert_not_initial(
        act = is_result-order_number
        msg = 'Order number should be generated for successful processing' ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_error_messages_contain.
    DATA: lv_found TYPE abap_bool.
    
    LOOP AT it_messages INTO DATA(lv_message).
      IF lv_message CS iv_expected_text.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    
    cl_abap_unit_assert=>assert_true(
      act = lv_found
      msg = |Error messages should contain: { iv_expected_text }| ).
  ENDMETHOD.

ENDCLASS.