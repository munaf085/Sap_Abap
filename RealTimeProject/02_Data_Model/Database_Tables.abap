*&---------------------------------------------------------------------*
*& Database Tables for E-Commerce Integration Platform
*& Author: Senior ABAP Developer (4+ Years Experience)
*& Date: 2024
*& Purpose: Complete data model for production ABAP application
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Table: ZAPI_INTEGRATION_LOG
*& Purpose: API call logging and monitoring
*&---------------------------------------------------------------------*
@EndUserText.label : 'API Integration Log'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zapi_integration_log {
  key client            : mandt not null;
  key log_id            : sysuuid_x16 not null;
  timestamp             : dats not null;
  time                  : tims not null;
  system_id             : char10;
  method                : char10;
  endpoint              : char255;
  status_code           : int4;
  duration              : dec15_3;
  success_flag          : char1;
  error_message         : string;
  request_size          : int4;
  response_size         : int4;
  user_name             : syuname;
  created_at            : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZECOMMERCE_CONFIG
*& Purpose: Configuration parameters for e-commerce integration
*&---------------------------------------------------------------------*
@EndUserText.label : 'E-Commerce Configuration'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #ALLOWED
define table zecommerce_config {
  key client            : mandt not null;
  key config_type       : char20 not null;
  key config_key        : char50 not null;
  config_value          : char255;
  description           : char100;
  active                : char1;
  created_by            : syuname;
  created_at            : timestampl;
  changed_by            : syuname;
  changed_at            : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZMATERIAL_SYNC
*& Purpose: Material synchronization status tracking
*&---------------------------------------------------------------------*
@EndUserText.label : 'Material Sync Status'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zmaterial_sync {
  key client            : mandt not null;
  key material_id       : matnr not null;
  last_sync_date        : dats;
  last_sync_time        : tims;
  sync_status           : char1;
  error_message         : string;
  ecommerce_id          : char50;
  sync_direction        : char1;
  retry_count           : int2;
  created_by            : syuname;
  created_at            : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZCUSTOMER_SYNC
*& Purpose: Customer synchronization status tracking
*&---------------------------------------------------------------------*
@EndUserText.label : 'Customer Sync Status'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zcustomer_sync {
  key client            : mandt not null;
  key customer_id       : kunnr not null;
  last_sync_date        : dats;
  last_sync_time        : tims;
  sync_status           : char1;
  error_message         : string;
  ecommerce_id          : char50;
  email_address         : char241;
  sync_direction        : char1;
  retry_count           : int2;
  created_by            : syuname;
  created_at            : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZORDER_MAPPING
*& Purpose: Order mapping between SAP and e-commerce systems
*&---------------------------------------------------------------------*
@EndUserText.label : 'Order System Mapping'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zorder_mapping {
  key client            : mandt not null;
  key sap_order_id      : vbeln_va not null;
  ecommerce_order_id    : char50 not null;
  ecommerce_platform    : char20;
  order_status_sap      : char2;
  order_status_ecom     : char20;
  total_amount          : curr23_4;
  currency              : waers;
  customer_id           : kunnr;
  created_date          : dats;
  created_time          : tims;
  last_updated          : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZINTEGRATION_QUEUE
*& Purpose: Message queue for failed integrations and retries
*&---------------------------------------------------------------------*
@EndUserText.label : 'Integration Message Queue'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zintegration_queue {
  key client            : mandt not null;
  key queue_id          : sysuuid_x16 not null;
  message_type          : char20 not null;
  object_type           : char10;
  object_id             : char50;
  payload               : string;
  target_system         : char20;
  status                : char1;
  retry_count           : int2;
  max_retries           : int2;
  next_retry_at         : timestampl;
  error_message         : string;
  created_by            : syuname;
  created_at            : timestampl;
  processed_at          : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZECOMMERCE_ANALYTICS
*& Purpose: Pre-aggregated analytics data for dashboard
*&---------------------------------------------------------------------*
@EndUserText.label : 'E-Commerce Analytics'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zecommerce_analytics {
  key client            : mandt not null;
  key analytics_date    : dats not null;
  key channel           : char20 not null;
  key sales_org         : vkorg not null;
  order_count           : int4;
  total_value           : curr23_4;
  avg_order_value       : curr23_4;
  currency              : waers;
  unique_customers      : int4;
  conversion_rate       : dec5_2;
  created_at            : timestampl;
  updated_at            : timestampl;
  
}

*&---------------------------------------------------------------------*
*& Table: ZPRICING_SYNC_LOG
*& Purpose: Pricing synchronization audit trail
*&---------------------------------------------------------------------*
@EndUserText.label : 'Pricing Sync Log'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zpricing_sync_log {
  key client            : mandt not null;
  key log_id            : sysuuid_x16 not null;
  material_id           : matnr not null;
  price_list            : char10;
  old_price             : curr23_4;
  new_price             : curr23_4;
  currency              : waers;
  effective_date        : dats;
  sync_timestamp        : timestampl;
  sync_status           : char1;
  error_message         : string;
  changed_by            : syuname;
  
}

*&---------------------------------------------------------------------*
*& Table: ZINVENTORY_SYNC_LOG
*& Purpose: Inventory synchronization audit trail
*&---------------------------------------------------------------------*
@EndUserText.label : 'Inventory Sync Log'
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zinventory_sync_log {
  key client            : mandt not null;
  key log_id            : sysuuid_x16 not null;
  material_id           : matnr not null;
  plant                 : werks_d;
  storage_location      : lgort_d;
  old_quantity          : menge_d;
  new_quantity          : menge_d;
  unit                  : meins;
  sync_timestamp        : timestampl;
  sync_direction        : char1;
  sync_status           : char1;
  error_message         : string;
  triggered_by          : syuname;
  
}