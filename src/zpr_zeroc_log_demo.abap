REPORT zpr_zeroc_log_demo.

CLASS lcl_table_payload DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_fi_doc,
             bukrs TYPE string,
             belnr TYPE string,
             gjahr TYPE string,
           END OF ty_fi_doc.
    TYPES ty_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mo_logger TYPE REF TO zcl_zeroc_log_manager.
    METHODS register_defaults.
    METHODS simulate_logging.
ENDCLASS.

CLASS lcl_table_payload IMPLEMENTATION.
  METHOD constructor.
    mo_logger = NEW zcl_zeroc_log_manager( iv_default_level = 'INFO' ).

    register_defaults( ).
  ENDMETHOD.

  METHOD run.
    simulate_logging( ).
  ENDMETHOD.

  METHOD register_defaults.
    mo_logger->register_module_defaults( VALUE #( module = 'SD' level = 'WARN' priority = 'HIGH' owner = 'SD_CORE' ) ).
    mo_logger->register_module_defaults( VALUE #( module = 'FI' level = 'INFO' priority = 'MEDIUM' owner = 'FIN_TEAM' ) ).
  ENDMETHOD.

  METHOD simulate_logging.
    mo_logger->log( iv_message = 'Sadece mesaj ile hızlı loglama.' ).

    mo_logger->log(
      iv_message = 'SD sipariş güncelleme hatası'
      is_context = VALUE #( module = 'SD' ) ).

    DATA(lt_items) = VALUE ty_string_table( ( `1000001` ) ( `1000002` ) ).
    mo_logger->log(
      iv_message = 'SD sipariş listesi'
      ir_payload = REF #( lt_items )
      is_context = VALUE #( module = 'SD' level = 'ERROR' ) ).

    DATA(ls_fi_doc) = VALUE ty_fi_doc( bukrs = '1000' belnr = '1800000010' gjahr = '2024' ).
    mo_logger->log(
      ir_payload = REF #( ls_fi_doc )
      is_context = VALUE #( module = 'FI' owner = sy-uname ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_table_payload( )->run( ).
