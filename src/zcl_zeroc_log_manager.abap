CLASS zcl_zeroc_log_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_context,
             module   TYPE string,
             level    TYPE string,
             priority TYPE string,
             owner    TYPE string,
           END OF ty_context.
    TYPES ty_context_tab TYPE STANDARD TABLE OF ty_context WITH EMPTY KEY.

    TYPES: BEGIN OF ty_log_entry,
             timestamp TYPE timestampl,
             message   TYPE string,
             level     TYPE string,
             module    TYPE string,
             priority  TYPE string,
             owner     TYPE string,
             payload   TYPE string,
           END OF ty_log_entry.

    METHODS constructor
      IMPORTING
        iv_default_level      TYPE string DEFAULT 'INFO'
        iv_use_console_target TYPE abap_bool DEFAULT abap_true.

    METHODS log
      IMPORTING
        iv_message TYPE string OPTIONAL
        ir_payload TYPE REF TO data OPTIONAL
        is_context TYPE ty_context OPTIONAL.

    METHODS add_target
      IMPORTING
        io_target TYPE REF TO object.

    METHODS clear_targets.

    METHODS register_module_defaults
      IMPORTING
        is_context TYPE ty_context.

    METHODS load_module_defaults
      IMPORTING
        it_contexts TYPE ty_context_tab.

  PRIVATE SECTION.
    TYPES ty_target_tab TYPE STANDARD TABLE OF REF TO object WITH EMPTY KEY.

    DATA mv_default_level TYPE string.
    DATA mt_targets       TYPE ty_target_tab.
    DATA mt_module_defaults TYPE SORTED TABLE OF ty_context WITH UNIQUE KEY module.

    METHODS create_entry
      IMPORTING
        iv_message TYPE string
        ir_payload TYPE REF TO data
        is_context TYPE ty_context
      RETURNING
        VALUE(rs_entry) TYPE ty_log_entry.

    METHODS merge_with_defaults
      IMPORTING
        is_context TYPE ty_context
      RETURNING
        VALUE(rs_context) TYPE ty_context.

    METHODS dispatch
      IMPORTING
        is_entry TYPE ty_log_entry.

    METHODS ensure_handle_log_method
      IMPORTING
        io_target TYPE REF TO object.

    CLASS-METHODS format_payload
      IMPORTING
        ir_payload TYPE REF TO data
      RETURNING
        VALUE(rv_payload) TYPE string.

    CLASS-METHODS create_console_target
      RETURNING
        VALUE(ro_target) TYPE REF TO object.
ENDCLASS.


CLASS zcl_zeroc_log_manager IMPLEMENTATION.
  METHOD constructor.
    mv_default_level = COND string( WHEN iv_default_level IS INITIAL THEN 'INFO' ELSE iv_default_level ).
    IF iv_use_console_target = abap_true.
      add_target( create_console_target( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD log.
    IF iv_message IS INITIAL AND ir_payload IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(ls_context) = merge_with_defaults( is_context ).
    DATA(ls_entry) = create_entry(
      iv_message = iv_message
      ir_payload = ir_payload
      is_context = ls_context ).

    dispatch( ls_entry ).
  ENDMETHOD.

  METHOD add_target.
    IF io_target IS NOT BOUND.
      RETURN.
    ENDIF.

    ensure_handle_log_method( io_target ).
    APPEND io_target TO mt_targets.
  ENDMETHOD.

  METHOD clear_targets.
    CLEAR mt_targets.
  ENDMETHOD.

  METHOD register_module_defaults.
    IF is_context-module IS INITIAL.
      RETURN.
    ENDIF.

    DELETE mt_module_defaults WHERE module = is_context-module.
    INSERT is_context INTO TABLE mt_module_defaults.
  ENDMETHOD.

  METHOD load_module_defaults.
    LOOP AT it_contexts ASSIGNING FIELD-SYMBOL(<ls_context>).
      register_module_defaults( <ls_context> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_entry.
    DATA(ls_entry) = VALUE ty_log_entry(
      message = iv_message
      level   = COND string( WHEN is_context-level IS INITIAL THEN mv_default_level ELSE is_context-level )
      module  = is_context-module
      priority = is_context-priority
      owner   = is_context-owner ).

    GET TIME STAMP FIELD ls_entry-timestamp.

    IF ir_payload IS BOUND.
      ls_entry-payload = format_payload( ir_payload ).
    ENDIF.

    rs_entry = ls_entry.
  ENDMETHOD.

  METHOD merge_with_defaults.
    rs_context = is_context.

    IF is_context-module IS NOT INITIAL.
      READ TABLE mt_module_defaults WITH KEY module = is_context-module INTO DATA(ls_defaults).
      IF sy-subrc = 0.
        IF rs_context-level IS INITIAL.
          rs_context-level = ls_defaults-level.
        ENDIF.
        IF rs_context-priority IS INITIAL.
          rs_context-priority = ls_defaults-priority.
        ENDIF.
        IF rs_context-owner IS INITIAL.
          rs_context-owner = ls_defaults-owner.
        ENDIF.
      ENDIF.
    ENDIF.

    IF rs_context-level IS INITIAL.
      rs_context-level = mv_default_level.
    ENDIF.
  ENDMETHOD.

  METHOD dispatch.
    LOOP AT mt_targets ASSIGNING FIELD-SYMBOL(<lo_target>).
      TRY.
          CALL METHOD <lo_target>->('HANDLE_LOG')
            EXPORTING
              is_entry = is_entry.
        CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_type cx_sy_ref_is_initial.
          " swallow target errors to protect producer
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD ensure_handle_log_method.
    TRY.
        DATA(lo_descr) = cl_abap_objectdescr=>describe_by_object_ref( io_target ).
      CATCH cx_sy_ref_is_initial.
        RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_method.
    ENDTRY.

    READ TABLE lo_descr->methods WITH KEY name = 'HANDLE_LOG' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_method.
    ENDIF.
  ENDMETHOD.

  METHOD format_payload.
    rv_payload = ''.

    IF ir_payload IS NOT BOUND.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <lx_payload> TYPE any.
    ASSIGN ir_payload->* TO <lx_payload>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        CALL TRANSFORMATION id SOURCE data = <lx_payload> RESULT XML DATA(lv_xml).
        rv_payload = cl_abap_codepage=>convert_from( lv_xml ).
      CATCH cx_root INTO DATA(lo_exc).
        rv_payload = |<<payload serialization failed: { lo_exc->get_text( ) }>>|.
    ENDTRY.
  ENDMETHOD.

  METHOD create_console_target.
    CLASS lcl_console_target DEFINITION FINAL CREATE PRIVATE.
      PUBLIC SECTION.
        METHODS handle_log
          IMPORTING
            is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry.
    ENDCLASS.

    CLASS lcl_console_target IMPLEMENTATION.
      METHOD handle_log.
        DATA(lv_header) = |[{ is_entry-timestamp }] { is_entry-level }|.
        IF is_entry-module IS NOT INITIAL.
          lv_header = |{ lv_header } ({ is_entry-module })|.
        ENDIF.
        IF is_entry-priority IS NOT INITIAL.
          lv_header = |{ lv_header } #{ is_entry-priority }|.
        ENDIF.
        IF is_entry-owner IS NOT INITIAL.
          lv_header = |{ lv_header } @{ is_entry-owner }|.
        ENDIF.
        WRITE: / lv_header.
        IF is_entry-message IS NOT INITIAL.
          WRITE: / is_entry-message.
        ENDIF.
        IF is_entry-payload IS NOT INITIAL.
          WRITE: / is_entry-payload.
        ENDIF.
      ENDMETHOD.
    ENDCLASS.

    ro_target = NEW lcl_console_target( ).
  ENDMETHOD.
ENDCLASS.
