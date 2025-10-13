CLASS lcl_zc_level_helper DEFINITION DEFERRED.
CLASS lcl_zc_target_manager DEFINITION DEFERRED.
CLASS lcl_zc_config DEFINITION DEFERRED.
CLASS lcl_zc_async_queue DEFINITION DEFERRED.

CLASS zcl_zeroc_log_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_log_entry,
             timestamp TYPE timestampl,
             level      TYPE string,
             module     TYPE string,
             message    TYPE string,
             data       TYPE string,
             metadata   TYPE string,
           END OF ty_log_entry.
    TYPES ty_log_entries TYPE STANDARD TABLE OF ty_log_entry WITH EMPTY KEY.
    TYPES: BEGIN OF ty_config,
             default_level TYPE string,
             async_mode    TYPE abap_bool,
           END OF ty_config.

    METHODS constructor
      IMPORTING
        iv_default_level TYPE string DEFAULT 'INFO'.

    METHODS log
      IMPORTING
        iv_level    TYPE string
        iv_message  TYPE string
        iv_module   TYPE string DEFAULT ''
        iv_data     TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS debug
      IMPORTING
        iv_message TYPE string
        iv_module  TYPE string DEFAULT ''
        iv_data    TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS info
      IMPORTING
        iv_message TYPE string
        iv_module  TYPE string DEFAULT ''
        iv_data    TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS warning
      IMPORTING
        iv_message TYPE string
        iv_module  TYPE string DEFAULT ''
        iv_data    TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS error
      IMPORTING
        iv_message TYPE string
        iv_module  TYPE string DEFAULT ''
        iv_data    TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS fatal
      IMPORTING
        iv_message TYPE string
        iv_module  TYPE string DEFAULT ''
        iv_data    TYPE string OPTIONAL
        iv_metadata TYPE string OPTIONAL.

    METHODS add_log_target
      IMPORTING
        io_target TYPE REF TO object.

    METHODS remove_log_target
      IMPORTING
        io_target TYPE REF TO object.

    METHODS set_log_level
      IMPORTING
        iv_level TYPE string.

    METHODS set_log_level_by_module
      IMPORTING
        iv_module TYPE string
        iv_level  TYPE string.

    METHODS get_effective_level
      IMPORTING
        iv_module TYPE string
      RETURNING
        VALUE(rv_level) TYPE string.

    METHODS set_config
      IMPORTING
        is_config TYPE ty_config.

    METHODS get_config
      RETURNING
        VALUE(rs_config) TYPE ty_config.

    METHODS enable_async_logging.
    METHODS disable_async_logging.
    METHODS flush.

    METHODS rotate_logs.
    METHODS cleanup_logs
      IMPORTING
        iv_days TYPE i.

  PRIVATE SECTION.
    CONSTANTS mc_default_async TYPE abap_bool VALUE abap_false.

    DATA mo_config         TYPE REF TO lcl_zc_config.
    DATA mo_target_manager TYPE REF TO lcl_zc_target_manager.
    DATA mo_async_queue    TYPE REF TO lcl_zc_async_queue.
    DATA mo_level_helper   TYPE REF TO lcl_zc_level_helper.

    METHODS create_entry
      IMPORTING
        iv_level    TYPE string
        iv_message  TYPE string
        iv_module   TYPE string
        iv_data     TYPE string
        iv_metadata TYPE string
      RETURNING
        VALUE(rs_entry) TYPE ty_log_entry.

    METHODS dispatch
      IMPORTING
        is_entry TYPE ty_log_entry.

    METHODS enqueue_async
      IMPORTING
        is_entry TYPE ty_log_entry.

    METHODS should_persist
      IMPORTING
        iv_level  TYPE string
        iv_module TYPE string
      RETURNING
        VALUE(rv_is_allowed) TYPE abap_bool.
ENDCLASS.

CLASS zcl_zeroc_log_manager IMPLEMENTATION.
  METHOD constructor.
    mo_level_helper   = NEW lcl_zc_level_helper( ).
    mo_target_manager = NEW lcl_zc_target_manager( ).
    mo_config         = NEW lcl_zc_config(
      iv_default_level = iv_default_level
      iv_async_enabled = mc_default_async ).
    mo_async_queue    = NEW lcl_zc_async_queue( ).
  ENDMETHOD.

  METHOD log.
    IF should_persist( iv_level = iv_level iv_module = iv_module ) = abap_false.
      RETURN.
    ENDIF.

    DATA(ls_entry) = create_entry(
      iv_level    = iv_level
      iv_message  = iv_message
      iv_module   = iv_module
      iv_data     = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
      iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).

    IF mo_config->is_async_enabled( ) = abap_true.
      enqueue_async( ls_entry ).
    ELSE.
      dispatch( ls_entry ).
    ENDIF.
  ENDMETHOD.

  METHOD debug.
    log( iv_level = mo_level_helper->level_debug( )
         iv_message = iv_message
         iv_module = iv_module
         iv_data = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
         iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).
  ENDMETHOD.

  METHOD info.
    log( iv_level = mo_level_helper->level_info( )
         iv_message = iv_message
         iv_module = iv_module
         iv_data = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
         iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).
  ENDMETHOD.

  METHOD warning.
    log( iv_level = mo_level_helper->level_warning( )
         iv_message = iv_message
         iv_module = iv_module
         iv_data = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
         iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).
  ENDMETHOD.

  METHOD error.
    log( iv_level = mo_level_helper->level_error( )
         iv_message = iv_message
         iv_module = iv_module
         iv_data = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
         iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).
  ENDMETHOD.

  METHOD fatal.
    log( iv_level = mo_level_helper->level_fatal( )
         iv_message = iv_message
         iv_module = iv_module
         iv_data = COND string( WHEN iv_data IS SUPPLIED THEN iv_data ELSE '' )
         iv_metadata = COND string( WHEN iv_metadata IS SUPPLIED THEN iv_metadata ELSE '' ) ).
  ENDMETHOD.

  METHOD add_log_target.
    mo_target_manager->add( io_target ).
  ENDMETHOD.

  METHOD remove_log_target.
    mo_target_manager->remove( io_target ).
  ENDMETHOD.

  METHOD set_log_level.
    mo_config->set_default_level( iv_level ).
  ENDMETHOD.

  METHOD set_log_level_by_module.
    mo_config->set_module_level( iv_module = iv_module iv_level = iv_level ).
  ENDMETHOD.

  METHOD get_effective_level.
    rv_level = mo_config->get_effective_level( iv_module ).
  ENDMETHOD.

  METHOD set_config.
    mo_config->update_from_config( is_config ).
  ENDMETHOD.

  METHOD get_config.
    rs_config = mo_config->to_config( ).
  ENDMETHOD.

  METHOD enable_async_logging.
    mo_config->enable_async( ).
  ENDMETHOD.

  METHOD disable_async_logging.
    mo_config->disable_async( ).
    flush( ).
  ENDMETHOD.

  METHOD flush.
    LOOP AT mo_async_queue->drain( ) INTO DATA(ls_entry).
      dispatch( ls_entry ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rotate_logs.
    mo_target_manager->rotate_all( ).
  ENDMETHOD.

  METHOD cleanup_logs.
    mo_target_manager->cleanup_all( iv_days ).
  ENDMETHOD.

  METHOD create_entry.
    rs_entry-timestamp = cl_abap_tstmp=>utc_now( ).
    rs_entry-level      = iv_level.
    rs_entry-module     = iv_module.
    rs_entry-message    = iv_message.
    rs_entry-data       = iv_data.
    rs_entry-metadata   = iv_metadata.
  ENDMETHOD.

  METHOD dispatch.
    mo_target_manager->dispatch( is_entry ).
  ENDMETHOD.

  METHOD enqueue_async.
    mo_async_queue->enqueue( is_entry ).
  ENDMETHOD.

  METHOD should_persist.
    DATA(lv_effective) = mo_config->get_effective_level( iv_module ).
    rv_is_allowed = mo_level_helper->is_level_allowed(
      iv_requested = iv_level
      iv_effective = lv_effective ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_zc_level_helper DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS level_debug RETURNING VALUE(rv_level) TYPE string.
    METHODS level_info RETURNING VALUE(rv_level) TYPE string.
    METHODS level_warning RETURNING VALUE(rv_level) TYPE string.
    METHODS level_error RETURNING VALUE(rv_level) TYPE string.
    METHODS level_fatal RETURNING VALUE(rv_level) TYPE string.
    METHODS is_level_allowed
      IMPORTING
        iv_requested TYPE string
        iv_effective TYPE string
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool.
  PRIVATE SECTION.
    METHODS level_order
      IMPORTING
        iv_level TYPE string
      RETURNING
        VALUE(rv_order) TYPE i.
ENDCLASS.

CLASS lcl_zc_level_helper IMPLEMENTATION.
  METHOD level_debug.
    rv_level = 'DEBUG'.
  ENDMETHOD.

  METHOD level_info.
    rv_level = 'INFO'.
  ENDMETHOD.

  METHOD level_warning.
    rv_level = 'WARNING'.
  ENDMETHOD.

  METHOD level_error.
    rv_level = 'ERROR'.
  ENDMETHOD.

  METHOD level_fatal.
    rv_level = 'FATAL'.
  ENDMETHOD.

  METHOD is_level_allowed.
    rv_allowed = xsdbool( level_order( iv_requested ) >= level_order( iv_effective ) ).
  ENDMETHOD.

  METHOD level_order.
    CASE iv_level.
      WHEN 'DEBUG'.
        rv_order = 1.
      WHEN 'INFO'.
        rv_order = 2.
      WHEN 'WARNING'.
        rv_order = 3.
      WHEN 'ERROR'.
        rv_order = 4.
      WHEN 'FATAL'.
        rv_order = 5.
      WHEN OTHERS.
        rv_order = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_zc_target_manager DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add
      IMPORTING
        io_target TYPE REF TO object.
    METHODS remove
      IMPORTING
        io_target TYPE REF TO object.
    METHODS dispatch
      IMPORTING
        is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry.
    METHODS rotate_all.
    METHODS cleanup_all
      IMPORTING
        iv_days TYPE i.
  PRIVATE SECTION.
    TYPES ty_targets TYPE STANDARD TABLE OF REF TO object WITH EMPTY KEY.
    DATA mt_targets TYPE ty_targets.
    METHODS call_if_exists
      IMPORTING
        io_target TYPE REF TO object
        iv_method TYPE string
        is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry OPTIONAL
        iv_days  TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_zc_target_manager IMPLEMENTATION.
  METHOD add.
    IF io_target IS NOT BOUND.
      RETURN.
    ENDIF.
    INSERT io_target INTO TABLE mt_targets.
  ENDMETHOD.

  METHOD remove.
    DELETE mt_targets WHERE table_line = io_target.
  ENDMETHOD.

  METHOD dispatch.
    LOOP AT mt_targets INTO DATA(lo_target).
      call_if_exists( io_target = lo_target iv_method = 'HANDLE_LOG' is_entry = is_entry ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rotate_all.
    LOOP AT mt_targets INTO DATA(lo_target).
      call_if_exists( io_target = lo_target iv_method = 'ROTATE' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD cleanup_all.
    LOOP AT mt_targets INTO DATA(lo_target).
      call_if_exists( io_target = lo_target iv_method = 'CLEANUP' iv_days = iv_days ).
    ENDLOOP.
  ENDMETHOD.

  METHOD call_if_exists.
    IF io_target IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_descr) = cl_abap_objectdescr=>describe_by_object_ref( io_target ).
    READ TABLE lo_descr->methods WITH TABLE KEY name = iv_method INTO DATA(ls_method).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE iv_method.
      WHEN 'HANDLE_LOG'.
        CALL METHOD io_target->('HANDLE_LOG')
          EXPORTING
            is_entry = is_entry.
      WHEN 'ROTATE'.
        CALL METHOD io_target->('ROTATE').
      WHEN 'CLEANUP'.
        CALL METHOD io_target->('CLEANUP')
          EXPORTING
            iv_days = iv_days.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_zc_config DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_default_level TYPE string
        iv_async_enabled TYPE abap_bool.
    METHODS set_default_level
      IMPORTING
        iv_level TYPE string.
    METHODS set_module_level
      IMPORTING
        iv_module TYPE string
        iv_level  TYPE string.
    METHODS get_effective_level
      IMPORTING
        iv_module TYPE string
      RETURNING
        VALUE(rv_level) TYPE string.
    METHODS update_from_config
      IMPORTING
        is_config TYPE zcl_zeroc_log_manager=>ty_config.
    METHODS to_config
      RETURNING
        VALUE(rs_config) TYPE zcl_zeroc_log_manager=>ty_config.
    METHODS enable_async.
    METHODS disable_async.
    METHODS is_async_enabled
      RETURNING
        VALUE(rv_enabled) TYPE abap_bool.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_module_level,
             module TYPE string,
             level  TYPE string,
           END OF ty_module_level.
    TYPES ty_module_levels TYPE STANDARD TABLE OF ty_module_level WITH EMPTY KEY.

    DATA mv_default_level TYPE string.
    DATA mt_module_levels TYPE ty_module_levels.
    DATA mv_async_enabled TYPE abap_bool.
ENDCLASS.

CLASS lcl_zc_config IMPLEMENTATION.
  METHOD constructor.
    mv_default_level = iv_default_level.
    mv_async_enabled = iv_async_enabled.
  ENDMETHOD.

  METHOD set_default_level.
    mv_default_level = iv_level.
  ENDMETHOD.

  METHOD set_module_level.
    DELETE mt_module_levels WHERE module = iv_module.
    INSERT VALUE #( module = iv_module level = iv_level ) INTO TABLE mt_module_levels.
  ENDMETHOD.

  METHOD get_effective_level.
    READ TABLE mt_module_levels WITH KEY module = iv_module INTO DATA(ls_module).
    IF sy-subrc = 0.
      rv_level = ls_module-level.
    ELSE.
      rv_level = mv_default_level.
    ENDIF.
  ENDMETHOD.

  METHOD update_from_config.
    IF is_config-default_level IS NOT INITIAL.
      mv_default_level = is_config-default_level.
    ENDIF.
    mv_async_enabled = is_config-async_mode.
  ENDMETHOD.

  METHOD to_config.
    rs_config-default_level = mv_default_level.
    rs_config-async_mode    = mv_async_enabled.
  ENDMETHOD.

  METHOD enable_async.
    mv_async_enabled = abap_true.
  ENDMETHOD.

  METHOD disable_async.
    mv_async_enabled = abap_false.
  ENDMETHOD.

  METHOD is_async_enabled.
    rv_enabled = mv_async_enabled.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_zc_async_queue DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS enqueue
      IMPORTING
        is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry.
    METHODS drain
      RETURNING
        VALUE(rt_entries) TYPE zcl_zeroc_log_manager=>ty_log_entries.
    METHODS is_empty
      RETURNING
        VALUE(rv_is_empty) TYPE abap_bool.
  PRIVATE SECTION.
    DATA mt_queue TYPE zcl_zeroc_log_manager=>ty_log_entries.
ENDCLASS.

CLASS lcl_zc_async_queue IMPLEMENTATION.
  METHOD enqueue.
    APPEND is_entry TO mt_queue.
  ENDMETHOD.

  METHOD drain.
    rt_entries = mt_queue.
    CLEAR mt_queue.
  ENDMETHOD.

  METHOD is_empty.
    rv_is_empty = xsdbool( mt_queue IS INITIAL ).
  ENDMETHOD.
ENDCLASS.
