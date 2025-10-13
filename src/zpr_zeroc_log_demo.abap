REPORT zpr_zeroc_log_demo.

"----------------------------------------------------------------------
" Konsol çıktısı üzerinden ZeroC log yöneticisinin örnek kullanımı.
"----------------------------------------------------------------------
CLASS lcl_console_target DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS handle_log
      IMPORTING
        is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry.
    METHODS rotate.
    METHODS cleanup
      IMPORTING
        iv_days TYPE i.
ENDCLASS.

CLASS lcl_console_target IMPLEMENTATION.
  METHOD handle_log.
    WRITE: / |[{ is_entry-timestamp }] { is_entry-level } ({ is_entry-module }): { is_entry-message }|.

    IF is_entry-data IS NOT INITIAL.
      WRITE: / |  Data     : { is_entry-data }|.
    ENDIF.

    IF is_entry-metadata IS NOT INITIAL.
      WRITE: / |  Metadata : { is_entry-metadata }|.
    ENDIF.
  ENDMETHOD.

  METHOD rotate.
    WRITE: / 'Rotate invoked for console target.'.
  ENDMETHOD.

  METHOD cleanup.
    WRITE: / |Cleanup invoked (days = { iv_days }).|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_logger) = NEW zcl_zeroc_log_manager( iv_default_level = 'INFO' ).
  DATA(lo_target) = NEW lcl_console_target( ).

  lo_logger->add_log_target( lo_target ).

  " INFO seviyesi varsayılan olduğu için DEBUG mesajı filtrelenecek.
  lo_logger->debug(
    iv_message = 'Bu satır gösterilmeyecek (seviyenin altında)'
    iv_module  = 'DEMO'
    iv_metadata = 'initial state' ).

  lo_logger->info(
    iv_message = 'ZeroC log yöneticisi demoya başladı'
    iv_module  = 'DEMO' ).

  lo_logger->set_log_level( 'DEBUG' ).

  lo_logger->debug(
    iv_message = 'Debug modu aktif'
    iv_module  = 'DEMO'
    iv_data    = 'payload=42'
    iv_metadata = 'step=1' ).

  lo_logger->set_log_level_by_module(
    iv_module = 'BATCH'
    iv_level  = 'ERROR' ).

  lo_logger->warning(
    iv_message = 'Batch modülü için uyarı mesajı'
    iv_module  = 'BATCH'
    iv_metadata = 'threshold=low' ).

  lo_logger->error(
    iv_message = 'Batch modülü hata verdi'
    iv_module  = 'BATCH'
    iv_data    = 'job=ZBATCH01'
    iv_metadata = 'severity=high' ).

  lo_logger->enable_async_logging( ).
  lo_logger->info(
    iv_message = 'Asenkron kuyruğa giden kayıt'
    iv_module  = 'DEMO'
    iv_metadata = 'step=2' ).

  lo_logger->flush( ).

  lo_logger->rotate_logs( ).
  lo_logger->cleanup_logs( iv_days = 14 ).

  lo_logger->remove_log_target( lo_target ).

  WRITE: / 'Demo tamamlandı.'.
