# ZeroCore ABAP Logger

S/4HANA 1909 SP05 üzerinde hızlı log yazmak için sadeleştirilmiş, nesne yönelimli bir altyapı.
Tek public sınıf (`ZCL_ZEROC_LOG_MANAGER`) üzerinden yönetilir, opsiyonel yardımcı hedefler özel `HANDLE_LOG`
metodu ile bağlanır.

## Proje Planı

Detaylı Agile proje planı ve teslimat durumları için `docs/PROJECT_PLAN.md` dosyasını inceleyin.
Tamamlanan işler plan üzerinde *Yapıldı* etiketi ile işaretlenmiştir.

## Hızlı Başlangıç

```abap
DATA(lo_logger) = NEW zcl_zeroc_log_manager( ).

lo_logger->log( |Merhaba ZeroCore!| ).
```

- Varsayılan seviye `INFO` ve konsol hedefi otomatik olarak kullanıma hazır gelir.
- Yalnızca mesaj vermek yeterlidir; ek bağlam veya payload parametresi zorunlu değildir.

## Metod Özeti

| Metod | Amaç |
| ----- | ---- |
| `log( iv_message, ir_payload, is_context )` | Mesajı ve isteğe bağlı veri/perspektif bilgisini kaydeder. |
| `add_target( io_target )` | `HANDLE_LOG` metodunu içeren özel bir hedef nesnesi ekler. |
| `clear_targets( )` | Tüm hedefleri temizler. |
| `register_module_defaults( is_context )` | Modül bazlı varsayılan seviye/öncelik/sahip bilgisi kaydeder. |
| `load_module_defaults( it_contexts )` | Birden fazla varsayılanı tek seferde yükler. |

### `log`

```abap
lo_logger->log( iv_message = 'Sadece mesaj' ).

DATA lt_sales TYPE STANDARD TABLE OF string WITH EMPTY KEY.
APPEND '4500000010' TO lt_sales.
APPEND '4500000011' TO lt_sales.

lo_logger->log(
  iv_message = 'SD sipariş listesi',
  ir_payload = REF #( lt_sales ),
  is_context = VALUE #( module = 'SD' level = 'ERROR' ) ).
```

- `ir_payload` herhangi bir tablo veya yapı referansı olabilir. İçerik otomatik olarak XML olarak serileştirilir.
- `is_context-module` için daha önce kayıtlı varsayılanlar bulunursa seviye/öncelik/sahip bilgisi otomatik tamamlanır.

### Hedef Eklemek

```abap
CLASS lcl_file_target DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS handle_log IMPORTING is_entry TYPE zcl_zeroc_log_manager=>ty_log_entry.
ENDCLASS.

CLASS lcl_file_target IMPLEMENTATION.
  METHOD handle_log.
    " Dosyaya yazma implementasyonu
  ENDMETHOD.
ENDCLASS.

lo_logger->add_target( NEW lcl_file_target( ) ).
```

`add_target` metoduna verilen nesnede `HANDLE_LOG` isimli bir metod bulunması yeterlidir.
Kayıt sırasında metodun varlığı doğrulanır.

### Modül Varsayılanları

Customizing tablolarından gelen bilgiler aşağıdaki yöntemle yüklenebilir:

```abap
DATA lt_defaults TYPE zcl_zeroc_log_manager=>ty_context_tab.

APPEND VALUE #( module = 'SD' level = 'WARN' priority = 'HIGH' owner = 'SD_CORE' ) TO lt_defaults.
APPEND VALUE #( module = 'FI' level = 'INFO' priority = 'MEDIUM' owner = 'FIN_TEAM' ) TO lt_defaults.

lo_logger->load_module_defaults( lt_defaults ).
```

`log` çağrısında ilgili modül belirtilirse, bu varsayılan bilgiler otomatik uygulanır ancak kullanıcı
aynı alanları isteğe bağlı olarak override edebilir.

## Örnek Program

`src/zpr_zeroc_log_demo.abap` raporu, mesaj + tablo/struct payload ve modül varsayılanı kullanımını konsol çıktısı üzerinde gösterir.

## Testler

ABAP kaynak kodu için otomatik test bulunmamaktadır. Kod, SAP GUI üzerinden rapor çalıştırılarak doğrulanabilir.
