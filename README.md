# ZeroCore
ABAP Dynamic Logger

## Proje Planı

SAP S/4HANA 1909 SP05 için hazırlanan Agile proje planı, mevcut ve planlanan özelliklerin detayları ile birlikte `docs/PROJECT_PLAN.md` dosyasında yer almaktadır.

# Log Kütüphanesi Metodları ve Kullanım Alanları

Bu döküman, log kütüphanesindeki temel metodları ve bu metodların nerelerde ve nasıl kullanılabileceğine dair örnek senaryoları içermektedir.

## 1. Log Kayıt Metodları

### `log(level, message, data)` (veya `log(level, message, data, metaData)`)

*   **İşlevi:** Temel log kaydetme metodu.
*   **Parametreler:**
    *   `level` (string): Log seviyesi (örneğin, `DEBUG`, `INFO`, `WARNING`, `ERROR`, `FATAL`).
    *   `message` (string): Log mesajı.
    *   `data` (any): Loglanacak ek veri (isteğe bağlı, JSON veya başka bir formatta).
    *   `metaData` (hash table - optional) Log mesajlarına meta veri ekleme imkanı (örneğin, timestamp, kullanıcı, işlem kodu vb.).
*   **Açıklama:** Tüm log seviyelerini desteklemelidir. Esnek veri ve meta veri ekleme imkanı sağlamalıdır.
*   **Kullanım Alanları:**
    *   Uygulamanın herhangi bir noktasında, önemli bir olay veya durum olduğunda kullanılır.
    *   Hata ayıklama, performans takibi, güvenlik denetimleri ve iş süreci takibi için idealdir.
    *   Belirli bir olayın veya adımın gerçekleştiğini belirtmek için.
*   **Örnek Senaryolar:**
    ```abap
    DATA: lo_logger TYPE REF TO zcl_zerocore_logger.
    CREATE OBJECT lo_logger.
    lo_logger->log( level   = 'INFO'
                    message = 'Kullanıcı giriş yapmaya çalışıyor'
                    data =  {'userName': 'testUser' }
                   metaData =  { 'tcode': 'SE38'}
                  ).
    ```
    ```abap
    TRY.
       "Veritabanı Bağlantı Kodları
      CATCH cx_sy_database.
         lo_logger->log( level = 'ERROR'
                        message = 'Veritabanı bağlantısı başarısız'
                        data = { 'errorCode': sy-subrc ,  'message':  cx_sy_database->get_text()}).
    ENDTRY.
    ```
    ```abap
     lo_logger->log( level = 'DEBUG'
                    message = 'Dosya okundu'
                    data = {'fileName': 'myData.txt', 'fileSize': 1024}).
    ```

### `debug(message, data)`, `info(message, data)`, `warning(message, data)`, `error(message, data)`, `fatal(message, data)`

*   **İşlevi:** Her log seviyesi için özel, kısaltılmış log metodları.
*   **Parametreler:**
    *   `message` (string): Log mesajı.
    *   `data` (any): Loglanacak ek veri (isteğe bağlı).
*   **Açıklama:** Kodu daha okunaklı hale getirmek için kullanılır.
*   **Kullanım Alanları:**
    *   `debug()`: Geliştirme aşamasında detaylı log kayıtları tutmak ve hata ayıklamak için.
    *   `info()`: Uygulamanın normal akışını takip etmek, kullanıcı hareketlerini izlemek için.
    *   `warning()`: Potansiyel sorunlar, beklenmeyen durumlar veya uyarılar için.
    *   `error()`: Uygulamada oluşan hataları kaydetmek için.
    *   `fatal()`: Uygulamanın kritik hatalar nedeniyle durması durumunda.
*   **Örnek Senaryolar:**
    ```abap
     lo_logger->debug( message = 'Değişken değeri: x=10' data = { 'x': 10 } ).
     lo_logger->info( message = 'Kullanıcı sipariş oluşturdu' data =  { 'orderId': '12345' } ).
     lo_logger->warning( message = 'Stok seviyesi düşük' data = {'material': 'A123' , 'stockLevel' : 5 } ).
     lo_logger->error( message = 'Yetkilendirme hatası' data =  { 'userName': 'testUser', 'permission': 'update' } ).
     lo_logger->fatal( message = 'Sistem kritik bir hata nedeniyle durdu' data = {'error': 'OutOfMemory'} ).
    ```

## 2. Log Kayıt Yönlendirme Metodları

### `addLogTarget(target)`

*   **İşlevi:** Yeni bir log hedefi ekler (örneğin, veritabanı, dosya, konsol).
*   **Parametreler:**
    *   `target` (object): Log hedefi nesnesi (arayüzü uygulayan bir sınıf).
*   **Açıklama:** Esnek log yönetimi sağlar. Birden fazla log hedefi desteklemelidir.
*   **Kullanım Alanları:**
    *   Uygulamanın loglarını farklı yerlere kaydetmek (örneğin, veritabanı, dosya, konsol).
    *   Test veya üretim ortamına göre farklı log hedefleri kullanmak.
    *   Log hedeflerini dinamik olarak eklemek veya kaldırmak.
*   **Örnek Senaryolar:**

    ```abap
       DATA: lo_db_target  TYPE REF TO  zcl_database_log_target.
       DATA: lo_file_target  TYPE REF TO  zcl_file_log_target.
       CREATE OBJECT lo_db_target.
       CREATE OBJECT lo_file_target.

       lo_logger->addLogTarget( target = lo_db_target ).  "Veritabanı hedefini ekle
       lo_logger->addLogTarget( target = lo_file_target ). "Dosya hedefini ekle

        "Log kayıtları buraya
        lo_logger->removeLogTarget( target = lo_db_target )."Veritabanı hedefini kaldır
    ```

### `removeLogTarget(target)`

*   **İşlevi:** Mevcut bir log hedefini kaldırır.
*   **Parametreler:**
    *   `target` (object): Log hedefi nesnesi.
*   **Açıklama:** Log hedeflerini dinamik olarak yönetmeye olanak tanır.

### `setLogLevel(level)`

*   **İşlevi:** Loglanacak minimum seviyeyi belirler.
*   **Parametreler:**
    *   `level` (string): Log seviyesi (`DEBUG`, `INFO`, `WARNING`, `ERROR`, `FATAL`).
*  **Açıklama:** Hangi log mesajlarının kaydedileceğini kontrol etmek için kullanılır.

*   **Kullanım Alanları:**
    *   Geliştirme aşamasında tüm log seviyelerini kaydetmek (DEBUG, INFO, WARNING, ERROR, FATAL).
    *   Üretim ortamında sadece kritik hataları kaydetmek (ERROR, FATAL).
*   **Örnek Senaryolar:**

    ```abap
     lo_logger->setLogLevel( level = 'ERROR' ). "Sadece ERROR seviyesinden yüksek loglar kaydet
    ```

### `setLogLevelByModule(module, level)`
*   **İşlevi:** Belirli bir modül veya uygulama parçası için log seviyesini belirler.
*   **Parametreler:**
    *   `module` (string): Modül veya uygulama parçasının adı.
    *   `level` (string): Log seviyesi (`DEBUG`, `INFO`, `WARNING`, `ERROR`, `FATAL`).
*   **Açıklama:** Farklı modüller için farklı log seviyeleri belirlemeyi sağlar.
*  **Kullanım Alanları:**
     *  Belirli modüller veya uygulama kısımları için farklı log seviyeleri ayarlamak.
*  **Örnek Senaryolar:**
     ```abap
     lo_logger->setLogLevelByModule( module = 'module_1'  level = 'DEBUG' ). "Modül 1 için detaylı loglama
     lo_logger->setLogLevelByModule( module = 'module_2' level = 'WARNING' )."Modül 2 için warning ve üzeri loglama
    ```

## 3. Log Yapılandırma Metodları

### `setConfig(config)`

*   **İşlevi:** Log kütüphanesinin yapılandırmasını ayarlar (örneğin, log formatı, dosya yolu, veritabanı bağlantısı).
*   **Parametreler:**
    *   `config` (object): Yapılandırma verilerini içeren nesne.
*   **Açıklama:** Farklı ihtiyaçlara göre kütüphaneyi özelleştirmeyi sağlar.
*   **Kullanım Alanları:**
    *   Log kütüphanesinin davranışını (örneğin, log formatı, dosya yolu, veritabanı bağlantısı) ayarlamak.
    *   Yapılandırma ayarlarını dinamik olarak değiştirmek veya görüntülemek.
*   **Örnek Senaryolar:**
    ```abap
    DATA ls_config  TYPE zcl_log_config.
        ls_config-log_format = 'JSON'.
        ls_config-file_path = '/tmp/myApp.log'.
        lo_logger->setConfig( config = ls_config ).
    ```

### `getConfig()`

*   **İşlevi:** Mevcut yapılandırma verilerini getirir.
*   **Açıklama:** Yapılandırma ayarlarını görüntüleme ve yönetme imkanı sunar.
*   **Kullanım Alanları:**
    * Yapılandırma ayarlarını görüntülemek veya farklı işlevlerde kullanmak.

  *  **Örnek Senaryolar:**
     ```abap
       DATA(read_config) = lo_logger->getConfig( ).
     ```

## 4. Log Temizleme ve Rotasyon Metodları

### `rotateLogs()`

*   **İşlevi:** Log dosyalarını rotasyon yapar (örneğin, günlük, haftalık, aylık).
*   **Açıklama:** Log dosyalarının yönetilebilir bir boyutta kalmasını sağlar.
*   **Kullanım Alanları:**
    *  Log dosyalarını belirli aralıklarla rotasyon yapmak.
  *   **Örnek Senaryolar:**
        ```abap
         lo_logger->rotateLogs( ).  "Log dosyalarını rotate et
        ```

### `cleanupLogs(days)`

*   **İşlevi:** Belirli bir süreden eski log kayıtlarını siler.
*   **Parametreler:**
    *   `days` (integer): Kaç gün öncesine kadar logların tutulacağı.
*   **Açıklama:** Log veritabanının büyümesini önler.
*   **Kullanım Alanları:**
    *  Log verilerini belirli bir süre sonra silerek depolama alanını yönetmek.
  *   **Örnek Senaryolar:**
    ```abap
        lo_logger->cleanupLogs( days = 30 ). "30 günden eski logları temizle
    ```

## 5. Ek Metodlar

### `flush()`

*   **İşlevi:** Bellekteki log kayıtlarını kalıcı ortama (örneğin, veritabanı, dosya) yazdırır.
*   **Açıklama:** Asenkron loglama durumunda, logların hemen kaydedilmesini sağlar.
*   **Kullanım Alanları:**
    *  Logları bellekte topladıktan sonra kalıcı olarak kaydetmek.
    *  Asenkron loglama kullanılıyorsa, logların hemen yazılmasını sağlamak.

*   **Örnek Senaryolar:**
  ```abap
    lo_logger->enableAsyncLogging( ).  "Asenkron loglamayı etkinleştir.
         lo_logger->info('Veriler işleniyor').
       lo_logger->flush( ).   "Logları bellekte tut ve yaz
       lo_logger->disableAsyncLogging( ). "Asenkron loglamayı kapat.
