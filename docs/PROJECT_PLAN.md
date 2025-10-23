# ZeroCore SAP S/4HANA 1909 SP05 Uyumlu Proje Planı

## 1. Bağlam ve Amaç
- **Platform:** SAP S/4HANA 1909 SP05 (ABAP Platform 1909).
- **Çözüm:** ZeroCore dinamik log kütüphanesinin kurumsal standartlara uygun hale getirilmesi ve bakım ekranları ile yönetilebilir kılınması.
- **Hedef:** Geliştirilebilir, temiz kod prensiplerine uygun, basit fakat nesne yönelimli (OOP) loglama altyapısı ve ilgili bakım araçlarını teslim etmek.

## 2. Varsayımlar ve Kısıtlar
- Kodlama dili ABAP, hedef sistem SAP S/4HANA 1909 SP05.
- Tüm sınıflar constructor injection ve bağımlılık tersine çevirmeyi desteklemeli; gereksiz `SET` metodları tanımlanmayacak.
- Log hedefleri ve konfigürasyonları customizing tabloları üzerinden bakım ekranları ile yönetilecek.
- Sistem performansı için log yazımı asenkron opsiyonu desteklenecek, fakat senkron mod da korunacak.

## 3. Mevcut Özellikler
Güncel sınıf tasarımına göre kütüphanede aşağıdaki yetkinlikler hazırdır:
- `log` metoduyla tek adımda mesaj + isteğe bağlı veri referansı gönderme.
- Modül bazlı varsayılan seviye/öncelik/sahip bilgisi tutma ve otomatik birleştirme.
- `HANDLE_LOG` metoduna sahip hedef nesneleri dinamik olarak ekleyip konsol hedefini varsayılan olarak kullanma.
- Payload verisini (tablo/struct) otomatik XML metnine dönüştürme.
- Agile planı içerisinde tamamlanan işleri proje planında *Yapıldı* olarak işaretleme.

## 4. Eklenmesi Planlanan Özellikler
- **ZCL_ZEROC_LOG_MANAGER:** Log kütüphanesi çekirdeğinin SAP S/4HANA 1909 SP05 için yeniden düzenlenmesi (temiz kod, minimal ve anlaşılır OOP). *(Yapıldı)*
- **ZPR_ZEROC_LOG_DEMO:** Logger sınıfının örnek kullanımını gösteren ABAP OO raporu. *(Yapıldı)*
- **ZIF_ZEROC_LOG_TARGET** arayüzü ile uyumlu standart hedef sınıfları (DB, Application Log, e-posta).
- **Customizing tabloları:**
  - ZEROC_CFG: Global log ayarları (default seviye, async modu, rotasyon periyodu).
  - ZEROC_MOD_CFG: Modül bazlı log seviyeleri.
  - ZEROC_TARGET: Aktif log hedefleri ve parametreleri.
- **Bakım ekranları (SM30 / Fiori Elements uyumlu OData servisleri):**
  - Log genel yapılandırması.
  - Modül bazlı seviye tanımları.
  - Log hedefi tanımları ve bağlantı parametreleri.
- **Denetim & İzleme:**
  - Güncel log kuyruğu durumunu izleyen rapor (ABAP OO raporu).
  - Temizlik/rotasyon işlerini tetikleyen background job örnekleri.
- **Test sınıfları:** ABAP Unit ile temel fonksiyonların doğrulanması.

## 5. Agile Yol Haritası
### 5.1 Sprint Genel Bakışı (her biri 2 hafta)
1. **Sprint 1 – Çekirdek Yeniden Yapılandırma**
   - Kütüphanenin S/4HANA 1909 uyumlu sınıf ve arayüzlerinin oluşturulması. *(Yapıldı)*
   - Log seviyeleri ve mesaj işleme akışının sadeleştirilmesi. *(Yapıldı)*
   - ABAP Unit ile çekirdek fonksiyon testlerinin hazırlanması.

2. **Sprint 2 – Customizing ve Bakım Ekranları**
   - ZEROC_CFG, ZEROC_MOD_CFG, ZEROC_TARGET tablolarının tasarımı ve aktivasyonu.
   - Bakım view'larının ve SM30 üzerinden erişilebilir ekranların oluşturulması.
   - Fiori Elements tabanlı OData servis taslaklarının hazırlanması (isteğe bağlı kapsam).

3. **Sprint 3 – Log Hedefleri & Operasyonel Araçlar**
   - Standart hedef sınıflarının (DB, Application Log, e-posta) uygulanması.
   - Kuyruk izleme raporu ve background job senaryolarının teslimi.
   - Performans ve yük testleri, dokümantasyon tamamlanması.

### 5.2 Ürün Backlogu (Epic -> Feature -> User Story)
- **EPIC 1: Çekirdek Loglama**
  - F1: Logger sınıfı yeniden yazımı. *(Yapıldı)*
    - US1: "Geliştirici olarak constructor ile log hedeflerini enjekte etmek istiyorum ki kod bağımlılığı minimum olsun."
    - US2: "Geliştirici olarak log seviyesini modül bazlı yapılandırmak istiyorum ki gereksiz kayıt oluşturmayayım."
- **EPIC 2: Yönetim & Customizing**
  - F2: Customizing tabloları.
    - US3: "Operasyon ekibi olarak log ayarlarını SM30'dan güncellemek istiyorum."
  - F3: Fiori bakım uygulaması (opsiyonel).
    - US4: "Fiori kullanıcısı olarak log hedeflerini UI üzerinden yönetmek istiyorum."
- **EPIC 3: Operasyon & İzleme**
  - F4: İzleme raporu.
    - US5: "Destek analisti olarak bekleyen log kayıtlarını görebilmek istiyorum."
  - F5: Otomasyon job'ları.
    - US6: "Basis uzmanı olarak log temizliği için periyodik job planlayabilmek istiyorum."

### 5.3 Tanım-açıklık (Definition of Ready / Done)
- **Definition of Ready:**
  - İş kuralları ve veri kaynakları netleştirilmiş.
  - Gerekli tablolar ve alanlar analiz edilmiş.
  - Bağımlılıklar (diğer takım/sistem) çözülmüş.
- **Definition of Done:**
  - ABAP kodu ATC/Code Inspector'dan hatasız geçer.
  - ABAP Unit testleri %80+ temel fonksiyon kapsamına sahip.
  - Dokümantasyon (teknik + kullanıcı) güncel.
  - Transport istekleri oluşturulmuş ve QA'ya taşınmaya hazır.

## 6. Mimari ve Kod Standartları
- Sınıf isimlendirmeleri `ZCL_ZEROC_*`, arayüzler `ZIF_ZEROC_*`, veri yapıları `ZSTR_`/`ZTY_` öneki kullanacak.
- Nesne oluşturma `CREATE OBJECT ... EXPORTING` yerine mümkünse `NEW` sintaksı ile yapılacak.
- `SET` metodları tanımlanmadan constructor parametreleri veya factory metodları kullanılacak.
- Yöntemlerin sorumluluğu tek olacak (SRP), metod uzunlukları kısa tutulacak.
- Hata yönetimi için `CX_ZEROC_*` türetilmiş istisna sınıfları kullanılacak.
- Kod okunabilirliği için Inline deklarasyonlar (`DATA(...)`) ve temiz yorumlar.

## 7. Riskler ve Azaltıcı Önlemler
- **Uyumluluk Riskleri:** 1909 SP05 spesifik API değişiklikleri -> SAP Notes taraması, sandbox testleri.
- **Performans:** Yüksek hacimli loglarda performans düşüşü -> Asenkron mod, arka plan job optimizasyonu.
- **Bakım Ekranı Kullanılabilirliği:** Son kullanıcı testi ile geri bildirim döngüleri.

## 8. Çıktılar ve Dokümantasyon
- Teknik tasarım dokümanı (TDD).
- Kullanıcı dokümanı (bakım ekranı kullanım kılavuzu).
- ABAP Unit sonuç raporu.
- Transport listesi ve görev logu.

## 9. Agile Seremoni ve Roller
- **Product Owner:** İş gereksinimlerini önceliklendirir, backlog'u yönetir.
- **Scrum Master:** Engel kaldırma, süreç fasilitasyonu.
- **Geliştirici(ler):** ABAP geliştirme, birim testleri, kod inceleme.
- **QA / Test Uzmanı:** Test senaryoları, entegrasyon testleri.
- **Seremoniler:**
  - Sprint Planlama (her sprint başı, 2 saat).
  - Günlük Scrum (15 dk).
  - Sprint Review & Retrospective (her sprint sonu, 2 saat toplam).

## 10. Takip ve Raporlama
- Jira/Charm üzerinde user story ve task takipleri.
- Her sprint sonunda burndown chart değerlendirmesi.
- Operasyon ve destek ekipleriyle aylık paydaş toplantıları.
