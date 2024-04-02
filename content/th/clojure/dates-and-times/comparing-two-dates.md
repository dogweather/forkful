---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:58.127137-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\
  \u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\
  \u0E2D\u0E07\u0E01\u0E31\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u2014\u0E27\
  \u0E31\u0E19\u0E43\u0E14\u0E2D\u0E22\u0E39\u0E48\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E19\
  \u0E49\u0E32, \u0E2B\u0E25\u0E31\u0E07, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E2B\u0E21\
  \u0E37\u0E2D\u0E19\u0E01\u0E31\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2A\u0E21\u0E1A\
  \u0E39\u0E23\u0E13\u0E4C?\u2026"
lastmod: '2024-03-17T21:57:55.820809-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\
  \u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\
  \u0E2D\u0E07\u0E01\u0E31\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u2014\u0E27\
  \u0E31\u0E19\u0E43\u0E14\u0E2D\u0E22\u0E39\u0E48\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E19\
  \u0E49\u0E32, \u0E2B\u0E25\u0E31\u0E07, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E2B\u0E21\
  \u0E37\u0E2D\u0E19\u0E01\u0E31\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2A\u0E21\u0E1A\
  \u0E39\u0E23\u0E13\u0E4C?\u2026"
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## อะไร & ทำไม?
การเปรียบเทียบวันที่หมายถึงการตรวจสอบว่าพวกมันเกี่ยวข้องกันอย่างไร—วันใดอยู่ก่อนหน้า, หลัง, หรือเหมือนกันอย่างสมบูรณ์? โปรแกรมเมอร์ทำสิ่งนี้เพื่อจัดการกับเส้นตาย, วางกำหนดการงาน, และติดตามข้อมูลที่เกี่ยวข้องกับเวลา

## วิธีการ:
Clojure ใช้ความสามารถในการร่วมมือกันของ Java เพื่อจัดการกับวันที่ มาพับแขนเสื้อขึ้นแล้วดำดิ่งเข้าไป:

```clojure
;; นำเข้าคลาส Date ของ Java
(import java.util.Date)

;; สร้างสองอินสแตนซ์ของวันที่
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; รอสักครู่
(def date2 (java.util.Date.))

;; เปรียบเทียบวันที่
(println (.before date1 date2)) ; true, date1 อยู่ก่อน date2
(println (.after date1 date2))  ; false, date1 ไม่อยู่หลังจาก date2
(println (.equals date1 date2)) ; false, date1 ไม่เหมือนกับ date2
```

ผลลัพธ์ตัวอย่างอาจดูแบบนี้ แต่มีเวลาที่แตกต่างกัน:

```
true
false
false
```

## การศึกษาลึก
ในอดีต นักพัฒนา Clojure มักใช้ `Date` ของ Java สำหรับการดำเนินการวันที่ โดยเรียกเมทอดต่าง ๆ ผ่านตัวดำเนินการจุดเหมือนที่เห็นก่อนหน้า ตัวเลือกทางเลือก ได้แก่ `clj-time`, ไลบรารี Clojure ที่ห่อหุ้ม Joda-Time

ตัวอย่างที่ใช้ `clj-time` จะดูเช่นนี้:

```clojure
;; เพิ่ม clj-time ลงใน dependency ของโปรเจกต์ของคุณ
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; สร้างสองอินสแตนซ์ของ date-time
(def date-time1 (time/now))
(Thread/sleep 1000) ;; รอหนึ่งวินาที
(def date-time2 (time/now))

;; เปรียบเทียบโดยใช้ฟังก์ชั่นของ clj-time
(println (time/before? date-time1 date-time2)) ; true
(println (time/after? date-time1 date-time2))  ; false
(println (time/equal? date-time1 date-time2))  ; false
```

มุมมองของ Clojure ต่อเวลาคือการใช้ประโยชน์จากไลบรารีของ Java ขณะที่ clj-time ร่วมมือกับ Joda-Time เพื่อสร้างประสบการณ์ Clojure ที่เหมาะสมมากยิ่งขึ้น

นับตั้งแต่ Java 8, `java.time` package—ซึ่งได้รับแรงบันดาลใจจาก Joda-Time—เป็นวิธีที่ต้องการในการจัดการกับวันที่และเวลาใน Java และโดยการขยาย, ใน Clojure ผ่าน interop การออกแบบที่ปรับปรุงและความสามารถเพิ่มเติมเช่นโซนเวลาทำให้ `java.time` เป็นตัวเลือกที่แข็งแกร่ง

## ดูเพิ่มเติม
- [Clojure's Java Interop](https://clojure.org/reference/java_interop)
- [คลังข้อมูล GitHub ของ clj-time](https://github.com/clj-time/clj-time)
- [คู่มือ API วันที่และเวลาของ Java](https://docs.oracle.com/javase/tutorial/datetime/)
