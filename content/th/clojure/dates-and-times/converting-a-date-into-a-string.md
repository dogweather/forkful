---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:40.521488-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Clojure \u0E40\
  \u0E23\u0E32\u0E43\u0E0A\u0E49\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 Java\
  \ \u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\
  \u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E04\u0E39\u0E48\u0E21\u0E37\u0E2D\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
lastmod: '2024-03-17T21:57:55.819955-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Clojure \u0E40\u0E23\u0E32\u0E43\u0E0A\u0E49\u0E04\u0E27\u0E32\
  \u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19 Java \u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E04\u0E39\u0E48\
  \u0E21\u0E37\u0E2D\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\
  \u0E27."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
ใน Clojure เราใช้ความสามารถของการใช้งาน Java ร่วมกันเพื่อจัดรูปแบบวันที่ นี่คือคู่มืออย่างรวดเร็ว:

```clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

;; สร้างวัตถุวันที่ (ใช้วันที่และเวลาปัจจุบัน)
(def now (Date.))

;; ตั้งค่ารูปแบบที่ต้องการ
(def formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; จัดรูปแบบวันที่เป็นสตริง
(def formatted-date (.format formatter now))

;; พิมพ์ออกมา
(println formatted-date)
;; ผลลัพธ์อาจเป็น: "2023-03-15 09:26:45" (ขึ้นอยู่กับวันที่และเวลาปัจจุบัน)
```

## การศึกษาลึก
การแปลงวันที่เป็นสตริงไม่ได้จำกัดอยู่กับ Clojure เท่านั้น มันเป็นการดำเนินการทั่วไปในภาษาโปรแกรมหลายภาษา ในอดีตความต้องการนี้เกิดขึ้นเมื่อคอมพิวเตอร์เริ่มจัดการกับวันที่เพราะการมีตัวแทนที่มนุษย์สามารถอ่านได้ทำให้เข้าใจและสื่อสารได้ง่ายขึ้น ในขณะที่เครื่องจักรชอบรูปแบบข้อมูลที่มีโครงสร้างมากกว่า

ใน Clojure เพราะว่ามันทำงานบน Java Virtual Machine (JVM) เรามักใช้ไลบรารีวันที่และเวลาของ Java เช่น `java.util.Date` และ `java.text.SimpleDateFormat` แม้ว่าคลาสเหล่านี้จะมีมานานแล้ว แต่กระบวนการใหม่ `java.time` ที่ถูกนำมาใช้ใน Java 8 นั้นเป็นทางเลือกที่ดีขึ้นด้วยความปลอดภัยสำหรับเธรดและ API ที่ใช้งานง่ายกว่า

Clojure ไม่มีไลบรารีการจัดรูปแบบวันที่ที่เป็นส่วนหนึ่งของภาษาหลัก ดังนั้นมันจึงเป็นเรื่องปกติที่จะใช้การทำงานร่วมกับ Java หรือไลบรารีจากบุคคลที่สาม เช่น `clj-time` (เป็น wrapper ของ Joda Time) สำหรับการใช้งาน Clojure ที่ธรรมชาติมากขึ้น

นี่คือวิธีที่คุณอาจใช้ `java.time` เพื่อจัดรูปแบบ:

```clojure
(import java.time.LocalDateTime)
(import java.time.format.DateTimeFormatter)

;; สร้างวัตถุวันที่ (วันที่และเวลาปัจจุบัน)
(def now (LocalDateTime/now))

;; ตั้งค่ารูปแบบที่ต้องการ
(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

;; จัดรูปแบบวันที่เป็นสตริง
(def formatted-date (.format now formatter))

;; พิมพ์ออกมา
(println formatted-date)
;; ผลลัพธ์คล้ายกับก่อนหน้านี้ ใช่วันที่และเวลาปัจจุบัน
```

วิธีนี้หลีกเลี่ยงปัญหาเรื่องการเปลี่ยนแปลงที่เกิดขึ้นกับ SimpleDateFormat และควรจะได้รับการพิจารณาในโค้ดใหม่ที่ความปลอดภัยของเธรดเป็นเรื่องสำคัญ

## ดูเพิ่มเติม
- คู่มือวันที่และเวลาของ Java 8: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- ClojureDocs, คลังเอกสารและตัวอย่างที่ได้รับการสนับสนุนจากชุมชน: [https://clojuredocs.org/](https://clojuredocs.org/)
- clj-time, ไลบรารีวันที่และเวลาสำหรับ Clojure: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
