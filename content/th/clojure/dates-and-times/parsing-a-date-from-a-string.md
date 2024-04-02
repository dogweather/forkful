---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:56.586899-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Clojure \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E43\u0E2B\
  \u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E21\u0E32\u0E01\u0E02\u0E36\
  \u0E49\u0E19 (\u0E40\u0E0A\u0E48\u0E19 \u0E27\u0E31\u0E15\u0E16\u0E38 DateTime \u0E02\
  \u0E2D\u0E07 Clojure)\u2026"
lastmod: '2024-03-17T21:57:55.817802-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Clojure \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E43\u0E2B\
  \u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E21\u0E32\u0E01\u0E02\u0E36\
  \u0E49\u0E19 (\u0E40\u0E0A\u0E48\u0E19 \u0E27\u0E31\u0E15\u0E16\u0E38 DateTime \u0E02\
  \u0E2D\u0E07 Clojure)\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## อะไรและทำไม?
การแยกวิเคราะห์วันที่จากสตริงใน Clojure เป็นเรื่องของการแปลงข้อความที่แสดงวันที่และเวลาให้เป็นรูปแบบที่ใช้งานได้มากขึ้น (เช่น วัตถุ DateTime ของ Clojure) กระบวนการนี้มีความสำคัญสำหรับการประมวลผลข้อมูล, การบันทึก, หรือแอปพลิเคชันใด ๆ ที่จัดการกับข้อมูลเชิงเวลา, ช่วยให้โปรแกรมเมอร์สามารถทำงาน, เปรียบเทียบ หรือจัดการกับวันที่ได้อย่างมีประสิทธิภาพ

## วิธีการ:
Clojure เป็นภาษาของ JVM ทำให้คุณสามารถใช้ไลบรารีวันที่และเวลาของ Java โดยตรง มาเริ่มต้นกับการใช้งาน Java interoperation ที่มีอยู่และจากนั้นสำรวจวิธีใช้งานไลบรารีของบุคคลที่สามที่นิยม, clj-time, เพื่อแนวทางการทำงานกับ Clojure ที่เข้าใจง่ายขึ้น

### การใช้งาน Java Interop
Clojure สามารถใช้ Java `java.time.LocalDate` ในการแปลงวันที่จากสตริงโดยตรง:
```clojure
(require '[clojure.java.io :as io])

; การแปลงวันที่โดยใช้ Java interop
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; ผลลัพธ์: 2023-04-01
```

### การใช้ clj-time
ไลบรารี Clojure ที่เข้าใจง่ายกว่าสำหรับการจัดการวันที่และเวลาคือ `clj-time` มันห่อหุ้ม Joda-Time, ไลบรารีที่ครอบคลุมสำหรับการดำเนินการวันที่และเวลา คุณต้องเพิ่ม `clj-time` ไปยัง dependencies ของโปรเจ็กต์ของคุณเป็นอันดับแรก นี่คือวิธีการแปลงสตริงวันที่โดยใช้ `clj-time`:

```clojure
; อย่าลืมเพิ่ม [clj-time "0.15.2"] ลงใน project.clj ของคุณภายใต้ :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; กำหนดตัวจัดรูปแบบ
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; ผลลัพธ์: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

ตัวอย่างเหล่านี้แสดงการวิเคราะห์วันที่พื้นฐาน ทั้งสองวิธีนี้มีประโยชน์ แต่ `clj-time` สามารถให้แนวทางที่เน้น Clojure มากขึ้นพร้อมกับฟังก์ชันการทำงานเพิ่มเติมสำหรับความต้องการที่ซับซ้อน
