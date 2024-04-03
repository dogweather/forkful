---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:43.326833-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Clojure \u0E40\u0E1B\u0E47\
  \u0E19\u0E20\u0E32\u0E29\u0E32 JVM \u0E0B\u0E36\u0E48\u0E07\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 String \u0E02\u0E2D\u0E07 Java \u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\
  \u0E02\u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19 Clojure."
lastmod: '2024-03-17T21:57:55.787838-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32 JVM \u0E0B\u0E36\
  \u0E48\u0E07\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 String\
  \ \u0E02\u0E2D\u0E07 Java \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\
  \u0E0D\u0E48\u0E43\u0E19 Clojure."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
Clojure เป็นภาษา JVM ซึ่งช่วยให้คุณสามารถใช้เมธอด String ของ Java ได้โดยตรง นี่คือตัวอย่างพื้นฐานของวิธีการทำให้สตริงเป็นตัวพิมพ์ใหญ่ใน Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure ไม่ได้ประกอบด้วยฟังก์ชันที่กำหนดไว้เฉพาะสำหรับการทำให้สตริงเป็นตัวพิมพ์ใหญ่ แต่ดังที่แสดงไว้ คุณสามารถทำได้ง่ายๆ โดยการรวมใช้งานฟังก์ชัน `clojure.string/upper-case`, `subs`, และ `str` 

สำหรับโซลูชันที่ง่ายและตัวเลือกการจัดการสตริงที่ซับซ้อนมากขึ้น คุณอาจหันไปใช้ไลบรารีของบุคคลที่สาม ลิบรารีหนึ่งที่ได้รับความนิยมในระบบนิเวศของ Clojure คือ `clojure.string` อย่างไรก็ตาม ณ การอัปเดตครั้งสุดท้ายของฉัน มันไม่ได้เสนอฟังก์ชัน `capitalize` โดยตรงเกินกว่าสิ่งที่ถูกสาธิตด้วยความสามารถหลักของ Clojure ดังนั้นวิธีที่แสดงไว้ข้างต้นจึงเป็นวิธีที่ง่ายโดยตรงโดยไม่ต้องรวมไลบรารีเพิ่มเติมที่เฉพาะเจาะจงสำหรับการทำตัวพิมพ์ใหญ่

จำไว้ว่า เมื่อคุณทำงานกับสตริงใน Clojure ที่โต้ตอบกับเมธอดของ Java คุณกำลังทำงานกับสตริงของ Java ซึ่งช่วยให้คุณสามารถใช้ประโยชน์จากเมธอดสตริงของ Java ทั้งหมดได้โดยตรงในโค้ด Clojure ของคุณหากจำเป็น
