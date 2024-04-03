---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:28.845075-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Clojure, \u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01,\
  \ \u0E04\u0E38\u0E13\u0E08\u0E30\u0E43\u0E0A\u0E49 function `clojure.string/lower-case`\
  \ \u0E14\u0E39\u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E07\u0E48\u0E32\u0E22\u0E41\
  \u0E04\u0E48\u0E44\u0E2B\u0E19."
lastmod: '2024-03-17T21:57:55.792301-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Clojure, \u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E40\u0E25\u0E47\u0E01, \u0E04\u0E38\u0E13\u0E08\u0E30\u0E43\u0E0A\u0E49\
  \ function `clojure.string/lower-case` \u0E14\u0E39\u0E27\u0E48\u0E32\u0E21\u0E31\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E41\u0E04\u0E48\u0E44\u0E2B\u0E19."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
ใน Clojure, การแปลงสตริงเป็นตัวพิมพ์เล็ก, คุณจะใช้ function `clojure.string/lower-case` ดูว่ามันง่ายแค่ไหน:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

ผลลัพธ์ที่ได้ออกมาชัดเจน:

```clojure
"hello, world!"
```

## ลงลึก
ในอดีต, การแปลง case ของข้อความได้ถูกนำมาใช้ตั้งแต่ยุคต้นของการคอมพิวเตอร์เพื่อทำให้การประมวลผลข้อมูลข้อความเป็นระเบียบเรียบร้อยมากขึ้น Clojure ฟังก์ชัน `clojure.string/lower-case` เป็นส่วนหนึ่งของไลบรารี `clojure.string` ซึ่งเป็นการรวบรวมเครื่องมือสำหรับการจัดการสตริงที่รวมอยู่ในภาษาหลัก

ทางเลือกอื่นสำหรับ `clojure.string/lower-case` รวมถึงการสร้างฟังก์ชันของคุณเองโดยการใช้ map กับการดัดแปลง `char` แต่สิ่งนี้เปรียบเสมือนการประดิษฐ์รถล้อใหม่เมื่อคุณมีฟังก์ชันในตัวที่ได้รับการเพิ่มประสิทธิภาพและทดสอบมาอย่างดี

ในภายใน, `clojure.string/lower-case` ใช้วิธี `toLowerCase` ของ Java เนื่องจาก Clojure ทำงานบน Java Virtual Machine (JVM) นี้รับประกันความเร็วสูงเนื่องจากใช้ประโยชน์จากไลบรารีของ Java ที่มีความเก๋ากาศ

## ดูเพิ่มเติม
- API `clojure.string` ของ Clojure: https://clojuredocs.org/clojure.string
- วิธีการ `String.toLowerCase()` ของ Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
