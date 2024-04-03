---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:35.870226-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\
  \u0E34\u0E19\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E42\u0E1E\u0E40\u0E25\u0E0A\u0E31\u0E48\
  \u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E40\u0E23\u0E32\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01 \u0E17\
  \u0E33\u0E44\u0E21? \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E01\
  \u2014\u0E0B\u0E36\u0E48\u0E07\u0E2A\u0E30\u0E14\u0E27\u0E01\u0E01\u0E27\u0E48\u0E32\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\
  \u0E21\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E1A\u0E1A\u0E40\u0E01\u0E48\u0E32\u0E46"
lastmod: '2024-03-17T21:57:55.791322-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\
  \u0E34\u0E19\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E42\u0E1E\u0E40\u0E25\u0E0A\u0E31\u0E48\
  \u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E40\u0E23\u0E32\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\
  \u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01 \u0E17\
  \u0E33\u0E44\u0E21."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีทำ:
```Clojure
;; พื้นฐานด้วย `str` และ `format`
(def name "World")
(str "Hello, " name "!")  ; => "Hello, World!"

;; การใช้ `format`, คล้ายกับการจัดรูปแบบแบบ printf
(format "Goodbye, %s!" name)  ; => "Goodbye, World!"

;; Clojure ไม่มีการอินเตอร์โพเลชั่นสตริงเข้ามาในตัวอย่างภาษาอื่น,
;; แต่เราสามารถใช้ `str` และ `format` อย่างครีเอทีฟได้.
```

## การดำดิ่งลงไป:
Clojure เป็นภาษาที่เรียบง่าย: ไม่มีการอินเตอร์โพเลชั่นสตริงเข้ามาในตัว. อย่างไรก็ตาม, `str` และ `format` เป็นตัวเลือกในการสร้างสตริงไดนามิก. ที่มาของมัน? จริยธรรมของความเรียบง่ายของ Clojure มันไว้ใจว่าเราสามารถจัดการการสร้างสตริงด้วยตัวเอง. 

สำหรับทางเลือกอื่น, เข้าสู่โลกของการทำ templating: `clostache` (การใช้ Clojure กับ Mustache) หรือ `hiccup` สำหรับบริบท HTML. พวกเขามาสะดวกเมื่อ `str` และ `format` ดูเรียบง่ายเกินไป.

ภายใต้ผ้าม่าน, `format` มอบหมายงานให้กับ `String.format` ของ Java, ข้อเท็จจริงนี้สะท้อนถึงความสามารถในการทำงานร่วมกันกับ Java ของ Clojure. ดังนั้น, แม้คุณจะไม่ได้รับความหวานนั้น, คุณมีพลังของ Java เมื่อคุณต้องการ.

## ดูเพิ่มเติม:
- เอกสาร Clojure เกี่ยวกับ `str`: https://clojuredocs.org/clojure.core/str
- เอกสาร Clojure เกี่ยวกับ `format`: https://clojuredocs.org/clojure.core/format
- คลังข้อมูล clostache ใน GitHub: https://github.com/fhd/clostache
- คลังข้อมูล hiccup ใน GitHub: https://github.com/weavejester/hiccup
