---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:48.006839-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Elm, \u0E04\
  \u0E38\u0E13\u0E43\u0E0A\u0E49 `String.length` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2B\
  \u0E32\u0E27\u0E48\u0E32\u0E21\u0E35\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E01\u0E35\u0E48\u0E15\u0E31\u0E27\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E1E\
  \u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E14\u0E39."
lastmod: '2024-03-17T21:57:56.119389-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Elm, \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49 `String.length` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E21\u0E35\u0E15\u0E31\u0E27\
  \u0E2D\u0E31\u0E01\u0E29\u0E23\u0E01\u0E35\u0E48\u0E15\u0E31\u0E27\u0E43\u0E19\u0E2A\
  \u0E15\u0E23\u0E34\u0E07 \u0E1E\u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E14\u0E39."
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 7
---

## วิธีการ:
ใน Elm, คุณใช้ `String.length` เพื่อหาว่ามีตัวอักษรกี่ตัวในสตริง พิจารณาดู:

```elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Hello, Elm!"))
  -- ผลลัพธ์: "11"
```

## ศึกษาลึก
ในอดีต, ฟังก์ชันวัดความยาวสตริงมีความสำคัญสำหรับการจัดการหน่วยความจำและการประมวลผลข้อความในภาษาที่มีการเข้าถึงข้อมูลระดับต่ำ Elm, ที่เป็นภาษาระดับสูง, ทำการซ่อนรายละเอียดเหล่านี้ไว้, และมีฟังก์ชันที่ให้มาพร้อมกับ `String.length`

มีสองประเด็นที่ควรสังเกต:
1. สตริงใน Elm ถูกเข้ารหัสด้วย UTF-16 `String.length` จะคืนค่าจำนวนหน่วยของรหัส UTF-16, ซึ่งอาจต่างจากจำนวน graphemes ของ Unicode (ตัวอักษรที่ผู้ใช้รับรู้) ในสตริงที่มีตัวอักษรซับซ้อน
2. ไม่มีทางเลือกอื่นนอกเหนือจาก `String.length` ใน Elm ถ้าคุณต้องการจำนวน graphemes, คุณอาจต้องใช้ฟังก์ชันที่กำหนดเองซึ่งคำนึงถึงความซับซ้อนของ Unicode

โดยภายใน, `String.length` จะวนลูปผ่านโครงสร้างข้อมูลสตริง, นับองค์ประกอบ ในฐานะฟังก์ชันที่บริสุทธิ์, ผลลัพธ์ของมันขึ้นอยู่กับข้อมูลที่ป้อนเข้ามาอย่างเดียว, รักษาอุดมการณ์การเขียนโปรแกรมแบบฟังก์ชันของ Elm

## ดูเพิ่มเติม
- เอกสาร `String` อย่างเป็นทางการของ Elm: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- UTF-16: [https://en.wikipedia.org/wiki/UTF-16](https://en.wikipedia.org/wiki/UTF-16)
