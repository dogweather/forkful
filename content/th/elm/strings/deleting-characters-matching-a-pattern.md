---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:06.330373-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\
  \u0E14\u0E0A\u0E38\u0E14\u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E15\
  \u0E32\u0E21\u0E01\u0E0E (\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A) \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.112764-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\
  \u0E14\u0E0A\u0E38\u0E14\u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E15\
  \u0E32\u0E21\u0E01\u0E0E (\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A) \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21,\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## อะไรและทำไม?
การลบอักขระที่ตรงกับรูปแบบหมายถึงการกำจัดชุดของอักขระที่เฉพาะเจาะจงออกจากข้อความ, ตามกฎ (รูปแบบ) นักพัฒนาทำเช่นนี้เพื่อการทำความสะอาดข้อความ, การประมวลผลข้อมูล, หรือเพื่อง่ายต่อการวิเคราะห์ข้อมูลนำเข้า.

## วิธีการ:
Elm ไม่รองรับ regex โดยตรง, แต่คุณสามารถจำลองการลบอักขระได้ นี่คือตัวอย่างการใช้ `String.filter` เพื่อลบตัวเลขออกจากสตริง

```Elm
import Browser
import Html exposing (text)

removeDigits : String -> String
removeDigits = String.filter (\char -> not (char >= '0' && char <= '9'))

main =
  text (removeDigits "Elm 0.19.1 is super 123 cool!")

-- ผลลัพธ์: "Elm . is super  cool!"
```

## การศึกษาเชิงลึก
Elm ขาด regex ในภาษาหลัก, แต่ต่างจากภาษาอื่นๆหลายตัว การเลือกใช้แบบนี้สอดคล้องกับเป้าหมายของ Elm ในการมีความเรียบง่ายและปลอดภัย Regex อาจมีข้อผิดพลาดและยากต่อการตรวจสอบข้อผิดพลาด, แต่ Elm สนับสนุนการดำเนินการสตริงที่ง่ายกว่าซึ่งครอบคลุมกรณีการใช้งานทั่วไปมากมาย

สำหรับกรณีที่ต้องการใช้ regex อย่างแท้จริง, การใช้งานต้องพึ่งพา JavaScript interop ผ่านทาง ports อย่างไรก็ตาม, Elm สนับสนุนให้หาวิธีการภายในภาษาก่อน โมดูล `String` ให้ฟังก์ชันต่างๆ เช่น `filter`, `replace`, และ `split` ซึ่งครอบคลุมการจัดการข้อความตามรูปแบบโดยไม่ต้องแนะนำความซับซ้อนของ regex

## ดูเพิ่มเติม
- [เอกสารข้อมูล Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Practical Elm for a Busy Developer](https://korban.net/elm/book/) - หนังสือที่รวมเครื่องมือการจัดการข้อความ
