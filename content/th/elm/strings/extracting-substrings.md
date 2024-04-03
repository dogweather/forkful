---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:28.177905-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\
  \u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\
  \u0E08\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\
  \u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\
  \u0E22\u0E01, \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E2B\u0E23\u0E37\u0E2D\u0E27\
  \u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\
  \u0E27\u0E19\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21"
lastmod: '2024-03-17T21:57:56.117485-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\
  \u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\
  \u0E08\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\
  \u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\
  \u0E22\u0E01, \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E2B\u0E23\u0E37\u0E2D\u0E27\
  \u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\
  \u0E27\u0E19\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไร & ทำไม?
การสกัดส่วนย่อยของข้อความหมายถึงการดึงส่วนที่เฉพาะเจาะจงออกมาจากสตริง โปรแกรมเมอร์ทำเช่นนี้เพื่อแยก, จัดการ, หรือวิเคราะห์ชิ้นส่วนของข้อมูลข้อความ

## วิธีการ:

Elm ทำให้มันง่าย สำหรับการเริ่มต้น มาใช้ `String.slice`:

```Elm
import String exposing (slice)

fullText : String
fullText = "Hello, Elm world!"

-- สกัด "Elm"
substring : String
substring = slice 7 10 fullText

-- ผลลัพธ์: "Elm"
```

ตอนนี้ มาทำให้มันมีความเคลื่อนไหวมากขึ้นด้วย `String.left` และ `String.right`:

```Elm
import String exposing (left, right)

-- หยิบ 5 ตัวอักษรแรก
leftString : String
leftString = left 5 fullText

-- ผลลัพธ์: "Hello"

-- หยิบ 5 ตัวอักษรสุดท้าย
rightString : String
rightString = right 5 fullText

-- ผลลัพธ์: "orld!"
```

## การศึกษาลึก

โดยประวัติศาสตร์การสกัดส่วนย่อยของข้อความนั้นเก่าเท่ากับการเขียนโปรแกรมเอง ใน Elm, เช่นเดียวกับภาษาฟังก์ชันอื่นๆ ฟังก์ชันสำหรับจัดการสตริงนั้นเป็น immutable - พวกมันส่งกลับสตริงใหม่แทนที่จะเปลี่ยนแปลงต้นฉบับ

มีทางเลือกอื่นๆ เช่น `String.dropLeft` และ `String.dropRight` มีอยู่ พวกมันตัดอักขระออกจากปลายใดปลายหนึ่งของสตริง:

```Elm
import String exposing (dropLeft, dropRight)

-- ตัดอักขระ 7 ตัวแรกออก
droppedLeftString : String
droppedLeftString = dropLeft 7 fullText

-- ผลลัพธ์: "Elm world!"

-- ตัดอักขระ 6 ตัวสุดท้ายออก
droppedRightString : String
droppedRightString = dropRight 6 fullText

-- ผลลัพธ์: "Hello, Elm"
```

ในแง่ของการดำเนินการ, ฟังก์ชันเหล่านี้ถูกสร้างเข้าไว้ในไลบรารีมาตรฐานของ Elm และสามารถจัดการกับ Unicode ได้, ถึงแม้ว่าจะมีข้อพิจารณาที่ต้องทำกับคู่สำรองของ Unicode และอักขระผสม

## ดูเพิ่มเติม

- เอกสารของโมดูล `String` ใน Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- คู่มือ Elm เกี่ยวกับสตริง: https://guide.elm-lang.org/strings/
- MDN Web Docs เกี่ยวกับ Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
