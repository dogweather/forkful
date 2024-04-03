---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:14.014099-06:00
description: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48 Elm \u0E40\
  \u0E23\u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 Dictionaries, \u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E42\u0E22\u0E07\u0E04\u0E35\u0E22\u0E4C\
  \u0E01\u0E31\u0E1A\u0E04\u0E48\u0E32\u0E43\u0E19\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32,\
  \ \u0E40\u0E1E\u0E34\u0E48\u0E21, \u0E41\u0E25\u0E30\u0E25\u0E1A\u0E04\u0E48\u0E32\
  \u0E19\u0E31\u0E49\u0E19\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E21\u0E32\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.121403-06:00'
model: gpt-4-0125-preview
summary: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48 Elm \u0E40\
  \u0E23\u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 Dictionaries, \u0E17\u0E33\u0E01\u0E32\
  \u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E42\u0E22\u0E07\u0E04\u0E35\u0E22\u0E4C\
  \u0E01\u0E31\u0E1A\u0E04\u0E48\u0E32\u0E43\u0E19\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\
  \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32,\
  \ \u0E40\u0E1E\u0E34\u0E48\u0E21, \u0E41\u0E25\u0E30\u0E25\u0E1A\u0E04\u0E48\u0E32\
  \u0E19\u0E31\u0E49\u0E19\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E21\u0E32\u0E01 \u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E41\u0E23\u0E01\u0E46 \u0E40\
  \u0E21\u0E37\u0E48\u0E2D\u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\
  \u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E42\u0E14\
  \u0E22\u0E44\u0E21\u0E48\u0E21\u0E35\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E17\u0E35\u0E48\
  \u0E40\u0E02\u0E49\u0E21\u0E07\u0E27\u0E14 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\
  \u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07\u0E1C\u0E39\u0E49\
  \u0E43\u0E0A\u0E49\u0E2B\u0E23\u0E37\u0E2D\u0E23\u0E32\u0E22\u0E01\u0E32\u0E23\u0E2A\
  \u0E34\u0E19\u0E04\u0E49\u0E32\u0E04\u0E07\u0E04\u0E25\u0E31\u0E07."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## วิธีการ:
ใน Elm, คุณทำงานกับ Dictionaries ในโมดูล `Dict`, ดังนั้นเรามาดูตัวอย่างอย่างรวดเร็วกัน:

```Elm
import Dict exposing (Dict)

-- การเริ่มต้น dictionary ที่มีคีย์เป็น String และค่าเป็น Int
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- การเพิ่มหรืออัพเดทค่า
updatedDict = Dict.insert "grape" 10 exampleDict

-- การดึงค่า (สังเกตุที่ใช้ประเภท Maybe, เนื่องจากคีย์อาจจะไม่มีอยู่)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- การลบคู่คีย์-ค่า
finalDict = Dict.remove "banana" updatedDict

-- การแปลง dictionary กลับไปเป็นลิสต์
dictToList = Dict.toList finalDict
```

ผลลัพธ์ตัวอย่างเมื่อแสดง `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

นี่แสดงถึงการดำเนินการพื้นฐาน: การสร้าง, อัพเดท, เข้าถึง, และการทำซ้ำผ่าน Dictionary.

## การศึกษาลึก
Dictionaries ใน Elm ภายในใช้โครงสร้างที่เรียกว่า AVL tree - ประเภทหนึ่งของ binary search tree ที่สามารถทำการสมดุลตัวเองได้ การเลือกใช้โครงสร้างนี้ช่วยให้การดำเนินการเช่น insert, get, และ remove มีประสิทธิภาพดี (ความซับซ้อนด้านเวลาเป็นลอการิทึม) และรักษาความง่ายในการจัดการข้อมูล

แม้ว่า `Dict` ของ Elm มีจุดแข็ง แต่ก็ไม่ใช่วิธีแก้ปัญหาที่เหมาะสมสำหรับทุกสถานการณ์ สำหรับคอลเลกชั่นที่มีการจัดเรียงหรือต้องการทำซ้ำเป็นลำดับ, List หรือ Array อาจจะเหมาะสมกว่า นอกจากนี้, เมื่อทำงานกับชุดของคีย์ที่แน่นอนที่รู้จัก, การใช้ประเภทที่กำหนดเอง (รุ่นของ enums ใน Elm) อาจจะเสนอความปลอดภัยของประเภทและความชัดเจนในโค้ดของคุณมากขึ้น

ในระบบของ Elm, `Dict` เสนอวิธีที่น่าเชื่อถือในการจัดการคอลเลกชั่นของคู่คีย์-ค่าที่คีย์เป็นเอกลักษณ์และลำดับไม่สำคัญ ในขณะที่อาจมีโครงสร้างใหม่ๆ หรือโครงสร้างที่ซับซ้อนขึ้นเกิดขึ้น, โมดูล `Dict` ยังคงเป็นเครื่องมือหลักในชุดเครื่องมือของโปรแกรมเมอร์ Elm เนื่องจากความง่ายและประสิทธิภาพในการจัดการ associative arrays
