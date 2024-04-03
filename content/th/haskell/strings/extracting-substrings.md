---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:46.037787-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19 Haskell, \u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\u0E31\u0E14\u0E41\u0E25\u0E30\u0E41\
  \u0E22\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\
  \u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E41\u0E25\u0E49\u0E27\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E40\u0E0A\u0E48\u0E19 `take`, `drop`, \u0E41\u0E25\u0E30 `substring` (\u0E08\u0E32\
  \u0E01 `Data.Text`)."
lastmod: '2024-03-17T21:57:56.257451-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Haskell, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E15\u0E31\u0E14\u0E41\u0E25\u0E30\u0E41\u0E22\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E41\u0E25\u0E49\u0E27\u0E43\u0E19\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E0A\u0E48\u0E19 `take`, `drop`,\
  \ \u0E41\u0E25\u0E30 `substring` (\u0E08\u0E32\u0E01 `Data.Text`)."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีทำ:
ใน Haskell, คุณสามารถตัดและแยกสตริงด้วยฟังก์ชันที่มีอยู่แล้วในตัวอย่างเช่น `take`, `drop`, และ `substring` (จาก `Data.Text`)

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- ตัวอย่างสตริงของเรา
let exampleStr = "Haskell makes sense!"

-- การเอาอักขระ 7 ตัวแรก
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- การทิ้งอักขระ 8 ตัวแรก
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- ฟังก์ชันที่กำหนดเองเพื่อดึงส่วนย่อยของสตริงโดยตำแหน่งและความยาว
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- การดึง "makes" (เริ่มจากตำแหน่ง 8, ความยาว 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

ตัวอย่างผลลัพธ์:
```
"Haskell"
"makes sense!"
"makes"
```

## ทำความเข้าใจลึกลงไป
การวิเคราะห์ส่วนย่อยของสตริงเป็นส่วนหนึ่งของ Haskell มานานแล้ว ตั้งแต่แรกเริ่มมันพึ่งพาลิสต์ เนื่องจากสตริงเป็นลิสต์ของอักขระ ประสิทธิภาพไม่ดีเท่าไหร่ `Data.Text` มาพร้อมกับการทำงานกับสตริงอย่างมีประสิทธิภาพ

ทางเลือกอื่น ๆ มีลิสต์การดำเนินการ, regex และไลบรารีการแยกวิเคราะห์ การดำเนินการลิสต์ง่ายกว่าแต่ช้ากว่าสำหรับสตริงขนาดใหญ่ Regex มีประสิทธิภาพแต่เกินความจำเป็นสำหรับงานง่าย ๆ ไลบรารีพาร์สเหมาะสำหรับการวิเคราะห์ที่ซับซ้อนแต่ก็สามารถจัดการกับส่วนย่อยของสตริงได้เช่นกัน

การทำงานฟังก์ชันส่วนย่อยของสตริงที่กําหนดเองใน Haskell สามารถทำได้อย่างง่ายดายโดยใช้ `take` และ `drop` จาก `Data.Text` ที่ให้การจัดการสตริงที่เร็วกว่าการดำเนินการโดยอิงลิสต์

## ดูเพิ่มเติม
- เอกสารข้อมูลของโมดูล `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Learn You a Haskell for Great Good! เพื่อการเรียนรู้สตริง Haskell อย่างง่าย: http://learnyouahaskell.com/starting-out#immutability
- Real World Haskell สำหรับการใช้งานจริง: http://book.realworldhaskell.org/read/
- Haskell Wiki สำหรับข้อมูลจากชุมชน: https://wiki.haskell.org/How_to_work_with_strings
