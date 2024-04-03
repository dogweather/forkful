---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:14.053253-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E43\u0E19 Haskell \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C, \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07 XML \u0E19\u0E31\u0E01\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E15\u0E49\u0E2D\u0E07\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A XML\u2026"
lastmod: '2024-03-17T21:57:56.292262-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E43\
  \u0E19 Haskell \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E01\
  \u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\
  , \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E30\u0E01\
  \u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07 XML \u0E19\u0E31\u0E01\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E15\u0E49\u0E2D\u0E07\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E01\u0E31\u0E1A XML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\
  \u0E1A\u0E01\u0E31\u0E1A\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\
  \u0E19\u0E41\u0E25\u0E30\u0E42\u0E1B\u0E23\u0E42\u0E15\u0E04\u0E2D\u0E25\u0E21\u0E32\
  \u0E01\u0E21\u0E32\u0E22\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49 XML \u0E40\u0E1B\u0E47\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\
  \u0E2D\u0E07\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32 \u0E40\u0E0A\u0E48\u0E19 \u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E27\u0E34\u0E2A\u0E41\u0E25\u0E30\
  \u0E44\u0E1F\u0E25\u0E4C\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไรและทำไม?

การทำงานกับ XML ใน Haskell ประกอบด้วยการแยกวิเคราะห์, การจัดการ, และการสร้างโครงสร้าง XML นักเขียนโปรแกรมต้องจัดการกับ XML เพื่อโต้ตอบกับแอปพลิเคชันและโปรโตคอลมากมายที่ใช้ XML เป็นรูปแบบข้อมูลของพวกเขา เช่น เว็บเซอร์วิสและไฟล์คอนฟิก

## วิธีการ:

Haskell เสนอไลบรารีเช่น `xml-conduit` สำหรับการจัดการกับ XML ตัวอย่างต่อไปนี้แสดงการแยกวิเคราะห์สตริง XML และการค้นหาองค์ประกอบ:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

ผลลัพธ์ตัวอย่าง:

```
["World!"]
```

## การศึกษาลึก

XML ย่อมาจาก eXtensible Markup Language, เป็นองค์ประกอบหลักในการอนุมานข้อมูลมานานก่อนที่ JSON จะเริ่มมีบทบาท. มันมีความเขียนยาว แต่มีความเข้มงวดและมาตรฐาน, ทำให้เหมาะกับสภาพแวดล้อมองค์กรที่เข้มงวด, ระบบสืบทอด, และอุตสาหกรรมเช่นการเงินและการดูแลสุขภาพ

Haskell มีไลบรารีสำหรับ XML หลายตัว; อย่างไรก็ตาม, `xml-conduit` ถือว่าเป็นหนึ่งในไลบรารีที่ทรงพลังและได้รับการใช้งานอย่างแพร่หลายเนื่องจากความสามารถในการสตรีมและแยกวิเคราะห์ที่มีประสิทธิภาพ, เป็นส่วนหนึ่งของครอบครัว `conduit` สำหรับการจัดการข้อมูลสตรีม

ทางเลือกอื่นๆ ได้แก่ `HXT` (Haskell XML Toolbox) ซึ่งใช้ arrows สำหรับการแยกวิเคราะห์และการแปลง, ให้กรอบความคิดที่แตกต่างสำหรับการจัดการ XML ถึงแม้ `HXT` จะมีความนิยมน้อยลงในขณะนี้เนื่องจากมีเส้นโค้งการเรียนรู้ที่ชัน, แต่ยังคงเป็นตัวเลือกที่แข็งแกร่งสำหรับบางกรณีการใช้งาน

เมื่อใช้งานการประมวลผล XML ใน Haskell, คุณต้องใส่ใจกับเรื่องการเข้ารหัส, เนื่องจากสตริงของ Haskell เป็น Unicode และข้อมูล XML อาจไม่เป็นเช่นนั้น นอกจากนี้, เนมสเปซ XML สามารถเพิ่มความซับซ้อนในการแยกวิเคราะห์ได้

## ดูเพิ่มเติมที่:

- เอกสารของแพ็คเกจ `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- หนังสือ "Real World Haskell", บทที่ 16, สำหรับการจัดการ XML: http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki เกี่ยวกับ XML: https://wiki.haskell.org/XML
