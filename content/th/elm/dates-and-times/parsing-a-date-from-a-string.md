---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:02.042364-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C (Parsing) \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Elm \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\
  \u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\
  \u0E25\u0E32\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48 Elm \u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.138334-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C (Parsing) \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 Elm \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\
  \u0E16\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\
  \u0E25\u0E32\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E17\u0E35\u0E48 Elm \u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## อะไรและทำไม?
การแยกวิเคราะห์ (Parsing) วันที่จากสตริงใน Elm หมายถึงการแปลงข้อมูลข้อความที่แสดงถึงวันที่และเวลาให้เป็นรูปแบบที่ Elm เข้าใจและจัดการได้ โดยเฉพาะอย่างยิ่งให้เป็นประเภท `Date` กระบวนการนี้มีความสำคัญสำหรับการจัดการข้อมูลที่ผู้ใช้ป้อนเข้ามา การแสดงวันที่ให้ถูกต้องตามท้องถิ่น และการคำนวณที่เกี่ยวข้องกับวันที่ โดยตรวจสอบให้แน่ใจว่าแอปพลิเคชัน Elm ของคุณสามารถประมวลผลข้อมูลเชิงเวลาได้อย่างชาญฉลาด

## วิธีการ:
Elm ไม่มีความสามารถภายในตัวที่แข็งแกร่งเท่ากับภาษาอื่น ๆ สำหรับการแยกวิเคราะห์วันที่ โดยพึ่งพาการทำงานร่วมกับ Javascript หรือไลบรารีสำหรับการดำเนินการที่ซับซ้อนยิ่งขึ้น อย่างไรก็ตาม คุณสามารถใช้แพ็กเกจ `elm/time` สำหรับการแยกวิเคราะห์พื้นฐานได้ และสำหรับความต้องการที่ซับซ้อนยิ่งขึ้น ไลบรารีของบุคคลที่สามอย่าง `justinmimbs/date` ได้รับการแนะนำอย่างกว้างขวาง

### การแยกวิเคราะห์โดยใช้ `elm/time`:
`elm/time` ให้โมดูล `Time` ซึ่งช่วยให้คุณสามารถทำงานกับเวลาแบบ timestamp แทนวันที่ที่อ่านได้โดยมนุษย์ ถึงแม้ว่ามันจะไม่สามารถแยกวิเคราะห์วันที่จากสตริงโดยตรง คุณสามารถแปลงสตริง ISO 8601 เป็น timestamp POSIX ซึ่งจากนั้นคุณสามารถทำงานกับมันได้

```elm
import Time exposing (Posix)

-- สมมติว่าคุณมีสตริงวันที่ ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- แปลงเป็น timestamp POSIX (ฟังก์ชันนี้ส่งคืน `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- ผลลัพธ์ตัวอย่าง: Ok <posix time value>
```

### การแยกวิเคราะห์โดยใช้ `justinmimbs/date`:
สำหรับการแยกวิเคราะห์ที่ซับซ้อน เช่น การจัดการกับรูปแบบที่ไม่ใช่ ISO `justinmimbs/date` เป็นตัวเลือกที่ดี นี่คือวิธีการใช้งานเพื่อแยกวิเคราะห์สตริงวันที่ที่กำหนดเอง:

1. ตรวจสอบให้แน่ใจว่าคุณได้ติดตั้งไลบรารี:

```shell
elm install justinmimbs/date
```

2. ใช้ฟังก์ชัน `Date.fromString` เพื่อแยกวิเคราะห์รูปแบบวันที่ที่กำหนดเอง:

```elm
import Date
import Result exposing (Result(..))

-- สมมุติว่าคุณมีรูปแบบสตริงวันที่กำหนดเอง `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- ฟังก์ชันเพื่อแยกวิเคราะห์รูปแบบที่กำหนด
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- ตัวอย่างการใช้งาน
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- ผลลัพธ์ตัวอย่าง: Ok (Date.fromCalendarDate 2023 Jan 1)
```

ในตัวอย่างเหล่านี้ ประเภท `Result` ทำหน้าที่บรรจุทั้งผลการแยกวิเคราะห์ที่ประสบความสำเร็จซึ่งให้ผลเป็นวันที่ (`Ok`) หรือข้อผิดพลาด (`Err`) ช่วยให้การจัดการข้อผิดพลาดในแอปพลิเคชัน Elm ของคุณเป็นไปอย่างแข็งแกร่ง
