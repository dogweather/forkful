---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:45.435810-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Elm, \u0E04\
  \u0E38\u0E13\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Date` \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48 \u0E41\u0E25\u0E30\u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08\
  \ `elm/time` \u0E43\u0E2B\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E21\u0E32\u0E14\u0E39\u0E42\
  \u0E04\u0E49\u0E14 Elm \u0E01\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.140223-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Elm, \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `Date` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48 \u0E41\u0E25\u0E30\u0E41\u0E1E\
  \u0E47\u0E04\u0E40\u0E01\u0E08 `elm/time` \u0E43\u0E2B\u0E49\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \ \u0E21\u0E32\u0E14\u0E39\u0E42\u0E04\u0E49\u0E14 Elm \u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
ใน Elm, คุณใช้โมดูล `Date` เพื่อทำงานกับวันที่ และแพ็คเกจ `elm/time` ให้ฟังก์ชันในการแปลงวันที่เป็นสตริง มาดูโค้ด Elm กัน:

```Elm
import Time exposing (Posix)
import Date

-- สมมติว่าเรามีเวลาในรูปแบบ Posix
posixTime : Posix
posixTime = Time.millisToPosix 1672569600000

-- แปลง Posix เป็น Date
date : Date.Date
date = Date.fromPosix posixTime

-- จัดรูปแบบวันที่เป็นสตริง
dateToString : String
dateToString = Date.toIsoString date

-- ผลลัพธ์
dateToString --> "2023-01-01T00:00:00.000Z"
```

บรรทัด `Date.toIsoString date` คือส่วนที่ทำให้การเปลี่ยนค่า `Date.Date` ของคุณเป็นสตริงที่จัดรูปแบบตาม ISO 8601

## ลงลึกซึ้ง
ตามประวัติศาสตร์, วิธีการจัดการวันที่และเวลาใน Elm ได้วิวัฒนาการตามภาษา โดยมีจุดมุ่งหมายเพื่อความแม่นยำและความสอดคล้องมากขึ้น ด้วยการใช้แพ็คเกจ `elm/time`, Elm ทำให้กระบวนการจัดการเวลาเป็นเรื่องง่ายยิ่งขึ้น

ทางเลือกในการแปลงวันที่ รวมถึงการใช้ตัวกำหนดรูปแบบที่กำหนดเองหากคุณต้องการวิธีการแสดงวันที่ที่เฉพาะเจาะจง โมดูล `Date` เองไม่ได้เสนอตัวเลือกรูปแบบที่กว้างขวาง หมายความว่าหากคุณต้องการบางอย่างที่ไม่ได้อยู่ในรูปแบบ ISO 8601 คุณจะหันไปใช้แพ็คเกจของชุมชน เช่น `justinmimbs/date` เพื่อความยืดหยุ่นในการกำหนดรูปแบบมากขึ้น

ในแง่ของการดำเนินการ, เมื่อคุณกำลังแปลงวันที่เป็นสตริงใน Elm, คุณกำลังจัดการกับเขตเวลาโดยอัตโนมัติ Elm แสดงวันที่ใน UTC โดยค่าเริ่มต้น ซึ่งหมายความว่า ไม่มีการเปลี่ยนแปลงเวลาอย่างไม่คาดคิดเมื่อแปลง ยกเว้นว่าคุณจัดการกับเขตเวลาด้วยตรรกะเพิ่มเติมโดยเจาะจง การเลือกออกแบบนี้มีจุดมุ่งหมายเพื่อลดข้อผิดพลาดและความไม่สอดคล้อง โดยเฉพาะเมื่อมีการจัดการกับเซิร์ฟเวอร์และไคลเอ็นต์ในเขตเวลาที่แตกต่างกัน

## ดูเพิ่มเติม
- แพ็คเกจ `Time` ของ Elm: [Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- การกำหนดรูปแบบวันที่ของชุมชน: [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- คู่มือวันที่ของ Elm: [Elm Guide - Time](https://guide.elm-lang.org/effects/time.html)
