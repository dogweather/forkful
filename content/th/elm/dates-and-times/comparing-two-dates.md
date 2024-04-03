---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:43.217386-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elm \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\
  \u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E2A\u0E21\u0E21\u0E15\u0E34\u0E27\u0E48\u0E32\
  \u0E04\u0E38\u0E13\u0E21\u0E35\u0E27\u0E31\u0E19\u0E17\u0E35\u0E2A\u0E2D\u0E07\u0E27\
  \u0E31\u0E19 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E08\u0E30\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\
  \u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E43\u0E14\u0E40\u0E01\u0E34\u0E14\u0E02\
  \u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19."
lastmod: '2024-03-17T21:57:56.141298-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\
  \u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E40\u0E1B\
  \u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E2A\u0E21\
  \u0E21\u0E15\u0E34\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E08\u0E30\u0E15\u0E23\
  \u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E43\
  \u0E14\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีการ:
Elm ทำให้การเปรียบเทียบวันทีเป็นเรื่องง่าย สมมติว่าคุณมีวันทีสองวัน นี่คือวิธีที่คุณจะตรวจสอบว่าวันทีใดเกิดขึ้นก่อน:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 < date2 then
        LT  -- date1 เกิดขึ้นก่อน date2
    else if date1 > date2 then
        GT  -- date1 เกิดขึ้นหลัง date2
    else
        EQ  -- วันที่เหมือนกัน

-- ตัวอย่างการใช้งาน:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- เพิ่มวันที่แรกใน POSIX time
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- และวันที่สองใน POSIX time
in
compareDates date1 date2
-- ผลลัพธ์จะเป็นอย่างใดอย่างหนึ่งระหว่าง LT, GT, หรือ EQ
```

คุณยังสามารถคำนวณความแตกต่างในหน่วยมิลลิวินาทีได้:

```Elm
timeDifference : Posix -> Posix -> Time.Duration
timeDifference date1 date2 =
    Time.millisToPosix date1 - Time.millisToPosix date2

-- ตัวอย่างการใช้งาน:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
timeDifference date1 date2
-- ผลลัพธ์: ระยะเวลาในหน่วยมิลลิวินาที
```

## ลงลึก
Elm เก็บวันทีในรูปของ `Posix`, ซึ่งเป็นการแสดงเวลาเป็นมิลลิวินาทีนับจาก Unix epoch (1 มกราคม 1970, UTC) นี่เป็นวิธีการที่ทั่วไป แบ่งปันรากฐานกับ Unix Time และมันทำให้การจัดการและเก็บข้อมูลวันทีเป็นเรื่องง่าย

แม้ว่าคลังคำสั่งหลักของ Elm จะมีการจัดการวันทีพื้นฐาน บางทางเลือกเช่น `justinmimbs/date` มีอยู่สำหรับปฏิบัติการที่ซับซ้อนกว่า

เมื่อทำการเปรียบเทียบวันที จำไว้ว่าเขตเวลาสามารถทำให้เรื่องซับซ้อนขึ้นได้ Elm's `Time` module สมมติว่า UTC, ซึ่งหมายความว่าคุณจะไม่เกิดปัญหาเรื่องการปรับเวลาตามฤดูกาล แต่คุณอาจต้องปรับสำหรับเขตเวลาท้องถิ่นในแอปพลิเคชันของคุณ

## ดูเพิ่มเติม
- Elm Time module: https://package.elm-lang.org/packages/elm/time/latest/
- แพคเกจ Date ของ Justin Mimbs สำหรับ Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Unix Time: https://en.wikipedia.org/wiki/Unix_time
