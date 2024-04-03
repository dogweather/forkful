---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:15.486135-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elm \u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Time` \u0E04\u0E38\u0E13\u0E08\
  \u0E30\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\
  \u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\
  \u0E27\u0E25\u0E32 POSIX timestamp, \u0E08\u0E32\u0E01\u0E19\u0E31\u0E49\u0E19\u0E41\
  \u0E1B\u0E25\u0E07\u0E44\u0E1B\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48."
lastmod: '2024-03-17T21:57:56.139319-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25\
  \ `Time` \u0E04\u0E38\u0E13\u0E08\u0E30\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E40\
  \u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E27\u0E25\u0E32 POSIX timestamp, \u0E08\u0E32\
  \u0E01\u0E19\u0E31\u0E49\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E44\u0E1B\u0E40\u0E1B\u0E47\
  \u0E19\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
Elm จัดการกับวันที่โดยใช้โมดูล `Time` คุณจะได้รับเวลาปัจจุบันในรูปแบบเวลา POSIX timestamp, จากนั้นแปลงไปเป็นวันที่

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- แปลง POSIX time ไปเป็นรูปแบบวันที่
                date = Time.toDate posixTime
            in
            -- อัปเดตโมเดลของคุณตามที่แนะนำที่นี่
            ({ model | date = date }, Cmd.none)

-- เพื่อเริ่มการเข้าถึงเวลาปัจจุบัน
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- ตัวอย่างผลลัพธ์:
-- date { year = 2023, month = Mar, day = 26 }
```

## ลึกซึ้ง
ในภาษาเว็บเก่าๆ การดึงข้อมูลวันที่เป็นโค้ดหนึ่งบรรทัด แต่ Elm มีความแตกต่าง มันทำให้การกระทำที่มีผลข้างเคียงเช่นการได้รับเวลาปัจจุบันเป็นสิ่งที่ชัดเจนผ่าน Elm Architecture สิ่งนี้ส่งเสริมความบริสุทธิ์และความสามารถในการบำรุงรักษาของโค้ด

ทางเลือกอื่น ๆ รวมถึงการใช้แพ็คเกจจากบุคคลที่สามหรือการจัดการวันที่ในโค้ดเซิร์ฟเวอร์ของคุณและส่งผ่านไปยัง Elm ผ่านธงหรือพอร์ต

ในแง่ของการเนินการ, `Time.now` ของ Elm ได้เวลาเป็นเวลา POSIX timestamp (milliseconds นับตั้งแต่ Unix epoch) การนี้ไม่เกี่ยวข้องกับเขตเวลา, และคุณสามารถจัดรูปแบบมันได้ตามความต้องการโดยใช้ฟังก์ชันจากโมดูล `Time`

## ดูเพิ่มเติม
- [เอกสารของ Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [คู่มือของ Elm เกี่ยวกับคำสั่งและการสมัครสมาชิก](https://guide.elm-lang.org/effects/)
