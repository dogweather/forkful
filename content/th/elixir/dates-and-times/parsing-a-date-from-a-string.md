---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:52.028755-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Elixir \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E01\u0E32\u0E23\u0E41\
  \u0E2A\u0E14\u0E07\u0E15\u0E31\u0E27\u0E41\u0E17\u0E19\u0E02\u0E2D\u0E07\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E40\u0E1B\u0E47\
  \u0E19\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E40\u0E19\u0E17\u0E35\u0E1F\u0E02\u0E2D\u0E07 Elixir \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23 \u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\
  \u0E35\u0E22\u0E1A\u2026"
lastmod: '2024-03-17T21:57:55.862708-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E19 Elixir \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E01\u0E32\u0E23\u0E41\
  \u0E2A\u0E14\u0E07\u0E15\u0E31\u0E27\u0E41\u0E17\u0E19\u0E02\u0E2D\u0E07\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E40\u0E1B\u0E47\
  \u0E19\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E40\u0E19\u0E17\u0E35\u0E1F\u0E02\u0E2D\u0E07 Elixir \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23 \u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\
  \u0E35\u0E22\u0E1A\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแยกวิเคราะห์วันที่จากสตริงใน Elixir ประกอบด้วยการแปลงการแสดงตัวแทนของวันที่และเวลาเป็นโครงสร้างวันที่เนทีฟของ Elixir ช่วยให้สามารถจัดการ การเปรียบเทียบ และการจัดรูปแบบของค่าวันที่และเวลาได้ยืดหยุ่นยิ่งขึ้น โปรแกรมเมอร์ทำงานนี้เพื่อจัดการกับข้อมูลที่ผู้ใช้ป้อน เพื่อการประมวลผลข้อมูลจากแหล่งข้อมูลภายนอก หรือโต้ตอบกับ API ที่สื่อสารวันที่และเวลาในรูปแบบสตริง

## วิธีการ:
ไลบรารีมาตรฐานของ Elixir รวมถึงพลังของโมดูล `:calendar` ของ Erlang ให้การสนับสนุนพื้นฐานสำหรับการแยกวันที่ สำหรับความต้องการที่ซับซ้อนมากขึ้น ไลบรารี `Timex` เป็นทางเลือกที่ได้รับความนิยม นำเสนอคุณสมบัติอย่างกว้างขวางสำหรับการทำงานกับวันที่ เวลา และเขตเวลา

### ใช้ไลบรารีมาตรฐานของ Elixir
```elixir
date_string = "2023-04-21"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)  # => ~D[2023-04-21]
```

เพื่อการทำเลียวเวลาที่รวมข้อมูลเขตเวลา คุณอาจใช้ฟังก์ชันของ Erlang โดยตรง เนื่องจากการจัดการเขตเวลาโดยตรงไม่ได้เป็นส่วนหนึ่งของโมดูลวันที่มาตรฐานของ Elixir
```elixir
datetime_string = "2023-04-21T15:30:00Z"
{:ok, datetime, 0} = DateTime.from_iso8601(datetime_string)
IO.inspect(datetime)  # => #DateTime<2023-04-21 15:30:00Z>
```

### การใช้ Timex
ขั้นแรก เพิ่ม `Timex` เข้าไปใน dependencies ของคุณใน mix.exs:
```elixir
def deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```
จากนั้น ให้รัน `mix deps.get` เพื่อดึง dependency ใหม่

นี่คือวิธีการใช้ Timex เพื่อแยกวันที่จากสตริง:
```elixir
import Timex

date_string = "April 21, 2023"
date = Timex.parse!(date_string, "{Mfull} {D}, {YYYY}")
IO.inspect(date)  # => ~N[2023-04-21 00:00:00]
```

Timex ช่วยในการแยกวิเคราะห์รูปแบบต่างๆ และยังรองรับวันที่ภาษาพูดได้ ทำให้มีความยืดหยุ่นสูงในการแยกวิเคราะห์วันที่และเวลาจากข้อมูลที่ผู้ใช้ป้อนหรือแหล่งข้อมูลภายนอก
