---
changelog:
- 2024-02-05, dogweather, reviewed and corrected
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:33.324457-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07 Elixir\
  \ \u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\u0E39\u0E25 `DateTime` \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E23\u0E35\
  \u0E22\u0E01\u0E14\u0E39\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\
  \u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E44\u0E14\u0E49\
  \ \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01 Elixir \u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E1A\u0E19 Erlang VM (BEAM)\u2026"
lastmod: '2024-03-17T21:57:55.863796-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07 Elixir \u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `DateTime` \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E40\u0E23\u0E35\u0E22\u0E01\u0E14\u0E39\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\
  \u0E1A\u0E31\u0E19\u0E44\u0E14\u0E49 \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\
  \u0E01 Elixir \u0E17\u0E33\u0E07\u0E32\u0E19\u0E1A\u0E19 Erlang VM (BEAM) \u0E14\
  \u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E08\u0E36\u0E07\u0E43\u0E0A\u0E49\u0E1B\u0E23\
  \u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E08\u0E32\u0E01\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E48\u0E19 Erlang \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\
  \u0E07\u0E32\u0E19\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E40\u0E27\
  \u0E25\u0E32\n\nElixir \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E48\
  \u0E19 `DateTime.utc_now/0` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E44\u0E14\u0E49\u0E23\
  \u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\
  \u0E32\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19 UTC."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
ไลบรารีมาตรฐานของ Elixir ผ่านโมดูล `DateTime` ช่วยให้สามารถเรียกดูวันที่และเวลาปัจจุบันได้ เนื่องจาก Elixir ทำงานบน Erlang VM (BEAM) ดังนั้นจึงใช้ประโยชน์จากฟังก์ชั่น Erlang พื้นฐานสำหรับการดำเนินงานเกี่ยวกับเวลา

### การใช้ไลบรารีมาตรฐานของ Elixir
Elixir มีฟังก์ชั่น `DateTime.utc_now/0` เพื่อได้รับวันที่และเวลาปัจจุบันใน UTC

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**ตัวอย่างผลลัพธ์:**
```
~U[2024-02-05 19:58:40.925931Z]
```

เพื่อได้รับเฉพาะวันที่ปัจจุบัน คุณอาจเลือกส่วนประกอบของปี, เดือน, และวัน:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**ตัวอย่างผลลัพธ์:**
```
~D[2023-05-04]
```

### การใช้ไลบรารี Timex
สำหรับความต้องการเกี่ยวกับวันที่-เวลาที่ซับซ้อนยิ่งขึ้น ไลบรารีของบุคคลที่สามที่เรียกว่า Timex สามารถใช้ได้ ก่อนอื่น, เพิ่ม `Timex` ลงในการพึ่งพา mix.exs ของคุณ:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

หลังจากติดตั้งการพึ่งพา (`mix deps.get`), คุณสามารถใช้ Timex เพื่อได้รับวันที่ปัจจุบัน:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**ตัวอย่างผลลัพธ์:**
```
~D[2023-05-04]
```

Timex มีฟังก์ชั่นการจัดการวันที่-เวลาอย่างละเอียดมากมาย ทำให้เป็นการเพิ่มเสริมที่ทรงพลังให้กับแอปพลิเคชั่น Elixir ของคุณ เฉพาะเมื่อต้องการจัดการกับเขตเวลา, การจัดรูปแบบ, และการแยกวิเคราะห์วันที่และเวลา

โดยการเข้าใจและใช้ศักยภาพที่มีอยู่ใน Elixir และไลบรารี Timex, คุณสามารถทำงานกับวันที่และเวลาในแอปพลิเคชั่น Elixir ของคุณได้โดยง่าย ปรับปรุงประสบการณ์ให้ตรงตามความต้องการของแอปพลิเคชั่นด้วยความแม่นยำและความสะดวก
