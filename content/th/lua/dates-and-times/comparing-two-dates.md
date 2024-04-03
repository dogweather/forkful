---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:12.678457-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\
  \u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E25\u0E31\u0E07\u0E2B\u0E23\u0E37\
  \u0E2D\u0E43\u0E19\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19\u0E01\u0E31\u0E1A\u0E2D\u0E35\u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\
  \u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E40\u0E2B\
  \u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.366939-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\
  \u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19\u0E2B\u0E25\u0E31\u0E07\u0E2B\u0E23\u0E37\
  \u0E2D\u0E43\u0E19\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19\u0E01\u0E31\u0E1A\u0E2D\u0E35\u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\
  \u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E40\u0E2B\
  \u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C \u0E27\u0E32\u0E07\u0E01\u0E33\u0E2B\u0E19\
  \u0E14\u0E01\u0E32\u0E23\u0E07\u0E32\u0E19 \u0E08\u0E31\u0E14\u0E40\u0E23\u0E35\u0E22\
  \u0E07\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 \u0E41\u0E25\u0E30\u0E2D\u0E37\u0E48\u0E19\
  \u0E46."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีทำ:
Lua ไม่มีฟังก์ชันการเปรียบเทียบวันที่แบบในตัว แต่เราสามารถใช้ฟังก์ชัน `os.time()` เพื่อแปลงวันที่เป็นรูปแบบตัวเลข (เวลายุค) และจากนั้นเปรียบเทียบพวกเขา ง่ายนิดเดียว

```Lua
-- แปลงวันที่เป็นเวลายุค
local date1 = os.time({year=2023, month=4, day=1})
local date2 = os.time({year=2023, month=4, day=15})

-- เปรียบเทียบวันที่
if date1 > date2 then
  print("Date1 มาหลังจาก Date2.")
elseif date1 < date2 then
  print("Date1 มาก่อน Date2.")
else
  print("Date1 เหมือนกับ Date2.")
end
```

ผลลัพธ์ตัวอย่างหากเรียกใช้กับวันที่เหล่านี้:

```
Date1 มาก่อน Date2.
```

## ศึกษาเพิ่มเติม
ในอดีต Lua ไม่มีประเภทข้อมูลวันที่ โปรแกรมเมอร์พึ่งพาฟังก์ชัน `os.time()` สำหรับการดำเนินงานวันที่-เวลา ซึ่งยังคงใช้งานอยู่ในปัจจุบัน `os.time()` คืนค่าเวลาเป็นวินาทีนับตั้งแต่ยุค (หรือเรียกว่า Unix time ซึ่งเริ่มต้นในวันที่ 1 มกราคม 1970) นี่เป็นสิ่งที่มีประโยชน์เนื่องจากมันแปลงวันที่เป็นตัวเลข ทำให้การเปรียบเทียบง่ายขึ้น

สำหรับทางเลือกอื่น คุณอาจเขียนตัวเปรียบเทียบที่กำหนดเองสำหรับตารางวันที่ เปรียบเทียบแต่ละฟิลด์ (ปี, เดือน, วัน) ด้วยตนเอง หรือใช้ไลบรารีวันที่ของบุคคลที่สามเช่น `LuaDate`

เมื่อใช้ `os.time()` ให้ระมัดระวังเกี่ยวกับเขตเวลาและการเปลี่ยนแปลงเวลาออมแสง ฟังก์ชันสมมติว่าคุณกำลังให้เวลาท้องถิ่นเว้นแต่คุณจะระบุอย่างอื่น

## ดูเพิ่มเติม
- Lua 5.4 คู่มืออ้างอิง: https://www.lua.org/manual/5.4/
- LuaDate, โมดูลวันที่และเวลา: https://github.com/Tieske/date
- การเข้าใจ Unix timestamp: https://en.wikipedia.org/wiki/Unix_time
