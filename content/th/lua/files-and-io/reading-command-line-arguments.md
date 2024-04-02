---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:34.336787-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07 (command line arguments) \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E43\u0E0A\u0E49\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21\u0E17\
  \u0E35\u0E48\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E2B\u0E25\u0E31\u0E07\
  \u0E08\u0E32\u0E01\u0E0A\u0E37\u0E48\u0E2D\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E43\u0E19\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\
  \u2026"
lastmod: '2024-03-17T21:57:56.369698-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07 (command line arguments) \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E43\u0E0A\u0E49\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21\u0E17\
  \u0E35\u0E48\u0E04\u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E2B\u0E25\u0E31\u0E07\
  \u0E08\u0E32\u0E01\u0E0A\u0E37\u0E48\u0E2D\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E43\u0E19\u0E04\u0E2D\u0E19\u0E42\u0E0B\u0E25\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## อะไรและทำไม?

การอ่านอาร์กิวเมนต์บรรทัดคำสั่ง (command line arguments) หมายถึงการเลือกใช้ส่วนเพิ่มเติมที่คุณพิมพ์หลังจากชื่อสคริปต์ของคุณในคอนโซล นักพัฒนาทำเช่นนี้เพื่อปรับเปลี่ยนพฤติกรรมของโปรแกรมโดยไม่ต้องเปลี่ยนโค้ด เช่น การเลือกไฟล์เพื่อเปิดหรือการตั้งค่าความละเอียดในผลลัพธ์

## วิธีการ:

นี่คือภาพรวมของวิธีการดึงอาร์กิวเมนต์เหล่านั้นใน Lua:

```Lua
-- บันทึกนี้เป็น 'greet.lua'
local name = arg[1] -- arg[1] คืออาร์กิวเมนต์บรรทัดคำสั่งตัวแรก
print("Hello, " .. (name or "stranger") .. "!")
```

เปิดเทอร์มินัลและรันมัน:

```
$ lua greet.lua LuaLearner
Hello, LuaLearner!
```

ไม่มีชื่อ? ไม่มีปัญหา:

```
$ lua greet.lua
Hello, stranger!
```

## ลงลึก

Lua ทำให้มันง่ายดายด้วยตาราง `arg` โลก. ทั่วไปมาแต่โบราณ (ดี, ตั้งแต่เกิดของ UNIX, อย่างน้อยที่สุด) ผู้คนได้ทำการอ่านอาร์กิวเมนต์บรรทัดคำสั่งในการเขียนโปรแกรม มันเป็นหนึ่งในการปรับแต่งที่สำคัญ

ใน Lua, `arg` เป็นอาร์เรย์ที่มี argument บรรทัดคำสั่งทั้งหมด `arg[0]` เป็นชื่อสคริปต์ และ `arg[1]` ขึ้นไปคือ argument จริงๆ คุณสามารถหยิบมันทั้งหมดขึ้นมาด้วยลูปถ้าคุณรู้สึกว่ามันเก๋ไก๋:

```Lua
for i = 1, #arg do
  print("Argument " .. i .. ": " .. arg[i])
end
```

มีทางเลือกอื่นหรือไม่? แน่นอน มีไลบรารี่อยู่ภายนอกสำหรับการแยกวิเคราะห์อาร์กิวเมนต์ที่ซับซ้อน (เช่น `Penlight`), แต่สำหรับหลายๆ กรณี, `arg` ทำให้ผลได้อย่างไม่มีปัญหา

เรื่องของรายละเอียดในการใช้งาน, จำไว้ว่าอาร์เรย์ของ Lua เริ่มนับที่ 1 (พวกเขาเริ่มนับที่ 1) ไม่ใช่ 0 เหมือนภาษาอื่นๆ นั้นเป็นเหตุผลว่าทำไม `arg[1]` เป็นอาร์กิวเมนต์ตัวแรกและไม่ใช่ `arg[0]`

## ดูเพิ่มเติม

สำหรับผู้ที่ต้องการเรียนรู้เพิ่มเติม นี่คือข้อมูลเพิ่มเติม:

- Lua 5.4 Reference Manual บนตาราง `arg`: https://www.lua.org/manual/5.4/manual.html#6.1
- "Programming in Lua" (ฉบับที่ 4) สำหรับการเข้าใจพื้นฐานของ Lua อย่างมั่นคง: https://www.lua.org/pil/contents.html
- Penlight, ไลบรารี่ยูทิลิตี้ของ Lua ที่มีการแยกวิเคราะห์อาร์กิวเมนต์ที่ดีขึ้น: https://github.com/lunarmodules/Penlight
