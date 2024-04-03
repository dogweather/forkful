---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:15.505335-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Lua, \u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr\
  \ \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `io.stderr:write()`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E1B\u0E22\u0E31\u0E07 standard\
  \ error."
lastmod: '2024-03-17T21:57:56.370619-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Lua, \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\
  \u0E1B\u0E22\u0E31\u0E07 stderr \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\
  \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19 `io.stderr:write()` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E41\
  \u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\
  \u0E1B\u0E22\u0E31\u0E07 standard error."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีการ:
ใน Lua, การเขียนไปยัง stderr สามารถทำได้โดยใช้ฟังก์ชัน `io.stderr:write()` นี่คือวิธีที่คุณสามารถเขียนข้อความแสดงข้อผิดพลาดไปยัง standard error:

```lua
io.stderr:write("Error: Invalid input.\n")
```

หากคุณต้องการแสดงผลแปรหรือรวมข้อมูลหลายชิ้นในการเขียน สามารถทำได้โดยการต่อข้อความภายในฟังก์ชัน write:

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**ผลลัพธ์ตัวอย่างบน stderr:**
```
Error: Invalid input.
```

สำหรับสถานการณ์ที่ซับซ้อนกว่าหรือเมื่อทำงานกับแอปพลิเคชันขนาดใหญ่ คุณอาจพิจารณาใช้ไลบรารีบันทึกข้อความจากบุคคลที่สาม เช่น LuaLogging ด้วย LuaLogging คุณสามารถนำข้อมูลบันทึกไปยังจุดหมายปลายทางต่างๆ รวมถึง stderr นี่คือตัวอย่างเบื้องต้น:

ก่อนอื่น ตรวจสอบให้แน่ใจว่าติดตั้ง LuaLogging โดยใช้ LuaRocks:

```
luarocks install lualogging
```

จากนั้น เพื่อเขียนข้อความแสดงข้อผิดพลาดไปยัง stderr โดยใช้ LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

วิธีนี้นำเสนอข้อได้เปรียบของการบันทึกข้อมูลที่มาตรฐานทั่วทั้งแอปพลิเคชัน โดยมีความยืดหยุ่นในการตั้งระดับของบันทึกข้อมูล (เช่น ERROR, WARN, INFO) ผ่าน API ที่ง่ายดาย
