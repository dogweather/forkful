---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:06.716323-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Lua \u0E01\u0E32\
  \u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E19\u0E31\
  \u0E49\u0E19\u0E17\u0E4D\u0E32\u0E44\u0E14\u0E49\u0E44\u0E21\u0E48\u0E22\u0E32\u0E01\
  \ \u0E04\u0E38\u0E13\u0E21\u0E31\u0E01\u0E08\u0E30\u0E43\u0E0A\u0E49\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `io.open()` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\
  \u0E1B\u0E34\u0E14 (\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07) \u0E44\
  \u0E1F\u0E25\u0E4C\u0E42\u0E14\u0E22\u0E23\u0E30\u0E1A\u0E38\u0E42\u0E2B\u0E21\u0E14\
  \u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23 --\u2026"
lastmod: '2024-04-05T21:54:02.138522-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Lua \u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\
  \u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E19\u0E31\u0E49\u0E19\u0E17\u0E4D\u0E32\u0E44\u0E14\u0E49\
  \u0E44\u0E21\u0E48\u0E22\u0E32\u0E01 \u0E04\u0E38\u0E13\u0E21\u0E31\u0E01\u0E08\u0E30\
  \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `io.open()` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E1B\u0E34\u0E14 (\u0E2B\u0E23\u0E37\u0E2D\u0E2A\
  \u0E23\u0E49\u0E32\u0E07) \u0E44\u0E1F\u0E25\u0E4C\u0E42\u0E14\u0E22\u0E23\u0E30\
  \u0E1A\u0E38\u0E42\u0E2B\u0E21\u0E14\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\
  \u0E19\u0E01\u0E32\u0E23 -- \u0E43\u0E19\u0E01\u0E23\u0E13\u0E35\u0E19\u0E35\u0E49\
  \u0E04\u0E37\u0E2D `\"w\"` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E40\u0E02\u0E35\u0E22\u0E19 \u0E2B\u0E32\u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E44\u0E21\
  \u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48 \u0E08\u0E30\u0E16\u0E39\u0E01\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E02\u0E36\u0E49\u0E19 \u0E2B\u0E32\u0E01\u0E21\u0E35\u0E2D\u0E22\
  \u0E39\u0E48 \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\u0E32\u0E22\u0E43\u0E19\u0E08\
  \u0E30\u0E16\u0E39\u0E01\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E31\u0E1A \u0E2A\u0E34\
  \u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E04\u0E37\u0E2D\u0E15\u0E49\u0E2D\u0E07\
  \u0E1B\u0E34\u0E14\u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\
  \u0E40\u0E02\u0E35\u0E22\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E41\
  \u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E16\
  \u0E39\u0E01\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\
  \u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E21\u0E41\u0E25\u0E30\u0E17\u0E23\u0E31\u0E1E\u0E22\
  \u0E32\u0E01\u0E23\u0E16\u0E39\u0E01\u0E1B\u0E25\u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\
  \u0E21\u0E32 \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E46 \u0E17\u0E35\u0E48\u0E40\u0E02\u0E35\u0E22\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E25\u0E07\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E37\u0E48\u0E2D \"example.txt\"\
  ."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:
ใน Lua การทำงานกับไฟล์เพื่อการเขียนนั้นทําได้ไม่ยาก คุณมักจะใช้ฟังก์ชัน `io.open()` เพื่อเปิด (หรือสร้าง) ไฟล์โดยระบุโหมดการดำเนินการ -- ในกรณีนี้คือ `"w"` สำหรับการเขียน หากไฟล์ไม่มีอยู่ จะถูกสร้างขึ้น หากมีอยู่ ข้อมูลภายในจะถูกเขียนทับ สิ่งสำคัญคือต้องปิดไฟล์หลังจากเขียนเพื่อให้แน่ใจว่าข้อมูลถูกบันทึกอย่างเหมาะสมและทรัพยากรถูกปล่อยออกมา

ตัวอย่างง่ายๆ ที่เขียนสตริงลงในไฟล์ชื่อ "example.txt":

```lua
-- เปิดไฟล์ในโหมดการเขียน
local file, err = io.open("example.txt", "w")

-- ตรวจสอบข้อผิดพลาดในการเปิดไฟล์
if not file then
    print("ไม่สามารถเปิดไฟล์ได้: ", err)
    return
end

-- ข้อความที่จะเขียนลงไฟล์
local text = "Hello, Lua!"

-- เขียนข้อความลงไฟล์
file:write(text)

-- ปิดไฟล์
file:close()

print("เขียนไฟล์เรียบร้อยแล้ว.")
```

**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์เรียบร้อยแล้ว.
```

**การเขียนหลายบรรทัด:**

เพื่อการเขียนหลายบรรทัด คุณสามารถใช้ `\n` สำหรับบรรทัดใหม่ในสตริงข้อความของคุณ หรือเรียก `file:write` หลายครั้ง

```lua
local lines = {
    "บรรทัดแรก",
    "บรรทัดที่สอง",
    "บรรทัดที่สาม"
}

local file = assert(io.open("multiple_lines.txt", "w"))

for _, line in ipairs(lines) do
    file:write(line, "\n")
end

file:close()

print("เขียนหลายบรรทัดเรียบร้อยแล้ว.")
```

**ผลลัพธ์ตัวอย่าง:**
```
เขียนหลายบรรทัดเรียบร้อยแล้ว.
```

**การใช้ไลบรารีจากบุคคลที่สาม:**

แม้ว่าไลบรารีมาตรฐานของ Lua จะเพียงพอสำหรับการดำเนินการกับไฟล์ที่ซับซ้อนมากขึ้น คุณอาจพิจารณาใช้ไลบรารีจากบุคคลที่สามอย่าง *Penlight* Penlight เพิ่มความสามารถของการดำเนินการกับไฟล์มาตรฐานของ Lua และให้วิธีที่ง่ายขึ้นในการทำงานกับไฟล์และไดเรกทอรี

หลังจากติดตั้ง Penlight คุณสามารถเขียนลงไฟล์ได้ดังนี้:

```lua
local pl = require "pl"
local path = require "pl.path"
local file = require "pl.file"

-- ข้อความที่จะเขียน
local text = "Hello, Penlight!"

-- ใช้ Penlight เพื่อเขียนลงไฟล์
local result, err = file.write("hello_penlight.txt", text)

if not result then
    print("ข้อผิดพลาดในการเขียนไฟล์: ", err)
else
    print("เขียนไฟล์เรียบร้อยแล้วด้วย Penlight.")
end
```

**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์เรียบร้อยแล้วด้วย Penlight.
```
