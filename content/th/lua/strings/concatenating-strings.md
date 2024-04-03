---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:38.578269-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\
  \u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E2D\u0E19\u0E17\u0E49\u0E32\u0E22\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E43\u0E2B\u0E21\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14\u0E44\
  \u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E04"
lastmod: '2024-03-17T21:57:56.346436-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\
  \u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E15\u0E2D\u0E19\u0E17\u0E49\u0E32\u0E22\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E43\u0E2B\u0E21\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14\u0E44\
  \u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E04\
  ."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
ใน Lua, คุณสามารถต่อสตริงด้วยตัวดำเนินการ `..` มาดูตัวอย่างการใช้งาน:

```lua
local hello = "Hello, "
local world = "world!"
local greeting = hello .. world

print(greeting)  -- ผลลัพธ์: Hello, world!
```

คุณยังสามารถติดต่อตัวเลขด้วยการเปลี่ยนแปลงข้อมูลเล็กน้อย:

```lua
local base = "I have "
local itemCount = 3
local message = base .. itemCount .. " apples"

print(message)  -- ผลลัพธ์: I have 3 apples
```

จำไว้ว่า, การแปลงข้อมูลที่ไม่ใช่สตริงเป็นสิ่งที่ต้องทำเอง:

```lua
local score = 9001
local displayScore = "Your score is: " .. tostring(score)

print(displayScore)  -- ผลลัพธ์: Your score is: 9001
```

## การทดลองในระดับลึก
การต่อสตริงอาจดูเรียบง่าย แต่มันมีความสำคัญ ในวันแรกๆ ของ Lua, มันถูกออกแบบมาสำหรับระบบฝังตัว ซึ่งหมายความว่าต้องทำให้สิ่งต่างๆ เรียบง่าย นั่นคือเหตุผลที่ `..` ถูกเลือกสำหรับสตริง - มันเรียบง่ายแต่ได้ผล

ทางเลือกอื่นๆ กับ `..` ประกอบด้วย:

- ฟังก์ชั่น `table.concat` สำหรับอาร์เรย์ของสตริง, มีประสิทธิภาพมากขึ้นสำหรับการต่อสตริงจำนวนมาก
- ฟังก์ชั่นของไลบรารีสตริง เช่น `string.format`, ที่ให้ความควบคุมมากขึ้นเกี่ยวกับการจัดรูปแบบ

ประสิทธิภาพการต่อสตริงของ Lua เป็นข้อกังวล, เฉพาะกับ `..` เนื่องจากการใช้งานแต่ละครั้งจะสร้างสตริงใหม่, ซึ่งอาจมีต้นทุนสูงในลูป ในการลดผลกระทบนี้, เมื่อต้องการต่อในลูป, ให้ใช้ตาราง:

```lua
local parts = {}
for i = 1, 10 do
    parts[i] = "Part " .. i
end
local combined = table.concat(parts, ", ")

print(combined)  -- ผลลัพธ์: Part 1, Part 2, ... Part 10
```

ภายใน, Lua จัดการสตริงใน hash table เพื่อปรับใช้หน่วยความจำให้เหมาะสมสุด, ดังนั้นสตริงที่เหมือนกันจะใช้พื้นที่จัดเก็บร่วมกัน แต่การต่อสตริงทำให้การใช้พื้นที่ร่วมกันนี้หายไปเนื่องจากสตริงใหม่ที่ถูกสร้างขึ้น

## ดูเพิ่มเติม
- เอกสารประกอบการใช้งาน Lua เกี่ยวกับสตริง: https://www.lua.org/manual/5.4/manual.html#6.4
- การเขียนโปรแกรมใน Lua (หนังสือ): https://www.lua.org/pil/contents.html
- เคล็ดลับการจัดการสตริง: https://lua-users.org/wiki/StringLibraryTutorial
