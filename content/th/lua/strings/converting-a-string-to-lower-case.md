---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:34.914521-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\
  \u0E47\u0E01\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\
  \u0E25\u0E35\u0E48\u0E22\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E43\
  \u0E2B\u0E0D\u0E48\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\u0E43\u0E19\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\
  \u0E23\u0E40\u0E25\u0E47\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E2D\u0E14\u0E04\u0E25\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.341608-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\
  \u0E47\u0E01\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\
  \u0E25\u0E35\u0E48\u0E22\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E43\
  \u0E2B\u0E0D\u0E48\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\u0E43\u0E19\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\
  \u0E23\u0E40\u0E25\u0E47\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E2D\u0E14\u0E04\u0E25\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E19,\u2026"
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแปลงสตริงเป็นตัวพิมพ์เล็กหมายถึงการเปลี่ยนตัวอักษรใหญ่ทั้งหมดในสตริงเป็นตัวอักษรเล็ก โปรแกรมเมอร์ทำเช่นนี้เพื่อความสอดคล้องกัน, โดยเฉพาะเมื่อเปรียบเทียบหรือประมวลผลข้อมูลข้อความที่ตัวพิมพ์ไม่ควรมีความสำคัญ, เช่น ข้อมูลป้อนเข้าโดยผู้ใช้หรือคำค้นหา

## วิธีการ:
ใน Lua, คุณจะลดภาระของตัวเองด้วย `string.lower()`. ป้อนสตริงเข้าไป, และจะได้สตริงตัวพิมพ์เล็กออกมา สังเกตดู:

```lua
local originalString = "Hello, World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- ผลลัพธ์: hello, world!
```

รันตัวอย่างนี้สิ ตัวพิมพ์ใหญ่ที่ดังกล่าวกลายเป็นตัวพิมพ์เล็กที่มีระดับเสียงเบาลง

## ลึกลงไป
ตั้งแต่ยุคแรกของการคอมพิวติ้ง, ทุกคนต้องการทำให้ข้อมูลข้อความมีลักษณะเป็นเนื้อเดียวกันเพื่อเหตุผลต่างๆ มากมาย, เช่น การเรียงลำดับหรือการเข้าสู่ระบบที่ไม่คำนึงถึงตัวพิมพ์ เมื่อพูดถึง Lua, `string.lower()` ถูกใช้งานเป็นเครื่องมือหลักตั้งแต่เริ่มต้น มันเรียบง่าย, เป็นส่วนหนึ่งของระบบ, และทำงานได้โดยไม่มีปัญหา

แต่มีอะไรอยู่ภายใต้ฝา? `string.lower()` ทำงานเดินผ่านแต่ละตัวอักษร, และถ้าเป็นตัวพิมพ์ใหญ่ (A ถึง Z), มันจะแปลงมัน Lua พึ่งพาค่า ASCII: 'A' (65) ถึง 'Z' (90) จะถูกเพิ่มไปเป็น 'a' (97) ถึง 'z' (122) ความแตกต่างคือ 32 ดังนั้น, `lowercase = uppercase + 32`

แล้วถ้า `string.lower()` ดูเหมือนจะธรรมดาเกินไป? คุณอาจดำเนินการเปลี่ยนผ่านตัวอักษรด้วยตนเองโดยการใช้ลูป, โดยใช้ค่า ASCII, หรือการจับคู่แบบพาลินโดรมด้วย `string.gsub()`:

```lua
local s = "Make Me Lowercase, Please"
s = s:gsub("%u", function (upper) return string.char(upper:byte() + 32) end)
print(s)  -- ผลลัพธ์: make me lowercase, please
```

แต่จริงๆ แล้ว, ทำไมต้องใช้เสียงพายเรือเมื่อคุณมีเครื่องยนต์ภายนอก (อ่านว่า: `string.lower()`)?

## ดูเพิ่มเติม
ขุดลึกลงไปในการจัดการข้อความของ Lua ด้วยสิ่งที่ดีๆ เหล่านี้:
- [Programming in Lua (ฉบับที่ 4)](https://www.lua.org/pil/contents.html) เพื่อเจาะลึกรายละเอียดของข้อความ
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4) เพื่อฟังก์ชั่นการจัดการสตริงที่เมื่อคุณพร้อมที่จะไปเลยไกลกว่าตัวพิมพ์เล็ก
