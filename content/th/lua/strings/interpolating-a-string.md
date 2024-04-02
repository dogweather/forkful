---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:42.886351-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\u0E02\u0E49\u0E32\
  \u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E21\u0E31\u0E19\u0E17\u0E33\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E49\u0E02\u0E2D\u0E1A\u0E40\u0E02\
  \u0E15\u0E41\u0E25\u0E30\u0E23\u0E31\u0E01\u0E29\u0E32\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E30\u0E2D\u0E32\u0E14\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14"
lastmod: '2024-03-17T21:57:56.340748-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\u0E02\u0E49\u0E32\
  \u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E21\u0E31\u0E19\u0E17\u0E33\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E49\u0E02\u0E2D\u0E1A\u0E40\u0E02\
  \u0E15\u0E41\u0E25\u0E30\u0E23\u0E31\u0E01\u0E29\u0E32\u0E04\u0E27\u0E32\u0E21\u0E2A\
  \u0E30\u0E2D\u0E32\u0E14\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## อะไรและทำไม?
การแทรกตัวแปรลงในสตริงช่วยให้คุณใส่ตัวแปรได้โดยตรงเข้าไปในสตริง มันทำเพื่อสร้างสตริงอย่างไร้ขอบเขตและรักษาความสะอาดของโค้ด 

## วิธีการ:
ใน Lua, ใช้ `..` เพื่อการต่อสตริงหรือ `string.format` เพื่อการแทรกสตริง ตัวอย่าง:
```Lua
local name = "Ada"
local greeting = "Hello, " .. name .. "!"
print(greeting) -- ผลลัพธ์: Hello, Ada!

local age = 30
local bio = string.format("%s อายุ %d ปี", name, age)
print(bio) -- ผลลัพธ์: Ada อายุ 30 ปี
```

## ลงลึก
ในอดีต, Lua ไม่มีฟังก์ชันการแทรกสตริงในตัวเหมือนภาษาอื่นๆ (เช่น Ruby, Python) การใช้ `..` เป็นวิธีหลัก โดย Lua 5.3 ได้เ introduced `string.format` เพื่อวิธีการที่สะอาดกว่า คล้ายกับ C ของ `printf` **ทางเลือก:** นอกจากการใช้งานตัวดำเนินการ `..` หรือ `string.format`, คุณยังสามารถเขียนฟังก์ชันการแทรกสตริงที่กำหนดเองซึ่งใช้ gsub เพื่อการจับคู่รูปแบบ แต่ทำไมต้องทำให้มันซับซ้อนล่ะ? ใช้เครื่องมือที่มีอยู่เพื่อการรักษาตัวเอง **รายละเอียดการปฏิบัติ:** โปรดทราบว่าการต่อสตริงบ่อยครั้งอาจนำไปสู่ปัญหาด้านประสิทธิภาพ `string.format` มีประโยชน์เมื่อคุณต้องการควบคุมรูปแบบ อย่างเฉพาะเจาะจง เช่น การระบุความแม่นยำของตัวเลขหรือการเติม

## ดูเพิ่มเติม
- คู่มือ Lua เกี่ยวกับสตริง: http://www.lua.org/manual/5.4/manual.html#6.4
- คู่มือโปรแกรมมิ่งใน Lua เกี่ยวกับสตริง: https://www.lua.org/pil/20.1.html
- วิกิผู้ใช้ Lua เกี่ยวกับสตริง: http://lua-users.org/wiki/StringLibraryTutorial
