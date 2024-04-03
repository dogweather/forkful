---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:54.969303-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E48\u0E19 `string.gsub` \u0E43\u0E19 Lua \u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E41\u0E23\u0E01\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\
  \u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48 \u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49."
lastmod: '2024-03-17T21:57:56.339554-06:00'
model: gpt-4-0125-preview
summary: "\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E48\u0E19 `string.gsub` \u0E43\
  \u0E19 Lua \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\
  \u0E41\u0E23\u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\
  \u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48 \u0E21\u0E31\
  \u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\
  ."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
ฟังก์ชั่น `string.gsub` ใน Lua เป็นตัวเลือกแรกสำหรับการค้นหาและแทนที่ มันทำงานอย่างนี้:

```lua
local text = "The quick brown fox jumps over the lazy dog."
local searchText = "lazy"
local replaceWith = "energetic"

local result = string.gsub(text, searchText, replaceWith)

print(result)
```

ผลลัพธ์:

```
The quick brown fox jumps over the energetic dog.
```

เพื่อแทนที่ทุกครั้งที่ปรากฏ, `gsub` ทำได้โดยค่าเริ่มต้น:

```lua
local text = "Apples are sweet. Apples are juicy."
local result = string.gsub(text, "Apples", "Oranges")

print(result)
```

ผลลัพธ์:

```
Oranges are sweet. Oranges are juicy.
```

## การศึกษาเจาะลึก
การค้นหาและแทนที่ข้อความไม่ใช่เฉพาะแต่ใน Lua เท่านั้น; มันเป็นคุณสมบัติทั่วไปในภาษาการเขียนโปรแกรม Lua ฟังก์ชั่น `string.gsub` ย้อนกลับไปสู่รากฐานการจัดการสตริงของมัน นำเสนอวิธีการตรงไปตรงมาในการจัดการรูปแบบและการแทนที่

จากประวัติศาสตร์, `gsub` (การแทนที่ระดับโลก) ได้รับอิทธิพลจากคำสั่ง `sed` ใน Unix และความสามารถในการจับคู่รูปแบบของ Perl ที่มีประสิทธิภาพสูง รูปแบบของ Lua ถึงแม้จะง่ายกว่า regular expressions ที่พบในภาษาอื่น ก็ยังสามารถจัดการกับการจับคู่ที่ซับซ้อนได้ด้วยความคิดสร้างสรรค์

ทางเลือกอื่นๆ สำหรับ `string.gsub` ประกอบด้วยการวนซ้ำผ่านสตริงด้วยตนเองและสร้างการแทนที่—วิธีที่มีโอกาสผิดพลาดมากขึ้น สำหรับการประมวลผลข้อความหนัก สามารถใช้ไลบรารีการประมวลผลที่เฉพาะเจาะจงได้

ในด้านการใช้งาน, `gsub` สามารถใช้ฟังก์ชั่นเป็นข้อโต้แย้งการแทนที่ ทำให้สามารถควบคุมการแทนที่ด้วยโปรแกรมได้

```lua
local result = string.gsub(text, "(%a+)", function(word)
  return #word > 4 and word:upper() or word
end)
```

ส่วนย่อยนี้จะทำให้คำที่ยาวกว่าสี่อักษรเป็นตัวพิมพ์ใหญ่

## อ่านเพิ่มเติม
- หนังสือ [Programming in Lua](https://www.lua.org/pil/), ให้ความรู้ลึกเกี่ยวกับแนวคิดการเขียนโปรแกรม Lua
- สำหรับความสามารถของรูปแบบสตริง Lua ที่สมบูรณ์, ตรวจสอบ [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
