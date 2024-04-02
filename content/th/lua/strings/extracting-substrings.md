---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:43.848131-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E22\
  \u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\
  \u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\
  \u0E30\u0E08\u0E07\u0E43\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\
  \u0E48\u0E43\u0E2B\u0E0D\u0E48\u0E02\u0E36\u0E49\u0E19"
lastmod: '2024-03-17T21:57:56.343470-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E22\
  \u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\
  \u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\
  \u0E30\u0E08\u0E07\u0E43\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\
  \u0E48\u0E43\u0E2B\u0E0D\u0E48\u0E02\u0E36\u0E49\u0E19"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไร & ทำไม?
การแยกสตริงย่อยหมายถึงการดึงส่วนที่เฉพาะเจาะจงของสตริงออกมา โปรแกรมเมอร์ทำเช่นนี้เพื่อแยกวิเคราะห์ หรือจัดการข้อมูลที่เฉพาะเจาะจงในข้อความที่ใหญ่ขึ้น

## วิธีการ:
ใน Lua, ใช้ฟังก์ชัน `string.sub`:

```lua
local text = "Hello, Lua!"
-- แยก 'Hello'
print(string.sub(text, 1, 5)) -- ผลลัพธ์: Hello

-- ดึง 'Lua'
print(string.sub(text, 8, 11)) -- ผลลัพธ์: Lua
```

หรือใช้ดัชนีเชิงลบเพื่อดึงตัวอักษรสุดท้าย:

```lua
-- ดึง 'Lua!' จากท้าย
print(string.sub(text, -4)) -- ผลลัพธ์: Lua!
```

ใช้รูปแบบการค้นหาเพื่อเจอและดึงข้อมูล:

```lua
local phrase = "The quick brown fox jumps"
-- ค้นหาและดึง 'quick'
print(phrase:match("(%a+) quick")) -- ผลลัพธ์: The
```

## ลงลึก
ในระยะเริ่มแรกของการเขียนโปรแกรม การจัดการสตริงมักเป็นไปอย่างลำบากและต้องใช้ลูปและเงื่อนไข ฟังก์ชัน `string.sub` ของ Lua เป็นส่วนหนึ่งของไลบรารีสตริงที่กว้างขวางขึ้น ทำให้การจัดการสตริงเป็นเรื่องง่าย `string.sub` เป็นทางเลือกแทนการจับคู่รูปแบบด้วย `string.match` ซึ่งมีความสามารถมากขึ้นแต่อาจเกินความจำเป็นสำหรับงานง่ายๆ

ฟังก์ชัน `string.sub` และการจับคู่รูปแบบนั้นมีพื้นฐานมาจากฟังก์ชันภาษา C เนื่องจากลักษณะของ Lua ที่เกี่ยวข้องกับภาษา C คุณจะไม่พบไลบรารีมาตรฐานขนาดใหญ่ใน Lua สำหรับสตริงเมื่อเทียบกับภาษาเช่น Python; มันให้ความสำคัญกับความเรียบง่ายและประสิทธิภาพ โปรดจำไว้ว่า ดัชนีใน Lua เริ่มที่ 1 ไม่ใช่ 0

## ดูเพิ่มเติม
- Lua 5.4 คู่มืออ้างอิงสำหรับสตริง: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'Programming in Lua' (ฉบับที่ 4) โดยเฉพาะบทเกี่ยวกับสตริง: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
