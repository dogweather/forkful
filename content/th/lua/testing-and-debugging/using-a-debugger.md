---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:37.850771-06:00
description: "Lua \u0E44\u0E21\u0E48\u0E21\u0E35\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E41\u0E15\
  \u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E14\
  \u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E19\u0E2D\
  \u0E01\u0E44\u0E14\u0E49 \u0E40\u0E0A\u0E48\u0E19 ZeroBrane Studio \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E27\u0E34\u0E18\
  \u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E08\u0E30\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E01\u0E31\u0E1A\u0E21\u0E31\u0E19: ```Lua -- \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C Lua\u2026"
lastmod: '2024-03-17T21:57:56.359072-06:00'
model: gpt-4-0125-preview
summary: "Lua \u0E44\u0E21\u0E48\u0E21\u0E35\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\
  \u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E14\u0E35\
  \u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \u0E44\u0E14\u0E49 \u0E40\u0E0A\u0E48\u0E19 ZeroBrane Studio \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E27\u0E34\u0E18\u0E35\
  \u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E08\u0E30\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\
  \u0E31\u0E1A\u0E21\u0E31\u0E19: ```Lua -- \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E2A\
  \u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C Lua\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
Lua ไม่มีดีบักเกอร์ภายในตัว แต่คุณสามารถใช้ดีบักเกอร์ภายนอกได้ เช่น ZeroBrane Studio นี่คือตัวอย่างวิธีที่คุณจะทำงานกับมัน:

```Lua
-- นี่คือสคริปต์ Lua ง่ายๆ ที่มีข้อผิดพลาดตั้งใจ
local function add(a, b)
    local result = a+ b -- อุ๊ปส์, มาเป็นว่าเราลืมกำหนด 'b'
    return result
end

print(add(10))
```

เมื่อคุณรันสิ่งนี้ในดีบักเกอร์, มันจะหยุดการทำงานที่จุดที่เกิดข้อผิดพลาด คุณจะเห็นบางอย่างเช่นนี้:

```
lua: example.lua:3: attempt to perform arithmetic on a nil value (local 'b')
stack traceback:
	example.lua:3: in function 'add'
	example.lua:7: in main chunk
	[C]: in ?
```

คุณสามารถตั้งจุดหยุด, เดินทางผ่านโค้ดของคุณ, และดูค่าตัวแปรเพื่อติดตามข้อผิดพลาดโดยไม่สูญเสียสติ

## ดำดิ่งลงไปลึกๆ
ความเรียบง่ายของ Lua ไม่ได้ขยายไปถึงการดีบักอย่างน่าเสียดาย แต่ไม่ต้องกังวล, ชุมชน Lua จะช่วยคุณ มีเครื่องมืออย่าง ZeroBrane Studio, LuaDec และอื่นๆ ที่เสนอความสามารถในการดีบัก ในอดีต, ดีบักเกอร์มีอยู่ไม่นานหลังจากโปรแกรมแรกเริ่มมีปัญหา ให้นักพัฒนามีวิธีแก้โค้ดของตนโดยไม่ต้องลองผิดลองถูกอย่างมืดบอด

กับ Lua, คุณมักจะพึ่งพาดีบักเกอร์ภายนอกหรือสร้างเข้าไปในสภาพแวดล้อมการพัฒนาของคุณ ZeroBrane Studio เป็นตัวอย่างของ IDE ที่รวมดีบักเกอร์ Lua เข้าไว้อย่างเต็มรูปแบบ มันช่วยให้คุณเดินทางระหว่างโค้ด, ตั้งจุดหยุด และดูค่าตัวแปร ในด้านการทำงานของตัวเอง, ดีบักเกอร์มักใช้ฮุกในการใส่จุดหยุดและความสามารถด้านการดีบักอื่นๆ

มีทางเลือกอื่นไหม? แน่นอน การใช้คำสั่ง `print` ที่รู้จักกันเป็น "การดีบักด้วย printf," บางครั้งก็สามารถทำให้ได้ผลโดยไม่ต้องใช้เครื่องมือที่ซับซ้อน

## ดูเพิ่มเติม
เพื่อดำเนินการดำเนินการดีบักของคุณต่อไป ตรวจสอบที่:

- ZeroBrane Studio: https://studio.zerobrane.com/
- วิกิผู้ใช้ Lua ในหัวข้อการดีบักโค้ด Lua: http://lua-users.org/wiki/DebuggingLuaCode
- คำอ้างอิงไลบรารี `debug` ในคู่มือ Lua: https://www.lua.org/manual/5.4/manual.html#6.10
