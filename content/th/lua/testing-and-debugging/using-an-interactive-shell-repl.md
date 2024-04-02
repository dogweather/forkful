---
changelog:
- 2024-01-30, dogweather, reviewed and added links
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:33.712274-06:00
description: "REPL \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Read-Eval-Print\
  \ Loop, \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\
  \u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E42\u0E04\u0E49\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\
  \u0E27\u0E14\u0E40\u0E23\u0E47\u0E27 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07, \u0E01\u0E32\u0E23\u0E14\
  \u0E35\u0E1A\u0E31\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.356361-06:00'
model: gpt-4-0125-preview
summary: "REPL \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Read-Eval-Print Loop,\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\
  \u0E2D\u0E21\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E14\u0E2A\u0E2D\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E17\u0E14\u0E25\u0E2D\u0E07, \u0E01\u0E32\u0E23\u0E14\u0E35\
  \u0E1A\u0E31\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไรและทำไม?
REPL ย่อมาจาก Read-Eval-Print Loop, หรือสภาพแวดล้อมการโต้ตอบที่คุณสามารถทดสอบโค้ดได้อย่างรวดเร็ว โปรแกรมเมอร์ใช้มันเพื่อการทดลอง, การดีบัก, และการเรียนรู้จุดเด่นของภาษา

## วิธีการ:
เพื่อเข้าใช้งาน REPL ของ Lua, เพียงแค่ป้อน `lua` ในเทอร์มินอลของคุณ เป็นตัวอย่างของเซสชันดังนี้:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
ในเซสชันนี้, เราประกาศตัวแปร, ทำการคำนวณพื้นฐาน, จัดการกับตาราง, และวนลูปผ่านรายการต่างๆ ในนั้น

## การศึกษาลึกลงไป
ลักษณะที่เบาและง่ายของ Lua ทำให้ REPL ของมันเป็นที่น่าตื่นตาตื่นใจสำหรับการสร้างต้นแบบ มันเริ่มต้นมาตั้งแต่เริ่มต้นของ Lua ในต้นยุค 1990s, ได้รับแรงบันดาลใจจาก shell โต้ตอบสำหรับภาษาอื่นๆ เช่น Lisp ตัวเลือกอื่นๆ ในภาษาอื่นๆ รวมถึง `irb` สำหรับ Ruby และ `python` สำหรับ Python, แต่ละอย่างมีชุดคุณสมบัติของตัวเอง Lua's REPL มีลักษณะเรียบง่าย; ฉะนั้น, อาจขาดคุณสมบัติขั้นสูงที่พบในตัวอื่นๆ เช่น เครื่องมือดีบักที่ซับซ้อน สำหรับประสบการณ์ที่กว้างขวางยิ่งขึ้น, เครื่องมือเช่น ZeroBrane Studio หรือ LuaDist's LuaRocks นำเสนอมากกว่า REPL พื้นฐาน

## ดูเพิ่มเติม
- [Lua 5.4 Reference Manual - The Standalone Lua Interpreter](https://www.lua.org/manual/5.4/manual.html#7)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
