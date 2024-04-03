---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:20.827241-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\
  \u0E25\u0E32\u0E14 (Debug Output) \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E48\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E36\u0E49\u0E19\u0E08\u0E2D\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E27\u0E48\u0E32\u0E21\u0E35\u0E2D\u0E30\u0E44\
  \u0E23\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E31\u0E1A\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E1A\u0E49\u0E32\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.357278-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\
  \u0E25\u0E32\u0E14 (Debug Output) \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E48\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E36\u0E49\u0E19\u0E08\u0E2D\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E27\u0E48\u0E32\u0E21\u0E35\u0E2D\u0E30\u0E44\
  \u0E23\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E31\u0E1A\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E1A\u0E49\u0E32\u0E07 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E32\u0E21\u0E2B\
  \u0E32\u0E2A\u0E32\u0E40\u0E2B\u0E15\u0E38\u0E02\u0E2D\u0E07\u0E1B\u0E31\u0E0D\u0E2B\
  \u0E32\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u2014\u0E2B\u0E23\u0E37\
  \u0E2D\u0E17\u0E35\u0E48\u0E40\u0E23\u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 bugs."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
นี่คือวิธีพื้นฐานในการพิมพ์ข้อมูลใน Lua:

```Lua
print("Hello, Debug World!")  -- แสดงสตริงบนคอนโซล

local number = 42
print("The number is:", number)  -- ผสมสตริงกับตัวเลขได้

local table = {name = "Lua", year = 1993}
print(table)  -- จะแสดงอ้างอิงของตาราง, ไม่ค่อยช่วยเหลือมาก
```

ผลลัพธ์ตัวอย่าง:
```
Hello, Debug World!
The number is: 42
table: 0x194a330
```

ถ้าอยากดูข้อมูลในตารางและแสดงมันออกมา, ทำตามนี้:

```Lua
for key, value in pairs(table) do
    print(key, "=", value)
end
```

ผลลัพธ์ตัวอย่าง:
```
name = Lua
year = 1993
```

## ศึกษาลึก
การพิมพ์ข้อมูลแก้ไขข้อผิดพลาดไม่ใช่เรื่องใหม่หรือเก๋า แต่มันเชื่อถือได้เหมือนค้อนขวานเก่าๆ ดูที่สมัยก่อน, การดีบักเก๋าๆ ไม่ได้มีให้เห็นกันง่ายๆ โปรแกรมเมอร์จึงเลือกที่จะพิมพ์เพื่อดูว่าปัญหาอยู่ที่ไหน Lua ฟังก์ชัน `print` นั้นง่ายมาก มันจะส่งข้อมูลไปยัง stdout—ซึ่งมักจะเป็นที่ตัวอุปกรณ์จบสุดของคุณ

มีทางเลือกอื่นไหม? Lua มีหลายทางเลือก มี `io.write()` ที่ให้ความควบคุมเพิ่มเติม อย่างเช่นการข้ามบรรทัดใหม่ มอดูลอย่าง `inspect` แสดงข้อมูลในตารางได้ดีกว่าการใช้ print ปกติ

จุดใช้งานของ `print` ในซอร์สโค้ดภาษา C ของ Lua นั้นพื้นฐานมาก ใช้ `tostring` กับแต่ละอาร์กิวเมนต์และส่งข้อมูลไปที่ `stdout` พร้อมกับบรรทัดใหม่ LuaJIT, เวอร์ชันคอมไพเลอร์ที่ทำงานจริงของ Lua, ใช้วิธีการ `print` เดียวกัน แต่มั่นคงมากขึ้น

## ดูเพิ่มเติม
รับภาพรวมที่กว้างขึ้น:

- เอกสารการใช้งาน `print` อย่างเป็นทางการของ Lua: https://www.lua.org/manual/5.4/manual.html#pdf-print
- แนะนำเรื่อง LuaJIT: http://luajit.org/intro.html
- รายละเอียดไลบรารี `io` สำหรับข้อมูลเบื้องต้นเกี่ยวกับ `io.write`: https://www.lua.org/manual/5.4/manual.html#6.8
- มอดูล `inspect.lua` สำหรับเมื่อคุณเหนื่อยกับตารางที่เงียบเชียบ: https://github.com/kikito/inspect.lua
