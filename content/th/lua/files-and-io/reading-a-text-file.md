---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:26.434762-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\
  \u0E2D\u0E07\u0E21\u0E31\u0E19\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E40\u0E23\
  \u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25, \u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E2B\u0E23\u0E37\u0E2D\u0E41\u0E2A\u0E14\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E44\
  \u0E27\u0E49 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32, \u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.371593-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E42\u0E2B\u0E25\u0E14\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\
  \u0E2D\u0E07\u0E21\u0E31\u0E19\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E40\u0E23\
  \u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25, \u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E2B\u0E23\u0E37\u0E2D\u0E41\u0E2A\u0E14\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E44\
  \u0E27\u0E49 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\
  \u0E04\u0E48\u0E32, \u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การอ่านไฟล์ข้อความหมายถึงการโหลดเนื้อหาของมันเข้าไปในโปรแกรมของคุณ เราทำเช่นนี้เพื่อประมวลผล, วิเคราะห์, หรือแสดงข้อมูลที่เก็บไว้ เช่น การตั้งค่า, บันทึก, หรือการป้อนข้อมูลจากผู้ใช้

## วิธีการ:

มาดูกันว่าเราจะอ่านไฟล์ข้อความทีละบรรทัดแล้วค่อยๆ อ่านทั้งหมดพร้อมกันได้อย่างไร

```Lua
-- อ่านไฟล์ทีละบรรทัด
local file = io.open("example.txt", "r") -- เปิดไฟล์เพื่ออ่าน
if file then
  for line in file:lines() do -- วนซ้ำไปทีละบรรทัดในไฟล์
    print(line)
  end
  file:close() -- ปิดไฟล์เมื่อคุณเสร็จสิ้น
else
  print("ไม่สามารถเปิดไฟล์")
end

-- อ่านไฟล์ทั้งหมดพร้อมกัน
local file = io.open("example.txt", "r") -- เปิดไฟล์เพื่ออ่าน
if file then
  local content = file:read("*a") -- อ่านเนื้อหาทั้งหมด
  print(content)
  file:close() -- ปิดไฟล์
else
  print("ไม่สามารถเปิดไฟล์")
end
```

ผลลัพธ์ตัวอย่างสำหรับทั้งสองส่วน, หาก `example.txt` ประกอบไปด้วย:
```
Hello, Lua!
```

ผลลัพธ์จะเป็น:
```
Hello, Lua!
```

## ศึกษาเพิ่มเติม

แต่เดิม, การอ่านไฟล์เป็นการดำเนินการพื้นฐาน, ต่อยอดมาจากคอมพิวเตอร์ยุคแรกๆ ใน Lua, การนี้ถูกจัดการผ่านโมเดลของไอโอง่ายๆ กับไลบรารี `io`

ในขณะที่ `io.lines` และ `io.read` เป็นวิธีการทั่วไปในการเข้าถึงเนื้อหาของไฟล์, มีทางเลือกอื่นๆ เช่น `lfs` (LuaFileSystem) สำหรับการดำเนินงานไฟล์ขั้นสูง

เมื่ออ่าน, Lua จัดการกับการบัฟเฟอร์อย่างลับๆ แต่สำหรับไฟล์ขนาดใหญ่, คุณควรอ่านเป็นชิ้นๆ เพื่อหลีกเลี่ยงการใช้หน่วยความจำสูง

การใช้ไลบรารี `io` เป็นสิ่งที่ตรงไปตรงมา, แต่จำไว้เสมอว่าปิดไฟล์เพื่อป้องกันการรั่วไหลของทรัพยากร หากเกิดข้อผิดพลาด, การดำเนินการไฟล์ของ Lua จะคืนค่า `nil` และข้อความแจ้งผิดพลาด, ซึ่งคุณควรจัดการเพื่อความเข้มแข็ง

## ดูเพิ่มเติมได้ที่:

- [Lua 5.4 Reference Manual: I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Learn Lua](https://learnxinyminutes.com/docs/lua/)
