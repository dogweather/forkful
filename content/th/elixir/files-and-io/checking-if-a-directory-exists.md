---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:41.724323-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E02\u0E2D\u0E07 Elixir\
  \ \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E17\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\
  \u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\
  \u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\
  \u0E48\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\u0E39\u0E25 `File` \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E44\u0E14\
  \u0E49."
lastmod: '2024-03-17T21:57:55.867942-06:00'
model: gpt-4-0125-preview
summary: "\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07 Elixir \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E17\
  \u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\
  \u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `File` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\
  \u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49\u0E21\u0E31\u0E19\u0E44\u0E14\u0E49."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
ไลบรารีมาตรฐานของ Elixir มีวิธีทางตรงไปตรงมาในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ผ่านโมดูล `File` นี่คือวิธีที่คุณสามารถใช้มันได้:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

ตัวอย่างผลลัพธ์, โดยสมมติว่าไดเรกทอรีไม่มีอยู่:
```
Directory does not exist.
```

สำหรับการโต้ตอบกับระบบไฟล์ที่ซับซ้อนกว่า, รวมถึงการตรวจสอบการมีอยู่ของไดเรกทอรี, คุณอาจพิจารณาใช้ไลบรารีของบุคคลที่สามเช่น `FileSystem` ในขณะที่ความสามารถมาตรฐานของ Elixir เพียงพอสำหรับกรณีจำนวนมาก, `FileSystem` สามารถเสนอการควบคุมที่ละเอียดอ่อนและการตอบกลับสำหรับแอปพลิเคชันที่ซับซ้อน อย่างไรก็ตาม, สำหรับความต้องการพื้นฐานในการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่, การยึดมั่นในโมดูล `File` ซึ่งเป็นแบบดั้งเดิมนั้นมักแนะนำเนื่องจากมันพร้อมใช้งานอยู่แล้วและไม่ต้องการสิ่งที่ขึ้นอยู่นอกเหนือไปจากนี้
