---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:02.641618-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Fish \u0E21\u0E35\u0E40\u0E27\
  \u0E17\u0E21\u0E19\u0E15\u0E23\u0E4C\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E1B\u0E23\u0E30\u0E40\u0E20\u0E17\u0E19\u0E35\
  \u0E49 \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `string`\
  \ \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E40\u0E2B\u0E19\
  \u0E37\u0E48\u0E2D\u0E22\u0E2B\u0E19\u0E48\u0E32\u0E22 \u0E25\u0E2D\u0E07\u0E14\u0E39\
  \u0E2A\u0E40\u0E1B\u0E25\u0E25\u0E4C\u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\
  ."
lastmod: '2024-03-17T21:57:56.632080-06:00'
model: gpt-4-0125-preview
summary: "Fish \u0E21\u0E35\u0E40\u0E27\u0E17\u0E21\u0E19\u0E15\u0E23\u0E4C\u0E43\u0E19\
  \u0E15\u0E31\u0E27\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E1B\u0E23\
  \u0E30\u0E40\u0E20\u0E17\u0E19\u0E35\u0E49 \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19 `string` \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\
  \u0E2D\u0E07\u0E40\u0E2B\u0E19\u0E37\u0E48\u0E2D\u0E22\u0E2B\u0E19\u0E48\u0E32\u0E22\
  \ \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E2A\u0E40\u0E1B\u0E25\u0E25\u0E4C\u0E40\u0E2B\
  \u0E25\u0E48\u0E32\u0E19\u0E35\u0E49."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\
  \u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## วิธีการ:
Fish มีเวทมนตร์ในตัวสำหรับงานประเภทนี้ ใช้ฟังก์ชัน `string` โดยไม่ต้องเหนื่อยหน่าย ลองดูสเปลล์เหล่านี้:

```fish
# ตัวอย่างกับอัญประกาศเดี่ยว
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # ผลลัพธ์: Hello, World!

# ตัวอย่างกับอัญประกาศคู่
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # ผลลัพธ์: Hello, Universe!
```

## การดำดิ่งลึกลงไป
ในยุคหินของบรรทัดคำสั่ง, คุณจะต้องต่อสู้กับ `sed` หรือ `awk` เพื่อถอดอัญประกาศ; มันเป็นเรื่องยุ่งยากจริงๆ ที่มีบาค์สแลชและธงลึกลับมากมาย Fish's `string` function เป็นจากยุคใหม่, ทำให้โค้ดสวยงามและมีสัญชาตญาณมากขึ้น

การเลือกอื่นในเชลล์อื่นอาจยังคงพึ่งพาเครื่องมือเก่าเหล่านี้หรืออาจใช้วิธีการภายในตัวเองเช่นการขยายพารามิเตอร์บาชหรือตัวแก้ไข zsh

ฟังก์ชัน `string` ไปไกลกว่าการตัดอัญประกาศ เป็นมีดพกสวิสสำหรับการดำเนินการสตริงใน Fish ด้วย `string`, คุณสามารถหั่น, ผ่า, แบ่ง, รวม, หรือแม้กระทั่งจับคู่ regex สตริงได้ทันทีในเทอร์มินัลของคุณ

## ดูเพิ่มเติม
ศึกษาเพิ่มเติมเกี่ยวกับ `string` ด้วยความช่วยเหลือจากเอกสารประกอบอย่างเป็นทางการ:
- [เอกสารประกอบ Fish Shell String](https://fishshell.com/docs/current/commands.html#string)

สำหรับความคิดถึงหรือเมื่อเขียนสคริปต์ด้วยเชลล์ที่เป็นทางการมากกว่า, ตรวจสอบได้ที่:
- [คู่มือ Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [การขยายพาราเมตรของ Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
