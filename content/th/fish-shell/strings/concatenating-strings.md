---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:07.650456-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\
  \u0E31\u0E19 \u0E19\u0E31\u0E01\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E23\u0E27\u0E21\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E40\
  \u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E1B\u0E23\u0E30\
  \u0E42\u0E22\u0E04\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E40\u0E2A\u0E49\u0E19\u0E17\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C"
lastmod: '2024-03-17T21:57:56.635758-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E19\u0E33\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\
  \u0E31\u0E19 \u0E19\u0E31\u0E01\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E23\u0E27\u0E21\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E40\
  \u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E1B\u0E23\u0E30\
  \u0E42\u0E22\u0E04\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E40\u0E2A\u0E49\u0E19\u0E17\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การเชื่อมต่อสตริงหมายถึงการนำสตริงมาต่อกัน นักเขียนโปรแกรมทำแบบนี้เพื่อรวมข้อความ, เช่น การสร้างประโยคจากคำหรือสร้างเส้นทางไฟล์

## วิธีทำ:
ใน Fish, ต่อสตริงด้วยการมีช่องว่างระหว่างกัน หรือใช้ `string join`

```fish
# ผสม 'Hello' และ 'World!' พร้อมช่องว่าง
echo 'Hello' 'World!'

# ผลลัพธ์: Hello World!

# ต่อตัวแปร
set greet "Howdy"
set who "Partner"
echo $greet $who

# ผลลัพธ์: Howdy Partner

# การต่อกันโดยไม่มีช่องว่างด้วย string join
set file "report"
set ext "txt"
string join '' $file '.' $ext

# ผลลัพธ์: report.txt
```

## ดำดิ่งลึกลงไป
การต่อสตริงมีมาตั้งแต่เริ่มต้นการเขียนโปรแกรม. ใน Fish, `string join` นั้นสะอาดกว่าวิธีเก่าๆ อย่างการใช้ `echo` ตามด้วยตัวแปรสตริงโดยไม่มีคำอ้างอิง. วิธีนี้ช่วยหลีกเลี่ยงโอเวอร์เฮดของ subcommand ซึ่งสามารถเป็นการชนะด้านประสิทธิภาพได้

ทางเลือกอื่น ๆ รวมถึงการใช้ `printf` ซึ่งให้การควบคุมรูปแบบมากขึ้น แต่มีความซับซ้อนเล็กน้อยสำหรับการดำเนินการต่อง่ายๆ ตัวอย่าง:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

คำสั่ง `string` ของ Fish เป็นส่วนหนึ่งของชุดเครื่องมือการจัดการสตริงในตัว ซึ่งถูกนำมาใช้เพื่อทำให้การประมวลผลข้อความง่ายขึ้น เป็นสิ่งที่ไม่เฉพาะเจาะจงกับ Fish แต่การรวมเข้าเป็นเครื่องมือในตัวช่วยให้ทุกอย่างเรียบง่าย

## ดูเพิ่มเติม
- เอกสารประกอบอย่างเป็นทางการของ Fish: [link](https://fishshell.com/docs/current/cmds/string.html)
- แบบฝึกหัดของชุมชน: [link](https://fishshell.com/docs/current/tutorial.html#tutorial)
- การอภิปรายเกี่ยวกับการจัดการสตริงใน shells: [link](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)
