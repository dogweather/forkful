---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:46.659422-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19 Fish Shell, \u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E14\
  \u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E20\u0E32\u0E22\u0E43\u0E19\u0E15\u0E31\u0E27\
  \ \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\
  \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E2B\u0E23\u0E37\u0E2D\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E01\
  \u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\
  \u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\
  \u2026"
lastmod: '2024-03-17T21:57:56.627353-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Fish Shell, \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\
  \u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\
  \u0E37\u0E2D\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E20\
  \u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\
  \u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48 \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E23\u0E27\u0E21\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `string` \u0E01\u0E31\
  \u0E1A subcommands \u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีทำ:
ใน Fish Shell, สามารถจัดการข้อความได้โดยตรงด้วยฟังก์ชันที่มีอยู่ภายในตัว โดยไม่ต้องใช้เครื่องมือหรือไลบรารีภายนอก การทำให้สตริงเป็นตัวพิมพ์ใหญ่ คุณสามารถรวมคำสั่ง `string` กับ subcommands ได้

```fish
# ตัวอย่างสตริง
set sample_string "hello world"

# ทำให้ตัวอักษรตัวแรกเป็นตัวพิมพ์ใหญ่
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

ผลลัพธ์:
```
Hello world
```

สำหรับสถานการณ์ที่ต้องการให้ตัวอักษรเริ่มต้นของทุกคำในข้อความเป็นตัวพิมพ์ใหญ่ (เช่น การแปลง "hello world" เป็น "Hello World"), คุณจะต้องวนลูปที่ละคำ โดยใช้ลอจิกการทำให้เป็นตัวพิมพ์ใหญ่กับแต่ละคำ:

```fish
# ตัวอย่างประโยค
set sentence "hello fish shell programming"

# ทำให้ตัวอักษรเริ่มต้นของแต่ละคำเป็นตัวพิมพ์ใหญ่
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# เชื่อมคำที่ทำให้เป็นตัวพิมพ์ใหญ่
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

ผลลัพธ์:
```
Hello Fish Shell Programming
```

โปรดทราบว่า Fish Shell ไม่มีการเสนอวิธีการใช้คำสั่งเดียวโดยตรงสำหรับการทำให้ทั้งประโยคเป็นตัวพิมพ์ใหญ่เหมือนที่ภาษาการเขียนโปรแกรมบางภาษารองรับผ่านวิธีการจัดการสตริงของพวกเขา ดังนั้น การรวม `string split`, `string sub`, `string upper`, เเละการเชื่อมกลับข้อความ เป็นวิธีการที่ถูกต้องตามไวยากรณ์ของ Fish Shell เพื่อบรรลุเป้าหมายนี้
