---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:25.162049-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E01\
  \u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\u0E04\u0E48\
  \u0E27\u0E34\u0E18\u0E35\u0E2B\u0E23\u0E39\u0E2B\u0E23\u0E32\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E1E\u0E39\u0E14\u0E27\u0E48\u0E32 '\u0E15\u0E34\u0E14\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\
  \u0E17\u0E49\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E17\u0E49\u0E32\u0E22' \u0E19\u0E31\
  \u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E27\u0E21\u0E04\u0E33\u0E41\u0E25\u0E30\u0E1B\
  \u0E23\u0E30\u0E42\u0E22\u0E04, \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.719511-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E01\
  \u0E31\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E1E\u0E35\u0E22\u0E07\u0E41\u0E04\u0E48\
  \u0E27\u0E34\u0E18\u0E35\u0E2B\u0E23\u0E39\u0E2B\u0E23\u0E32\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E1E\u0E39\u0E14\u0E27\u0E48\u0E32 '\u0E15\u0E34\u0E14\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\
  \u0E17\u0E49\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E17\u0E49\u0E32\u0E22' \u0E19\u0E31\
  \u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E27\u0E21\u0E04\u0E33\u0E41\u0E25\u0E30\u0E1B\
  \u0E23\u0E30\u0E42\u0E22\u0E04, \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21,\u2026"
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## อะไร & ทำไม?
การต่อสตริงกันเป็นเพียงแค่วิธีหรูหราในการพูดว่า 'ติดพวกมันเข้าด้วยกันท้ายต่อท้าย' นักพัฒนาทำแบบนี้เพื่อรวมคำและประโยค, สร้างข้อความ, หรือใส่ค่าเข้าไปในข้อความอย่างไดนามิก

## วิธีการ:
ใน Ruby, คุณสามารถต่อสตริงด้วยตัวดำเนินการ `+` หรือเมธอด `<<` ซึ่งแก้ไขสตริงในที่นั้น นี่คือวิธีเชื่อมคำ - หรือนั่นคือ, คำ:

```Ruby
# การใช้ตัวดำเนินการ + ซึ่งส่งคืนสตริงใหม่
greeting = "Hello, " + "world!"
puts greeting # ผลลัพธ์: Hello, world!

# การใช้เมธอด << ซึ่งเปลี่ยนแปลงสตริงต้นฉบับ
name = "Alice"
name << ", meet Bob"
puts name # ผลลัพธ์: Alice, meet Bob
```

## ดำดิ่งลึก
การต่อสตริงได้เกิดขึ้นใน Ruby ตั้งแต่เกิดของมัน แต่ด้วยเวลา, ภาษาได้มอบวิธีมากขึ้นในการร้อยสตริงเข้าด้วยกัน

เราได้กล่าวถึง `+` และ `<<`, แต่ยังมีเมธอด `concat` และการใส่แทรก (interpolation)

- การใช้ `concat`: เมธอดนี้เหมือนกับ `<<` แต่ช่วยให้คุณเพิ่มสตริงหลายตัวได้ในครั้งเดียว
```Ruby
phrase = "Roses are red"
phrase.concat(", violets are blue")
puts phrase # ผลลัพธ์: Roses are red, violets are blue
```

- การใส่แทรก: นำตัวแปรเข้าสู่สตริงโดยไม่ต้องต่อพวกมันโดยตรง มันสะอาดกว่าและเป็นที่ต้องการสำหรับการใส่ตัวแปร:
```Ruby
mood = "excited"
message = "I am #{mood} to learn Ruby!"
puts message # ผลลัพธ์: I am excited to learn Ruby!
```

การใส่แทรกทำการเรียก `to_s` อัตโนมัติสำหรับตัวแปรใด ๆ, รับรองว่าประเภทที่ไม่ใช่สตริงสามารถทำงานร่วมกับสตริงได้

นอกจากนี้, จำไว้ว่า - มันไม่ได้เกี่ยวกับการติดคำกันเท่านั้น; Ruby ยังคงระมัดระวังในเรื่องประสิทธิภาพด้วย เมื่อคุณใช้ `+`, Ruby สร้างสตริงใหม่ ตลอดเวลาหรือในลูป, อาจเป็นการใช้หน่วยความจำอย่างหนัก. ตรงกันข้าม, `<<` และ `concat` แก้ไขสตริงต้นฉบับ, ซึ่งมักจะมีประสิทธิภาพมากกว่า

## ดูเพิ่มเติม
- เอกสาร Ruby ประกอบด้วย String: https://ruby-doc.org/core-3.1.2/String.html
- บทความเกี่ยวกับการใส่แทรกสตริง Ruby: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- คู่มือการใช้งาน operators ใน Ruby: https://www.tutorialspoint.com/ruby/ruby_operators.htm
