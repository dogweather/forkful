---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:27.949797-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Fish, \u0E04\
  \u0E38\u0E13\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `string` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\
  \u0E15\u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.633002-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Fish, \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\
  \u0E31\u0E48\u0E07 `string` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
ใน Fish, คุณใช้คำสั่ง `string` เพื่อจัดการกับสตริง นี่คือวิธีการ:

### จับจากต้น:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -l 4 # แสดงผล 'Fish'
```

### ตัดจากปลาย:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s -4 # แสดงผล 'fun!'
```

### ช่วงเฉพาะ:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s 6 -l 5 # แสดงผล 'Shell'
```

## การศึกษาลึก
ในอดีต, เราจะแยกและตัดสตริงใน Fish โดยใช้เครื่องมือภายนอกเช่น `cut`, `awk`, หรือ `sed` ตอนนี้, `string` เป็นฟังก์ชันในตัวที่เราไปใช้บ่อยที่สุดตั้งแต่เปิดตัวใน Fish 2.3.0 มันเร็วกว่า, อ่านง่ายกว่า, และผสานรวมกับสคริปต์ของเราอย่างไม่มีรอยต่อ

`string sub` ไม่ใช่ตัวเลือกเดียวของคุณ ฟังก์ชัน `string` อื่น ๆ สามารถแยกสตริง, แทนที่ส่วนต่างๆ, หรือรวมมัน ความเน้นอยู่ที่การใช้ทรัพยากรน้อยที่สุดและความเข้าใจง่าย

เกี่ยวกับการดำเนินการ, เมื่อคุณดึงส่วนย่อยของสตริงออกมา, Fish จะอ่านสตริงและแสดงเฉพาะส่วนที่คุณระบุ, ทั้งนี้โดยเคารพการเข้ารหัสตัวอักษรและหลีกเลี่ยงข้อผิดพลาดทั่วไปในการดึงส่วนย่อยของสตริง, เช่น การแบ่งตัวอักษรครึ่งหนึ่ง

## ดูเพิ่มเติม
- เอกสารการใช้งาน Fish ทางการเกี่ยวกับ `string`: https://fishshell.com/docs/current/cmds/string.html
- บทเรียนชุมชนเกี่ยวกับการเขียนสคริปต์แบบ Fish: https://fishshell.com/docs/current/tutorial.html
- การพูดคุยใน Stack Overflow เกี่ยวกับการจัดการสตริงใน Fish: https://stackoverflow.com/questions/tagged/fish
