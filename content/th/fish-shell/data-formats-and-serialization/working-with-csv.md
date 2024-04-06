---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:36.398050-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E42\u0E14\u0E22\u0E15\
  \u0E31\u0E27\u0E21\u0E31\u0E19\u0E40\u0E2D\u0E07, Fish Shell \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\u0E2D\
  \u0E01\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23 CSV \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21\
  , \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E1B\
  \u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E08\u0E32\u0E01\u0E22\u0E39\u0E17\u0E34\
  \u0E25\u0E34\u0E15\u0E35 Unix \u0E40\u0E0A\u0E48\u0E19 `awk`,\u2026"
lastmod: '2024-03-17T21:57:56.666606-06:00'
model: gpt-4-0125-preview
summary: "\u0E42\u0E14\u0E22\u0E15\u0E31\u0E27\u0E21\u0E31\u0E19\u0E40\u0E2D\u0E07\
  , Fish Shell \u0E44\u0E21\u0E48\u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E2D\u0E2D\u0E01\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E42\u0E14\
  \u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 CSV \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\
  \u0E23\u0E01\u0E47\u0E15\u0E32\u0E21, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E0A\u0E49\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E08\
  \u0E32\u0E01\u0E22\u0E39\u0E17\u0E34\u0E25\u0E34\u0E15\u0E35 Unix \u0E40\u0E0A\u0E48\
  \u0E19 `awk`, `sed`, \u0E41\u0E25\u0E30 `cut` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E43\u0E0A\u0E49\u0E40\u0E04\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E17\u0E35\u0E48\u0E40\u0E0A\u0E35\
  \u0E48\u0E22\u0E27\u0E0A\u0E32\u0E0D\u0E40\u0E0A\u0E48\u0E19 `csvkit` \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\
  \u0E49\u0E2D\u0E19\u0E21\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19\n"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
โดยตัวมันเอง, Fish Shell ไม่มีฟังก์ชันที่ออกแบบมาโดยเฉพาะสำหรับการจัดการ CSV อย่างไรก็ตาม, คุณสามารถใช้ประโยชน์จากยูทิลิตี Unix เช่น `awk`, `sed`, และ `cut` สำหรับการดำเนินการพื้นฐาน หรือใช้เครื่องมือที่เชี่ยวชาญเช่น `csvkit` สำหรับงานที่ซับซ้อนมากขึ้น

### การอ่านไฟล์ CSV และการพิมพ์คอลัมน์แรก:
การใช้ `cut` เพื่อแยกคอลัมน์แรก:
```fish
cut -d ',' -f1 data.csv
```
ตัวอย่างผลลัพธ์:
```
Name
Alice
Bob
```

### การกรองแถว CSV ตามค่าคอลัมน์:
การใช้ `awk` เพื่อค้นหาแถวที่คอลัมน์ที่สองตรงกับ "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
ตัวอย่างผลลัพธ์:
```
Bob,42,London
```

### การแก้ไขไฟล์ CSV (เช่น, การเพิ่มคอลัมน์):
การใช้ `awk` เพื่อเพิ่มคอลัมน์ด้วยค่าแบบคงที่ "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
ตัวอย่างผลลัพธ์ใน `modified.csv`:
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### การใช้ `csvkit` สำหรับการดำเนินการที่ซับซ้อนกว่า:
ก่อนอื่น, ตรวจสอบว่าคุณมี `csvkit` ติดตั้งอยู่แล้วหรือไม่ ถ้ายังไม่ได้ติดตั้ง, ติดตั้งผ่าน pip: `pip install csvkit`.

**การแปลงไฟล์ CSV เป็น JSON:**
```fish
csvjson data.csv > data.json
```
ตัวอย่างผลลัพธ์ `data.json`:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**การกรองด้วย `csvgrep` ของ `csvkit`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
คำสั่งนี้ทำภารกิจการกรองโดยใช้ `csvkit`, โดยเน้นคอลัมน์ที่ 2 สำหรับค่า "42".

สรุปแล้ว, แม้ว่า Fish Shell เองอาจไม่มีความสามารถโดยตรงในการจัดการ CSV, แต่การรวมกันอย่างไม่มีรอยต่อกับยูทิลิตี Unix และการมีเครื่องมือเช่น `csvkit` เป็นตัวเลือกที่มีประสิทธิภาพสำหรับการทำงานกับไฟล์ CSV.
