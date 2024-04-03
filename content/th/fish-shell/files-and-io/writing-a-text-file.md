---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:55.274935-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E43\u0E19 Fish, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `echo` \u0E23\u0E27\
  \u0E21\u0E01\u0E31\u0E1A operator \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23 redirection\u2026"
lastmod: '2024-03-17T21:57:56.662802-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Fish, \u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\
  \u0E48\u0E07 `echo` \u0E23\u0E27\u0E21\u0E01\u0E31\u0E1A operator \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23 redirection \u0E44\u0E21\u0E48\u0E21\u0E35\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\
  \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21\u0E42\
  \u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E19 Fish,\
  \ \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\
  \u0E48\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19 shell\
  \ \u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22\
  \u0E41\u0E25\u0E30\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E20\
  \u0E32\u0E1E\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\
  \u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\u0E19\u0E35\u0E49\u0E41\u0E25\u0E49\u0E27."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:
เพื่อเขียนไฟล์ข้อความใน Fish, คุณสามารถใช้คำสั่ง `echo` รวมกับ operator สำหรับการ redirection ไม่มีไลบรารีของบุคคลที่สามที่นิยมโดยเฉพาะสำหรับการเขียนไฟล์ใน Fish, เนื่องจากคำสั่งที่มีอยู่ใน shell นั้นเรียบง่ายและมีประสิทธิภาพสำหรับวัตถุประสงค์นี้แล้ว.

### เขียนข้อความในไฟล์ใหม่หรือเขียนทับไฟล์ที่มีอยู่:
```fish
echo "Hello, Fish Shell!" > output.txt
```
คำสั่งนี้เขียน "Hello, Fish Shell!" ลงใน `output.txt`, สร้างไฟล์หากไม่มีอยู่หรือเขียนทับไฟล์ถ้ามีอยู่แล้ว.

### เพิ่มข้อความเข้าไปในไฟล์ที่มีอยู่:
หากคุณต้องการเพิ่มข้อความในส่วนท้ายของไฟล์โดยไม่ลบเนื้อหาที่มีอยู่ปัจจุบัน ให้ใช้ operator สำหรับการ append `>>`:
```fish
echo "Adding new line to file." >> output.txt
```

### เขียนข้อความหลายบรรทัด:
คุณสามารถเขียนข้อความหลายบรรทัดลงในไฟล์โดยใช้ echo กับตัวแบ่งบรรทัด `\n`, หรือคุณสามารถเชื่อมโยงคำสั่ง echo หลายคำสั่งด้วย semicolons:
```fish
echo "First Line\nSecond Line" > output.txt
# หรือ
echo "First Line" > output.txt; echo "Second Line" >> output.txt
```

### ผลลัพธ์ตัวอย่าง:
เพื่อดูเนื้อหาของ `output.txt` หลังจากรันคำสั่งข้างต้น ให้ใช้คำสั่ง `cat`:
```fish
cat output.txt
```
```plaintext
First Line
Second Line
```
การแทนที่หรือเพิ่มข้อความตามที่แสดงไว้ทำให้สามารถจัดการเนื้อหาของไฟล์ได้ตามความต้องการของคุณ แสดงวิธีง่ายๆ แต่ทรงพลังในการทำงานกับไฟล์ข้อความใน Fish Shell.
