---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:06.333134-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C: `\u0E2A\u0E27\u0E31\u0E2A\u0E14\u0E35, \u0E42\u0E25\u0E01!`."
lastmod: '2024-04-05T21:54:02.153118-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:


### ลบช่องว่างด้านหน้า/หลัง:
```Bash
text="   สวัสดี, โลก!   "
trimmed=$(echo "$text" | xargs)
echo "$trimmed"
```
ผลลัพธ์: `สวัสดี, โลก!`

### ลบตัวเลขทั้งหมด:
```Bash
text="B4sh i5 amaz1ng!"
cleaned=${text//[^a-zA-Z ]/}
echo "$cleaned"
```
ผลลัพธ์: `Bsh i amazng`

### แทนที่อักขระที่เฉพาะเจาะจง:
```Bash
text="Hello-World!"
cleaned=${text//-/_}
echo "$cleaned"
```
ผลลัพธ์: `Hello_World!`

## ลงลึก
ในตอนแรก, เครื่องมือประมวลผลข้อความเช่น `sed` และ `awk` ถูกใช้เป็นหลักสำหรับการจัดการสตริง Bash ได้รวมการจับคู่รูปแบบและการจัดการสตริงเข้ากับเชลล์โดยตรงในภายหลัง, ให้พลังในการใช้งานมากมายโดยไม่ต้องใช้คำสั่งภายนอก

คำสั่ง `${parameter/pattern/string}` เป็นหนึ่งวิธีที่คุณจะแทนที่การจับคู่แรกของ `pattern` ด้วย `string` หากต้องการลบการจับคู่ทั้งหมด, เพียงเพิ่ม `/` เพิ่มเติมเข้าไปตามที่ได้แสดงไว้ในตัวอย่างด้านบน

ทางเลือกอื่น ๆ ได้แก่ การใช้เครื่องมือ UNIX คลาสสิค เช่น `sed`, `awk`, `tr`, หรือภาษาสคริปต์ร่วมสมัย เช่น Python หรือ Perl

ในการทำงานด้านใน, Bash ใช้ globbing และเครื่องหมายการค้นหาสำหรับการตรงกับรูปแบบ, แต่เมื่อคุณเห็นโครงสร้าง `${text//pattern/}` นั่นคุณกำลังจัดการกับการขยายพารามิเตอร์ของ Bash—ฟีเจอร์ที่มีประโยชน์มากสำหรับการจัดการสตริง

## ดูเพิ่มเติม
- Bash Manual บนการขยายพารามิเตอร์: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- บทความเกี่ยวกับการประมวลผลข้อความใน Linux: https://www.linuxjournal.com/content/pattern-matching-bash
- หนังสือ Sed & Awk 101 Hacks eBook: https://www.thegeekstuff.com/ebooks/sed_awk_101_hacks
