---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:24.098087-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\
  \u0E08\u0E07\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E43\u0E2B\
  \u0E49\u0E01\u0E25\u0E32\u0E22\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E2D\u0E37\u0E48\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E1E\u0E40\u0E14\u0E15\u0E42\u0E04\u0E49\u0E14\
  \ \u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u2026"
lastmod: '2024-03-17T21:57:56.629357-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\
  \u0E08\u0E07\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E43\u0E2B\
  \u0E49\u0E01\u0E25\u0E32\u0E22\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E2D\u0E37\u0E48\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E1E\u0E40\u0E14\u0E15\u0E42\u0E04\u0E49\u0E14\
  \ \u0E41\u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การค้นหาและเปลี่ยนแปลงข้อความคือการหาสตริงที่เฉพาะเจาะจงและเปลี่ยนให้กลายเป็นข้อความอื่น โปรแกรมเมอร์ทำเช่นนี้เพื่ออัพเดตโค้ด แก้ไขข้อผิดพลาด หรือเพื่อจัดรูปแบบข้อมูลใหม่ — มันช่วยประหยัดเวลาได้มาก

## วิธีการ:
ลองเปลี่ยนทุกตัวอย่างของ 'cat' เป็น 'dog' ในสตริง

```Fish Shell
echo "One cat, two cats, three cats." | string replace -a 'cat' 'dog'
```
ผลลัพธ์ตัวอย่าง:
```
One dog, two dogs, three dogs.
```
เปลี่ยนข้อความในไฟล์ชื่อ `pets.txt`:

```Fish Shell
string replace -a 'cat' 'dog' < pets.txt > updated_pets.txt
```

ใช้ตัวแปรสำหรับรูปแบบ:

```Fish Shell
set old "cat"
set new "dog"
string replace -a $old $new < pets.txt > updated_pets.txt
```

## ลงลึก
การค้นหาและแทนที่ได้มีอยู่ในตัวแก้ไขข้อความตั้งแต่ยุคแรกๆ เช่น `sed` สำหรับการแก้ไขสตรีมใน Unix — นั่นเป็นสิ่งที่เจ๋งสุดๆ ในยุคเก่า Fish ได้ทำให้สิ่งนี้ง่ายขึ้นด้วยคำสั่ง `string` ไม่ต้องปวดหัวกับ regex อีกต่อไป ถ้าคุณไม่อยาก มีทางเลือกอื่นหรือ? แน่นอน: `sed`, `awk`, สคริปต์ Perl, แม้แต่ `vim` macros แต่คำสั่ง `string` ของ Fish มีเสน่ห์และลดโอกาสข้อผิดพลาดสำหรับกรณีทั่วไป

## ดูเพิ่มเติม:
- คู่มือการใช้งานทางการของ Fish Shell สำหรับคำสั่ง `string`: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed by Example, ส่วนที่ 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- การเขียนโปรแกรมภาษา AWK — ฟังก์ชันสตริง: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
