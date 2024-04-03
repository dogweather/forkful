---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:55.295267-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.399994-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
```Bash
#!/bin/bash

# กำหนดตัวแปร
name="Gizmo"

# พิมพ์ตัวแปรเพื่อการดีบัก
echo "Debug: ชื่อตัวแปรคือ $name"

# เงื่อนไขพร้อมกับการดีบัก
if [[ $name == "Gizmo" ]]; then
    echo "Debug: เข้าสู่ if-statement."
    # ทำบางอย่าง
fi

# ลูปพร้อมกับการดีบัก
for i in {1..3}; do
    echo "Debug: การวนซ้ำลูป $i"
    # ทำบางอย่างในลูป
done
```

ผลลัพธ์:
```
Debug: ชื่อตัวแปรคือ Gizmo
Debug: เข้าสู่ if-statement.
Debug: การวนซ้ำลูป 1
Debug: การวนซ้ำลูป 2
Debug: การวนซ้ำลูป 3
```

## ดำดิ่งลงไปเพิ่มเติม
ในต้นแบบ การดีบักหมายถึงการลบบั๊กทางกายภาพที่ก่อกวนคอมพิวเตอร์ในยุคแรก ๆ ในปัจจุบัน เป็นเรื่องของการกำจัดบั๊กในโค้ด ข้อมูลการดีบักคือแว่นขยายของโปรแกรมเมอร์

ทางเลือกสำหรับ `echo` ในสคริปต์ bash รวมถึง `printf` สำหรับตัวเลือกการจัดรูปแบบมากขึ้นหรือการเขียนไปยังไฟล์โดยใช้การเปลี่ยนทาง `>` สำหรับบันทึกแบบถาวร

Bash ยังรองรับการดีบักที่มีเงื่อนไขด้วยคำสั่งในตัว `set -x` เพื่อติดตามคำสั่งและอาร์กิวเมนต์ของมันในขณะที่ถูกเรียกใช้ `set -x` นั้นเหมาะสำหรับการดีบักทั้งสคริปต์

## ดูเพิ่มเติม
- หน้า `man` ของ Bash: `man bash`
- คู่มือการเขียนสคริปต์ขั้นสูง: [Bash Guide for Beginners by Machtelt Garrels](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Stack Overflow สำหรับการแก้ปัญหา: [stackoverflow.com](https://stackoverflow.com/questions/tagged/bash)
