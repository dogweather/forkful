---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:58.468876-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Bash \u0E21\u0E35\u0E27\u0E34\
  \u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\
  \u0E23\u0E07\u0E21\u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C \u0E40\u0E17\u0E04\u0E19\u0E34\u0E04\
  \u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22\u0E17\u0E35\u0E48\u0E2A\u0E38\
  \u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 operator \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E17\u0E32\u0E07\u0E01\u0E32\u0E23\u0E44\u0E2B\u0E25\u0E02\u0E2D\u0E07\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 (`>`, `>>`) \u0E41\u0E25\u0E30\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u2026"
lastmod: '2024-03-17T21:57:56.415945-06:00'
model: gpt-4-0125-preview
summary: "Bash \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\
  \u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \ \u0E40\u0E17\u0E04\u0E19\u0E34\u0E04\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\
  \u0E2D\u0E22\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E43\u0E0A\u0E49 operator \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E32\u0E07\u0E01\u0E32\u0E23\u0E44\
  \u0E2B\u0E25\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 (`>`, `>>`) \u0E41\
  \u0E25\u0E30\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `tee` \u0E19\u0E35\u0E48\u0E40\u0E1B\
  \u0E47\u0E19\u0E01\u0E32\u0E23\u0E14\u0E39\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E17\u0E31\u0E49\u0E07\u0E2A\u0E2D\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\n\n\u0E01\u0E32\u0E23\
  \u0E43\u0E0A\u0E49\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\
  \u0E32\u0E07, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E40\u0E02\u0E49\u0E32\
  \u0E44\u0E1F\u0E25\u0E4C\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E42\u0E2D\u0E40\u0E1B\
  \u0E2D\u0E40\u0E23\u0E40\u0E15\u0E2D\u0E23\u0E4C `>` \u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E25\u0E07\u0E44\u0E1F\u0E25\u0E4C,\
  \ \u0E17\u0E33\u0E01\u0E32\u0E23\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E32\
  \u0E01\u0E44\u0E1F\u0E25\u0E4C\u0E19\u0E31\u0E49\u0E19\u0E21\u0E35\u0E2D\u0E22\u0E39\
  \u0E48\u0E41\u0E25\u0E49\u0E27 \u0E02\u0E13\u0E30\u0E17\u0E35\u0E48 `>>` \u0E40\u0E1E\
  \u0E34\u0E48\u0E21\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E25\u0E07\u0E44\u0E1F\
  \u0E25\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E25\u0E1A\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E2D\u0E2D\
  \u0E01."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:
Bash มีวิธีการที่ตรงไปตรงมาสำหรับการเขียนไฟล์ เทคนิคที่พบบ่อยที่สุดคือการใช้ operator สำหรับการเปลี่ยนทางการไหลของข้อมูล (`>`, `>>`) และคำสั่ง `tee` นี่เป็นการดูวิธีการทั้งสองอย่างอย่างรวดเร็ว

การใช้การเปลี่ยนทาง, คุณสามารถเขียนผลลัพธ์เข้าไฟล์โดยตรง โอเปอเรเตอร์ `>` เขียนเนื้อหาลงไฟล์, ทำการแทนที่หากไฟล์นั้นมีอยู่แล้ว ขณะที่ `>>` เพิ่มเนื้อหาลงไฟล์ที่มีอยู่โดยไม่ลบเนื้อหาออก

```bash
# เขียนไฟล์ด้วย >
echo "Hello, World!" > myfile.txt

# เพิ่มเนื้อหาไฟล์ด้วย >>
echo "This is a new line." >> myfile.txt
```

หากคุณตรวจสอบเนื้อหาของ `myfile.txt` หลังจากรันคำสั่งข้างต้น คุณจะพบ:

```
Hello, World!
This is a new line.
```

คำสั่ง `tee` เป็นตัวเลือกที่ดีเมื่อคุณต้องการเขียนลงไฟล์และดูผลลัพธ์บนหน้าจอ (stdout) ในเวลาเดียวกัน โดยเริ่มต้น, `tee` จะเขียนทับไฟล์ แต่ด้วยธง `-a`, มันจะเพิ่มเนื้อหาลงไฟล์

```bash
# เขียนและแสดงผลลัพธ์ใช้ tee
echo "Hello, again!" | tee myfile.txt

# เพิ่มเนื้อหาและแสดงผลใช้ tee -a
echo "Adding another line." | tee -a myfile.txt
```

หลังจากรันคำสั่งเหล่านี้ `myfile.txt` จะแสดง:

```
Hello, again!
Adding another line.
```

ในขณะที่ Bash เองมีความสามารถในการจัดการไฟล์ที่แข็งแกร่งผ่านการเปลี่ยนทางและคำสั่งเช่น `tee`, การจัดการข้อมูลที่ซับซ้อนขึ้นหรือสถานการณ์ที่ซับซ้อนกว่าอาจต้องการเรียกใช้เครื่องมือภายนอกหรือภาษาสคริปต์ (เช่น Awk, Sed, Python) ที่มีฟังก์ชันการประมวลผลข้อความที่ซับซ้อนยิ่งขึ้น อย่างไรก็ตาม, สำหรับงานเขียนไฟล์ที่ตรงไปตรงมาที่สุด, วิธีการข้างต้นเป็นเพียงพอและถูกใช้กันอย่างแพร่หลาย
