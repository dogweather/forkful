---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:15.604355-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E42\u0E04\u0E49\u0E14\u0E40\
  \u0E1B\u0E47\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E21\u0E32\
  \u0E22\u0E04\u0E27\u0E32\u0E21\u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E1A\u0E25\u0E47\
  \u0E2D\u0E01\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E19\u0E33\u0E01\u0E25\u0E31\u0E1A\u0E21\u0E32\u0E43\u0E0A\u0E49\u0E0B\
  \u0E49\u0E33\u0E44\u0E14\u0E49\u0E41\u0E25\u0E30\u0E17\u0E33\u0E07\u0E32\u0E19\u0E40\
  \u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14 \u0E07\u0E48\u0E32\u0E22\u0E15\
  \u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08\u2026"
lastmod: '2024-03-17T21:57:56.402723-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\u0E07\u0E42\u0E04\u0E49\u0E14\u0E40\
  \u0E1B\u0E47\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E21\u0E32\
  \u0E22\u0E04\u0E27\u0E32\u0E21\u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\
  \u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E1A\u0E25\u0E47\
  \u0E2D\u0E01\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E19\u0E33\u0E01\u0E25\u0E31\u0E1A\u0E21\u0E32\u0E43\u0E0A\u0E49\u0E0B\
  \u0E49\u0E33\u0E44\u0E14\u0E49\u0E41\u0E25\u0E30\u0E17\u0E33\u0E07\u0E32\u0E19\u0E40\
  \u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14 \u0E07\u0E48\u0E32\u0E22\u0E15\
  \u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E43\u0E08 \u0E41\u0E25\u0E30\
  \u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
สร้างฟังก์ชันง่ายๆ ใน Bash:

```Bash
greet() {
  echo "สวัสดี, $1!"
}
```

ใช้งานโดยการเรียกฟังก์ชันพร้อมพารามิเตอร์:

```Bash
greet "โลก"  # ผลลัพธ์: สวัสดี, โลก!
```

ฟังก์ชันสามารถส่งค่ากลับโดยใช้ `return` สำหรับรหัสสถานะตัวเลข (ไม่ใช่สำหรับส่งข้อมูลกลับ):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # ผลลัพธ์: 7
```

โปรดทราบว่า `$?` จะจับค่าส่งกลับของคำสั่งล่าสุด ซึ่งเป็นผลรวมตัวเลขของ `add`.

## การศึกษาลึก
ใน Bash, ฟังก์ชันเป็นวิธีหนึ่งในการจัดกลุ่มโค้ดตั้งแต่เวอร์ชันแรกๆ โดยประวัติศาสตร์แล้ว การใช้ฟังก์ชันสอดคล้องกับหลักการโปรแกรมมิ่งแบบโครงสร้างที่เริ่มใช้ในยุค 1960 เพื่อปรับปรุงคุณภาพโค้ด

ทางเลือกอื่นๆ จากฟังก์ชัน รวมถึงการใช้ไฟล์สคริปต์หรือใช้นามแฝง แต่เหล่านี้ไม่ได้เสนอระดับการซ้อนกันและการนำกลับมาใช้ซ้ำเท่ากับฟังก์ชัน

รายละเอียดการทำงานที่น่าสังเกตใน Bash คือฟังก์ชันถือเป็นส่วนประกอบชั้นยอด; พวกเขาไม่มีคำหลักประกาศเฉพาะเช่น `function` ในภาษาอื่นๆ ถึงแม้ว่า `function` ใน Bash เป็นตัวเลือกที่ไม่จำเป็นเพื่ออ่านง่าย ขอบเขตของฟังก์ชันก็น่าสนใจ—ตัวแปรเป็นแบบโกลบอลเป็นค่าเริ่มต้นเว้นแต่จะประกาศเป็นแบบโลคอล ซึ่งอาจนำไปสู่พฤติกรรมที่ไม่คาดคิดถ้าไม่จัดการอย่างเหมาะสม

## ดูเพิ่มเติม
- คู่มือ Bash เกี่ยวกับ Shell Functions: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- คู่มือ Bash-Scripting ขั้นสูง: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" สำหรับแนวคิดและปฏิบัติการสคริปต์ฟังก์ชันอย่างลึกซึ้ง.
