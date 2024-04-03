---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:16.391649-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.628386-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
```Fish Shell
# ลบตัวเลขออกจากสตริง
set string "Fish123Shell"
echo $string | string replace -ra '[0-9]' ''
# ผลลัพธ์: FishShell

# ตัดทุกอย่างออกยกเว้นตัวอักษรพิมพ์เล็ก
set noisy_string "F!i@s#h$%S^h&e*l(l)__+"
echo $noisy_string | string match -r '[a-z]+'
# ผลลัพธ์: ishhell
```

## การดำดิ่งลึก
ใน Fish Shell, ความมหัศจรรย์เกิดขึ้นด้วยเครื่องมือ `string`, เครื่องมือในตัวที่มีประโยชน์สำหรับการดำเนินการกับสตริง - ซึ่งถูกแนะนำในเวอร์ชัน 2.3.0. ก่อนหน้านี้, ผู้ใช้จะต้องพึ่งพาเครื่องมือมาตรฐานของ UNIX เช่น `sed` หรือ `awk`. ทำไมถึงเปลี่ยน? เพื่อความง่ายและการรวม. การมีเครื่องมือในตัวทำให้การจัดการสตริงง่ายดายยิ่งขึ้น, ทำให้สคริปต์อ่านและดูแลรักษาได้ง่ายขึ้น.

มีทางเลือกอื่นไหม? แน่นอน, เครื่องมือเก่าแก่ `sed` ยังสามารถทำงานได้:

```Fish Shell
set old_school_string "Fish@Shell2023"
echo $old_school_string | sed 's/[0-9]//g'
# ผลลัพธ์: Fish@Shell
```

แต่ทำไมไม่ใช้เครื่องมือของ Fish เอง? สำหรับการนำไปใช้, `string replace` มีตัวเลือก `-r` ที่เปิดใช้งานรูปแบบ regex. `-a` ใช้คำสั่งกับการจับคู่ทั้งหมดและการเพิ่ม '' ที่ปลายทำให้เปลี่ยนไปเป็นอย่างอื่น, คือ, ลบ. ใช้ `string match` เมื่อค้นหารูปแบบที่จะเก็บ, ไม่ใช่สิ่งที่จะทิ้ง.

## ดูเพิ่มเติม
- เอกสารประกอบการใช้งานอย่างเป็นทางการของ Fish Shell เกี่ยวกับ `string`: https://fishshell.com/docs/current/cmds/string.html
- บทเรียน Regex สำหรับการดำดิ่งลงไปในรูปแบบ: https://www.regular-expressions.info/
- Sed & Awk, พลังของข้อความที่มีมายาวนาน: แนะนำสั้นๆ: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html
