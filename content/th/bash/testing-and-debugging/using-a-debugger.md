---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:05.810629-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Bash \u0E44\u0E21\u0E48\u0E21\
  \u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A debugger \u0E43\u0E19\u0E15\
  \u0E31\u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E1A\u0E32\u0E07\u0E20\u0E32\u0E29\
  \u0E32\u0E2D\u0E37\u0E48\u0E19 \u0E46 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E0A\u0E48\u0E19 `set -x` \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\
  \u0E48\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E31\u0E1B\u0E40\u0E01\u0E23\u0E14\
  \ \u0E21\u0E35\u2026"
lastmod: '2024-03-17T21:57:56.401773-06:00'
model: gpt-4-0125-preview
summary: "Bash \u0E44\u0E21\u0E48\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\
  \u0E1A debugger \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\
  \u0E1A\u0E32\u0E07\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E37\u0E48\u0E19 \u0E46 \u0E41\
  \u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E0A\u0E48\
  \u0E19 `set -x` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\
  \u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\
  \u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\
  \u0E31\u0E1B\u0E40\u0E01\u0E23\u0E14 \u0E21\u0E35 `bashdb` \u0E0B\u0E36\u0E48\u0E07\
  \u0E40\u0E1B\u0E47\u0E19 debugger \u0E17\u0E35\u0E48\u0E40\u0E2B\u0E21\u0E32\u0E30\
  \u0E2A\u0E21\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E14\u0E34\u0E19\u0E1C\u0E48\u0E32\
  \u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E39\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\
  \u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
Bash ไม่มาพร้อมกับ debugger ในตัวเหมือนบางภาษาอื่น ๆ แต่คุณสามารถใช้คำสั่งในตัวเช่น `set -x` เพื่อติดตามสิ่งที่เกิดขึ้น หรือเพื่อการอัปเกรด มี `bashdb` ซึ่งเป็น debugger ที่เหมาะสมในการเดินผ่านโค้ดของคุณ นี่คือการดูอย่างรวดเร็ว:

```Bash
# การใช้ set -x เพื่อ debug
set -x
echo "เริ่มการดีบัก"
my_var="สวัสดี, โลกของการดีบัก!"
echo $my_var
set +x

# การใช้ bashdb
# ติดตั้ง bashdb ด้วย package manager ของคุณ เช่น apt, yum, brew.
# ดีบักสคริปต์ที่ชื่อ my_script.sh:
bashdb my_script.sh
```

ผลลัพธ์เมื่อเรียกใช้ด้วย `set -x`:
```Bash
+ echo 'เริ่มการดีบัก'
เริ่มการดีบัก
+ my_var='สวัสดี, โลกของการดีบัก!'
+ echo 'สวัสดี, โลกของการดีบัก!'
สวัสดี, โลกของการดีบัก!
+ set +x
```

## ดำดิ่งลึกลงไป
ในอดีต, การดีบักสคริปต์ Bash หมายถึงการฝังคำสั่ง `echo` ลงในโค้ดของคุณ แต่แล้ว `set -x` ก็ปรากฏขึ้น ให้เราดูการดำเนินการรันไทม์โดยไม่ต้องพิมพ์ด้วยตนเอง และสำหรับผู้ที่มีความต้องการควบคุมมากขึ้น `bashdb` ก็ปรากฏขึ้น ได้แรงบันดาลใจจาก debugger gdb สำหรับภาษา C/C++.

ส่วนทางเลือกอื่น ๆ นอกเหนือจากคำสั่ง `set` (`-x`, `-v`, `-e`), ตัวเลือกอื่น ๆ รวมถึงการเปลี่ยนเส้นทางข้อมูลออกไปยังไฟล์เพื่อการวิเคราะห์หรือการใช้เครื่องมือภายนอกเช่น ShellCheck สำหรับการวิเคราะห์แบบสแตติก

ในเรื่องของการใช้งาน, `set -x` นั้นง่าย มันเป็นตัวเลือกของ Bash ในตัวที่พิมพ์คำสั่งและอาร์กิวเมนต์ของพวกเขาขณะที่ถูกเรียกใช้ `bashdb`, ในทางกลับกัน ช่วยให้คุณสามารถเดินผ่านโค้ด, ตั้งจุดหยุด, และประเมินนิพจน์ - สิ่งต่าง ๆ ที่ให้คุณมีโอกาสต่อสู้กับบัคที่ยากจะค้นพบมากขึ้น

## ดูเพิ่มเติม
- โปรเจค Bash Debugger: http://bashdb.sourceforge.net/
- "Pro Bash Programming" โดย Chris Johnson และ Jayant Varma สำหรับการเขียนสคริปต์ขั้นสูง
- ShellCheck สำหรับการวิเคราะห์แบบสแตติก: https://www.shellcheck.net/
