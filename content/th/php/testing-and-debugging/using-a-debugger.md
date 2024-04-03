---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:39.945668-06:00
description: "PHP \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E41\u0E1A\
  \u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E0A\u0E37\u0E48\u0E2D\
  \ Xdebug \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19: \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\
  \u0E48\u0E19, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E43\u0E2B\u0E49\u0E41\
  \u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35 Xdebug\
  \ \u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E41\u0E25\u0E30\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u0E04\u0E48\u0E32\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C `php.ini`\u2026"
lastmod: '2024-03-17T21:57:56.316734-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E41\u0E1A\u0E1A\
  \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E0A\u0E37\u0E48\u0E2D Xdebug\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19:\n\n\u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\
  \u0E19, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E43\u0E2B\u0E49\u0E41\u0E19\
  \u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35 Xdebug \u0E15\
  \u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E41\u0E25\u0E30\u0E01\u0E33\u0E2B\u0E19\u0E14\
  \u0E04\u0E48\u0E32\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C `php."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
PHP มาพร้อมกับตัวดีบักเกอร์แบบโต้ตอบที่ชื่อ Xdebug นี่คือวิธีการใช้งาน:

ก่อนอื่น, ตรวจสอบให้แน่ใจว่าคุณมี Xdebug ติดตั้งและกำหนดค่าในไฟล์ `php.ini` ของคุณ:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

ต่อไป, เขียนสคริปต์ PHP ง่ายๆ ที่มีบั๊ก:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // อุ๊บส์! นี่ควรจะเป็นเครื่องหมายบวก ไม่ใช่ลบ
}

$result = add(1, 2);
echo "Result is: $result"; // ผลลัพธ์ควรจะเป็น 3, ไม่ใช่ -1
```

โดยใช้ IDE เช่น PhpStorm, ตั้งจุดหยุดโดยการคลิกข้างเลขบรรทัด รันตัวดีบักเกอร์และสังเกตการเปลี่ยนแปลงของตัวแปรเมื่อคุณเดินผ่านขั้นตอนการปฏิบัติการ เมื่อคุณเดินผ่านฟังก์ชัน `add`, คุณจะสังเกตเห็นว่า `$result` กลายเป็น -1 ซึ่งเป็นสิ่งที่ไม่คาดคิด

## การศึกษาลึก:
ในอดีต, PHP ถูกใช้หลักๆ สำหรับสคริปต์ขนาดเล็ก และการดีบักเป็นเพียงการเพิ่มคำสั่ง `var_dump()` และ `print_r()` ลงในโค้ด ตามเวลาที่ผ่านไป, ด้วย PHP กลายเป็นผู้เล่นสำคัญในการพัฒนาเว็บ, เครื่องมือที่ซับซ้อนมากขึ้นเช่น Xdebug และ Zend Debugger จึงถูกนำมาใช้

ทางเลือกให้กับ Xdebug รวมถึง pcov และ phpdbg พวกเขาเสนอคุณสมบัติต่างๆ แต่อาจจะไม่เต็มรูปแบบเหมือนกับ Xdebug phpdbg เป็นตัวดีบักเกอร์ที่เบากว่า, โดยเฉพาะสำหรับ PHP ซึ่งถูกแจกจ่ายร่วมกับ PHP ตั้งแต่เวอร์ชัน 5.6, และ pcov เป็นไดร์เวอร์การครอบคลุมโค้ด

เมื่อนำตัวดีบักเกอร์มาใช้, โปรดจำไว้ว่าคุณไม่ควรให้ตัวดีบักเกอร์ทำงานบนเซิร์ฟเวอร์ผลิตภัณฑ์ของคุณ, เพราะมันสามารถเปิดเผยความเสี่ยงด้านความปลอดภัยและชะลอประสิทธิภาพ

## ดูเพิ่มเติมได้ที่:
- [เอกสารการใช้งาน Xdebug](https://xdebug.org/docs/)
- [คู่มือการดีบักของ PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net เกี่ยวกับ phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov บน GitHub](https://github.com/krakjoe/pcov)
