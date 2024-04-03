---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:44.215676-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1B\u0E34\u0E14\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 PHP REPL \u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\
  \u0E23\u0E31\u0E19 `php -a` \u0E43\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\
  \u0E19\u0E31\u0E25\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E27\u0E48\u0E32\u0E21\u0E31\
  \u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23."
lastmod: '2024-03-17T21:57:56.313790-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1B\u0E34\u0E14\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 PHP REPL \u0E42\
  \u0E14\u0E22\u0E01\u0E32\u0E23\u0E23\u0E31\u0E19 `php -a` \u0E43\u0E19\u0E40\u0E17\
  \u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\u0E31\u0E25\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E44\u0E23."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
เปิดใช้งาน PHP REPL โดยการรัน `php -a` ในเทอร์มินัลของคุณ นี่คือตัวอย่างว่ามันทำงานอย่างไร:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

คุณสามารถกำหนดฟังก์ชันได้เช่นกัน:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## ศึกษาเพิ่มเติม
REPL มีอยู่ในรูปแบบหนึ่งนับตั้งแต่ยุคแรกๆ ของ LISP ในช่วงปี 1960 REPL ของ PHP ไม่ก้าวหน้าเมื่อเปรียบเทียบกับภาษาอื่นๆ เช่น Python หรือ JavaScript มันไม่สามารถบันทึกสถานะระหว่างเซสชัน และขาดฟีเจอร์เช่นการเติมคำอัตโนมัติ สำหรับ PHP REPL ที่มีคุณสมบัติมากกว่า ควรพิจารณาทางเลือกอื่นๆ เช่น `psysh` หรือ `boris` Shell ของบุคคลที่สามเหล่านี้เสนอเครื่องมือตรวจสอบที่ดีกว่า, การเติมคำด้วยแท็บ, และแม้กระทั่งตัวดีบักเกอร์

ใต้ฝาครอบ, REPL ของ PHP ทำงานโดยการคอมไพล์และดำเนินการทุกบรรทัดของโค้ดที่ป้อนเข้ามา ข้อจำกัดของวิธีนี้กลายเป็นเรื่องชัดเจนเมื่อมีเรื่องเช่นการประกาศคลาสใหม่, ซึ่งไม่สามารถทำได้ในเซสชั่นเดียวกัน มันเหมาะสำหรับการทดสอบง่ายๆ แต่อาจยุ่งยากสำหรับงานที่ซับซ้อน

## ดูเพิ่มเติม
- [คู่มือ PHP - Shell แบบโต้ตอบ](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: คอนโซลนักพัฒนาสำหรับรันไทม์, ดีบักเกอร์แบบโต้ตอบ และ REPL สำหรับ PHP](https://psysh.org/)
- [Boris: REPL ขนาดเล็กสำหรับ PHP](https://github.com/borisrepl/boris)
