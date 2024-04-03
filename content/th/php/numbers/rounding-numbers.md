---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:28.794551-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\
  \u0E31\u0E14\u0E17\u0E34\u0E49\u0E07\u0E17\u0E28\u0E19\u0E34\u0E22\u0E21\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E44\u0E14\u0E49\u0E04\u0E27\u0E32\u0E21\u0E41\
  \u0E21\u0E48\u0E19\u0E22\u0E33\u0E17\u0E35\u0E48\u0E15\u0E31\u0E49\u0E07\u0E44\u0E27\
  \u0E49, \u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E40\u0E15\u0E47\u0E21 \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E1B\u0E31\
  \u0E14\u0E40\u0E28\u0E29\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\
  \u0E49\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.306386-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\
  \u0E31\u0E14\u0E17\u0E34\u0E49\u0E07\u0E17\u0E28\u0E19\u0E34\u0E22\u0E21\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E44\u0E14\u0E49\u0E04\u0E27\u0E32\u0E21\u0E41\
  \u0E21\u0E48\u0E19\u0E22\u0E33\u0E17\u0E35\u0E48\u0E15\u0E31\u0E49\u0E07\u0E44\u0E27\
  \u0E49, \u0E1A\u0E48\u0E2D\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E40\u0E15\u0E47\u0E21 \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E1B\u0E31\
  \u0E14\u0E40\u0E28\u0E29\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\
  \u0E49\u0E19, \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E1B\u0E23\u0E30\u0E2A\u0E34\u0E17\
  \u0E18\u0E34\u0E20\u0E32\u0E1E, \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E34\
  \u0E15\u0E23\u0E01\u0E31\u0E1A\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
weight: 13
---

## วิธีการ:
PHP มีวิธีในการปัดเศษตัวเลขหลายวิธี: `round()`, `ceil()`, และ `floor()`. นี่คือวิธีทำงานของพวกเขา:

```php
echo round(3.14159);   // คืนค่าเป็น 3
echo round(3.14159, 2); // คืนค่าเป็น 3.14

echo ceil(3.14159);    // คืนค่าเป็น 4, ปัดเศษขึ้นเสมอ

echo floor(3.14159);   // คืนค่าเป็น 3, ปัดเศษลงเสมอ
```

## บทพิเศษ
การปัดเศษตัวเลขเป็นสิ่งจำเป็นในคณิตศาสตร์และการคำนวณตั้งแต่สมัยโบราณเพื่อรับมือกับทศนิยมไม่สิ้นสุดที่ไม่ปฏิบัติได้ ใน PHP, `round()` สามารถรับพารามิเตอร์ความแม่นยำและโหมด, ซึ่งส่งผลต่อการทำงานของมัน – `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, ฯลฯ, กำหนดว่าจะปฏิบัติการอย่างไรเมื่อเจอกับสถานการณ์ ".5" ความแม่นยำเป็นสิ่งสำคัญในแอปพลิเคชันทางการเงินที่การปัดเศษอาจถูกกำกับด้วยกฎหมาย, ส่งผลต่อวิธีการใช้ `round()` ในโค้ด

ทางเลือกในการใช้ฟังก์ชันเริ่มต้น ได้แก่ วิธีการปัดเศษแบบกำหนดเองหรือฟังก์ชัน BC Math สำหรับการคำนวณความแม่นยำแบบไม่จำกัด ซึ่งมีประโยชน์สำหรับสถานการณ์ที่ต้องการควบคุมมากขึ้นหรือจัดการกับตัวเลขขนาดใหญ่ซึ่งความแม่นยำแบบพื้นฐานอาจสะดุด

## ดูเพิ่มเติม
สำรวจเพิ่มเติมในคู่มือ PHP:
- [ฟังก์ชัน `round` ของ PHP](https://php.net/manual/en/function.round.php)
- [ฟังก์ชัน `ceil` ของ PHP](https://php.net/manual/en/function.ceil.php)
- [ฟังก์ชัน `floor` ของ PHP](https://php.net/manual/en/function.floor.php)
- [BC Math สำหรับคำนวณความแม่นยำแบบไม่จำกัด](https://php.net/manual/en/book.bc.php)
