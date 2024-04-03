---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:43.595257-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E43\u0E0A\u0E49 `strtolower`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\
  \u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\u0E43\u0E19\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\
  \u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19."
lastmod: '2024-03-17T21:57:56.297677-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E43\u0E0A\u0E49 `strtolower` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E31\
  \u0E49\u0E07\u0E2B\u0E21\u0E14\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\
  \u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E07\u0E32\u0E19."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
PHP ใช้ `strtolower` เพื่อทำให้ตัวอักษรทั้งหมดในสตริงเป็นตัวพิมพ์เล็ก นี่คือวิธีการทำงาน:

```php
<?php
$originalString = "HeLLo WoRLD!";
$lowerCaseString = strtolower($originalString);

echo $lowerCaseString; // แสดงผล: hello world!
?>
```

หากคุณต้องการจัดการกับการเข้ารหัสตัวอักษรหลายไบต์ เช่น UTF-8 ให้ใช้ `mb_strtolower` แทน:

```php
<?php
$originalString = "İstanbul";
$lowerCaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowerCaseString; // แสดงผล: istanbul (แปลง İ เป็น i ได้อย่างถูกต้อง)
?>
```

## ลงลึก
ในอดีต, ฟังก์ชัน `strtolower` ของ PHP ถูกใช้เป็นฟังก์ชันหลักสำหรับการแปลงตัวพิมพ์ นำมาใช้ในเวอร์ชันต้น ๆ ของ PHP อย่างไรก็ตาม เมื่อแอปพลิเคชัน PHP กลายเป็นสากลมากขึ้น ความจำเป็นในการจัดการการเข้ารหัสตัวอักษรหลายไบต์ทำให้มีการใช้ `mb_strtolower`

ตัวเลือกอื่นที่ถือว่าเป็นทางเลือกของ `strtolower` และ `mb_strtolower` ได้แก่การใช้ regular expressions กับฟังก์ชัน `mb_ereg_replace_callback` หรือ `preg_replace_callback` แต่สำหรับการแปลงตัวพิมพ์ง่าย ๆ แล้ว พวกมันอาจจะมากเกินไป

ใน PHP, สตริงมักจะพิจารณาตามไบต์ ไม่ใช่ตามตัวอักษร หมายความว่าไบต์ละหนึ่งตัวอักษร นี่ทำงานได้ดีสำหรับการเข้ารหัสแบบไบต์เดียวเช่น ASCII ซึ่งตัวอักษรแต่ละตัวจริง ๆ แล้วเป็นหนึ่งไบต์ สำหรับการเข้ารหัสหลายไบต์, `mb_strtolower` ทำความเข้าใจการเข้ารหัสตัวอักษรและปฏิบัติต่อตัวอักษรอย่างที่ควรจะเป็น

## ดูเพิ่มเติม
- คู่มือ PHP เกี่ยวกับ `strtolower`: https://www.php.net/manual/en/function.strtolower.php
- คู่มือ PHP เกี่ยวกับ `mb_strtolower`: https://www.php.net/manual/en/function.mb-strtolower.php
- UTF-8 และ Unicode สำหรับนักพัฒนา PHP: https://www.php.net/manual/en/book.mbstring.php
