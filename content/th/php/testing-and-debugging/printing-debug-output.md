---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:17.784133-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E01\u0E32\u0E23\u0E41\u0E2A\
  \u0E14\u0E07\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\
  \u0E01\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E41\u0E1B\u0E25\u0E07\u0E21\u0E32\u0E01\u0E19\u0E31\u0E01: \u0E44\u0E14\u0E49\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E21\u0E32\
  \u0E15\u0E31\u0E49\u0E07\u0E41\u0E15\u0E48\u0E27\u0E31\u0E19\u0E41\u0E23\u0E01\u0E46\
  \ \u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E19\u0E22\u0E38\u0E04\u0E42\u0E1A\u0E23\u0E32\u0E13\u0E43\u0E0A\
  \u0E49 printf() \u0E43\u0E19\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01 \u0E1E\
  \u0E35\u0E40\u0E2D\u0E0A\u0E1E\u0E35 (PHP) \u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-04-05T22:51:14.354320-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\u0E21\u0E48\u0E44\u0E14\u0E49\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E21\u0E32\u0E01\
  \u0E19\u0E31\u0E01."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีทำ:
```PHP
<?php
// การแสดงผลพื้นฐาน
$variable = 'Debugging rocks!';
echo $variable;

// ใช้ print_r สำหรับ arrays
$myArray = ['apple', 'orange', 'banana'];
echo '<pre>'; // ทำให้อ่านง่าย
print_r($myArray);
echo '</pre>';

// var_dump สำหรับรายละเอียด
$anotherArray = ['key' => 'value', 'anotherKey' => 123];
var_dump($anotherArray);

// ส่งไปยัง error log
error_log('This goes to the logs for stealthier debugs.');
?>
```
ผลลัพธ์ตัวอย่าง:
```
Debugging rocks!
Array
(
    [0] => apple
    [1] => orange
    [2] => banana
)
array(2) {
  ["key"]=>
  string(5) "value"
  ["anotherKey"]=>
  int(123)
}
```

## การดำดิ่งลึก:
การแสดงผลข้อมูลดีบักไม่ได้เปลี่ยนแปลงมากนัก: ได้รับการใช้งานมาตั้งแต่วันแรกๆ ที่โปรแกรมเมอร์ในยุคโบราณใช้ printf() ในการดีบัก พีเอชพี (PHP) ใช้ `echo`, `print`, `print_r()`, และ `var_dump()` อย่างเต็มที่ มันอาจไม่หรูหรา แต่ก็ใช้งานได้ผล PHP ในยุคสมัยใหม่ยังมี Xdebug, ซึ่งสามารถเดินผ่านโค้ดและแสดงผลลัพธ์อย่างหรูหรากว่าได้ สำหรับบันทึก, คุณมี `error_log()`, ซึ่งส่งข้อความลับไปยังบันทึกของเซิร์ฟเวอร์โดยไม่เปิดเผยต่อผู้ใช้ แต่ละเครื่องมือมีที่มาที่ไปของมันเอง: `echo` และ `print` สำหรับการใช้งานที่รวดเร็วและง่ายดาย; `print_r()` สำหรับข้อมูลอะเรย์ที่เป็นมิตรกับมนุษย์; `var_dump()` เพื่อให้รายละเอียดเกี่ยวกับประเภทและความยาว; `error_log()` ใช้สำหรับเมื่อคุณอยากดำดิ่งแบบนักสืบในไซต์ที่กำลังใช้งานจริง

## ดูเพิ่มเติมที่:
- คู่มือ PHP สำหรับ `echo`: https://www.php.net/manual/en/function.echo.php
- เพิ่มเติมเกี่ยวกับ `print_r()`: https://www.php.net/manual/en/function.print-r.php
- รายละเอียดของ `var_dump()`: https://www.php.net/manual/en/function.var-dump.php
- ดำดิ่งลงไปในการบันทึกโดยใช้ `error_log()`: https://www.php.net/manual/en/function.error-log.php
- Xdebug, เพื่อนรักของนักดีบัก: https://xdebug.org/docs/display
