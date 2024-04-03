---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:52.689789-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 PHP \u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E42\u0E14\u0E22\u0E43\u0E0A\
  \u0E49\u0E1A\u0E25\u0E47\u0E2D\u0E04 `try-catch` \u0E41\u0E25\u0E30\u0E04\u0E38\u0E13\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1B\u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\
  \u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E14\u0E49\u0E27\u0E22\u0E15\
  \u0E31\u0E27\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\
  \u0E1E\u0E25\u0E32\u0E14\u0E41\u0E1A\u0E1A\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\
  \u0E07\u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E22\u0E01\u0E40\u0E27\u0E49\u0E19."
lastmod: '2024-03-17T21:57:56.319837-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 PHP \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1A\u0E25\u0E47\u0E2D\u0E04 `try-catch`\
  \ \u0E41\u0E25\u0E30\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1B\
  \u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\
  \u0E32\u0E23\u0E14\u0E49\u0E27\u0E22\u0E15\u0E31\u0E27\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E41\u0E1A\u0E1A\
  \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\u0E07\u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\
  \u0E22\u0E01\u0E40\u0E27\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
ใน PHP คุณสามารถจัดการข้อผิดพลาดโดยใช้บล็อค `try-catch` และคุณสามารถปรับแต่งกระบวนการด้วยตัวจัดการข้อผิดพลาดแบบกำหนดเองและข้อยกเว้น

```php
// ตัวอย่างการใช้งาน try-catch พื้นฐาน
try {
  // ทำสิ่งที่มีความเสี่ยง
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // จัดการกับข้อผิดพลาด
  echo "Error: " . $e->getMessage();
}

// การตั้งค่าตัวจัดการข้อผิดพลาดแบบกำหนดเอง
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// การใช้ข้อยกเว้น
class MyException extends Exception {}

try {
  // ทำอะไรบางอย่างและโยนข้อยกเว้นแบบกำหนดเอง
  throw new MyException("Custom error!");
} catch (MyException $e) {
  // จัดการกับข้อยกเว้นแบบกำหนดเอง
  echo $e->getMessage();
}

// ตัวอย่างผลลัพธ์:
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// Custom error!
```

## การศึกษาลึก
ในอดีต ข้อผิดพลาดใน PHP มักจะเกี่ยวกับการเตือนและข้อความแจ้งเตือนที่ไม่หยุดการทำงานของสคริปต์ เมื่อภาษาพัฒนา เริ่มมีการจัดการข้อผิดพลาดแบบวัตถุที่เข้มงวดขึ้นผ่านคลาส Exception ที่เ introduced ใน PHP 5 หลังจากนั้น PHP 7 ออกมาพร้อมกับคลาส Error ที่แยกชัดเจนระหว่างข้อผิดพลาดและข้อยกเว้น

ก่อนมีบล็อค `try-catch` PHP ใช้ `set_error_handler()` ในการจัดการข้อผิดพลาด `try-catch` ทำให้โค้ดสะอาดและทันสมัยยิ่งขึ้น แต่ตัวจัดการข้อผิดพลาดแบบกำหนดเองยังมีความจำเป็น โดยเฉพาะสำหรับโค้ดเก่าหรือเมื่อคุณต้องจับข้อผิดพลาดที่ปกติจะไม่ใช่ข้อยกเว้น

อินเทอร์เฟส `Throwable` ใน PHP 7+ หมายความว่าไม่ว่าจะเป็น Error หรือ Exception คุณสามารถจับได้ทั้งคู่ นี่เป็นสิ่งที่สะดวกเพราะตอนนี้คุณไม่พลาดข้อผิดพลาดที่เกิดขึ้นในระหว่างการทำงานที่ยากต่อการติดตามก่อนหน้านี้

ทางเลือกนอกเหนือจากกลไกภายในสำหรับการจัดการข้อผิดพลาดของ PHP ได้แก่ ไลบรารีและเฟรมเวิร์คที่มีระบบการจัดการข้อผิดพลาดของตัวเอง ซึ่งมีคุณสมบัติเพิ่มเติมเช่น การบันทึกข้อผิดพลาดลงไฟล์หรือการแสดงหน้าข้อผิดพลาดที่เป็นมิตรกับผู้ใช้

## ดูเพิ่มเติม
- เอกสารของ PHP อย่างเป็นทางการเกี่ยวกับข้อยกเว้น: https://www.php.net/manual/en/language.exceptions.php
- PHP The Right Way เกี่ยวกับการรายงานข้อผิดพลาด: https://phptherightway.com/#error_reporting
- PHP Manual เกี่ยวกับการจัดการข้อผิดพลาด: https://www.php.net/manual/en/book.errorfunc.php
