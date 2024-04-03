---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:29.748028-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E21\u0E32\u0E1E\u0E23\
  \u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\
  \u0E01\u0E32\u0E23 Log \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22 \u0E41\
  \u0E04\u0E48\u0E43\u0E2A\u0E48 `error_log()` \u0E25\u0E07\u0E43\u0E19\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \ Log \u0E02\u0E2D\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\
  \u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.318823-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23 Log \u0E02\u0E49\u0E2D\u0E1C\
  \u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E07\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22 \u0E41\u0E04\u0E48\u0E43\u0E2A\u0E48 `error_log()`\
  \ \u0E25\u0E07\u0E43\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\
  \u0E13\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E44\u0E1B\u0E22\u0E31\u0E07 Log \u0E02\u0E2D\u0E07\u0E40\u0E0B\u0E34\
  \u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C \u0E04\u0E38\u0E13\u0E22\u0E31\u0E07\
  \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1B\u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\
  \u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\
  \u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\
  \u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีการ:
PHP มาพร้อมกับฟังก์ชันการ Log ข้อผิดพลาดที่ใช้งานง่าย แค่ใส่ `error_log()` ลงในโค้ดของคุณเพื่อส่งข้อความไปยัง Log ของเซิร์ฟเวอร์ คุณยังสามารถปรับแต่งมันเพื่อเขียนไปยังไฟล์ที่เฉพาะเจาะจงได้

```php
<?php
// การ Log ข้อความแบบข้อมูลทั่วไป
error_log("This is an info log entry.");

// การ Log ข้อความข้อผิดพลาด
error_log("This is an error log entry.", 0);

// การ Log ไปยังไฟล์ที่ระบุ
file_put_contents('/path/to/your/custom.log', "A custom log entry.\n", FILE_APPEND);

// การใช้ Monolog สำหรับการ Log แบบมีโครงสร้าง
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// สร้าง logger
$logger = new Logger('name');
// ตอนนี้เพิ่ม handler บางตัว
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// คุณสามารถใช้ logger ของคุณได้แล้ว
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

สิ่งนี้จะทำการผลิต Log ของคุณออกไปยัง Log ของเซิร์ฟเวอร์หรือไฟล์ที่คุณระบุในรูปแบบข้อความธรรมดา

## การศึกษาลึก:
ในอดีต, นักพัฒนา PHP พึ่งพาฟังก์ชัน `error_log()` หรือ Log ของ Apache/Nginx เพื่อจับปัญหา, แต่สิ่งนี้อาจก่อให้เกิดความวุ่นวายกับความจำเป็นในการแยกวิเคราะห์ไฟล์ข้อความธรรมดาและไม่มีวิธีง่ายๆ ในการกรองหรือเรียงลำดับ. การมีไลบรารี่สำหรับการ Log เช่น Monolog, ซึ่งได้นำเสนอยุคของการ Log แบบมีโครงสร้างใน PHP เหล่านี้ช่วยให้คุณสามารถควบคุมได้ดียิ่งขึ้นด้วยการเสนอช่องทางการ Log หลายช่อง, ระดับความรุนแรง, และผลลัพธ์ที่จัดรูปแบบ (เช่น JSON, ซึ่งมีประโยชน์มากสำหรับการแยกวิเคราะห์โดยโปรแกรม)

อื่นๆที่ไม่ใช่ Monolog ได้แก่ Log4php, KLogger, และ Log4php ของ Apache จากมุมมองการปฏิบัติ, การ Log ที่แข็งแกร่งต้องการไม่เพียงแค่การฝากข้อมูลที่ใดก็ได้, แต่ควรพิจารณาเรื่องเช่นการหมุนเวียน Log, กลยุทธ์การเก็บถาวร, และการรวมกับเครื่องมือตรวจสอบเพื่อให้มีประโยชน์อย่างแท้จริง

คุณควรเก็บ [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/) ในใจ, ซึ่งเป็นแนวทางสำหรับอินเตอร์เฟสสำหรับไลบรารี่การ Log, ทำให้มั่นใจว่าการทำงานร่วมกันและวิธีการเข้าถึงกลไกการ Log ได้อย่างเป็นเอกภาพ

## ดูเพิ่มเติมที่:
- [Monolog GitHub Repository](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface Specification](https://www.php-fig.org/psr/psr-3/)
- [PHP Error Log Documentation](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: คลาสการ Log สำหรับ PHP ที่เรียบง่าย](https://github.com/katzgrau/KLogger)
- [Log4php: กรอบการ Log ที่หลากหลายสำหรับ PHP](https://logging.apache.org/log4php/)

เริ่มต้นด้วยฟังก์ชันที่ใช้งานได้เลย, แต่สำหรับการเข้าใกล้ที่สามารถบำรุงรักษาได้และขยายขนาดได้, ควรลงทุนเวลาในการพอเพียงกับไลบรารีเช่น Monolog. มีความสุขกับการ Log!
