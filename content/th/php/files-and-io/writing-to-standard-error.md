---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:28.797953-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E1B\u0E22\u0E31\
  \u0E07\u0E2A\u0E41\u0E15\u0E19\u0E14\u0E32\u0E23\u0E4C\u0E14\u0E40\u0E2D\u0E2D\u0E40\
  \u0E23\u0E2D\u0E23\u0E4C (stderr) \u0E43\u0E19 PHP \u0E40\u0E1B\u0E47\u0E19\u0E40\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\
  \u0E09\u0E31\u0E22\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E41\u0E22\u0E01\u0E2D\u0E2D\u0E01\
  \u0E08\u0E32\u0E01\u0E2A\u0E41\u0E15\u0E19\u0E14\u0E32\u0E23\u0E4C\u0E14\u0E40\u0E2D\
  \u0E32\u0E17\u0E4C\u0E1E\u0E38\u0E15 (stdout)\u2026"
lastmod: '2024-03-17T21:57:56.328395-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E1B\u0E22\u0E31\
  \u0E07\u0E2A\u0E41\u0E15\u0E19\u0E14\u0E32\u0E23\u0E4C\u0E14\u0E40\u0E2D\u0E2D\u0E40\
  \u0E23\u0E2D\u0E23\u0E4C (stderr) \u0E43\u0E19 PHP \u0E40\u0E1B\u0E47\u0E19\u0E40\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\
  \u0E32\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\
  \u0E09\u0E31\u0E22\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E41\u0E22\u0E01\u0E2D\u0E2D\u0E01\
  \u0E08\u0E32\u0E01\u0E2A\u0E41\u0E15\u0E19\u0E14\u0E32\u0E23\u0E4C\u0E14\u0E40\u0E2D\
  \u0E32\u0E17\u0E4C\u0E1E\u0E38\u0E15 (stdout)\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การเขียนข้อความผิดพลาดไปยังสแตนดาร์ดเออเรอร์ (stderr) ใน PHP เป็นเรื่องของการนำส่งข้อความผิดพลาดหรือการวินิจฉัยปัญหาแยกออกจากสแตนดาร์ดเอาท์พุต (stdout) ช่วยให้นักพัฒนาสามารถจัดการกับกระแสข้อมูลเอาท์พุตได้ดีขึ้นสำหรับการแก้ไขปัญหาและการบันทึกการทำงาน โปรแกรมเมอร์ใช้เทคนิคนี้เพื่อให้แน่ใจว่าข้อความผิดพลาดไม่จะไม่รบกวนไปยังเอาท์พุตของโปรแกรม ทำให้ง่ายต่อการตรวจสอบและแก้ไขปัญหาในแอปพลิเคชัน

## วิธีการ:

ใน PHP, การเขียนไปยัง stderr สามารถทำได้โดยใช้ฟังก์ชัน `fwrite()` ร่วมกับค่าคงที่ที่กำหนดไว้ล่วงหน้า `STDERR` ซึ่งแทนกระแสเอาท์พุตข้อความผิดพลาด

```php
<?php
// เขียนข้อความง่ายๆ ไปยัง stderr
fwrite(STDERR, "This is an error message.\n");
```

ตัวอย่างเอาท์พุตเมื่อสคริปต์ถูกเรียกใช้จากคอมมานด์ไลน์:
```
This is an error message.
```

เพื่อแสดงการใช้งานที่มีประโยชน์มากขึ้น ลองพิจารณาสถานการณ์ที่คุณกำลังแยกวิเคราะห์ข้อมูลจากผู้ใช้และพบข้อมูลที่ไม่คาดคิด:
```php
<?php
$input = 'unexpected data';

// จำลองการเกิดข้อผิดพลาดในการประมวลผลข้อมูลจากผู้ใช้
if ($input === 'unexpected data') {
    fwrite(STDERR, "Error: Unexpected input received.\n");
    exit(1); // ออกจากโปรแกรมด้วยค่าที่ไม่เป็นศูนย์เพื่อระบุว่าเกิดข้อผิดพลาด
}
```

ในขณะที่ความสามารถในตัวของ PHP ในการจัดการกับ stderr โดยทั่วไปแล้วเพียงพอสำหรับการใช้งานแล้ว แต่เมื่อต้องการจัดการกับแอปพลิเคชันที่ซับซ้อนมากขึ้นหรือต้องการให้การบันทึกผิดพลาดของ stderr รวมเข้ากับระบบภายนอก ไลบรารีของบุคคลที่สามเช่น Monolog สามารถเป็นทางเลือกที่แข็งแกร่ง Monolog เป็นไลบรารีการบันทึกข้อมูลที่สามารถจัดการกับ stderr รวมทั้งเป้าหมายอื่นๆ (ไฟล์, ซอกเก็ต ฯลฯ)

การใช้ Monolog เขียนไปยัง stderr:

ขั้นตอนแรก, ตรวจสอบให้แน่ใจว่าคุณได้ติดตั้ง Monolog ผ่าน Composer:
```
composer require monolog/monolog
```

จากนั้น, คุณสามารถกำหนดค่า Monolog ให้ใช้ `StreamHandler` ที่มุ่งเป้าไปที่ `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// สร้างช่องทางบันทึก
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// เพิ่มข้อความบันทึกไปยัง stderr
$log->warning('This is a warning message.');
```

โค้ดข้างต้นใช้ประโยชน์จาก Monolog เพื่อส่งข้อความแจ้งเตือนไปยัง stderr ซึ่งมีความเหมาะสมโดยเฉพาะสำหรับแอปพลิเคชันที่ต้องการการกำหนดค่าการบันทึกข้อมูลรายละเอียดหรือการตรวจสอบบันทึกภายนอก
