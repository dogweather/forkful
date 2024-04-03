---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:29.915818-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E27\
  \u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E18\
  \u0E23\u0E23\u0E21\u0E14\u0E32 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19, \u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  ,\u2026"
lastmod: '2024-03-17T21:57:56.323618-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E27\
  \u0E31\u0E15\u0E16\u0E38\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E18\
  \u0E23\u0E23\u0E21\u0E14\u0E32 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19, \u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  , \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\u0E21\
  \u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E17\u0E35\u0E48\u0E41\u0E15\u0E01\u0E15\u0E48\
  \u0E32\u0E07\u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## วิธีการ:
ใน PHP, ฟังก์ชัน `date()` จะจัดรูปแบบ timestamp เป็นสตริงที่อ่านง่ายขึ้น วัตถุ `DateTime` มีวัตถุประสงค์ที่คล้ายกันด้วยเมธอด `format()` นี่คือการใช้พวกเขาในการปฏิบัติ:

```php
<?php
// การใช้ฟังก์ชัน date()
echo date('Y-m-d H:i:s') . "\n"; // ผลลัพธ์: 2023-04-03 14:30:00 (ตัวอย่าง)

// การใช้วัตถุ DateTime
$dateTime = new DateTime();
echo $dateTime->format('Y-m-d H:i:s') . "\n"; // ผลลัพธ์: เหมือนกัน
?>
```
ผลลัพธ์ตัวอย่างสะท้อนถึงวันที่และเวลาที่โค้ดถูกรัน

## ลงลึก
ในอดีต, PHP ได้พัฒนาไปในด้านการจัดการวันที่และเวลา PHP เวอร์ชันแรกๆ มีคุณสมบัติการจัดการวันที่ไม่มากนัก คลาส `DateTime` ที่ถูกนำเสนอใน PHP 5.2.0, ได้ให้การจัดการแบบวัตถุ, การสนับสนุนโซนเวลา, และความหลากหลายมากขึ้น

ทางเลือกอื่นๆ สำหรับ `date()` และ `DateTime` ได้แก่:
- `strftime()` (การจัดรูปแบบที่ตระหนักถึงสถานที่)
- `DateTimeImmutable` (เวอร์ชันไม่สามารถเปลี่ยนแปลงของ `DateTime`)
- คลาสส่วนขยายเช่น `Carbon` สำหรับความต้องการที่ซับซ้อนมากขึ้น

ในภายใน, ทั้ง `date()` และ `DateTime` พึ่งพาการตั้งค่าโซนเวลาของเซิร์ฟเวอร์เว้นแต่จะระบุไว้อย่างอื่น คลาส `DateTimeZone` สามารถจัดการโซนเวลาได้

## ดูเพิ่มเติม
- [คู่มือ PHP: ฟังก์ชันวันที่และเวลา](https://www.php.net/manual/en/book.datetime.php)
- [PHP อย่างถูกวิธี: วันที่และเวลา](https://phptherightway.com/#date_and_time)
- [Carbon: ส่วนขยาย API PHP ที่ง่ายสำหรับ DateTime](https://carbon.nesbot.com/)
