---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:05.421821-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E1C\u0E2A\u0E32\u0E19\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\
  \u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E2D\
  \u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19"
lastmod: '2024-03-17T21:57:56.296780-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E1C\u0E2A\u0E32\u0E19\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\u0E40\u0E02\u0E49\u0E32\
  \u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E2D\
  \u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การแทรกสตริงช่วยให้คุณสามารถใส่ค่าของตัวแปรเข้าไปในสตริงโดยตรง โปรแกรมเมอร์ใช้เพื่อผสานตัวแปรเข้ากับข้อความ ทำให้โค้ดสะอาดและอ่านง่ายขึ้น

## วิธีการ:

ใน PHP คุณสามารถแทรกสตริงโดยใช้เครื่องหมายคำพูดคู่หรือไวยากรณ์ heredoc:

```php
$name = "World";
echo "Hello, $name!"; // ผลลัพธ์: Hello, World!

// การใช้ curly braces สำหรับตัวแปรที่ซับซ้อนกว่า
$object = new stdClass();
$object->greeting = "Hello";
echo "{$object->greeting}, $name!"; // ผลลัพธ์: Hello, World!

// ไวยากรณ์ Heredoc สำหรับสตริงหลายบรรทัด
$heredoc = <<<EOT
This is a string that contains $name within it.
You can write as much as you want here.
EOT;
echo $heredoc; // ผลลัพธ์: This is a string that contains World within it.
```

หมายเหตุ: เครื่องหมายคำพูดเดี่ยวจะไม่แทรก:

```php
echo 'Hello, $name!'; // ผลลัพธ์: Hello, $name!
```

## การศึกษาลึก

ก่อนที่ PHP จะมีการแทรก การต่อสตริงกับตัวดำเนินการจุด (.) เป็นวิธีที่ใช้ ตัวอย่างเช่น:

```php
echo 'Hello, ' . $name . '!';
```

การแทรกช่วยให้การทำสิ่งนี้ง่ายขึ้นโดยการประมวลผลตัวแปรโดยตรงภายในสตริง

การแทรกสตริงได้มีมาตั้งแต่ PHP 4 แต่การใช้นิพจน์ที่ซับซ้อนภายใน curly braces ได้รับความยืดหยุ่นมากขึ้นด้วย PHP 7 ด้วยการปรับปรุงเหล่านี้ PHP ได้ทำให้การฝังตัวแปรใดๆ รวมถึงคุณสมบัติของอ็อบเจ็กต์และองค์ประกอบของอาร์เรย์เข้ากับสตริงง่ายขึ้น

มีทางเลือกอื่นๆ ในการแทรก เช่น การใช้ `sprintf()` สำหรับสตริงที่มีการกำหนดรูปแบบหรือ `implode()` สำหรับอาร์เรย์ การทางเลือกเหล่านี้บางครั้งอาจมอบการควบคุมการจัดรูปแบบสตริงที่ดีกว่า โดยเฉพาะสำหรับการท้องถิ่นและโครงสร้างที่ซับซ้อน

ในด้านการใช้งาน PHP มองหาตัวแปรภายในสตริงเมื่ออยู่ในเครื่องหมายคำพูดคู่หรือไวยากรณ์ heredoc และแทนที่ด้วยค่าของตัวแปร ตัวแปรวิเคราะห์จะไม่สนใจตัวหน้าดอลลาร์ ($) ในสตริงที่มีเครื่องหมายคำพูดเดี่ยว รักษาเหมือนกับอักขระปกติ

## ดูเพิ่มเติม

- [PHP: Strings](http://php.net/manual/en/language.types.string.php) - เอกสารแนะนำทางการของ PHP เกี่ยวกับสตริง
- [PHP: Heredoc syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - ส่วนละเอียดของ PHP manual เกี่ยวกับ Heredoc
- [PHP: String Operators](https://www.php.net/manual/en/language.operators.string.php) - เพิ่มเติมเกี่ยวกับการต่อสตริงและตัวดำเนินการจุด
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - เอกสารของฟังก์ชัน `sprintf()` สำหรับการจัดรูปแบบสตริง
