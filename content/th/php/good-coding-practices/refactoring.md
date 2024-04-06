---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:59.843029-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E19\
  \u0E33\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14 PHP\
  \ \u0E41\u0E1A\u0E1A\u0E04\u0E25\u0E32\u0E2A\u0E2A\u0E34\u0E01\u0E21\u0E32 Refactoring\
  \ \u0E01\u0E31\u0E19\u0E14\u0E39 \u0E01\u0E48\u0E2D\u0E19\u0E01\u0E32\u0E23 Refactoring\
  \ \u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32\u0E2D\u0E32\u0E08\
  \u0E08\u0E30\u0E14\u0E39\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E19\u0E35\u0E49."
lastmod: '2024-04-05T21:54:02.050902-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E19\u0E33\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E42\u0E04\u0E49\u0E14 PHP \u0E41\u0E1A\u0E1A\u0E04\u0E25\u0E32\u0E2A\u0E2A\
  \u0E34\u0E01\u0E21\u0E32 Refactoring \u0E01\u0E31\u0E19\u0E14\u0E39 \u0E01\u0E48\
  \u0E2D\u0E19\u0E01\u0E32\u0E23 Refactoring \u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\
  \u0E40\u0E23\u0E32\u0E2D\u0E32\u0E08\u0E08\u0E30\u0E14\u0E39\u0E40\u0E2B\u0E21\u0E37\
  \u0E2D\u0E19\u0E19\u0E35\u0E49."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
ลองนำตัวอย่างโค้ด PHP แบบคลาสสิกมา Refactoring กันดู

ก่อนการ Refactoring โค้ดของเราอาจจะดูเหมือนนี้:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

แต่เราสามารถปรับโค้ดนี้ให้ดีขึ้นเพื่อความชัดเจนและ modular ดังนี้:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
ด้วยการแยกฟังก์ชัน `printOrderDetails` ออกเป็นฟังก์ชันย่อย ๆ ทำให้โค้ดของเราอ่านง่ายและง่ายต่อการตรวจสอบข้อผิดพลาด

## การศึกษาลึก
Refactoring มีรากฐานมาจากชุมชนการโปรแกรม smalltalk ในช่วงต้น 1990s และได้รับความนิยมมากขึ้นจากหนังสือที่สำคัญของ Martin Fowler ชื่อ "Refactoring: Improving the Design of Existing Code" (1999) ในขณะที่การ Refactoring สามารถใช้ได้กับภาษาโปรแกรมใด ๆ PHP ที่มีความละเอียดอ่อนทางไดนามิก ช่วยให้เกิดความท้าทายและโอกาสพิเศษ

ทางเลือกอื่น ๆ ต่อการ Refactoring อาจรวมถึงการเขียนโค้ดใหม่จากต้น ซึ่งมักจะเสี่ยงและใช้เวลามากขึ้น ในระบบนิเวศของ PHP, เครื่องมือเช่น PHPStan และ Rector สามารถสังเกตและดำเนินการ Refactoring โดยอัตโนมัติได้ ตามลำดับ ในด้านการบริหารจัดการ, การเก็บ Refactoring ไว้เล็ก ๆ และการทดสอบอย่างกว้างขวางด้วย unit tests เป็นวิธีการสำคัญในการรับประกันการ Refactoring ที่ประสบความสำเร็จโดยไม่นำเข้าบัก

## ดูเพิ่มเติม
- หนังสือ Refactoring ของ Martin Fowler: https://martinfowler.com/books/refactoring.html
- PHPStan, เครื่องมือวิเคราะห์สถิติ PHP: https://phpstan.org/
- Rector, เครื่องมือสำหรับการ Refactoring โค้ด PHP โดยอัตโนมัติ: https://getrector.org/
- การทดสอบ Unit ใน PHP ด้วย PHPUnit: https://phpunit.de/
