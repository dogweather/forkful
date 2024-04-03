---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:49.374367-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E43\u0E2B\u0E49\u0E01\
  \u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E1E\u0E37\u0E49\u0E19\
  \u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\
  \u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E2A\u0E48\u0E27\u0E19\u0E02\u0E22\u0E32\u0E22 `ext-intl` \u0E01\u0E31\u0E1A\u0E04\
  \u0E25\u0E32\u0E2A `NumberFormatter` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\
  \u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
lastmod: '2024-03-17T21:57:56.305437-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\
  \u0E38\u0E19\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E2A\u0E48\u0E27\u0E19\u0E02\u0E22\u0E32\u0E22\
  \ `ext-intl` \u0E01\u0E31\u0E1A\u0E04\u0E25\u0E32\u0E2A `NumberFormatter` \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
PHP ให้การสนับสนุนพื้นฐานสำหรับจำนวนเชิงซ้อนโดยใช้ส่วนขยาย `ext-intl` กับคลาส `NumberFormatter` นี่คือตัวอย่าง:

```php
// ตรวจสอบให้แน่ใจว่าส่วนขยาย intl ได้ถูกโหลด
if (!extension_loaded('intl')) {
    die("The intl extension is not enabled. Please enable it to run this code.");
}

function addComplexNumbers($a, $b) {
    // ใช้ NumberFormatter เพื่อวิเคราะห์และจัดรูปแบบจำนวนเชิงซ้อน
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // วิเคราะห์จำนวนเชิงซ้อนจากสตริง
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // ทำการบวก
    $sum = $numA + $numB;

    // จัดรูปแบบผลลัพธ์เป็นจำนวนเชิงซ้อน
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // ผลลัพธ์: 7+10i
```

## ดำน้ำลึก
ก่อน `ext-intl`, PHP ไม่มีการสนับสนุนจำนวนเชิงซ้อนโดยตรง นักพัฒนาใช้ฟังก์ชันหรือคลังคลาสที่กำหนดเองเพื่อจัดการกับจำนวนเชิงซ้อน การดำเนินการที่ซับซ้อนอาจเป็นเรื่องน่าเบื่อและเสี่ยงต่อข้อผิดพลาด, แต่ `ext-intl` มอบวิธีการนานาชาติในการนำเสนอและวิเคราะห์จำนวนเชิงซ้อนที่ประสานกับห้องสมุด ICU

อย่างไรก็ตาม, สำหรับการดำเนินการทางคณิตศาสตร์ที่หนักหน่วง, บางคนอาจใช้คลังภายนอกที่เขียนด้วยภาษาที่เป็นมิตรกับคณิตศาสตร์มากขึ้น (เช่น C หรือ Python) และส่วนต่อพ่วงกับพวกเขาผ่าน PHP ในด้านการทำงาน, `ext-intl` จัดการเรื่องนี้อยู่เบื้องหลัง, แน่ใจว่าการคำนวณแม่นยำขณะซ่อนความซับซ้อนจากนักพัฒนา

ในอดีต, จำนวนเชิงซ้อนถูกมองไม่ดีเนื่องจากถูกเรียกว่า 'จินตภาพ', แต่พวกมันได้กลายเป็นพื้นฐานในหลายสาขาวิชาการและคณิตศาสตร์, เผยให้เห็นมากขึ้นเกี่ยวกับความสำคัญในโลกแห่งความจริงมากกว่าสถานะของจินตภาพที่เคยบอกไว้

## ดูเพิ่มเติม
- [คู่มือ PHP สำหรับ NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia เกี่ยวกับจำนวนเชิงซ้อน](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: The Right Way - การทำงานกับประเภทของข้อมูล](https://phptherightway.com/#data_types)
