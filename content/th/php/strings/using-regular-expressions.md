---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:34.852051-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E23\u0E2D\u0E07\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E2D\u0E2D\u0E01\u0E41\u0E1A\
  \u0E1A\u0E1B\u0E01\u0E15\u0E34\u0E1C\u0E48\u0E32\u0E19\u0E2B\u0E49\u0E2D\u0E07\u0E2A\
  \u0E21\u0E38\u0E14 PCRE (Perl Compatible Regular Expressions) \u0E0B\u0E36\u0E48\
  \u0E07\u0E21\u0E35\u0E0A\u0E38\u0E14\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E48\
  \u0E19\u0E17\u0E35\u0E48\u0E2B\u0E25\u0E32\u0E01\u0E2B\u0E25\u0E32\u0E22 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\
  \u0E07\u0E32\u0E19:\u2026"
lastmod: '2024-03-17T21:57:56.301019-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\
  \u0E07\u0E2D\u0E2D\u0E01\u0E41\u0E1A\u0E1A\u0E1B\u0E01\u0E15\u0E34\u0E1C\u0E48\u0E32\
  \u0E19\u0E2B\u0E49\u0E2D\u0E07\u0E2A\u0E21\u0E38\u0E14 PCRE (Perl Compatible Regular\
  \ Expressions) \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E0A\u0E38\u0E14\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E48\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E25\u0E32\u0E01\
  \u0E2B\u0E25\u0E32\u0E22 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:
PHP รองรับการแสดงออกแบบปกติผ่านห้องสมุด PCRE (Perl Compatible Regular Expressions) ซึ่งมีชุดฟังก์ชั่นที่หลากหลาย นี่คือวิธีการใช้งาน:

### การจับคู่รูปแบบ:
เพื่อตรวจสอบว่ามีรูปแบบใด ๆ ภายในสตริงหรือไม่ ใช้ `preg_match()` ฟังก์ชั่นนี้จะคืนค่า 1 หากพบรูปแบบในสตริงและ 0 หากไม่พบ

```php
if (preg_match("/\bweb\b/i", "PHP เป็นภาษาสคริปต์สำหรับเว็บ")) {
    echo "พบการจับคู่";
} else {
    echo "ไม่พบการจับคู่";
}
// ผลลัพธ์: พบการจับคู่
```

### การค้นหาการจับคู่ทั้งหมด:
`preg_match_all()` ใช้เมื่อคุณต้องการค้นหาทุกครั้งที่พบรูปแบบภายในสตริง

```php
$text = "แมวและหมา";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// ผลลัพธ์: Array ( [0] => แมว [1] => และ [2] => หมา )
```

### การแทนที่ข้อความ:
ในการแทนที่ข้อความที่ตรงกับการแสดงออกแบบปกติ ใช้ `preg_replace()` มีความสามารถอย่างมากในการจัดรูปแบบและทำความสะอาดข้อมูล

```php
$originalText = "15 เมษายน 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// ผลลัพธ์: เมษายน1,2003
```

### การแบ่งสตริง:
คุณสามารถแบ่งสตริงออกเป็นอาร์เรย์โดยใช้ `preg_split()` โดยระบุรูปแบบสำหรับตัวคั่น

```php
$text = "PHP เป็น, ภาษาสคริปต์ที่ได้รับความนิยมอย่างมาก,";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// ผลลัพธ์: Array ( [0] => PHP เป็น [1] => ภาษาสคริปต์ที่ได้รับความนิยมอย่างมาก [2] => )
```

นอกจากนี้ สำหรับรูปแบบและภารกิจ regex ที่ซับซ้อน กรอบงานและไลบรารีเช่น ส่วนประกอบ `Finder` ของ Symfony หรือชุดฟังก์ชั่นช่วยเหลือของ Laravel อาจมอบชั้นการดำเนินงานที่สะดวกยิ่งขึ้น อย่างไรก็ตาม การเข้าใจและใช้ฟังก์ชั่น PCRE ที่มีอยู่ใน PHP เป็นสิ่งสำคัญสำหรับการประมวลผลและตรวจสอบข้อความอย่างมีประสิทธิภาพโดยตรงภายในสคริปต์ PHP
