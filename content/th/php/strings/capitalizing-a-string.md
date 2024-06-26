---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:50.956546-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E23\u0E2D\u0E07\u0E23\
  \u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E15\u0E48\u0E32\u0E07\
  \u0E46 \u0E21\u0E32\u0E01\u0E21\u0E32\u0E22\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\
  \u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48 \u0E41\u0E15\u0E48\u0E25\u0E30\u0E15\u0E31\u0E27\
  \u0E21\u0E35\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\
  \u0E17\u0E35\u0E48\u0E41\u0E15\u0E01\u0E15\u0E48\u0E32\u0E07\u0E01\u0E31\u0E19 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\
  \u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\
  \u0E14\u0E49."
lastmod: '2024-03-17T21:57:56.293176-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E21\u0E32\u0E01\u0E21\u0E32\u0E22\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48 \u0E41\u0E15\
  \u0E48\u0E25\u0E30\u0E15\u0E31\u0E27\u0E21\u0E35\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\
  \u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\u0E17\u0E35\u0E48\u0E41\u0E15\u0E01\u0E15\u0E48\
  \u0E32\u0E07\u0E01\u0E31\u0E19 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\
  \u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
PHP รองรับฟังก์ชันต่างๆ มากมายในการทำให้สตริงตัวพิมพ์ใหญ่ แต่ละตัวมีวัตถุประสงค์ที่แตกต่างกัน นี่คือวิธีที่คุณสามารถใช้งานได้:

### ทำให้ตัวอักษรแรกของสตริงเป็นตัวพิมพ์ใหญ่:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // แสดงผล: Hello, world!
```

### ทำให้ตัวอักษรแรกของแต่ละคำเป็นตัวพิมพ์ใหญ่:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // แสดงผล: Hello, World!
```

### แปลงสตริงทั้งหมดเป็นตัวพิมพ์ใหญ่:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // แสดงผล: HELLO, WORLD!
```

สำหรับสถานการณ์ที่ต้องการการปรับแต่งเพิ่มเติมหรือใช้โซลูชันจากบุคคลที่สาม สามารถใช้ไลบรารีเช่น `mbstring` (สำหรับสตริงที่เป็น multibyte) โดยเฉพาะเมื่อต้องการจัดการกับการสากลภาษาที่อักขระอาจมีขนาดใหญ่กว่าชุด ASCII พื้นฐาน

### ใช้ mbstring เพื่อทำให้สตริง UTF-8 เป็นตัวพิมพ์ใหญ่:
ตรวจสอบให้แน่ใจว่าคุณมีส่วนขยาย `mbstring` เปิดใช้งานในการกำหนดค่า PHP ของคุณ จากนั้น:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // แสดงผล: Élégant
```

วิธีนี้ช่วยให้สามารถทำตัวพิมพ์ใหญ่สตริงที่มีอักขระที่ไม่ใช่ ASCII ได้อย่างแม่นยำ โดยปฏิบัติตามความพิถีพิถันของภาษาต่างๆ
