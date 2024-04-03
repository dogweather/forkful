---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:47.035222-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PHP \u0E21\u0E35\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E22\u0E48\u0E2D\u0E22 \u0E21\u0E32\u0E14\u0E39\u0E17\u0E35\u0E48\
  \ `substr`, `mb_substr`, \u0E41\u0E25\u0E30 `strstr` \u0E01\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.300147-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E2B\u0E25\
  \u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E22\u0E48\u0E2D\u0E22 \u0E21\u0E32\
  \u0E14\u0E39\u0E17\u0E35\u0E48 `substr`, `mb_substr`, \u0E41\u0E25\u0E30 `strstr`\
  \ \u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
PHP มีฟังก์ชันหลายอย่างในการดึงข้อความย่อย มาดูที่ `substr`, `mb_substr`, และ `strstr` กัน

```PHP
$string = "Hello, World! Programming is fun.";

// การดึง 'World' โดยใช้ substr.
echo substr($string, 7, 5); // ผลลัพธ์: World

// ตัวอย่างสตริง UTF-8 โดยใช้ mb_substr สำหรับตัวอักษรหลายไบต์
$utf8String = "こんにちは世界";
echo mb_substr($utf8String, 5, 2); // ผลลัพธ์: 世

// ดึงทุกอย่างหลังจากจุลภาคด้วย strstr
echo strstr($string, ","); // ผลลัพธ์: , World! Programming is fun.
```

## ลงลึก
ในช่วงเวลาแรกๆ ของ PHP, `substr()` คือวิธีหลักในการดึงชิ้นส่วนของสตริงออกมา อย่างไรก็ตาม `substr()` มีข้อจำกัด (และยังคงมี): ไม่ทำงานได้ดีกับตัวอักษรที่ไม่ใช่ภาษาอังกฤษ (เช่น ภาษาญี่ปุ่นหรืออาหรับ)

นำเสนอ `mb_substr()`, คู่หูที่ปลอดภัยกับมัลติไบต์และเคารพต่อตัวอักษรจากการเข้ารหัสต่างๆ รับประกันว่าเมื่อคุณดึงข้อความย่อยออกมา คุณจะไม่ฉีกผ่านตัวอักษรตรงกลางไบต์ ซึ่งมีความสำคัญต่อการใช้งานในระดับสากล

`strstr()`, อีกด้านหนึ่ง หาครั้งแรกของข้อความย่อยและให้ทุกอย่างหลังจากนั้น มี `strchr()` ซึ่งเป็นชื่ออื่นของ `strstr()`

ในขณะที่ `substr()` และ `mb_substr()` อนุญาตให้คุณระบุได้อย่างแน่นอนว่าจะเริ่มต้นที่ไหนและจะดึงออกมามากเพียงใด `strstr()` เป็นเหมือนเครื่องมือ "หาและให้ที่เหลือมาหาฉัน"

## ดูเพิ่มเติม
นี่คือการอ่านเพิ่มเติมหากคุณต้องการรับรู้มากขึ้น:

- เอกสารอย่างเป็นทางการของ PHP สำหรับฟังก์ชันสตริง: https://www.php.net/manual/en/ref.strings.php
- การศึกษาลึกลงไปในฟังก์ชันสตริงมัลติไบต์ของ PHP: https://www.php.net/manual/en/book.mbstring.php
- เพิ่มเติมเกี่ยวกับการเข้ารหัสตัวอักษรและทำไมมันถึงสำคัญ: http://kunststube.net/encoding/
