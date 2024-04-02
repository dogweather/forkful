---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:38.666892-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E43\u0E19 PHP \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\
  \u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 HTML \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E07\u0E32\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\
  \u0E15\u0E34\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25, \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E08\u0E32\u0E01\u0E40\u0E27\u0E47\u0E1A,\u2026"
lastmod: '2024-03-17T21:57:56.309899-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E43\u0E19 PHP \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\
  \u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 HTML \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E07\u0E32\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\
  \u0E15\u0E34\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25, \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E08\u0E32\u0E01\u0E40\u0E27\u0E47\u0E1A,\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## อะไรและทำไม?
การแยกวิเคราะห์ HTML ใน PHP คือการสกัดข้อมูลเฉพาะจากเอกสาร HTML โปรแกรมเมอร์ทำงานนี้เพื่ออัตโนมัติการสกัดข้อมูล, การเก็บข้อมูลจากเว็บ, หรือการรวมเนื้อหาจากหน้าเว็บต่างๆ ภายในแอปพลิเคชันของพวกเขา เพิ่มฟังก์ชันการทำงานโดยไม่ต้องมีการแทรกแซงด้วยตนเอง

## วิธีการ:
สำหรับการแยกวิเคราะห์ HTML, โปรแกรมเมอร์ PHP สามารถใช้ฟังก์ชันที่มีอยู่แล้วหรือพึ่งพาไลบรารีที่แข็งแกร่งเช่น Simple HTML DOM Parser ที่นี่ เราจะสำรวจตัวอย่างโดยใช้ `DOMDocument` ของ PHP และ Simple HTML DOM Parser

### การใช้ `DOMDocument`:
คลาส `DOMDocument` ของ PHP เป็นส่วนหนึ่งของส่วนขยาย DOM ทำให้สามารถแยกวิเคราะห์และจัดการเอกสาร HTML และ XML ได้ นี่คือตัวอย่างอย่างรวดเร็วในการใช้ `DOMDocument` เพื่อค้นหารูปภาพทั้งหมดในเอกสาร HTML:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>หน้าตัวอย่าง</title>
</head>
<body>
    <img src="image1.jpg" alt="รูปที่ 1">
    <img src="image2.jpg" alt="รูปที่ 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

ตัวอย่างผลลัพธ์:
```
image1.jpg
image2.jpg
```

### การใช้ Simple HTML DOM Parser:
สำหรับงานที่ซับซ้อนขึ้นหรือไวยากรณ์ที่ง่ายขึ้น คุณอาจต้องการใช้ไลบรารีของบุคคลที่สาม Simple HTML DOM Parser เป็นตัวเลือกยอดนิยม ให้ส่วนติดต่อที่คล้ายกับ jQuery สำหรับการนำทางและจัดการโครงสร้าง HTML นี่คือวิธีการใช้งาน:

ก่อนอื่น, ติดตั้งไลบรารีโดยใช้ Composer:
```
composer require simple-html-dom/simple-html-dom
```

จากนั้น, จัดการ HTML เพื่อหาลิงค์ทั้งหมดเป็นตัวอย่าง:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

ส่วนของโค้ดนี้จะดึงเนื้อหา HTML ของ 'http://www.example.com' มาวิเคราะห์และพิมพ์ลิงก์ทั้งหมดออกมา นำทางไปที่ URL ที่คุณต้องการวิเคราะห์แทนที่จะเป็น `'http://www.example.com'`

โดยการใช้วิธีเหล่านี้ นักพัฒนา PHP สามารถวิเคราะห์เนื้อหา HTML อย่างมีประสิทธิภาพ ปรับแต่งการสกัดข้อมูลตามความต้องการของพวกเขา หรือรวมเนื้อหาเว็บภายนอกเข้ากับโปรเจกต์ของพวกเขาได้อย่างราบรื่น
