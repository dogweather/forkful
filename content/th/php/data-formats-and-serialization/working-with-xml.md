---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:13.678372-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19 XML \u0E14\u0E49\u0E27\u0E22 SimpleXML."
lastmod: '2024-03-17T21:57:56.336736-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19 XML \u0E14\u0E49\u0E27\u0E22\
  \ SimpleXML."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
การอ่าน XML ด้วย SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Don't forget this</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // แสดงผล: Tove
echo $xml->from;     // แสดงผล: Jani
echo $xml->heading;  // แสดงผล:Reminder
echo $xml->body;     // แสดงผล:Don't forget this
```

การเขียน XML ด้วย DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Don't forget this');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

ตัวอย่างผลลัพธ์:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget this</body>
</note>
```

## ลงลึก
XML หรือ eXtensible Markup Language ได้รับการยอมรับเป็นมาตรฐานในการตัวแทนข้อมูลข้อมูลตั้งแต่มีคำแนะนำจาก W3C ในปี 1998 มันเป็นภาษาที่มีคำอธิบายมาก เข้าใจง่าย และมีการกำหนดไว้แบบเข้มงวด ทำให้เป็นตัวเลือกที่น่าเชื่อถือสำหรับไฟล์การกำหนดค่า การแลกเปลี่ยนข้อมูล และอื่นๆ อย่างไรก็ตาม มันถูกทำให้ด้อยค่าบ้างโดย JSON สำหรับเว็บ API เนื่องจากความเรียบง่ายและลักษณะเบา

โปรแกรมเมอร์มักเลือก XML เมื่อพวกเขาต้องการการตรวจสอบเอกสารที่มีให้โดย XML Schemas หรือเมื่อทำงานในระบบนิเวศที่พึ่งพามันมาก (เช่น รูปแบบไฟล์ของ Microsoft Office) การจัดการ XML ใน PHP ทำได้ง่ายดายด้วยการขยาย SimpleXML สำหรับการดำเนินการพื้นฐาน สำหรับการจัดการที่ซับซ้อนยิ่งขึ้น DOMDocument มอบชุดคุณสมบัติที่เข้มแข็งซึ่งอนุญาตให้มีการควบคุมได้มากขึ้น เช่น การจัดการชื่อพื้นที่และการตรวจสอบโครงสร้าง

## ดูเพิ่มเติม
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
