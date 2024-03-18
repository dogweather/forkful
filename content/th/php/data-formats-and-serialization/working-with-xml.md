---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:13.678372-06:00
description: "XML \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E21\u0E32\u0E23\
  \u0E4C\u0E01\u0E2D\u0E31\u0E1B\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\
  \u0E25\u0E30\u0E01\u0E32\u0E23\u0E02\u0E19\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E40\u0E1B\u0E34\u0E14\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\u0E23\u0E30\u0E2B\
  \u0E27\u0E48\u0E32\u0E07\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\
  \u0E48\u0E19\u0E41\u0E25\u0E30\u0E23\u0E30\u0E1A\u0E1A -\u2026"
lastmod: '2024-03-17T21:57:56.336736-06:00'
model: gpt-4-0125-preview
summary: "XML \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E21\u0E32\u0E23\u0E4C\
  \u0E01\u0E2D\u0E31\u0E1B\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E25\
  \u0E30\u0E01\u0E32\u0E23\u0E02\u0E19\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\
  \u0E1B\u0E34\u0E14\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\u0E23\u0E30\u0E2B\u0E27\
  \u0E48\u0E32\u0E07\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E48\
  \u0E19\u0E41\u0E25\u0E30\u0E23\u0E30\u0E1A\u0E1A -\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
XML เป็นภาษามาร์กอัปที่ใช้สำหรับเก็บข้อมูลและการขนส่งข้อมูล โปรแกรมเมอร์ทำงานกับ XML เพื่อเปิดใช้งานการทำงานร่วมกันระหว่างแอปพลิเคชั่นและระบบ - คิดถึงการแลกเปลี่ยนข้อมูลและการตั้งค่าการกำหนดค่า

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
