---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:29.787982-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\
  \u0E27\u0E19 XML."
lastmod: '2024-03-17T21:57:56.626184-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 XML."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
นี่คือวิธีการแยกส่วน XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>Don't forget me this weekend!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// ผลลัพธ์: User
```

และการสร้าง XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// ผลลัพธ์: <note><to>User</to></note>
```

## ศึกษาเพิ่มเติม
XML ย่อมาจาก eXtensible Markup Language, เป็นรูปแบบข้อมูลที่มีมาตั้งแต่ปลายทศวรรษ 90 มันกำหนดชุดของกฎสำหรับการเข้ารหัสเอกสารที่ทั้งมนุษย์และเครื่องจักรสามารถอ่านได้ ตามประวัติศาสตร์, XML ได้รับความนิยมเนื่องจากความยืดหยุ่นและโครงสร้างชั้นเชิง, ทำให้เป็นทางเลือกสำหรับเว็บเซอร์วิส เช่น SOAP, และไฟล์การตั้งค่าจำนวนมาก

สิ่งที่เป็นทางเลือกของ XML รวมถึง JSON (JavaScript Object Notation), ซึ่งได้รับความนิยมเนื่องจากความง่ายในการใช้งานกับ JavaScript และน้ำหนักที่เบากว่า YAML เป็นอีกตัวเลือกหนึ่ง, มีค่าเพราะว่าเป็นมิตรกับมนุษย์และเป็นตัวเลือกทั่วไปสำหรับการกำหนดค่า

XML ถูกใช้งานใน JavaScript โดยใช้งาน DOMParser และ XMLSerializer interfaces XML DOM (Document Object Model) ช่วยให้สามารถนำทางและแก้ไขเอกสาร XML เหมือนกับที่คุณจะทำกับ HTML แม้ว่า JSON จะได้รับความนิยม, แต่การเข้าใจ XML ยังคงเป็นกุญแจสำคัญ เนื่องจากระบบ legacy จำนวนมากและอุตสาหกรรมบางแห่งยังคงพึ่งพามันสำหรับการแลกเปลี่ยนข้อมูล

## ดูเพิ่มเติม
- MDN Web Docs (XML Parsing): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM Tutorial): https://www.w3schools.com/xml/dom_intro.asp
- "XML คืออะไร?": https://www.w3.org/XML/
