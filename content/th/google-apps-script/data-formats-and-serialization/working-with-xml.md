---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:54.183871-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E43\u0E19 Google Apps Script \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E41\u0E22\u0E01, \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\
  \u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML\
  \ \u0E0B\u0E36\u0E48\u0E07\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E41\
  \u0E25\u0E30\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u2026"
lastmod: '2024-03-17T21:57:55.744224-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E43\
  \u0E19 Google Apps Script \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E41\u0E22\u0E01, \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\u0E25\
  \u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E0B\
  \u0E36\u0E48\u0E07\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E40\u0E27\u0E47\u0E1A\u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไรและทำไม?

การทำงานกับ XML ใน Google Apps Script ช่วยให้โปรแกรมเมอร์สามารถแยก, จัดการ, และสร้างข้อมูล XML ซึ่งจำเป็นสำหรับบริการเว็บและการตั้งค่า โปรแกรมเมอร์เลือกใช้วิธีนี้เพื่อรวมกับระบบเก่า, ทำการ web scraping, หรือสื่อสารกับ API จำนวนมากที่ยังคงพึ่งพา XML มากกว่า JSON สำหรับการแลกเปลี่ยนข้อมูล

## วิธีทำ:

Google Apps Script มี `XmlService` สำหรับทำงานกับข้อมูล XML ด้านล่างนี้เราจะแสดงวิธีการแยกสตริง XML, แก้ไขเนื้อหา และสร้างสตริง XML ใหม่

การแยกสตริง XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // บันทึก: Hello
}
```

เพื่อแก้ไข XML คุณอาจต้องการเพิ่มลูกโหนดใหม่:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // บันทึกสตริง XML ใหม่ที่มีการเพิ่มลูกโหนด
}
```

การสร้างสตริง XML จากต้น:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // แสดงผล: <root><child>Hello World</child></root>
}
```

## ดำน้ำลึก

ในอดีต XML (Extensible Markup Language) เป็นมาตรฐาน de facto สำหรับการแลกเปลี่ยนข้อมูลก่อนที่ JSON จะเกิดขึ้นเป็นทางเลือกที่มีน้ำหนักเบา XML มีไวยากรณ์แบบละเอียดและโมเดลการแยกวิเคราะห์ที่เข้มงวดให้รูปแบบข้อมูลที่แข็งแรงแม้จะมีขนาดใหญ่ ใน Google Apps Script, API `XmlService` ครอบคลุมการสร้าง, การแยกวิเคราะห์ และการจัดการข้อมูล XML, โดยยอมรับความสำคัญที่ยังคงมีอยู่ในระบบเก่าและองค์กรต่างๆ, บริการเว็บ SOAP และไฟล์การตั้งค่าสำหรับแอปพลิเคชัน

แม้ว่า JSON จะเป็นที่นิยมในการพัฒนาเว็บสมัยใหม่เนื่องจากความเรียบง่ายและความสะดวกในการใช้กับ JavaScript แต่ XML ยังคงเกี่ยวข้องในพื้นที่ที่ต้องการการตรวจสอบเอกสารและโครงสร้างเชิงชั้นที่สำคัญ อย่างไรก็ตาม สำหรับโปรเจกต์ใหม่ๆ โดยเฉพาะที่เน้นไปที่ web APIs, JSON มักเป็นตัวเลือกที่เหมาะสมกว่าเนื่องจากรูปแบบที่เบาและการรวมตัวกับ JavaScript ได้อย่างราบรื่น

การเข้าใจ XML และการจัดการใน Google Apps Script มีความสำคัญสูงสุดสำหรับนักพัฒนาที่ทำงานในสภาพแวดล้อมที่ต้องการการรวมกับระบบเก่าหรือ API ขององค์กรบางอย่าง อย่างไรก็ตาม เมื่อเริ่มโครงการใหม่หรือเมื่อความยืดหยุ่นเป็นสิ่งสำคัญ การประเมินความจำเป็นของ XML กับทางเลือกอื่นๆ เช่น JSON ถือเป็นสิ่งที่ควรทำ
