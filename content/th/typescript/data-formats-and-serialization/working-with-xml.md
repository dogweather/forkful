---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:19.571610-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  , \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A XML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E25\u0E01\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\
  \u0E2B\u0E27\u0E48\u0E32\u0E07\u0E23\u0E30\u0E1A\u0E1A\u0E15\u0E48\u0E32\u0E07\u0E46\
  ,\u2026"
lastmod: '2024-03-17T21:57:55.969519-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01, \u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \ \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A XML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E25\u0E01\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\
  \u0E2B\u0E27\u0E48\u0E32\u0E07\u0E23\u0E30\u0E1A\u0E1A\u0E15\u0E48\u0E32\u0E07\u0E46\
  ,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไรและทำไม?
การทำงานกับ XML หมายถึงการแยก, จัดการ, และเขียนข้อมูล XML โดยใช้การเขียนโปรแกรม นักพัฒนาจัดการกับ XML เพื่อแลกเปลี่ยนข้อมูลระหว่างระบบต่างๆ, สำหรับไฟล์การตั้งค่า, หรือเมื่อทำงานกับมาตรฐานเช่น SOAP ที่พึ่งพา XML

## วิธีทำ:
```TypeScript
import { parseString } from 'xml2js';

// ตัวอย่าง XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Don't forget the meeting!</body>
             </note>`;

// แปลง XML เป็น JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// ถ้าการแปลงสำเร็จ, ผลลัพธ์อาจจะเป็นแบบนี้:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Don't forget the meeting!'] } 
}
```

## ศึกษาเพิ่มเติม
XML, หรือ Extensible Markup Language, ได้รับการใช้งานมาตั้งแต่ปลายยุค 90's ความเป็นลักษณะการอธิบายตนเองและรูปแบบที่อ่านได้โดยมนุษย์ทำให้มันได้รับความนิยมอย่างเร็วสำหรับการใช้งานหลายประเภท เช่น ฟีด RSS, การจัดการการตั้งค่า, และแม้กระทั่งรูปแบบเอกสารสำนักงานอย่างเอกสาร Microsoft Office Open XML แต่, XML มีความเห็ดอ้วนเมื่อเปรียบเทียบกับ JSON, และกระแสที่เปลี่ยนแปลงได้ ความสนใจได้หันไปที่ JSON สำหรับ API บนเว็บเนื่องจากมีน้ำหนักเบาและเข้ากันได้กับ JavaScript โดยธรรมชาติ

อย่างไรก็ตาม, XML ไม่ได้ตาย. มันถูกใช้ในระบบขององค์กรขนาดใหญ่และสำหรับมาตรฐานเอกสารที่ยังไม่ได้เปลี่ยนไปใช้ JSON เครื่องมือเช่น `xml2js` สำหรับ TypeScript หรือ `lxml` ใน Python พิสูจน์ว่ายังมีความต้องการสำหรับการจัดการ XML ในการเขียนโปรแกรม

TypeScript ไม่มีการสนับสนุน XML โดยตรงเช่นเดียวกับ JSON แต่คุณทำงานกับไลบรารี `xml2js` เป็นตัวอย่างหนึ่ง มันเปลี่ยน XML เป็น JSON, ทำให้ข้อมูลสะดวกสำหรับผู้เชี่ยวชาญ JavaScript ในการดำเนินงาน

## ดูเพิ่มเติม
- [เอกสาร MDN Web เกี่ยวกับ XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [แพ็คเกจ xml2js npm](https://www.npmjs.com/package/xml2js)
- [บทเรียน XML จาก W3Schools](https://www.w3schools.com/xml/)
