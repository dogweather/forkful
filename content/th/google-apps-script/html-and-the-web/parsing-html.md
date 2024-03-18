---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:06.194266-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E43\u0E19 Google Apps Script \u0E40\u0E01\u0E35\u0E48\
  \u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 HTML\u2026"
lastmod: '2024-03-17T21:57:55.714115-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E43\u0E19 Google Apps Script \u0E40\u0E01\u0E35\u0E48\
  \u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 HTML\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การแยกวิเคราะห์ HTML ใน Google Apps Script เกี่ยวข้องกับการดึงข้อมูลจากเนื้อหา HTML ซึ่งมีประโยชน์อย่างยิ่งเมื่อต้องมีปฏิสัมพันธ์กับหน้าเว็บหรือแหล่งข้อมูลที่อิงกับเว็บ โปรแกรมเมอร์ทำเช่นนี้เพื่ออัตโนมัติการรวบรวมข้อมูล จัดการเนื้อหาบนเว็บ หรือรวมฟังก์ชันเว็บกับ Google Apps เช่น Sheets และ Docs

## การทำ:
Google Apps Script ไม่มีวิธีในตัวสำหรับการแยกวิเคราะห์ HTML อย่างไรก็ตาม คุณสามารถใช้บริการ `UrlFetchApp` เพื่อดึงเนื้อหา HTML แล้วใช้วิธีการหรือ regex (Regular Expressions) ของ JavaScript สำหรับการแยกวิเคราะห์ ด้านล่างเป็นตัวอย่างพื้นฐานของวิธีการดึงและแยกวิเคราะห์แท็ก title จากเว็บเพจ

```javascript
function parseHTMLTitle(url) {
  // ดึงเนื้อหา HTML ของเว็บเพจ
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // ใช้ regex ที่ง่ายเพื่อหาเนื้อหาของแท็ก <title>
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // ตรวจสอบว่าพบ title หรือไม่และส่งคืน
  if (match && match.length > 1) {
    return match[1];
  }

  return 'ไม่พบ title';
}

// ตัวอย่างการใช้
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // แสดงผล title ของเว็บเพจ
```

สำหรับการแยกวิเคราะห์ HTML ที่ซับซ้อนยิ่งขึ้น คุณสามารถใช้ `XmlService` เพื่อแยกวิเคราะห์ HTML เป็น XML ทั้งนี้ โปรดทราบว่า สิ่งนี้ต้องการให้ HTML เป็น XML ที่มีรูปแบบที่ถูกต้อง ซึ่งไม่เสมอไป:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // จากที่นี่ เดินทางตามต้นไม้ XML ด้วยวิธีการของ XmlService
    // เช่น การหาองค์ประกอบหรือแอตทริบิวต์เฉพาะ
  } catch(e) {
    Logger.log('Error parsing HTML: ' + e.toString());
  }
}
```

## การศึกษาลึก:
ในอดีต การแยกวิเคราะห์ HTML ในสภาพแวดล้อมเช่น Google Apps Script ถือว่าท้าทายเนื่องจากขาด Document Object Model (DOM) หรือห้องสมุดสำหรับการแยกวิเคราะห์ที่ประจำอยู่ในบริบทการเขียนโปรแกรมอื่นๆ ตัวอย่างเช่น JavaScript ในเบราว์เซอร์ มี DOM พร้อมใช้งานอยู่แล้ว และสภาพแวดล้อม Node.js มีการเข้าถึงไลบรารีจำนวนมากใน NPM เช่น `cheerio` หรือ `jsdom` สำหรับการแยกวิเคราะห์ HTML

Google Apps Script พึ่งพาการใช้ `UrlFetchApp` สำหรับคำขอเว็บแล้วจัดการข้อมูลการตอบกลับโดยใช้ regex หรือวิธีการแยกวิเคราะห์ XML แม้ว่า regex อาจมีประโยชน์สำหรับงานการแยกวิเคราะห์ที่ง่าย แต่โดยทั่วไปไม่แนะนำสำหรับ HTML ที่ซับซ้อนเนื่องจากความเสี่ยงของข้อผิดพลาดและลักษณะที่เปราะบางของโค้ด XML การแยกวิเคราะห์ด้วย `XmlService` ให้วิธีการที่มีโครงสร้างมากขึ้น แต่ต้องการ HTML/XML ที่ถูกต้อง ซึ่งอาจเป็นข้อจำกัดเมื่อต้องจัดการกับหน้าเว็บที่แปลกประหลาด

สำหรับความต้องการการแยกวิเคราะห์ที่ซับซ้อนหรือเมื่อมีการจัดการกับ HTML ที่ไม่สวยงาม หนึ่งในกลยุทธ์ทางเลือกอาจรวมถึงการใช้บริการเว็บภายนอก Google Apps Script บริการนี้อาจประมวลผลเนื้อหา HTML โดยใช้เทคนิคหรือห้องสมุดสำหรับการแยกวิเคราะห์ที่มั่นคงยิ่งขึ้น แล้วส่งคืนข้อมูลที่ประมวลผลในรูปแบบที่ Google Apps Script สามารถใช้งานได้ง่าย วิธีนี้อย่างไรก็ตาม ส่งผลให้เกิดความล่าช้าของเครือข่ายและความซับซ้อนของการจัดการบริการเว็บเพิ่มเติม

แม้จะมีความท้าทายเหล่านี้ การแยกวิเคราะห์ HTML ภายใน Google Apps Script ยังคงเป็นเครื่องมือที่ทรงพลัง โดยเฉพาะเมื่อรวมกับบริการและ API อื่นๆ ของ Google โดยให้ช่วงของความเป็นไปได้ในการอัตโนมัติที่สามารถเพิ่มประสิทธิผลและความสามารถในการประมวลผลข้อมูลได้อย่างมาก
