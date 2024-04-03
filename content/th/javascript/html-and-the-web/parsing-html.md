---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:35.272080-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \ HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\
  \u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\
  \u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E01\u0E31\u0E1A\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A\
  , \u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34,\u2026"
lastmod: '2024-03-17T21:57:56.599455-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \ HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\
  \u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E42\u0E15\
  \u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E01\u0E31\u0E1A\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A\
  , \u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34, \u0E2B\u0E23\u0E37\u0E2D\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\u0E30\u0E2A\
  \u0E07\u0E04\u0E4C\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E27\u0E47\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## อะไรและทำไม?
การแยกคำสั่ง HTML หมายถึงการสกัดข้อมูลจากเอกสาร HTML นักพัฒนาทำอย่างนี้เพื่อโต้ตอบหรือจัดการกับเนื้อหาเว็บ, การสกัดข้อมูลอัตโนมัติ, หรือเพื่อวัตถุประสงค์ในการเก็บข้อมูลจากเว็บ

## วิธีการ:
ลองแยกคำสั่ง HTML โดยใช้ API `DOMParser` ใน JavaScript

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // ผลลัพธ์: Hello, world!
```

ตอนนี้, มาจับอะไรที่เฉพาะเจาะจงกว่านั้น, เช่น องค์ประกอบที่มีคลาส:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // ผลลัพธ์: Hello, again!
```

## ลงลึก
การแยกคำสั่ง HTML เป็นสิ่งที่มีมาตั้งแต่อินเทอร์เน็ตเริ่มต้น เดิมทีเป็นเรื่องของเบราว์เซอร์—เบราว์เซอร์แยกคำสั่ง HTML เพื่อแสดงหน้าเว็บ ตลอดเวลา, นักพัฒนาต้องการใช้ประโยชน์จากกระบวนการนี้, นำไปสู่ API เช่น `DOMParser`

มีทางเลือกอื่นหรือไม่? แน่นอน เรามีไลบรารีเช่น `jQuery` และเครื่องมืออย่าง `BeautifulSoup` สำหรับ Python แต่ `DOMParser` ของ JavaScript เป็นตัวเดียวที่เร็วและในตัว, ไม่ต้องการไลบรารีเพิ่มเติม

ในด้านการประยุกต์ใช้งาน, เมื่อคุณแยกคำสั่ง HTML ด้วย `DOMParser`, มันสร้างวัตถุ `Document` นึกถึงมันเหมือนกับแบบจำลองเชิงลำดับของ HTML ของคุณ เมื่อคุณได้รับมัน, คุณสามารถนำทางและจัดการมันได้เหมือนกับ DOM ของหน้าเว็บปกติ

นี่คือสิ่งที่—การแยกคำสั่งสามารถพบกับ HTML ที่ไม่ถูกต้องได้ เบราว์เซอร์เป็นอะไรที่ใจกว้าง, แต่ `DOMParser` อาจไม่ใช่ เพราะฉะนั้น, สำหรับงานที่ซับซ้อนหรือ HTML ที่ไม่เรียบร้อย, ไลบรารีของบุคคลที่สามอาจทำงานทำความสะอาดได้ดียิ่งขึ้น

## ดูเพิ่มเติม
- คู่มือ MDN Web Docs สำหรับ API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- ความสามารถในการแยกคำสั่งของ jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, การประยุกต์ใช้ jQuery หลักที่รวดเร็ว, ยืดหยุ่น & กระชับสำหรับเซิร์ฟเวอร์: [Cheerio.js](https://cheerio.js.org/)
- สำหรับการแยกคำสั่งที่ไม่ใช่ JS: ไลบรารี BeautifulSoup ของ Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
