---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:48.691548-06:00
description: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\
  \ (Parsing) HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07 \u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32, \u0E2A\u0E01\u0E31\u0E14, \u0E2B\u0E23\u0E37\u0E2D \u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E42\u0E04\u0E49\u0E14 HTML\u2026"
lastmod: '2024-03-17T21:57:55.941945-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\
  \ (Parsing) HTML \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07 \u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32, \u0E2A\u0E01\u0E31\u0E14, \u0E2B\u0E23\u0E37\u0E2D \u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E42\u0E04\u0E49\u0E14 HTML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E34\
  \u0E14\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E1A\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\
  \u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\u0E4C\u2014\u0E2D\u0E32\u0E08\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E40\u0E1A\u0E23\u0E32\u0E27\u0E4C\u0E40\u0E0B\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E42\u0E14\u0E22\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\
  ."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
เริ่มต้นด้วยการติดตั้งไลบรารีอย่าง `node-html-parser` นี่คือคำสั่งในเทอร์มินัล:

```bash
npm install node-html-parser
```

ต่อไป, มาวิเคราะห์ HTML พื้นฐานใน TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

และหากคุณต้องการค้นหาเฉพาะกล้วย:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## ศึกษาลึกลงไป
การวิเคราะห์ HTML ไม่ใช่เรื่องใหม่—มันได้ถูกใช้มาตั้งแต่ช่วงต้นของเว็บ ในช่วงแรกๆ นักพัฒนาอาจใช้ Regular Expressions แต่วิธีนั้นกลับกลายเป็นเรื่องยุ่งยากอย่างรวดเร็ว จากนั้น DOM Parser ก็กลายมาเป็นวิธีที่มั่นคง แต่ถูกจำกัดเฉพาะในเบราว์เซอร์

ไลบรารีอย่าง `node-html-parser` ช่วยทำให้ปัญหาเหล่านี้ง่ายขึ้น พวกมันให้คุณค้นหา HTML ได้เหมือนกับ jQuery, แต่ทำงานด้านเซิร์ฟเวอร์กับ Node.js มันรวดเร็ว, อดทนต่อ HTML ที่ไม่สมบูรณ์, และมิตรกับ DOM

ยังมี `jsdom`, ซึ่งจำลองสภาพแวดล้อมทั้งหมดของเบราว์เซอร์ มันหนักหน่อย แต่ให้รายละเอียดมากกว่า, สร้าง Document Object Model (DOM) ที่สมบูรณ์สำหรับการจัดการและการติดต่อสื่อสาร

อย่าลืม Cheerio ด้วย มันผสมผสานความเร็วกับไวยากรณ์เหมือน jQuery และมีขนาดเล็ก นั่งอยู่ครึ่งทางระหว่างทั้งสอง

## ดูเพิ่มเติม
หากคุณกระหายที่จะรู้เพิ่มเติม, ลองดูที่นี่:
- [มาตรฐานการวิเคราะห์และการอนุมาน DOM จาก W3C](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser บน GitHub](https://github.com/taoqf/node-html-parser)
- [ที่เก็บ jsdom บน GitHub](https://github.com/jsdom/jsdom)
- [เว็บไซต์ของ Cheerio](https://cheerio.js.org/)
