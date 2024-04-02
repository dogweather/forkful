---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:15.277315-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E14\u0E35\u0E1A\
  \u0E31\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E44\u0E1F\u0E09\u0E32\u0E22\u0E2A\u0E48\u0E2D\
  \u0E07\u0E17\u0E32\u0E07\u0E43\u0E19\u0E0B\u0E2D\u0E01\u0E21\u0E37\u0E14\u0E02\u0E2D\
  \u0E07\u0E42\u0E04\u0E49\u0E14; \u0E21\u0E31\u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E1A\
  \u0E1A\u0E31\u0E01\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E2B\u0E47\u0E19\u0E27\u0E48\
  \u0E32\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E2D\u0E30\u0E44\u0E23\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E40\u0E27\
  \u0E25\u0E32\u0E17\u0E35\u0E48\u0E23\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.947328-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E14\u0E35\u0E1A\
  \u0E31\u0E01\u0E40\u0E1B\u0E47\u0E19\u0E44\u0E1F\u0E09\u0E32\u0E22\u0E2A\u0E48\u0E2D\
  \u0E07\u0E17\u0E32\u0E07\u0E43\u0E19\u0E0B\u0E2D\u0E01\u0E21\u0E37\u0E14\u0E02\u0E2D\
  \u0E07\u0E42\u0E04\u0E49\u0E14; \u0E21\u0E31\u0E19\u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E08\u0E31\u0E1A\
  \u0E1A\u0E31\u0E01\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E2B\u0E47\u0E19\u0E27\u0E48\
  \u0E32\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E2D\u0E30\u0E44\u0E23\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E40\u0E27\
  \u0E25\u0E32\u0E17\u0E35\u0E48\u0E23\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## อะไรและทำไม?
การแสดงผลดีบักเป็นไฟฉายส่องทางในซอกมืดของโค้ด; มันช่วยให้คุณสามารถจับบักได้โดยการช่วยให้คุณเห็นว่าโค้ดของคุณทำงานอะไรอยู่ในเวลาที่รัน นักโปรแกรมเมอร์ทำแบบนี้เพราะ โน่น, เราเป็นมนุษย์และโค้ดของเราไม่ใช่ที่สมบูรณ์แบบตั้งแต่ครั้งแรกเสมอไป

## วิธีการ:
ต้องการพิมพ์ผลดีบักใน TypeScript หรือ? วิธีใช้ Console เป็นตัวเลือกหลักของคุณ ดู `console.log`, `console.error`, และเพื่อนๆ ในการทำงาน:

```TypeScript
// ข้อความบันทึกพื้นฐาน
console.log('Look Ma, I am debugging!');

// ข้อความบันทึกแบบกลุ่ม
console.group('User Details');
console.log('Name: John Doe');
console.log('Age: 34');
console.groupEnd();

// ตาราง
console.table([{ a: 1, b: 'Y' }, { a: 'Z', b: 2 }]);

// ผลดีบักประเภท Error
console.error('Oops! Something went wrong.');

// ผลดีบักประเภท Warning
console.warn('This is a warning.');

// ผลดีบักประเภท Debug
console.debug('This is a debug message.');
```

ตัวอย่างผลลัพธ์:
```
Look Ma, I am debugging!
User Details
    Name: John Doe
    Age: 34
(index) a  b
0       1  "Y"
1       "Z" 2
Oops! Something went wrong.
This is a warning.
This is a debug message.
```

## ลงลึก
กลับไปในวันนั้น, เรามี `alert()` - มันเข้ามาด้านหน้าและบล็อกงานจนกระทั่งจัดการได้ ตอนนี้, เมธอด `console` ครอบคลุม พวกมันน้อยกวนน้อยลงและมาพร้อมกับซุปเปอร์พาวเวอร์: จัดหมวดหมู่ข้อความ, พิมพ์ตาราง, หรือสไตล์ผลลัพธ์

มีทางเลือกอื่นหรือ? แน่นอน คุณอาจเขียนไปยังไฟล์หรือส่งข้อความผ่านเครือข่ายเพื่อการบันทึกแบบรีโมท สำหรับบราวเซอร์, เครื่องมือเช่น Chrome's DevTools ให้คุณควบคุมระดับและรูปแบบของการบันทึกได้มากขึ้น

ในเรื่องการใช้งาน, `console` ใน TypeScript กลายเป็น JavaScript เมื่อถึงเวลาที่รัน, และนั่นคือที่ที่การกระทำทั้งหมดเกิดขึ้น ประเภท TypeScript ที่หรูหราไม่เปลี่ยนเกมในที่นี้ - เป็น `console` ธรรมดาภายใต้ฮูด, ไม่ว่าจะเป็นบราวเซอร์หรือ Node

## ดูเพิ่มเติม
- [MDN Web Docs บน Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [เอกสาร Console ของ Node.js](https://nodejs.org/api/console.html)
- [คู่มือ TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
