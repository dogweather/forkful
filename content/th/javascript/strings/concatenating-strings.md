---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:43.606399-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E08\u0E32\u0E01\
  \u0E1B\u0E25\u0E32\u0E22\u0E2A\u0E38\u0E14\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1B\u0E25\
  \u0E32\u0E22\u0E2A\u0E38\u0E14\u0E2D\u0E35\u0E01\u0E2B\u0E19\u0E36\u0E48\u0E07.\
  \ \u0E40\u0E23\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21 \u0E25\u0E34\u0E07\u0E01\u0E4C URL \u0E2B\u0E23\u0E37\u0E2D\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E14 \u0E46 \u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E0A\u0E34\u0E49\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E32\u0E01\u0E41\u0E2B\u0E25\
  \u0E48\u0E07\u0E15\u0E48\u0E32\u0E07 \u0E46"
lastmod: '2024-03-17T21:57:56.594023-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u0E21\u0E32\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E19\u0E08\u0E32\u0E01\
  \u0E1B\u0E25\u0E32\u0E22\u0E2A\u0E38\u0E14\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E1B\u0E25\
  \u0E32\u0E22\u0E2A\u0E38\u0E14\u0E2D\u0E35\u0E01\u0E2B\u0E19\u0E36\u0E48\u0E07."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## อะไรและทำไม?
การต่อสตริงหมายถึงการนำพวกมันมาต่อกันจากปลายสุดไปยังปลายสุดอีกหนึ่ง. เราทำเช่นนี้เพื่อสร้างข้อความ ลิงก์ URL หรือข้อความใด ๆ ที่มีชิ้นส่วนจากแหล่งต่าง ๆ

## วิธีการ:
ใน JavaScript, คุณมีวิธีการไม่กี่วิธีในการต่อสตริง แบบเก่า: `+` แบบใหม่: template literals (เทมเพลตที่เป็นตัวอักษร). นี่คือวิธีมองของพวกเขา

**การใช้งาน + operator:**
```javascript
let hello = "Hello, ";
let world = "world!";
let greeting = hello + world;
console.log(greeting); // "Hello, world!"
```

**การใช้งาน template literals:**
```javascript
let user = "Jane";
let welcomeMessage = `Hi, ${user}! Welcome back.`;
console.log(welcomeMessage); // "Hi, Jane! Welcome back."
```

## ลงลึก
ในอดีต, `+` เป็นวิธีที่ไป, แต่มันกลายเป็นสิ่งที่ยุ่งเหยิงเมื่อมีตัวแปรมากมาย. เข้าสู่ ES6 ในปี 2015, นำเสนอ template literals (เครื่องหมาย backtick `\``). นี้หมายถึงข้อความที่ดูสะอาดทำให้สามารถสอดตัวแปรและคำนวณไว้ในข้อความของคุณโดยไม่ต้องเหนื่อยล้า.

**ทำไม `+` ถึงเป็นเรื่องยาก:**
- ยากต่อการอ่านเมื่อมีตัวแปรหลายตัว.
- ง่ายที่จะพลาดการเว้นวรรค ทำให้คำติดกัน.
- และ, ใครต้องการมี `+` มากมาย?

**ทำไม template literals ถึงดี:**
- การอ่านง่าย: เหมือนเป็นประโยคภาษาอังกฤษที่มีช่องว่างที่ถูกกรอกเข้าไป.
- รองรับหลายบรรทัด: คุณสามารถสร้างข้อความที่ยาวหลายบรรทัดโดยไม่ต้องใช้ `+` หรือ `\n`.
- การแทรกนิพจน์: ใส่ตัวแปร, คำนวณคณิตศาสตร์, ได้ในครั้งเดียว.

**นี่คือตัวอย่างการใช้หลายบรรทัดและนิพจน์:**
```javascript
let apples = 3;
let oranges = 5;
let fruitSummary = `You have ${apples + oranges} pieces of fruit: 
${apples} apples and 
${oranges} oranges.`;
console.log(fruitSummary);
```
ส่งผลให้ได้สรุปที่เป็นระเบียบโดยไม่ต้องใช้เทคนิค `+` ใดๆ.

จากมุมมองทางเทคนิค, การต่อสตริงสร้างสตริงใหม่ทุกครั้งที่คุณใช้ `+`. สำหรับคอมพิวเตอร์, นั่นเป็นเหมือนกับการทำขนมใหม่ทุกครั้งที่คุณต้องการเพิ่มถั่ว. ไม่มีประสิทธิภาพ. Template literals เหมือนมีแม่พิมพ์ที่คุณสามารถใส่ส่วนผสมทั้งหมดเข้าไปในครั้งเดียว - ประสิทธิภาพดีขึ้น, โดยเฉพาะกับสตริงขนาดใหญ่หรือในลูป.

## ดูเพิ่มเติม
- MDN Web Docs เกี่ยวกับ template literals (สำหรับการอ่านเพิ่มเติม): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- วิธีการและคุณสมบัติของสตริง (มีประโยชน์เมื่อจัดการกับสตริง): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
