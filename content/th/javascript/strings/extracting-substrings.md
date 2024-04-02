---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:55.610663-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\
  \u0E07\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2D\u0E2D\u0E01\
  \u0E21\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E15\u0E31\u0E14\u0E41\u0E25\u0E30\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25, \u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07, \u0E2B\u0E23\u0E37\u0E2D\
  \u0E1B\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25"
lastmod: '2024-03-17T21:57:56.591305-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E22\u0E48\u0E2D\u0E22\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\
  \u0E07\u0E02\u0E2D\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2D\u0E2D\u0E01\
  \u0E21\u0E32 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E15\u0E31\u0E14\u0E41\u0E25\u0E30\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\
  \u0E32\u0E21\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25, \u0E19\u0E33\u0E40\u0E02\u0E49\u0E32\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07, \u0E2B\u0E23\u0E37\u0E2D\
  \u0E1B\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไร & ทำไม?
การดึงข้อความย่อยหมายถึงการเลือกส่วนหนึ่งของข้อความออกมา โปรแกรมเมอร์ตัดและแยกข้อความเพื่อแยกข้อมูล, นำเข้ารูปแบบบางอย่าง, หรือปรับข้อความสำหรับการแสดงผล

## วิธีการ:

### การใช้เมธอด `substring`:
```javascript
let text = "JavaScript is awesome!";
let extracted = text.substring(0, 10);
console.log(extracted); // ผลลัพธ์: JavaScript
```

### การใช้เมธอด `slice`:
```javascript
let text = "JavaScript is awesome!";
let sliced = text.slice(-9, -1);
console.log(sliced); // ผลลัพธ์: awesome
```

### การใช้เมธอด `substr` (ไม่แนะนำให้ใช้):
```javascript
let text = "JavaScript is awesome!";
let substrd = text.substr(11, 7);
console.log(substrd); // ผลลัพธ์: awesome
```

## การศึกษาเพิ่มเติม
การดึงข้อความย่อยไม่ใช่เรื่องใหม่ – มันเก่าแก่เท่ากับการเขียนโปรแกรมเอง เมธอด `substring` และ `slice` ใน JavaScript เป็นเครื่องมือจากยุค 1990 ส่วนหนึ่งของชุดคุณสมบัติเริ่มต้นของภาษา เมธอด `substr` ก็อยู่ที่นั่นด้วย แต่ปัจจุบันได้กลายเป็นโค้ดเก่าแก่และควรหลีกเลี่ยงในแอปพลิเคชันสมัยใหม่

ความแตกต่างคืออะไร? `substring` และ `slice` คล้ายกัน – ทั้งสองรับพารามิเตอร์เริ่มต้นและสิ้นสุด - แต่จัดการกับค่าลบได้ต่างกัน: `slice` สามารถจัดการกับดัชนีลบได้ โดยนับจากปลาย ในขณะที่ `substring` ถือค่าลบเป็นศูนย์ ทั้งหมดนี้เมธอดไม่เปลี่ยนแปลงข้อความต้นฉบับ พวกมันผลิตข้อความใหม่

## ดูเพิ่มเติม
- Mozilla Developer Network บน Strings: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- การจัดการข้อความด้วย JavaScript: [W3Schools - เมธอดข้อความของ JavaScript](https://www.w3schools.com/js/js_string_methods.asp)
- พื้นฐานข้อมูลข้อความของ JavaScript: [JavaScript.info - Strings](https://javascript.info/string)
