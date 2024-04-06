---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:56.312396-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 JavaScript,\
  \ \u0E44\u0E21\u0E48\u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E15\u0E31\
  \u0E27\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\
  \u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07, \u0E41\u0E15\u0E48\
  \u0E21\u0E31\u0E19\u0E04\u0E48\u0E2D\u0E19\u0E02\u0E49\u0E32\u0E07\u0E07\u0E48\u0E32\
  \u0E22\u0E17\u0E35\u0E48\u0E08\u0E30\u0E17\u0E33\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\
  \u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\
  \u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19."
lastmod: '2024-03-17T21:57:56.585681-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 JavaScript, \u0E44\u0E21\u0E48\u0E21\u0E35\u0E27\u0E34\u0E18\
  \u0E35\u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\
  \u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E15\u0E23\
  \u0E07, \u0E41\u0E15\u0E48\u0E21\u0E31\u0E19\u0E04\u0E48\u0E2D\u0E19\u0E02\u0E49\
  \u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E17\u0E35\u0E48\u0E08\u0E30\u0E17\u0E33\u0E01\
  \u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E1E\u0E37\u0E49\u0E19\
  \u0E10\u0E32\u0E19\n"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
ใน JavaScript, ไม่มีวิธีในตัวเพื่อทำให้สตริงเป็นตัวพิมพ์ใหญ่โดยตรง, แต่มันค่อนข้างง่ายที่จะทำการดำเนินการนี้โดยใช้วิธีการจัดการสตริงพื้นฐาน

### การใช้ JavaScript มาตรฐาน
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // ผลลัพธ์: "Hello world"
```

### รุ่น ES6
ด้วย template literals ของ ES6, ฟังก์ชันนี้สามารถเขียนได้อย่างกระชับมากขึ้น:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // ผลลัพธ์: "Hello ES6"
```

### การใช้ Lodash
Lodash เป็นไลบรารียูทิลิตี้ของบุคคลที่สามที่ได้รับความนิยมซึ่งเสนอฟังก์ชันหลากหลายเพื่อจัดการและทำงานกับค่าใน JavaScript,รวมถึงสตริง ในการทำให้สตริงเป็นตัวพิมพ์ใหญ่โดยใช้ Lodash:
```javascript
// ก่อนอื่น, ต้องติดตั้ง lodash หากคุณยังไม่ได้ติดตั้ง: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // ผลลัพธ์: "Lodash example"
```
_สังเกตว่า Lodash ไม่เพียงแต่ทำให้ตัวอักษรแรกเป็นตัวพิมพ์ใหญ่ แต่ยังแปลงส่วนที่เหลือของสตริงเป็นตัวพิมพ์เล็กซึ่งแตกต่างเล็กน้อยจากการทำงานของ JavaScript ทั่วไป_

### การใช้ CSS (สำหรับวัตถุประสงค์ในการแสดงผลเท่านั้น)
หากวัตถุประสงค์คือการทำให้ข้อความเป็นตัวพิมพ์ใหญ่เพื่อแสดงในส่วนต่อประสานผู้ใช้, สามารถใช้ CSS ได้:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- แสดงเป็น "Hello css" -->
```
**หมายเหตุ:** วิธีนี้เปลี่ยนการแสดงผลของข้อความบนเว็บเพจโดยไม่เปลี่ยนแปลงสตริงเองใน JavaScript.
