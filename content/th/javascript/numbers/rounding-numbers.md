---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:23.646250-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\
  \u0E29\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E19 JavaScript \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49 `Math.round()`, `Math.ceil()`, \u0E41\u0E25\u0E30 `Math.floor()`."
lastmod: '2024-03-17T21:57:56.596791-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\
  \u0E19 JavaScript \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 `Math.round()`, `Math.ceil()`,\
  \ \u0E41\u0E25\u0E30 `Math.floor()`."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
weight: 13
---

## วิธีการ:
นี่คือวิธีการปัดเศษตัวเลขใน JavaScript โดยใช้ `Math.round()`, `Math.ceil()`, และ `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (เนื่องจาก .567 มากกว่า .5)

console.log(roundedDown); // พิมพ์: 2
console.log(roundedUp);   // พิมพ์: 3
console.log(rounded);     // พิมพ์: 3
```

เพื่อกำหนดจำนวนทศนิยมให้คงที่, ใช้ `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (คืนค่าเป็นสตริง)

console.log(twoDecimals); // พิมพ์: "2.57"
```

แปลงสตริงกลับเป็นตัวเลขด้วยเครื่องหมายบวกหนึ่งตัวหรือ `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // พิมพ์: 2.57
```

## ศึกษาลึกลงไป
การปัดเศษตัวเลขไม่ใช่เรื่องใหม่; มันเก่าแก่เท่ากับตัวเลขเอง ใน JavaScript, `Math.round()` ใช้วิธี "round half up" ในการตัดสิน: ถ้าส่วนทศนิยมเป็น 0.5, จะปัดขึ้นไปยังตัวเลขที่ใกล้เคียงที่สุด

หากต้องการควบคุมมากขึ้น, `toFixed()` อาจเป็นทางออกสำหรับคุณ, แต่จำไว้ว่า, มันคืนค่าเป็นสตริง การแปลงกลับเป็นตัวเลขอาจเป็นขั้นตอนเพิ่มเติมแต่ช่วยให้คุณยังคงทำงานกับประเภทตัวเลขได้

มีทางเลือกอื่นหรือไม่? ไลบรารีเช่น `lodash` นำเสนอ `_.round(number, [precision=0])` เพื่อการควบคุมที่มีนัยสำคัญมากขึ้น หรือบริการ `Intl.NumberFormat` ให้รูปแบบการจัดรูปแบบความแม่นยำสูง ไม่ใช่แค่การปัดเศษเท่านั้น

พูดถึงความแม่นยำ, ระวังถึงปัญหาของจุดลอยตัวใน JavaScript  0.1 + 0.2` ไม่เท่ากับ `0.3` สมบูรณ์เนื่องจากวิธีที่ตัวเลขถูกเก็บ บางครั้ง, การปัดเศษอาจจำเป็นในการแก้ไขข้อผิดพลาดจากจุดลอยตัว

## ดูเพิ่มเติม
- เอกสารการคำนวณของ Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- การปัดเศษทางการเงินด้วย `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- การปัดเศษด้วย `lodash`: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
