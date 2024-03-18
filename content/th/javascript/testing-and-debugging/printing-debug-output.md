---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:22.698641-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E43\u0E19 JavaScript\
  \ \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23, \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  , \u0E2B\u0E23\u0E37\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\
  \u0E19 \u0E46 \u0E17\u0E35\u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E40\
  \u0E2B\u0E47\u0E19\u0E20\u0E32\u0E1E\u0E23\u0E27\u0E21\u0E27\u0E48\u0E32\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u2026"
lastmod: '2024-03-17T21:57:56.603978-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\
  \u0E1E\u0E18\u0E4C\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E43\u0E19 JavaScript\
  \ \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23, \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  , \u0E2B\u0E23\u0E37\u0E2D\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\
  \u0E19 \u0E46 \u0E17\u0E35\u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E40\
  \u0E2B\u0E47\u0E19\u0E20\u0E32\u0E1E\u0E23\u0E27\u0E21\u0E27\u0E48\u0E32\u0E42\u0E04\
  \u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E17\u0E33\u0E07\u0E32\u0E19\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u2026"
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การพิมพ์ผลลัพธ์การดีบักใน JavaScript คือการแสดงตัวแปร, ข้อผิดพลาด, หรือข้อมูลอื่น ๆ ที่ช่วยให้เห็นภาพรวมว่าโค้ดของคุณทำงานอย่างไร นักพัฒนาทำเช่นนี้เพื่อจับบัก, เข้าใจการทำงานของโค้ด, และตรวจสอบว่าโค้ดทำงานตามที่ต้องการหรือไม่

## วิธีทำ:

Javascript ทำให้การพิมพ์ผลลัพธ์การดีบักง่ายมากด้วยการใช้ `console.log()` นี่คือวิธีการ:

```javascript
console.log('Hello, debug world!');

let number = 42;
console.log('The number is:', number);

function add(a, b) {
  console.log(`Adding ${a} + ${b}`);
  return a + b;
}

let result = add(3, 4);
console.log('Result:', result);
```

ผลลัพธ์ตัวอย่างในคอนโซลบราวเซอร์หรือเทอร์มินัล Node.js จะปรากฏดังนี้:

```
Hello, debug world!
The number is: 42
Adding 3 + 4
Result: 7
```

## ลงลึก

เมธอด `console.log()` มาจาก Console API ซึ่งได้เป็นเพื่อนในการดีบักในเบราว์เซอร์และสภาพแวดล้อม Node.js มานาน แต่ไม่ได้มีเพียงแค่ `log`; คุณยังมี `console.warn()`, `console.error()`, และ `console.info()`, ที่ทำงานแสดงข้อความด้วยระดับความรุนแรงที่ต่างกัน

นานมาแล้ว, นักพัฒนาจะใช้ `alert()` สำหรับการดีบัก แต่นั่นกลายเป็นสิ่งที่น่าเบื่ออย่างรวดเร็วเพราะมันขัดขวางการโต้ตอบของผู้ใช้ด้วยกล่องโต้ตอบที่ปรากฏขึ้น

ยังมี `console.dir()` ที่ให้มุมมองคล้าย JSON ของอ็อบเจกต์, ซึ่งเหมาะสำหรับการตรวจสอบอย่างลึกซึ้ง หากคุณต้องการติดตามเวลาที่ใช้, `console.time()` และ `console.timeEnd()` จะเป็นเพื่อนของคุณ

สำหรับผู้ที่ชื่นชอบผลลัพธ์ที่สะอาด, `console.table()` จะแสดงข้อมูลในรูปแบบตารางอย่างเรียบร้อย และเมื่อคุณไปไกลกว่าการดีบักแบบง่ายๆ และเข้าสู่ดินแดนของประสิทธิภาพ, Console API มีเครื่องมือเพิ่มเติมเช่น `console.trace()` สำหรับข้อมูลสแต็กการเรียก, `console.profile()` สำหรับการโปรไฟล์ประสิทธิภาพ, และอื่นๆ อีก

วิธีที่เมธอด `console` ถูกนำมาใช้อาจแตกต่างกันไปในสภาพแวดล้อม JavaScript ต่างๆ แต่แก่นแท้ยังคงเหมือนเดิม: พวกมันช่วยให้นักพัฒนาเข้าใจสิ่งที่กำลังเกิดขึ้นภายใต้ฝาหน้าได้อย่างรวดเร็วและง่ายดาย

## ดูเพิ่มเติม

- MDN Web Docs บน Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- คู่มือเอกสาร `console` ของ Node.js: https://nodejs.org/api/console.html
- คู่มือการใช้คำสั่ง console: https://getfirebug.com/wiki/index.php/Console_API
