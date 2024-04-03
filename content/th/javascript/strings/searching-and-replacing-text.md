---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:25.309782-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 JavaScript,\
  \ `String.prototype.replace()` \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\
  \u0E2D\u0E01\u0E2B\u0E25\u0E31\u0E01 \u0E43\u0E2B\u0E49\u0E1C\u0E48\u0E32\u0E19\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E2B\u0E23\u0E37\u0E2D regex \u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E41\
  \u0E25\u0E30\u0E44\u0E21\u0E48\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01."
lastmod: '2024-03-17T21:57:56.587538-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 JavaScript, `String.prototype.replace()` \u0E04\u0E37\u0E2D\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2B\u0E25\u0E31\u0E01 \u0E43\u0E2B\
  \u0E49\u0E1C\u0E48\u0E32\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E23\u0E37\u0E2D\
  \ regex \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\
  \u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E41\u0E25\u0E30\u0E44\u0E21\u0E48\u0E22\
  \u0E38\u0E48\u0E07\u0E22\u0E32\u0E01."
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
ใน JavaScript, `String.prototype.replace()` คือตัวเลือกหลัก ให้ผ่านสตริงหรือ regex และการแทนที่ นี่คือวิธีที่รวดเร็วและไม่ยุ่งยาก:

```javascript
let str = "I love to code in JavaScript!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // ผลลัพธ์: I love to code in TypeScript!
```

ตอนนี้, ด้วย regex สำหรับการแทนที่แบบทั่วโลก:

```javascript
let story = "The quick brown fox jumps over the lazy dog. The fox is clever.";
let newStory = story.replace(/fox/g, "cat");
console.log(newStory); // ผลลัพธ์: The quick brown cat jumps over the lazy dog. The cat is clever.
```

## การศึกษาลึกล้ำ
ย้อนกลับไป, `String.prototype.replace()` ได้รับการนำมาใช้ใน JS ตั้งแต่ยุคแรก ๆ—ยุค Netscape 2 ตอนนี้, ES6 มาพร้อมกับ template literals และ arrow functions, ซึ่งเพิ่มความคมชัดและความสามารถในการอ่านโค้ดที่ใช้ regex

ทางเลือกอื่น? แน่นอน หากคุณกำลังทำงานกับการประมวลผลข้อความขนาดใหญ่ คุณอาจเลือกใช้ Node.js streams หรือใช้ไลบรารีภายนอกเพื่อจัดการกับรูปแบบที่ซับซ้อน ประสิทธิภาพ และผลลัพธ์

เกี่ยวกับการใช้งาน, `replace()` โดยตัวมันเองนั้นง่าย แต่รูปแบบ regex สามารถเป็นได้หลากหลาย เริ่มต้นง่ายๆ, เรียนรู้อักขระพิเศษ (`.` ตรงกับอักขระใด ๆ, `*` สำหรับรูปแบบที่ซ้ำ), และทดสอบด้วยเครื่องมือเช่น regex101

## ดูเพิ่มเติม
- เอกสารการแทนที่ MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 สำหรับทดสอบนิพจน์: https://regex101.com/
- ข้อมูล JavaScript เกี่ยวกับ regex: https://javascript.info/regexp-introduction
