---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:07.352152-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E15\
  \u0E32\u0E21\u0E41\u0E1A\u0E1A\u0E41\u0E1C\u0E19\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E40\u0E1B\
  \u0E47\u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A, \u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\
  \u0E02\u0E23\u0E30\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23,\u2026"
lastmod: '2024-03-17T21:57:56.586663-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E15\
  \u0E32\u0E21\u0E41\u0E1A\u0E1A\u0E41\u0E1C\u0E19\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E40\u0E1B\
  \u0E47\u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A, \u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\
  \u0E02\u0E23\u0E30\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\
  \u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19\
  \u0E01\u0E48\u0E2D\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\
  \u0E25."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
ใช้ `replace()` ร่วมกับ regular expression ธง `g` จะทำการแทนที่ทุกการจับคู่, ไม่ใช่เพียงครั้งแรกเท่านั้น

```javascript
let message = "S0m3 messy-string_with! อักขระที่ไม่ต้องการ.";
let cleanMessage = message.replace(/[0-9_!-]/g, '');
console.log(cleanMessage); // ผลลัพธ์: "Sm messystringwith อักขระที่ไม่ต้องการ."
```

## ขุดลึก
JavaScript ใช้ regular expressions (`RegExp`) สำหรับการจับคู่แบบแผนมานานแล้ว ฟังก์ชัน `replace()` เป็นตัวเลือกหลักสำหรับการแก้ไขสตริงตั้งแต่เริ่มต้นของภาษา มีทางเลือกอื่นๆ เช่น `split()` และ `join()` หรือการใช้ลูปในการสร้างสตริงใหม่ แต่ไม่สะดวกเท่า

นี่คือการอธิบายแยกส่วน:
- ใช้ `replace()` สำหรับการแก้ไขแบบบรรทัดเดียวที่ง่ายดาย 
- Regular expressions มีความสามารถในการจับคู่แบบแผนที่ยอดเยี่ยม
- ระวังประสิทธิภาพของ `RegExp` ในลูปที่แน่นหรือสตริงขนาดใหญ่

คำแนะนำเกี่ยวกับปฏิบัติการสมัยใหม่: แบบแผนเช่น `/[^a-z]/gi` จะลบทุกอย่างที่ไม่ใช่ตัวอักษร, โดยเคารพต่อความไม่สำคัญของตัวพิมพ์เล็กหรือใหญ่ด้วยธง `i` การนำเสนอ template literals ใน ECMAScript 2015 ทำให้การแทนที่ที่ซับซ้อนง่ายขึ้น, เพิ่มความสามารถในการอ่าน

Regular expressions ยังทำให้บางโปรแกรมเมอร์รู้สึกหวาดกลัวเนื่องจากความซับซ้อนของไวยากรณ์ อย่างไรก็ตาม ด้วยการพัฒนาของ JavaScript สมัยใหม่, เครื่องมือและวิธีการเช่นฟังก์ชันการจัดการสตริง (`trim()`, `padStart()`, `padEnd()` ฯลฯ) มีให้เพื่อทำให้งานปกติง่ายขึ้น อาจจะไม่ต้องใช้ regex

## ดูเพิ่มเติม
- [MDN Web Docs เกี่ยวกับ replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExr: เรียนรู้, สร้าง, & ทดสอบ RegEx](https://regexr.com/)
- [การอ้างอิง RegExp ของ JavaScript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
