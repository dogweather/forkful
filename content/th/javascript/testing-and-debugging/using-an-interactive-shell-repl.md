---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:43.048901-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Node.js \u0E21\u0E32\u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A REPL \u0E17\u0E35\u0E48\u0E40\u0E02\u0E49\
  \u0E32\u0E16\u0E36\u0E07\u0E44\u0E14\u0E49\u0E1C\u0E48\u0E32\u0E19\u0E17\u0E32\u0E07\
  \u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\u0E2D\u0E25 \u0E40\u0E1B\u0E34\u0E14\
  \u0E21\u0E31\u0E19\u0E02\u0E36\u0E49\u0E19 \u0E41\u0E25\u0E30\u0E04\u0E38\u0E13\u0E01\
  \u0E47\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\u0E23\u0E34\
  \u0E48\u0E21\u0E44\u0E14\u0E49\u0E40\u0E25\u0E22 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
lastmod: '2024-03-17T21:57:56.603099-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A REPL\
  \ \u0E17\u0E35\u0E48\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E44\u0E14\u0E49\
  \u0E1C\u0E48\u0E32\u0E19\u0E17\u0E32\u0E07\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\
  \u0E19\u0E2D\u0E25 \u0E40\u0E1B\u0E34\u0E14\u0E21\u0E31\u0E19\u0E02\u0E36\u0E49\u0E19\
  \ \u0E41\u0E25\u0E30\u0E04\u0E38\u0E13\u0E01\u0E47\u0E1E\u0E23\u0E49\u0E2D\u0E21\
  \u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\u0E23\u0E34\u0E48\u0E21\u0E44\u0E14\u0E49\u0E40\
  \u0E25\u0E22 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
Node.js มาพร้อมกับ REPL ที่เข้าถึงได้ผ่านทางเทอร์มินอล เปิดมันขึ้น และคุณก็พร้อมที่จะเริ่มได้เลย นี่คือตัวอย่าง:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

ตรงไปตรงมา, ใช่ไหม? กำหนดตัวแปร ฟังก์ชัน หรือรันลูป เมื่อเสร็จแล้ว `.exit` จะพาคุณกลับไปยังโลกแห่งความจริง

## การดำดิ่งลึก
REPLs มีมาตั้งแต่ปี 1960 – LISP เป็นผู้บุกเบิกแนวคิดนี้ ความคิด: ให้ข้อเสนอแนะทันทีกับโปรแกรมเมอร์ ทางเลือกอื่น? นอกจาก Node.js REPL ยังมีคอนโซลที่ใช้งานบนเบราว์เซอร์เช่น Chrome DevTools, สนามทดสอบออนไลน์เช่น JSFiddle หรือ IDE แบบเต็มรูปแบบเช่น VSCode ที่มีสนามเด็กเล่นแบบโต้ตอบ

ใต้ฝาเครื่อง, ขั้นตอนการทำงานของ REPL โดยปกติจะ:
1. อ่านข้อมูลนำเข้า
2. คอมไพล์และรันโค้ด
3. พิมพ์ผลลัพธ์
4. ลูปกลับ

มันเป็นวงจรที่ง่ายแต่มีประสิทธิภาพ ซึ่งได้มีอิทธิพลอย่างมากต่อการเขียนโค้ดแบบโต้ตอบ

## ดูเพิ่มเติม
- [เอกสารแนะนำ REPL ของ Node.js](https://nodejs.org/api/repl.html)
- [Mozilla เสนอแนะการเข้าใจโมดูล JavaScript บน REPLs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
