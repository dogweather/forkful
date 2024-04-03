---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:54.051015-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 TypeScript,\
  \ \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E2B\u0E21\u0E32\u0E22 backticks `` ` `` \u0E41\u0E25\u0E30 syntax `${expression}`."
lastmod: '2024-03-17T21:57:55.929637-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E17\u0E23\u0E01\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E43\u0E19 TypeScript, \u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E40\u0E04\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22 backticks `` ` `` \u0E41\u0E25\
  \u0E30 syntax `${expression}`."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
เพื่อแทรกสตริงใน TypeScript, คุณใช้เครื่องหมาย backticks `` ` `` และ syntax `${expression}`:

```TypeScript
let user = 'Charlie';
let age = 27;

// การแทรกสตริง
let greeting = `Hi, I'm ${user} and I'm ${age} years old.`;

console.log(greeting);  // ผลลัพธ์: Hi, I'm Charlie and I'm 27 years old.
```

## ดำดิ่งลงไป:
การแทรกสตริงไม่ได้มีอยู่เฉพาะใน TypeScript; แต่ยังมีใน JavaScript ตั้งแต่ ES6 และหลายภาษาอื่นๆ ก่อนหน้านี้, เราใช้วิธีเชื่อมต่อสตริงด้วยเครื่องหมาย `+`, ซึ่งดูเหมือนนี้:

```TypeScript
let greeting = 'Hi, I\'m ' + user + ' and I\'m ' + age + ' years old.';
```

วิธีการ `+` ใช้ได้, แต่มันยุ่งยากและอ่านยากขึ้น, เฉพาะอย่างยิ่งเมื่อมีตัวแปรหลายตัว. ด้วยการแทรก, โครงสร้างเทมเพลตจะสะอาดขึ้นและข้อผิดพลาดง่ายต่อการหลีกเลี่ยง

มันทำงานอย่างไรภายใต้ฮูด? สตริงที่ถูกแทรกคือ "syntactic sugar" - แบบที่ง่ายขึ้นในการใช้งานคุณสมบัติที่ซับซ้อนกว่าที่เรียกว่า "template literals". เมื่อคอมไพล์, การแทรกที่เป็นมิตรและอ่านง่ายของคุณจะถูกแปลงไปเป็นรูปแบบที่เครื่องมือ JavaScript สามารถเข้าใจได้, บ่อยครั้งเกี่ยวข้องกับการใช้การเชื่อมต่อหรือวิธีการจัดการสตริงอื่นๆ

ทางเลือกในการแทรกนอกจากนี้อาจเป็นการใช้ฟังก์ชันเทมเพลตหรือไลบรารี่, แต่สำหรับกรณีส่วนใหญ่, การแทรกด้วย backticks เป็นเครื่องมือที่สะดวกที่สุดสำหรับงานนี้

## ดูเพิ่มเติมที่:
- [เครือข่ายนักพัฒนาของ Mozilla เกี่ยวกับ Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [เอกสารการใช้งาน TypeScript](https://www.typescriptlang.org/docs/)
- [คุณสมบัติและไวยากรณ์ของ ES6](http://es6-features.org/#StringInterpolation)
