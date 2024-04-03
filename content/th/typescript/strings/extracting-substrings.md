---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:51.690851-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 TypeScript,\
  \ \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\u0E31\u0E14\u0E41\
  \u0E25\u0E30\u0E41\u0E1A\u0E48\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E14\u0E49\u0E27\
  \u0E22\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E48\u0E19 `substring()`,\
  \ `slice()`, \u0E41\u0E25\u0E30 `includes()` \u0E02\u0E2D\u0E07 ES6 \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E20\u0E32\u0E22\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07."
lastmod: '2024-03-17T21:57:55.932694-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 TypeScript, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E15\u0E31\u0E14\u0E41\u0E25\u0E30\u0E41\u0E1A\u0E48\u0E07\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E14\u0E49\u0E27\u0E22\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\
  \u0E0A\u0E48\u0E19 `substring()`, `slice()`, \u0E41\u0E25\u0E30 `includes()` \u0E02\
  \u0E2D\u0E07 ES6 \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\
  \u0E19\u0E2B\u0E32\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E20\u0E32\u0E22\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
ใน TypeScript, คุณสามารถตัดและแบ่งสตริงด้วยวิธีการเช่น `substring()`, `slice()`, และ `includes()` ของ ES6 สำหรับการค้นหาข้อความภายในสตริง

```TypeScript
let fullString: string = "Hello, TypeScript enthusiasts!";

// คัดลอกจากอักขระที่ 7 ถึง 18
let substr: string = fullString.substring(7, 18);
console.log(substr); // แสดงผล: TypeScript

// การทำงานเดียวกันแต่ใช้ slice()
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // แสดงผล: TypeScript

// ตรวจสอบว่ามีส่วนย่อยของสตริงหรือไม่
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // แสดงผล: true
```

## ทำความเข้าใจลึกซึ้ง
มีครั้งหนึ่ง, การจัดการสตริงนั้นยุ่งยากมาก—นึกถึงฟังก์ชันสตริงของภาษา C JavaScript และ TypeScript ตอนนี้เสนอวิธีการที่จัดการกับ Unicode, เคารพการเข้ารหัสอักขระ, และทำงานโดยตรงกับออบเจ็กต์สตริง `substring()` และ `slice()` คล้ายกันแต่มีความแตกต่าง: `slice()` สามารถรับดัชนีลบ, คิดย้อนกลับจากท้าย `substring()` ถือว่าเป็นศูนย์ ในสถานการณ์ที่ต้องการประสิทธิภาพ, การเลือกหนึ่งมากกว่าอีกอันอาจมีความสำคัญ, แต่สำหรับการใช้งานประจำวัน, มันค่อนข้างเป็นไปในทิศทางเดียวกัน

```TypeScript
// ใช้ดัชนีลบกับ slice
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // แสดงผล: Hello, Type
```

สำหรับ `includes()`, มันเป็นข้อได้เปรียบในเรื่องของการอ่านมากกว่า `indexOf()` แบบคลาสสิก, ทำให้ความตั้งใจของคุณชัดเจนทันทีที่มองเห็น ไม่ต้องใช้ `if (string.indexOf('some text') !== -1)`; เพียงแค่ `if (string.includes('some text'))` เท่านั้น

## ดูเพิ่มเติม
- คู่มือ TypeScript เกี่ยวกับสตริง, สำหรับข้อมูลเพิ่มเติมเกี่ยวกับการใช้งานชนิด 'string': [TypeScript String](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- MDN Web Docs เกี่ยวกับวิธีการของสตริงใน JavaScript, ที่ใช้ได้กับ TypeScript: [MDN String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- เพื่อการเข้าใจเพิ่มเติมเกี่ยวกับ Unicode และ JavaScript (และด้วยเหตุนี้จึงเป็น TypeScript), ตรวจสอบ [Understanding JavaScript's internal character encoding: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)
