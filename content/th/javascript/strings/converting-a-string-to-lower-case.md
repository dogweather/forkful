---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:53.971510-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 JavaScript,\
  \ \u0E40\u0E23\u0E32\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E40\u0E25\u0E47\
  \u0E01\u0E14\u0E49\u0E27\u0E22\u0E40\u0E21\u0E18\u0E2D\u0E14 `.toLowerCase()` \u0E21\
  \u0E31\u0E19\u0E07\u0E48\u0E32\u0E22\u0E21\u0E32\u0E01."
lastmod: '2024-03-17T21:57:56.589369-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 JavaScript, \u0E40\u0E23\u0E32\u0E41\u0E1B\u0E25\u0E07\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E40\u0E25\u0E47\u0E01\u0E14\u0E49\u0E27\u0E22\u0E40\u0E21\u0E18\u0E2D\
  \u0E14 `.toLowerCase()` \u0E21\u0E31\u0E19\u0E07\u0E48\u0E32\u0E22\u0E21\u0E32\u0E01\
  ."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
ใน JavaScript, เราแปลงสตริงเป็นตัวอักษรเล็กด้วยเมธอด `.toLowerCase()` มันง่ายมาก:

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "hello, world!"
```

เมื่อใช้งาน, ทุกตัวอักษรในสตริงเดิมจะถูกแปลงเป็นตัวอักษรเล็กหากเป็นไปได้:

```javascript
let mixedCase = "jAvAScript ROCKs!";
let lowerCased = mixedCase.toLowerCase();
console.log(lowerCased); // "javascript rocks!"
```

โปรดทราบว่าตัวอักษรที่ไม่มีตัวเล็กจะไม่มีการเปลี่ยนแปลง

## ลงลึก
ในอดีต, การจัดการข้อความหมายความว่าต้องระมัดระวังเกี่ยวกับการเข้ารหัสตัวอักษรและการแปลงแบบด้วยตนเอง แต่ใน JavaScript สมัยใหม่, `.toLowerCase()` ช่วยซ่อนความซับซ้อนเหล่านั้นไว้ ด้านล่าง, มันใช้แมพปิ้ง Unicode เพื่อแปลงตัวอักษร, ดังนั้นมันจึงทำงานได้กับมากกว่าเพียง A-Z

มีวิธีการอื่นๆ ที่มีอยู่ เช่น:
- `toLocaleLowerCase()`: นี้เคารพต่อละแวกท้องถิ่นของผู้ใช้ ทำให้มันเป็นสิ่งสำคัญสำหรับภาษาบางภาษาที่กฎของการใช้ตัวเล็กเป็นแบบบริบทเฉพาะ
- นิพจน์ปกติ: ก่อนหน้า `toLowerCase()`, นักพัฒนาอาจใช้ regex เพื่อทดแทนตัวอักษรตัวใหญ่ด้วยตนเอง

เกี่ยวกับรายละเอียด, จำไว้ `.toLowerCase()` ไม่เปลี่ยนสตริงเดิม (สตริงใน JavaScript ไม่สามารถเปลี่ยนแปลงได้) คุณจะได้รับสตริงใหม่เสมอ มันยังจัดการกับทุกตัวอักษรที่ถูกรู้จักว่าเป็นตัวใหญ่โดยมาตรฐาน Unicode ซึ่งหมายความว่าคุณได้รับการคุ้มครองในทุกภาษาและสคริปต์

## อ่านเพิ่มเติม
- [เอกสาร MDN เกี่ยวกับ toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [มาตรฐาน Unicode สำหรับการใช้ตัวเล็กตัวใหญ่](https://unicode.org/reports/tr21/tr21-5.html)
- [ตัวเล็กและตัวใหญ่โดยมี Locale: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
