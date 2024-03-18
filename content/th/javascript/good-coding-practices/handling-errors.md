---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:48.127799-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E17\
  \u0E35\u0E48\u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E21\u0E37\
  \u0E48\u0E2D\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E21\u0E35\
  \u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E21\u0E48\u0E04\
  \u0E32\u0E14\u0E04\u0E34\u0E14\u2026"
lastmod: '2024-03-17T21:57:56.609462-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E17\
  \u0E35\u0E48\u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E40\u0E21\u0E37\
  \u0E48\u0E2D\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E21\u0E35\
  \u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E21\u0E48\u0E04\
  \u0E32\u0E14\u0E04\u0E34\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การจัดการข้อผิดพลาดคือการที่คุณจัดการเมื่อโค้ดของคุณมีปัญหาอย่างไม่คาดคิด มันสำคัญเพราะช่วยให้โปรแกรมของคุณล้มเหลวอย่างมีเกียรติและให้คำแนะนำแก่ผู้ใช้อย่างชัดเจน แทนที่จะแค่ล่มจมลงโดยไม่มีสัญญาณเตือน

## วิธีการ:

นี่คือบล็อก `try-catch` แบบคลาสสิก:

```javascript
try {
  // รหัสที่อาจทำให้เกิดข้อผิดพลาด
  let result = potentiallyRiskyOperation();
  console.log('Success:', result);
} catch (error) {
  // สิ่งที่ควรทำหากมีการโยนข้อผิดพลาด
  console.error('Oops:', error.message);
}
```

ผลลัพธ์ตัวอย่างเมื่อไม่มีข้อผิดพลาดเกิดขึ้น:
```
Success: 42
```

และเมื่อมีข้อผิดพลาด:
```
Oops: Something went wrong
```

สำหรับรหัสแบบอะซิงโครนัส ที่เกี่ยวข้องกับโพรมิส ใช้ `try-catch` ในฟังก์ชัน `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data fetched:', data);
  } catch (error) {
    console.error('Error fetching data:', error.message);
  }
}

fetchData();
```

## พิจารณาลึกซึ้ง

การจัดการข้อผิดพลาดใน JavaScript ได้พัฒนาต่อมา ย้อนกลับไปในวันนั้น (ES3, ประมาณ 1999), เรามีเพียงบล็อก `try-catch` ไม่ยืดหยุ่นมากนัก แต่ก็ทำหน้าที่ได้ดี

ES6 (2015) ได้นำเสนอ Promises และให้เรา `.then()` และ `.catch()`, ช่วยให้เราสามารถจัดการกับข้อผิดพลาดแบบอะซิงโครนัสได้อย่างมีมารยาทมากขึ้น

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data fetched:', data))
  .catch(error => console.error('Error fetching data:', error.message));
```

สำหรับรายละเอียดการนำไปใช้งาน หากมีข้อผิดพลาดถูกโยน, เอ็นจิน JavaScript จะสร้างขอบเขตการโยนข้อผิดพลาด `Error` ที่มีคุณสมบัติที่มีประโยชน์อย่างเช่น `message` และ `stack` คุณยังสามารถสร้างประเภทข้อผิดพลาดที่กำหนดเองได้ โดยการขยายคลาส `Error` – สะดวกสำหรับแอปที่ซับซ้อนขึ้น

ทางเลือกอื่น? คุณอาจเลือกที่จะไม่ใส่ใจการจัดการข้อผิดพลาด (ไม่ใช่ความคิดที่ดี), ใช้คอลแบ็คที่มีพารามิเตอร์ซึ่งข้อผิดพลาดมาก่อน (สวัสดี, สไตล์ Node.js), หรือใช้ไลบรารีและเฟรมเวิร์กที่นำเสนอวิธีที่มีความหรูหรามากขึ้น

## ดูเพิ่มเติม

สำหรับข้อมูลเพิ่มเติมเกี่ยวกับการจัดการข้อผิดพลาด:

- MDN เกี่ยวกับ try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- คู่มือสำหรับ Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- การสร้างและโยนข้อผิดพลาดที่กำหนดเอง: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
