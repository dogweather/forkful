---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:39.403554-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 TypeScript,\
  \ \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E31\u0E01\u0E1B\u0E23\
  \u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E1A\u0E25\u0E47\u0E2D\u0E01 `try`,\
  \ `catch`, \u0E41\u0E25\u0E30 `finally`."
lastmod: '2024-03-17T21:57:55.953043-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 TypeScript, \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E21\u0E31\u0E01\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E1A\
  \u0E25\u0E47\u0E2D\u0E01 `try`, `catch`, \u0E41\u0E25\u0E30 `finally`."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
ใน TypeScript, การจัดการกับข้อผิดพลาดมักประกอบด้วยบล็อก `try`, `catch`, และ `finally`

```typescript
function riskyOperation() {
  throw new Error("Something went wrong!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Caught an error:", error.message);
  } finally {
    console.log("This always runs, error or not.");
  }
}

handleErrors();
```

ผลลัพธ์ตัวอย่าง:

```
Caught an error: Something went wrong!
This always runs, error or not.
```

ตัวอย่างการใช้งานแบบ Async กับ promises:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // จำลองข้อผิดพลาด
    reject("Failed miserably");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Caught async error:", error);
  }
}

handleAsyncErrors();
```

ผลลัพธ์ตัวอย่าง:

```
Caught async error: Failed miserably
```

## การเจาะลึก
การจัดการกับข้อผิดพลาดเป็นหัวใจหลักของการเขียนโปรแกรมตั้งแต่เริ่มแรก ใน TypeScript ซึ่งสร้างต่อจาก JavaScript, การจัดการข้อผิดพลาดกลายเป็นเรื่องที่ทนทานยิ่งขึ้นด้วยการนำเสนอ async/await ใน ECMAScript 2017 ก่อนหน้านั้น เรามักพึ่งพาฟังก์ชันย้อนกลับและ promises เพื่อจัดการกับข้อผิดพลาดในโค้ดที่ไม่ใช่แบบซิงโครนัส

อีกทางเลือกหนึ่งสำหรับ `try/catch` ใน TypeScript คือการใช้ขอบเขตข้อผิดพลาดที่ให้มาโดยเฟรมเวิร์กเช่น React สำหรับการจัดการฝั่งเซิร์ฟเวอร์ เราสามารถใช้มิดเดิลแวร์ในแพลตฟอร์มเช่น Express.js เพื่อจัดการข้อผิดพลาดได้อย่างเป็นศูนย์กลาง

ในแง่ของการปฏิบัติ, TypeScript ไม่มีกลไกการจัดการข้อผิดพลาดของตัวเองแต่อาศัยของ JavaScript คลาสข้อผิดพลาดที่กำหนดเองสามารถขยายจากคลาส `Error` เพื่อให้ข้อมูลข้อผิดพลาดที่ละเอียดยิ่งขึ้น

## ดูเพิ่มเติม
- [MDN เกี่ยวกับ try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await บน MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [การใช้ขอบเขตข้อผิดพลาดใน React](https://reactjs.org/docs/error-boundaries.html)
- [การจัดการข้อผิดพลาดบน Express.js](https://expressjs.com/en/guide/error-handling.html)
