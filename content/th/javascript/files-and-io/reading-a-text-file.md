---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:20.236488-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 .txt \u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\
  \u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25: \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32 config, logs,\u2026"
lastmod: '2024-03-17T21:57:56.619607-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 .txt \u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\
  \u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25: \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32 config, logs,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## อะไร & ทำไม?
การอ่านไฟล์ข้อความหมายถึงการดึงข้อมูลจากเอกสาร .txt เข้าไปในโปรแกรมของคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อเข้าถึงและจัดการข้อมูล: การตั้งค่า config, logs, การส่งออก, และอื่นๆ อย่างง่ายดาย

## วิธีการ:
นี่คือวิธีที่คุณจะทำการอ่านไฟล์ข้อความใน JavaScript ปัจจุบัน:

**ใช้ Node.js กับ Promises (Async/Await)**:

```javascript
const fs = require('fs').promises;

async function readFile(filePath) {
  try {
    const data = await fs.readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error('เกิดข้อผิดพลาดในการพยายามอ่านไฟล์:', error);
  }
}

readFile('example.txt');
```

ผลลัพธ์ตัวอย่าง (เนื้อหาของ `example.txt`):

```
สวัสดี, นี่คือไฟล์ข้อความ!
```

**ใช้ fetch API ในเบราว์เซอร์**:

```javascript
async function fetchTextFile(fileUrl) {
  try {
    const response = await fetch(fileUrl);
    const text = await response.text();
    console.log(text);
  } catch (error) {
    console.error('อุ้ย, เกิดบางอย่างไม่ถูกต้องในการดึงไฟล์:', error);
  }
}

fetchTextFile('example.txt');
```

## ขุดลึก
เดิมที, การอ่านไฟล์ใน JavaScript ส่วนใหญ่เป็นเรื่องของฝั่งเซิร์ฟเวอร์, ที่จัดการโดย Node.js ตามที่ JS ลุกลามเข้าไปในเบราว์เซอร์ด้วย HTML5, APIs เช่น `FileReader` และ `fetch` ได้ปรากฏขึ้น, ทำให้การอ่านไฟล์ฝั่งไคลเอนต์เป็นไปได้อย่างง่ายดาย โดยไม่ต้องเหนื่อย

มีทางเลือกอื่นหรือไม่? โอ้, มีอีกหลายทาง เช่น Streams สามารถจัดการไฟล์ขนาดใหญ่โดยไม่จำเป็นต้องใช้หน่วยความจำมาก Workers ป้องกันไม่ให้ UI หยุดทำงาน Libraries ทำให้งานที่ซับซ้อนง่ายขึ้น แต่ละอย่างมีที่มาที่ไปของมัน

ภายใต้หลังคา, การอ่านไฟล์อาจเกี่ยวข้องกับการจัดการบัฟเฟอร์, การเข้ารหัสตัวละคร (UTF-8 เป็นต้น), และการจัดการข้อผิดพลาด ต้องระมัดระวังเรื่องความปลอดภัยด้วย; เบราว์เซอร์จำกัดการเข้าถึงไฟล์ด้วยเหตุผลที่ดี

## ดูเพิ่มเติม
ขยายการเรียนรู้ของคุณด้วยทรัพยากรเหล่านี้:

- เอกสาร API FileReader ของ MDN: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- เอกสารระบบไฟล์ของ Node.js: [Node.js fs](https://nodejs.org/api/fs.html)
- Stream API สำหรับไฟล์ขนาดใหญ่: [Node.js stream](https://nodejs.org/api/stream.html)
- การเข้าใจ fetch API: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
