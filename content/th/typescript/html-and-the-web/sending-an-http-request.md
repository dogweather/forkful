---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:12.260588-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\
  \u0E23\u0E4C\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\
  \u0E2D\u0E23\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E17\u0E33\u0E40\u0E1E\u0E23\u0E32\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E2B\u0E31\
  \u0E27\u0E43\u0E08\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E42\
  \u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\
  \u0E2D\u0E23\u0E4C\u0E27\u0E34\u0E2A API\u2026"
lastmod: '2024-03-17T21:57:55.941006-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E02\u0E2D\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\
  \u0E23\u0E4C\u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\
  \u0E2D\u0E23\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E17\u0E33\u0E40\u0E1E\u0E23\u0E32\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E2B\u0E31\
  \u0E27\u0E43\u0E08\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E42\
  \u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E40\u0E0B\
  \u0E2D\u0E23\u0E4C\u0E27\u0E34\u0E2A API \u0E41\u0E25\u0E30\u0E17\u0E23\u0E31\u0E1E\
  \u0E22\u0E32\u0E01\u0E23\u0E23\u0E30\u0E22\u0E30\u0E44\u0E01\u0E25."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
ใน TypeScript คุณจะใช้ Fetch API เป็นหลักในการส่งคำขอ HTTP นี่คือตัวอย่างโค้ดแบบเร่งรัด โดยใช้ `async/await` เพื่อความง่าย:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch error:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

ตัวอย่างผลลัพธ์สำหรับคำขอที่สำเร็จ:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## ศึกษาเพิ่มเติม
คำขอ HTTP เป็นสิ่งที่สำคัญตั้งแต่เริ่มต้นของเว็บ มันคือวิธีที่เบราว์เซอร์และเซิร์ฟเวอร์สื่อสารกัน ก่อนที่จะมี `fetch` มี XMLHttpRequest (XHR) ที่ทำงานนี้แต่รู้สึกเหมือนกับการทำเอกสาร `fetch` ซึ่งเป็นทางเลือกแบบใหม่ เป็นพื้นฐานของ promise, สะอาดกว่า และเป็นส่วนหนึ่งของวัตถุ window ในเบราว์เซอร์สมัยใหม่ส่วนใหญ่

ทางเลือกอื่นๆ ของ `fetch` ใน TypeScript รวมถึงไลบรารีอย่าง Axios ซึ่งมีคุณสมบัติเพิ่มเติมและบางครั้งง่ายต่อการจัดการ Axios แปลงข้อมูล JSON โดยอัตโนมัติ จัดการการยกเลิกคำขอ และมีการจัดการข้อผิดพลาดที่ดีกว่า

อยู่เบื้องหลัง ไทป์สคริปต์จะคอมไพล์ลงไปเป็นจาวาสคริปต์ เมื่อคุณส่งคำขอ HTTP โดยใช้ `fetch` คุณก็จะใช้ API Fetch ที่ติดตั้งมากับเบราว์เซอร์อยู่แล้ว การตรวจสอบประเภทข้อมูลของไทป์สคริปต์ช่วยเสริมความมั่นคงของโค้ดของคุณ โดยจับข้อผิดพลาดที่เกี่ยวข้องกับประเภทข้อมูลในระหว่างการคอมไพล์

## ดูเพิ่มเติม
- MDN Web Docs เกี่ยวกับ Fetch: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub Repo: https://github.com/axios/axios
- เปรียบเทียบไลบรารีคำขอ HTTP: https://www.npmtrends.com/axios-vs-fetch
